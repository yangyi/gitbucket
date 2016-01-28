package gitbucket.core.controller

import gitbucket.core.account.html
import gitbucket.core.api._
import gitbucket.core.helper
import gitbucket.core.model.GroupMember
import gitbucket.core.service._
import gitbucket.core.ssh.SshUtil
import gitbucket.core.util.ControlUtil._
import gitbucket.core.util.Directory._
import gitbucket.core.util.Implicits._
import gitbucket.core.util.StringUtil._
import gitbucket.core.util._

import io.github.gitbucket.scalatra.forms._
import org.apache.commons.io.FileUtils
import org.eclipse.jgit.api.Git
import org.eclipse.jgit.dircache.DirCache
import org.eclipse.jgit.lib.{FileMode, Constants}
import org.scalatra.i18n.Messages
import slick.dbio.DBIO

import scala.concurrent.ExecutionContext.Implicits.global


class AccountController extends AccountControllerBase
  with AccountService with RepositoryService with ActivityService with WikiService with LabelsService with SshKeyService
  with OneselfAuthenticator with UsersAuthenticator with GroupManagerAuthenticator with ReadableUsersAuthenticator
  with AccessTokenService with WebHookService


trait AccountControllerBase extends AccountManagementControllerBase {
  self: AccountService with RepositoryService with ActivityService with WikiService with LabelsService with SshKeyService
    with OneselfAuthenticator with UsersAuthenticator with GroupManagerAuthenticator with ReadableUsersAuthenticator
    with AccessTokenService with WebHookService =>

  case class AccountNewForm(userName: String, password: String, fullName: String, mailAddress: String,
                            url: Option[String], fileId: Option[String])

  case class AccountEditForm(password: Option[String], fullName: String, mailAddress: String,
                             url: Option[String], fileId: Option[String], clearImage: Boolean)

  case class SshKeyForm(title: String, publicKey: String)

  case class PersonalTokenForm(note: String)

  val newForm = mapping(
    "userName"    -> trim(label("User name"    , text(required, maxlength(100), identifier, uniqueUserName))),
    "password"    -> trim(label("Password"     , text(required, maxlength(20)))),
    "fullName"    -> trim(label("Full Name"    , text(required, maxlength(100)))),
    "mailAddress" -> trim(label("Mail Address" , text(required, maxlength(100), uniqueMailAddress()))),
    "url"         -> trim(label("URL"          , optional(text(maxlength(200))))),
    "fileId"      -> trim(label("File ID"      , optional(text())))
  )(AccountNewForm.apply)

  val editForm = mapping(
    "password"    -> trim(label("Password"     , optional(text(maxlength(20))))),
    "fullName"    -> trim(label("Full Name"    , text(required, maxlength(100)))),
    "mailAddress" -> trim(label("Mail Address" , text(required, maxlength(100), uniqueMailAddress("userName")))),
    "url"         -> trim(label("URL"          , optional(text(maxlength(200))))),
    "fileId"      -> trim(label("File ID"      , optional(text()))),
    "clearImage"  -> trim(label("Clear image"  , boolean()))
  )(AccountEditForm.apply)

  val sshKeyForm = mapping(
    "title"     -> trim(label("Title", text(required, maxlength(100)))),
    "publicKey" -> trim(label("Key"  , text(required, validPublicKey)))
  )(SshKeyForm.apply)

  val personalTokenForm = mapping(
    "note"     -> trim(label("Token", text(required, maxlength(100))))
  )(PersonalTokenForm.apply)

  case class NewGroupForm(groupName: String, url: Option[String], fileId: Option[String], members: String)
  case class EditGroupForm(groupName: String, url: Option[String], fileId: Option[String], members: String, clearImage: Boolean)

  val newGroupForm = mapping(
    "groupName" -> trim(label("Group name" ,text(required, maxlength(100), identifier, uniqueUserName))),
    "url"       -> trim(label("URL"        ,optional(text(maxlength(200))))),
    "fileId"    -> trim(label("File ID"    ,optional(text()))),
    "members"   -> trim(label("Members"    ,text(required, members)))
  )(NewGroupForm.apply)

  val editGroupForm = mapping(
    "groupName"  -> trim(label("Group name"  ,text(required, maxlength(100), identifier))),
    "url"        -> trim(label("URL"         ,optional(text(maxlength(200))))),
    "fileId"     -> trim(label("File ID"     ,optional(text()))),
    "members"    -> trim(label("Members"     ,text(required, members))),
    "clearImage" -> trim(label("Clear image" ,boolean()))
  )(EditGroupForm.apply)

  case class RepositoryCreationForm(owner: String, name: String, description: Option[String], isPrivate: Boolean, createReadme: Boolean)
  case class ForkRepositoryForm(owner: String, name: String)

  val newRepositoryForm = mapping(
    "owner"        -> trim(label("Owner"          , text(required, maxlength(100), identifier, existsAccount))),
    "name"         -> trim(label("Repository name", text(required, maxlength(100), repository, uniqueRepository))),
    "description"  -> trim(label("Description"    , optional(text()))),
    "isPrivate"    -> trim(label("Repository Type", boolean())),
    "createReadme" -> trim(label("Create README"  , boolean()))
  )(RepositoryCreationForm.apply)

  val forkRepositoryForm = mapping(
    "owner" -> trim(label("Repository owner", text(required))),
    "name"  -> trim(label("Repository name",  text(required)))
  )(ForkRepositoryForm.apply)

  case class AccountForm(accountName: String)

  val accountForm = mapping(
    "account" -> trim(label("Group/User name", text(required, validAccountName)))
  )(AccountForm.apply)

  /**
   * Displays user information.
   */
  get("/:userName") {
    val userName = params("userName")
    // TODO [Slick3]Move to Slick3
    getAccountByUserName(userName).map { account =>
      params.getOrElse("tab", "repositories") match {
        // Public Activity
        case "activity" =>
          gitbucket.core.account.html.activity(account,
            if(account.isGroupAccount) Nil else getGroupsByUserName(userName),
            getActivitiesByUser(userName, true))

        // Members
        case "members" if(account.isGroupAccount) => {
          val members = getGroupMembers(account.userName)
          gitbucket.core.account.html.members(account, members.map(_.userName),
            context.loginAccount.exists(x => members.exists { member => member.userName == x.userName && member.isManager }))
        }

        // Repositories
        case _ => {
          val members = getGroupMembers(account.userName)
          gitbucket.core.account.html.repositories(account,
            if(account.isGroupAccount) Nil else getGroupsByUserName(userName),
            getVisibleRepositories(context.loginAccount, context.baseUrl, Some(userName)),
            context.loginAccount.exists(x => members.exists { member => member.userName == x.userName && member.isManager }))
        }
      }
    } getOrElse NotFound
  }

  get("/:userName.atom") {
    val userName = params("userName")
    val action = for {
      activities <- getActivitiesByUser(userName, true)
    } yield {
      contentType = "application/atom+xml; type=feed"
      helper.xml.feed(activities)
    }
    // TODO [Slick3]run action
  }

  get("/:userName/_avatar"){
    val userName = params("userName")
    val action = for {
      account <- getAccountByUserName(userName)
    } yield account.flatMap(_.image).map { image =>
      RawData(FileUtil.getMimeType(image), new java.io.File(getUserUploadDir(userName), image))
    } getOrElse {
      contentType = "image/png"
      Thread.currentThread.getContextClassLoader.getResourceAsStream("noimage.png")
    }
    // TODO [Slick3]run action
  }

  /**
   * https://developer.github.com/v3/users/#get-a-single-user
   */
  get("/api/v3/users/:userName") {
    val userName = params("userName")
    val account = for {
      account <- getAccountByUserName(userName)
    } yield account.map { account =>
      JsonFormat(ApiUser(account))
    } getOrElse NotFound
    // TODO [Slick3]run action
  }

  /**
   * https://developer.github.com/v3/users/#get-the-authenticated-user
   */
  get("/api/v3/user") {
    context.loginAccount.map { account =>
      JsonFormat(ApiUser(account))
    } getOrElse Unauthorized
  }


  get("/:userName/_edit")(oneselfOnly {
    val userName = params("userName")
    val action = for {
      account <- getAccountByUserName(userName)
    } yield account.map { account =>
      html.edit(account, flash.get("info"))
    } getOrElse NotFound
    // TODO [Slick3]run action
  })

  post("/:userName/_edit", editForm)(oneselfOnly { form =>
    val userName = params("userName")
    val action = for {
      account <- getAccountByUserName(userName)
      result  <- account.map { account =>
        updateAccount(account.copy(
          password = form.password.map(sha1).getOrElse(account.password),
          fullName = form.fullName,
          mailAddress = form.mailAddress,
          url = form.url)
        ).map { _ =>
          updateImage(userName, form.fileId, form.clearImage)
          flash += "info" -> "Account information has been updated."
          redirect(s"/${userName}/_edit")
        }
      } getOrElse DBIO.successful(NotFound)
    } yield result
    // TODO [Slick3]run action
  })

  get("/:userName/_delete")(oneselfOnly {
    val userName = params("userName")
    val action = for {
      account <- getAccountByUserName(userName, true)
      result  <- account.map { account =>
        removeUserRelatedData(userName)
        updateAccount(account.copy(isRemoved = true))
      } getOrElse DBIO.successful(0)
    } yield {
      session.invalidate
      redirect("/")
    }
    // TODO [Slick3]run action
  })

  get("/:userName/_ssh")(oneselfOnly {
    val userName = params("userName")
    val account = for {
      account <- getAccountByUserName(userName)
      result  <- account.map { account =>
        getPublicKeys(account.userName).map { sshKeys =>
          html.ssh(account, sshKeys)
        }
      } getOrElse DBIO.successful(NotFound)
    } yield result
    // TODO [Slick3]run action
  })

  post("/:userName/_ssh", sshKeyForm)(oneselfOnly { form =>
    val userName = params("userName")
    val action = for {
      _ <- addPublicKey(userName, form.title, form.publicKey)
    } yield redirect(s"/${userName}/_ssh")
    // TODO [Slick3]run action
  })

  get("/:userName/_ssh/delete/:id")(oneselfOnly {
    val userName = params("userName")
    val sshKeyId = params("id").toInt
    val action = for {
      _ <- deletePublicKey(userName, sshKeyId)
    } yield redirect(s"/${userName}/_ssh")
    // TODO [Slick3]run action
  })

  get("/:userName/_application")(oneselfOnly {
    val userName = params("userName")
    val action = for {
      account <- getAccountByUserName(userName)
      result  <- account.map { account =>
        getAccessTokens(account.userName).map { accessTokens =>
          var tokens = accessTokens
          val generatedToken = flash.get("generatedToken") match {
            case Some((tokenId: Int, token: String)) => {
              val gt = tokens.find(_.accessTokenId == tokenId)
              gt.map { t =>
                tokens = tokens.filterNot(_ == t)
                (t, token)
              }
            }
            case _ => None
          }
          html.application(account, tokens, generatedToken)
        }
      } getOrElse DBIO.successful(NotFound)
    } yield result
    // TODO [Slick3]run action
  })

  post("/:userName/_personalToken", personalTokenForm)(oneselfOnly { form =>
    val userName = params("userName")
    val action = for {
      account <- getAccountByUserName(userName)
      result  <- account.map { _ =>
        val (tokenId, token) = generateAccessToken(userName, form.note)
        flash += "generatedToken" -> (tokenId, token)
      }
    } yield redirect(s"/${userName}/_application")
    // TODO [Slick3]run action
  })

  get("/:userName/_personalToken/delete/:id")(oneselfOnly {
    val userName = params("userName")
    val tokenId = params("id").toInt
    val action = for {
      _ <- deleteAccessToken(userName, tokenId)
    } yield redirect(s"/${userName}/_application")
    // TODO [Slick3]run action
  })

  get("/register"){
    if(context.settings.allowAccountRegistration){
      if(context.loginAccount.isDefined){
        redirect("/")
      } else {
        html.register()
      }
    } else NotFound
  }

  post("/register", newForm){ form =>
    if(context.settings.allowAccountRegistration){
      val action = for {
        _ <- createAccount(form.userName, sha1(form.password), form.fullName, form.mailAddress, false, form.url)
      } yield {
        updateImage(form.userName, form.fileId, false)
        redirect("/signin")
      }
      // TODO [Slick3]run action
    } else NotFound
  }

  get("/groups/new")(usersOnly {
    html.group(None, List(GroupMember("", context.loginAccount.get.userName, true)))
  })

  post("/groups/new", newGroupForm)(usersOnly { form =>
    val action = for {
      _ <- createGroup(form.groupName, form.url)
      _ <- updateGroupMembers(form.groupName, form.members.split(",").map {
        _.split(":") match {
          case Array(userName, isManager) => (userName, isManager.toBoolean)
        }
      }.toList)
    } yield {
      updateImage(form.groupName, form.fileId, false)
      redirect(s"/${form.groupName}")
    }
    // TODO [Slick3]run action
  })

  get("/:groupName/_editgroup")(managersOnly {
    defining(params("groupName")){ groupName =>
      val action = for {
        account <- getAccountByUserName(groupName, true)
        members <- getGroupMembers(groupName)
      } yield html.group(account, members)
      // TODO [Slick3]run action
    }
  })

  get("/:groupName/_deletegroup")(managersOnly {
    // TODO [Slick3]Move to Slick3
    defining(params("groupName")){ groupName =>
      // Remove from GROUP_MEMBER
      updateGroupMembers(groupName, Nil)
      // Remove repositories
      getRepositoryNamesOfUser(groupName).foreach { repositoryName =>
        deleteRepository(groupName, repositoryName)
        FileUtils.deleteDirectory(getRepositoryDir(groupName, repositoryName))
        FileUtils.deleteDirectory(getWikiRepositoryDir(groupName, repositoryName))
        FileUtils.deleteDirectory(getTemporaryDir(groupName, repositoryName))
      }
    }
    redirect("/")
  })

  post("/:groupName/_editgroup", editGroupForm)(managersOnly { form =>
    // TODO [Slick3]Move to Slick3
    defining(params("groupName"), form.members.split(",").map {
      _.split(":") match {
        case Array(userName, isManager) => (userName, isManager.toBoolean)
      }
    }.toList){ case (groupName, members) =>
      getAccountByUserName(groupName, true).map { account =>
        updateGroup(groupName, form.url, false)

        // Update GROUP_MEMBER
        updateGroupMembers(form.groupName, members)
        // Update COLLABORATOR for group repositories
        getRepositoryNamesOfUser(form.groupName).foreach { repositoryName =>
          removeCollaborators(form.groupName, repositoryName)
          members.foreach { case (userName, isManager) =>
            addCollaborator(form.groupName, repositoryName, userName)
          }
        }

        updateImage(form.groupName, form.fileId, form.clearImage)
        redirect(s"/${form.groupName}")

      } getOrElse NotFound
    }
  })

  /**
   * Show the new repository form.
   */
  get("/new")(usersOnly {
    val action = for {
      groups <- getGroupsByUserName(context.loginAccount.get.userName)
    } yield html.newrepo(groups, context.settings.isCreateRepoOptionPublic)
    // TODO [Slick3]run action
  })

  /**
   * Create new repository.
   */
  post("/new", newRepositoryForm)(usersOnly { form =>
    // TODO [Slick3] Move to Slick3
    LockUtil.lock(s"${form.owner}/${form.name}"){
      if(getRepository(form.owner, form.name, context.baseUrl).isEmpty){
        createRepository(form.owner, form.name, form.description, form.isPrivate, form.createReadme)
      }

      // redirect to the repository
      redirect(s"/${form.owner}/${form.name}")
    }
  })

  /**
   * Create user repository
   * https://developer.github.com/v3/repos/#create
   */
  post("/api/v3/user/repos")(usersOnly {
    // TODO [Slick3] Move to Slick3
    val owner = context.loginAccount.get.userName
    (for {
      data <- extractFromJsonBody[CreateARepository] if data.isValid
    } yield {
      LockUtil.lock(s"${owner}/${data.name}") {
        if(getRepository(owner, data.name, context.baseUrl).isEmpty){
          createRepository(owner, data.name, data.description, data.`private`, data.auto_init)
          val repository = getRepository(owner, data.name, context.baseUrl).get
          JsonFormat(ApiRepository(repository, ApiUser(getAccountByUserName(owner).get)))
        } else {
          ApiError(
            "A repository with this name already exists on this account", 
            Some("https://developer.github.com/v3/repos/#create")
          )
        }
      }
    }) getOrElse NotFound
  })

  /**
   * Create group repository
   * https://developer.github.com/v3/repos/#create
   */
  post("/api/v3/orgs/:org/repos")(managersOnly {
    // TODO [Slick3] Move to Slick3
    val groupName = params("org")
    (for {
      data <- extractFromJsonBody[CreateARepository] if data.isValid
    } yield {
      LockUtil.lock(s"${groupName}/${data.name}") {
        if(getRepository(groupName, data.name, context.baseUrl).isEmpty){
          createRepository(groupName, data.name, data.description, data.`private`, data.auto_init)
          val repository = getRepository(groupName, data.name, context.baseUrl).get
          JsonFormat(ApiRepository(repository, ApiUser(getAccountByUserName(groupName).get)))
        } else {
          ApiError(
            "A repository with this name already exists for this group", 
            Some("https://developer.github.com/v3/repos/#create")
          )
        }
      }
    }) getOrElse NotFound
  })

  get("/:owner/:repository/fork")(readableUsersOnly { repository =>
    val loginAccount   = context.loginAccount.get
    val loginUserName  = loginAccount.userName
    val groups         = getGroupsByUserName(loginUserName)

    val action = for {
      groups <- getGroupsByUserName(loginUserName)
      managerPermissions <- DBIO.sequence(groups.map { group =>
        for {
          members <- getGroupMembers(group)
        } yield {
          context.loginAccount.exists(x => members.exists {
            member => member.userName == x.userName && member.isManager
          })
        }
      })
    } yield helper.html.forkrepository(repository, (groups zip managerPermissions).toMap)
    // TODO [Slick3]run action
  })

  post("/:owner/:repository/fork", accountForm)(readableUsersOnly { (form, repository) =>
    val loginAccount  = context.loginAccount.get
    val loginUserName = loginAccount.userName
    val accountName   = form.accountName

    // TODO [Slick3]Moving to Slick3 is hard...
    LockUtil.lock(s"${accountName}/${repository.name}"){
      if(getRepository(accountName, repository.name, baseUrl).isDefined ||
          (accountName != loginUserName && !getGroupsByUserName(loginUserName).contains(accountName))){
        // redirect to the repository if repository already exists
        redirect(s"/${accountName}/${repository.name}")
      } else {
        // Insert to the database at first
        val originUserName = repository.repository.originUserName.getOrElse(repository.owner)
        val originRepositoryName = repository.repository.originRepositoryName.getOrElse(repository.name)

        createRepository(
          repositoryName       = repository.name,
          userName             = accountName,
          description          = repository.repository.description,
          isPrivate            = repository.repository.isPrivate,
          originRepositoryName = Some(originRepositoryName),
          originUserName       = Some(originUserName),
          parentRepositoryName = Some(repository.name),
          parentUserName       = Some(repository.owner)
        )

        // Add collaborators for group repository
        val ownerAccount = getAccountByUserName(accountName).get
        if(ownerAccount.isGroupAccount){
          getGroupMembers(accountName).foreach { member =>
            addCollaborator(accountName, repository.name, member.userName)
          }
        }

        // Insert default labels
        insertDefaultLabels(accountName, repository.name)

        // clone repository actually
        JGitUtil.cloneRepository(
          getRepositoryDir(repository.owner, repository.name),
          getRepositoryDir(accountName, repository.name))

        // Create Wiki repository
        JGitUtil.cloneRepository(
          getWikiRepositoryDir(repository.owner, repository.name),
          getWikiRepositoryDir(accountName, repository.name))

        // Record activity
        recordForkActivity(repository.owner, repository.name, loginUserName, accountName)
        // redirect to the repository
        redirect(s"/${accountName}/${repository.name}")
      }
    }
  })

  private def createRepository(owner: String, name: String, description: Option[String], isPrivate: Boolean, createReadme: Boolean) {
    val ownerAccount  = getAccountByUserName(owner).get
    val loginAccount  = context.loginAccount.get
    val loginUserName = loginAccount.userName

    // TODO [Slick3]Moving to Slick3 is hard...

    // Insert to the database at first
    createRepository(name, owner, description, isPrivate)

    // Add collaborators for group repository
    if(ownerAccount.isGroupAccount){
      getGroupMembers(owner).foreach { member =>
        addCollaborator(owner, name, member.userName)
      }
    }

    // Insert default labels
    insertDefaultLabels(owner, name)

    // Create the actual repository
    val gitdir = getRepositoryDir(owner, name)
    JGitUtil.initRepository(gitdir)

    if(createReadme){
      using(Git.open(gitdir)){ git =>
        val builder  = DirCache.newInCore.builder()
        val inserter = git.getRepository.newObjectInserter()
        val headId   = git.getRepository.resolve(Constants.HEAD + "^{commit}")
        val content  = if(description.nonEmpty){
          name + "\n" +
          "===============\n" +
          "\n" +
          description.get
        } else {
          name + "\n" +
          "===============\n"
        }

        builder.add(JGitUtil.createDirCacheEntry("README.md", FileMode.REGULAR_FILE,
          inserter.insert(Constants.OBJ_BLOB, content.getBytes("UTF-8"))))
        builder.finish()

        JGitUtil.createNewCommit(git, inserter, headId, builder.getDirCache.writeTree(inserter),
          Constants.HEAD, loginAccount.fullName, loginAccount.mailAddress, "Initial commit")
      }
    }

    // Create Wiki repository
    createWikiRepository(loginAccount, owner, name)

    // Record activity
    recordCreateRepositoryActivity(owner, name, loginUserName)
  }

  private def insertDefaultLabels(userName: String, repositoryName: String): DBIO[Unit] = {
    DBIO.seq(
      createLabel(userName, repositoryName, "bug", "fc2929"),
      createLabel(userName, repositoryName, "duplicate", "cccccc"),
      createLabel(userName, repositoryName, "enhancement", "84b6eb"),
      createLabel(userName, repositoryName, "invalid", "e6e6e6"),
      createLabel(userName, repositoryName, "question", "cc317c"),
      createLabel(userName, repositoryName, "wontfix", "ffffff")
    )
  }

  // TODO [Slick3]DB access in validation...
  private def existsAccount: Constraint = new Constraint(){
    override def validate(name: String, value: String, messages: Messages): Option[String] =
      if(getAccountByUserName(value).isEmpty) Some("User or group does not exist.") else None
  }

  private def uniqueRepository: Constraint = new Constraint(){
    override def validate(name: String, value: String, params: Map[String, String], messages: Messages): Option[String] =
      params.get("owner").flatMap { userName =>
        getRepositoryNamesOfUser(userName).find(_ == value).map(_ => "Repository already exists.")
      }
  }

  private def members: Constraint = new Constraint(){
    override def validate(name: String, value: String, messages: Messages): Option[String] = {
      if(value.split(",").exists {
        _.split(":") match { case Array(userName, isManager) => isManager.toBoolean }
      }) None else Some("Must select one manager at least.")
    }
  }

  private def validPublicKey: Constraint = new Constraint(){
    override def validate(name: String, value: String, messages: Messages): Option[String] = SshUtil.str2PublicKey(value) match {
     case Some(_) => None
     case None => Some("Key is invalid.")
    }
  }

  private def validAccountName: Constraint = new Constraint(){
    override def validate(name: String, value: String, messages: Messages): Option[String] = {
      getAccountByUserName(value) match {
        case Some(_) => None
        case None => Some("Invalid Group/User Account.")
      }
    }
  }
}
