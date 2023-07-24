

/* To prevent any potential data loss issues, you should review this script in detail before running it outside the context of the database designer.*/
BEGIN TRANSACTION
SET QUOTED_IDENTIFIER ON
SET ARITHABORT ON
SET NUMERIC_ROUNDABORT OFF
SET CONCAT_NULL_YIELDS_NULL ON
SET ANSI_NULLS ON
SET ANSI_PADDING ON
SET ANSI_WARNINGS ON
COMMIT
BEGIN TRANSACTION
GO
CREATE TABLE dbo.COMPANY
	(
	CompanyID nchar(2) NOT NULL,
	CompanyName nvarchar(25) NOT NULL,
	EmailAddress nvarchar(25) NOT NULL,
	PhoneNo numeric(10, 0) NOT NULL,
	Website nvarchar(25) NOT NULL,
	TypeofAccount nvarchar(25) NOT NULL
	)  ON [PRIMARY]
GO
ALTER TABLE dbo.COMPANY ADD CONSTRAINT
	PK_COMPANY PRIMARY KEY CLUSTERED 
	(
	CompanyID
	) WITH( STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON) ON [PRIMARY]

GO
ALTER TABLE dbo.COMPANY SET (LOCK_ESCALATION = TABLE)
GO
COMMIT
select Has_Perms_By_Name(N'dbo.COMPANY', 'Object', 'ALTER') as ALT_Per, Has_Perms_By_Name(N'dbo.COMPANY', 'Object', 'VIEW DEFINITION') as View_def_Per, Has_Perms_By_Name(N'dbo.COMPANY', 'Object', 'CONTROL') as Contr_Per BEGIN TRANSACTION
GO
CREATE TABLE dbo.CUSTOMER
	(
	CustID nchar(2) NOT NULL,
	CustName nvarchar(25) NOT NULL,
	EmailAddress nvarchar(25) NOT NULL,
	PhoneNo numeric(10, 0) NOT NULL,
	City nvarchar(25) NOT NULL,
	ZipCode numeric(5, 0) NOT NULL,
	State nvarchar(25) NOT NULL,
	CompanyID nchar(2) NOT NULL,
	Refers_CustID nchar(2) NULL
	)  ON [PRIMARY]
GO
ALTER TABLE dbo.CUSTOMER ADD CONSTRAINT
	PK_CUSTOMER PRIMARY KEY CLUSTERED 
	(
	CustID
	) WITH( STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON) ON [PRIMARY]

GO
ALTER TABLE dbo.CUSTOMER ADD CONSTRAINT
	FK_CUSTOMER_COMPANY FOREIGN KEY
	(
	CompanyID
	) REFERENCES dbo.COMPANY
	(
	CompanyID
	) ON UPDATE  NO ACTION 
	 ON DELETE  NO ACTION 
	
GO
ALTER TABLE dbo.CUSTOMER ADD CONSTRAINT
	FK_CUSTOMER_CUSTOMER FOREIGN KEY
	(
	Refers_CustID
	) REFERENCES dbo.CUSTOMER
	(
	CustID
	) ON UPDATE  NO ACTION 
	 ON DELETE  NO ACTION 
	
GO
ALTER TABLE dbo.CUSTOMER SET (LOCK_ESCALATION = TABLE)
GO
COMMIT
select Has_Perms_By_Name(N'dbo.CUSTOMER', 'Object', 'ALTER') as ALT_Per, Has_Perms_By_Name(N'dbo.CUSTOMER', 'Object', 'VIEW DEFINITION') as View_def_Per, Has_Perms_By_Name(N'dbo.CUSTOMER', 'Object', 'CONTROL') as Contr_Per BEGIN TRANSACTION
GO
CREATE TABLE dbo.QUOTE
	(
	QuoteID nchar(2) NOT NULL,
	QuoteDescription nvarchar(25) NOT NULL,
	TotalAmount int NOT NULL,
	DiscountRate int NOT NULL,
	TaxRate int NOT NULL,
	Note nvarchar(50) NULL,
	AcceptionStatus nvarchar(10) NOT NULL,
	CustID nchar(2) NOT NULL
	)  ON [PRIMARY]
GO
ALTER TABLE dbo.QUOTE ADD CONSTRAINT
	PK_QUOTE PRIMARY KEY CLUSTERED 
	(
	QuoteID
	) WITH( STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON) ON [PRIMARY]

GO
ALTER TABLE dbo.QUOTE ADD CONSTRAINT
	FK_QUOTE_CUSTOMER FOREIGN KEY
	(
	CustID
	) REFERENCES dbo.CUSTOMER
	(
	CustID
	) ON UPDATE  NO ACTION 
	 ON DELETE  NO ACTION 
	
GO
ALTER TABLE dbo.QUOTE SET (LOCK_ESCALATION = TABLE)
GO
COMMIT
select Has_Perms_By_Name(N'dbo.QUOTE', 'Object', 'ALTER') as ALT_Per, Has_Perms_By_Name(N'dbo.QUOTE', 'Object', 'VIEW DEFINITION') as View_def_Per, Has_Perms_By_Name(N'dbo.QUOTE', 'Object', 'CONTROL') as Contr_Per BEGIN TRANSACTION
GO
CREATE TABLE dbo.INSTALLATION
	(
	InstallationID nchar(2) NOT NULL,
	QuoteID nchar(2) NOT NULL,
	Address nvarchar(25) NOT NULL,
	InstTime numeric(2, 0) NOT NULL,
	Date date NOT NULL
	)  ON [PRIMARY]
GO
ALTER TABLE dbo.INSTALLATION ADD CONSTRAINT
	PK_INSTALLATION PRIMARY KEY CLUSTERED 
	(
	InstallationID
	) WITH( STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON) ON [PRIMARY]

GO
ALTER TABLE dbo.INSTALLATION ADD CONSTRAINT
	FK_INSTALLATION_QUOTE FOREIGN KEY
	(
	QuoteID
	) REFERENCES dbo.QUOTE
	(
	QuoteID
	) ON UPDATE  NO ACTION 
	 ON DELETE  NO ACTION 
	
GO
ALTER TABLE dbo.INSTALLATION SET (LOCK_ESCALATION = TABLE)
GO
COMMIT
select Has_Perms_By_Name(N'dbo.INSTALLATION', 'Object', 'ALTER') as ALT_Per, Has_Perms_By_Name(N'dbo.INSTALLATION', 'Object', 'VIEW DEFINITION') as View_def_Per, Has_Perms_By_Name(N'dbo.INSTALLATION', 'Object', 'CONTROL') as Contr_Per BEGIN TRANSACTION
GO
CREATE TABLE dbo.INSTALLER
	(
	InstallerID nchar(2) NOT NULL,
	Name nvarchar(25) NOT NULL,
	PhoneNo numeric(10, 0) NOT NULL
	)  ON [PRIMARY]
GO
ALTER TABLE dbo.INSTALLER ADD CONSTRAINT
	PK_INSTALLER PRIMARY KEY CLUSTERED 
	(
	InstallerID
	) WITH( STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON) ON [PRIMARY]

GO
ALTER TABLE dbo.INSTALLER SET (LOCK_ESCALATION = TABLE)
GO
COMMIT
select Has_Perms_By_Name(N'dbo.INSTALLER', 'Object', 'ALTER') as ALT_Per, Has_Perms_By_Name(N'dbo.INSTALLER', 'Object', 'VIEW DEFINITION') as View_def_Per, Has_Perms_By_Name(N'dbo.INSTALLER', 'Object', 'CONTROL') as Contr_Per BEGIN TRANSACTION
GO
CREATE TABLE dbo.DONEBY
	(
	InstallationID nchar(2) NOT NULL,
	InstallerID nchar(2) NOT NULL,
	Date date NOT NULL
	)  ON [PRIMARY]
GO
ALTER TABLE dbo.DONEBY ADD CONSTRAINT
	PK_DONEBY PRIMARY KEY CLUSTERED 
	(
	InstallationID
	) WITH( STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON) ON [PRIMARY]

GO
ALTER TABLE dbo.DONEBY ADD CONSTRAINT
	FK_DONEBY_INSTALLATION FOREIGN KEY
	(
	InstallationID
	) REFERENCES dbo.INSTALLATION
	(
	InstallationID
	) ON UPDATE  NO ACTION 
	 ON DELETE  NO ACTION 
	
GO
ALTER TABLE dbo.DONEBY ADD CONSTRAINT
	FK_DONEBY_INSTALLER FOREIGN KEY
	(
	InstallerID
	) REFERENCES dbo.INSTALLER
	(
	InstallerID
	) ON UPDATE  NO ACTION 
	 ON DELETE  NO ACTION 
	
GO
ALTER TABLE dbo.DONEBY SET (LOCK_ESCALATION = TABLE)
GO
COMMIT
select Has_Perms_By_Name(N'dbo.DONEBY', 'Object', 'ALTER') as ALT_Per, Has_Perms_By_Name(N'dbo.DONEBY', 'Object', 'VIEW DEFINITION') as View_def_Per, Has_Perms_By_Name(N'dbo.DONEBY', 'Object', 'CONTROL') as Contr_Per BEGIN TRANSACTION
GO
CREATE TABLE dbo.UNIT
	(
	UnitID nchar(2) NOT NULL,
	Name nvarchar(25) NOT NULL,
	Color nvarchar(25) NOT NULL,
	InstallationLocation nvarchar(25) NOT NULL
	)  ON [PRIMARY]
GO
ALTER TABLE dbo.UNIT ADD CONSTRAINT
	PK_UNIT PRIMARY KEY CLUSTERED 
	(
	UnitID
	) WITH( STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON) ON [PRIMARY]

GO
ALTER TABLE dbo.UNIT SET (LOCK_ESCALATION = TABLE)
GO
COMMIT
select Has_Perms_By_Name(N'dbo.UNIT', 'Object', 'ALTER') as ALT_Per, Has_Perms_By_Name(N'dbo.UNIT', 'Object', 'VIEW DEFINITION') as View_def_Per, Has_Perms_By_Name(N'dbo.UNIT', 'Object', 'CONTROL') as Contr_Per BEGIN TRANSACTION
GO
CREATE TABLE dbo.[CONTAINS]
	(
	QuoteID nchar(2) NOT NULL,
	UnitID nchar(2) NOT NULL,
	Quantity int NOT NULL,
	Price int NOT NULL
	)  ON [PRIMARY]
GO
ALTER TABLE dbo.[CONTAINS] ADD CONSTRAINT
	FK_CONTAINS_QUOTE FOREIGN KEY
	(
	QuoteID
	) REFERENCES dbo.QUOTE
	(
	QuoteID
	) ON UPDATE  NO ACTION 
	 ON DELETE  NO ACTION 
	
GO
ALTER TABLE dbo.[CONTAINS] ADD CONSTRAINT
	FK_CONTAINS_UNIT FOREIGN KEY
	(
	UnitID
	) REFERENCES dbo.UNIT
	(
	UnitID
	) ON UPDATE  NO ACTION 
	 ON DELETE  NO ACTION 
	
GO
ALTER TABLE dbo.[CONTAINS] SET (LOCK_ESCALATION = TABLE)
GO
COMMIT
select Has_Perms_By_Name(N'dbo.[CONTAINS]', 'Object', 'ALTER') as ALT_Per, Has_Perms_By_Name(N'dbo.[CONTAINS]', 'Object', 'VIEW DEFINITION') as View_def_Per, Has_Perms_By_Name(N'dbo.[CONTAINS]', 'Object', 'CONTROL') as Contr_Per BEGIN TRANSACTION
GO
CREATE TABLE dbo.DESIGNER
	(
	DesignerID nchar(2) NOT NULL,
	Name nvarchar(25) NOT NULL
	)  ON [PRIMARY]
GO
ALTER TABLE dbo.DESIGNER ADD CONSTRAINT
	PK_DESIGNER PRIMARY KEY CLUSTERED 
	(
	DesignerID
	) WITH( STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON) ON [PRIMARY]

GO
ALTER TABLE dbo.DESIGNER SET (LOCK_ESCALATION = TABLE)
GO
COMMIT
select Has_Perms_By_Name(N'dbo.DESIGNER', 'Object', 'ALTER') as ALT_Per, Has_Perms_By_Name(N'dbo.DESIGNER', 'Object', 'VIEW DEFINITION') as View_def_Per, Has_Perms_By_Name(N'dbo.DESIGNER', 'Object', 'CONTROL') as Contr_Per BEGIN TRANSACTION
GO
CREATE TABLE dbo.CONSULTATION
	(
	ConsID nchar(2) NOT NULL,
	Date date NOT NULL,
	Time text NOT NULL,
	Location nvarchar(25) NOT NULL,
	CustNeed nchar(25) NOT NULL,
	CustID nchar(2) NOT NULL,
	DesignerID nchar(2) NOT NULL
	)  ON [PRIMARY]
	 TEXTIMAGE_ON [PRIMARY]
GO
ALTER TABLE dbo.CONSULTATION ADD CONSTRAINT
	PK_CONSULTATION PRIMARY KEY CLUSTERED 
	(
	ConsID
	) WITH( STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON) ON [PRIMARY]

GO
ALTER TABLE dbo.CONSULTATION ADD CONSTRAINT
	FK_CONSULTATION_DESIGNER FOREIGN KEY
	(
	DesignerID
	) REFERENCES dbo.DESIGNER
	(
	DesignerID
	) ON UPDATE  NO ACTION 
	 ON DELETE  NO ACTION 
	
GO
ALTER TABLE dbo.CONSULTATION SET (LOCK_ESCALATION = TABLE)
GO
COMMIT
select Has_Perms_By_Name(N'dbo.CONSULTATION', 'Object', 'ALTER') as ALT_Per, Has_Perms_By_Name(N'dbo.CONSULTATION', 'Object', 'VIEW DEFINITION') as View_def_Per, Has_Perms_By_Name(N'dbo.CONSULTATION', 'Object', 'CONTROL') as Contr_Per 
