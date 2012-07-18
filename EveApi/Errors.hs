module EveApi.Errors where

import Prelude -- remove this when splitting into another library

import Data.Text

-- XXX: for dates, use parseTime defaultTimeLocale "%F %T"
data EveApiError = EveApiUserError EveApiUserError
                 | EveApiServerError EveApiServerError
                 | EveApiMiscError EveApiMiscError
    deriving (Show, Eq, Ord)

-- Errors with extra info should carry it here, but that requires parsing error
-- messages.  Until that is implemented, just pass through the entire message.
data EveApiUserError
        = ExpectedZeroBeforeRefOrTransID
--        | WalletExhausted UTCTime -- retry after time
        | WalletExhausted Text
--        | UnexpectedBeforeRefOrTransID Integer Integer
        | UnexpectedBeforeRefOrTransID Text
--        | WeekOfDataAlreadyReturned UTCTime -- retry after time
        | WeekOfDataAlreadyReturned Text
        | InvalidCharID
        | AuthMissing
        | InvalidBeforeRefID
        | InvalidAccountID
        | InvalidAccountKeyRange -- "must be in the range 1000 to 1006"
        | InvalidBeforeTransID
        | InvalidIngeger Text
        | VersionMismatch
        | VersionEscalationNotAllowed
        | InvalidItemID
--        | AssetsAlreadyDownloaded UTCTime -- retry after time
        | AssetsAlreadyDownloaded Text
--        | IndustryJobsAlreadyDownloaded UTCTime -- retry after time
        | IndustryJobsAlreadyDownloaded Text
--        | MarketOrdersAlreadyDownloaded UTCTime -- retry after time
        | MarketOrdersAlreadyDownloaded Text
        | KillLogNotCached
--        | KillLogExhausted UTCTime -- time new kills will be available
        | KillLogExhausted Text
        | UnexpectedBeforeKillID
        | InvalidBeforeKillID
        | BadNameList
        | BadIDList
        | CharNotEnlisted
        | CorpNotEnlisted
        | InvalidIDInList
        | BadEventIDs
--        | RepeatedIDs Integer
        | RepeatedIDs Text
--        | TooManyIDs Integer
        | TooManyIDs Text
        | BadOwnerOrTypeIDs
        | CalendarEventListNotPopulated
        | CalendarEventNotFound
        | CalendarEventAttendeesNotAccessible
        | BadContractID
        | InvalidItemIDs -- nonexistent or not owned by key owner
        | SecurityLevelTooLow
        | CharacterNotOnAccount
        | KeyAuthFailure
        | AuthFailure1
        | AuthFailure2
        | AuthFailureFinal
        | NotAccountant
        | NotAvailableForNPCCorp
        | NotAccountantOrTrader
        | NotDirector
        | AuthFailure3
        | AccountStatusLoginDenied -- e.g. banned, unsubscribed
        | AuthFailureFinal2
        | NotFactoryManager
        | CorpKeyStaleRoles -- owner lost roles needed to use the key
        | AccessMaskPermissionDenied
        | KeyExpired
        | LegacyKeyRefused
    deriving (Show, Eq, Ord)

data EveApiServerError -- Don't know what types these are, so assuming text
--        = InvalidGetID Text
        = InvalidGetID Text
--        | InvalidGetSkillpointsForLevel Text Text
        | InvalidGetSkillpointsForLevel Text
--        | InvalidRace Text
        | InvalidRace Text
--        | InvalidGender Text
        | InvalidGender Text
--        | InvalidBloodline Text
        | InvalidBloodline Text
--        | InvalidAttribute Text
        | InvalidAttribute Text
--        | InvalidReftype Text
        | InvalidReftype Text
--        | AttributeIDHasNullComponents Text
        | AttributeIDHasNullComponents Text
        | CharacterHasNoCorp
--        | InvalidAccountKey Text
        | InvalidAccountKey Text
        | ServerInvalidCharID
        | GetCharacterRolesFailed
        | InvalidCorpID
        | GetUserInfoFailed
        | CSVHeaderRowCountMismatch
        | GetTQCurrentTimeFailed
        | GetStarbaseDetailsFailed
        | DatabaseAccessFailed
        | WebLoginFailed
        | GetCharacterInfoFailed
        | GetCorpInfoFailed
        | GetContractInfoFailed
        | GetMarketOrderInfoFailed
    deriving (Show, Eq, Ord)

data EveApiMiscError
        = WebDatabaseTempDisabled
        | BackendDatabaseTempDisabled
--        | RateLimited Text
        | RateLimited Text
        | BlockedTooManyErrors
        | TestError
    deriving (Show, Eq, Ord)
