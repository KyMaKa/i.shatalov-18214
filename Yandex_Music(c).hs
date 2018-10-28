import Data.Map

-- 1) самый простой способ создать свой тип данных - обычное перечисление
data MusicBand = Interpol | Blur | CivilDefense | Editors | Brandenburg deriving (Eq, Show, Ord)



-- 2) Через ключевое слово deriving Вы говорите компилятору, чтобы он реализовал за Вас интерфейсы равенства,
--    порядка и вывода на экран.
-- Еще раз для танкистов: eq - значения типа можно сравнивать на равенство,
--                        ord - отношение порядка
--                        show - тип можно выводить на консольный экран



-- 3)Способ создать свой тип посложнее - через структуру с полями.
-- Еще раз напоминаю: СЛЕВА от равенства - конструктор ТИПА, СПРАВА - конструкторы данных.
-- Не путайте никогда их и постарайтесь понять разницу, это действительно важно

data Person = Authorized String String MusicBand | NotAuthorized String String deriving Show

getBand::Person->MusicBand
getBand (Authorized _ _ band) = band

type Song = String

me = NotAuthorized "Ivan" "Shatalov"
sonOfMotherFriend = Authorized "Ivan" "Ivanov" Brandenburg

getRecomendatedSong::Person->Maybe Song
getRecomendatedSong person = getBestSong (getBand person) bestSongs
                             where bestSongs = Data.Map.fromList [(Interpol,"Stay In Touch"), (Editors, "Two Hearted Spider"), (CivilDefense,"Russian Field Of Expirements"),(Brandenburg,"Only You")]
                                                                -- не делайте так, упаси Боже.  Сначала сделайте два списка, а потом напишите zip

getBestSong band songs = Data.Map.lookup band songs




{-
--4) человеческий способ создания типа данных - структура с именованынми полями.
-- Напоминаю, что названия полей - автоматически генерируемые функции, поэтому в одном модуле не может быть структур,
-- у которых название полей совпадают (это прям полнейший языковой провал, ну что поделать)

data Unit = Zergling {
        firstName::String,
        lastName::String,
        favourBand::MusicBand
        }  deriving (Show,Ord,Eq)


data Cruiser = Cruiser {
    attack1::Integer,
    defense1::Integer,
    speed1::Integer
} deriving (Show)


-- НАПОМИНАЮ ПРО НАШУ ПОПЫТКУ НАПИСАТЬ СТАРКРАФТ

-- 5) объявляем интерфейс (хоть он и называется класс). Запись означает - чтобы ТИП, мог быть ТИПОМ юнита
-- (больше капса, но чем скорее Вы это поймете, тем будет лучше для всех), он должен реализовать функцию move с такой сигнатурой


class UnitInterface unit where {
    move::unit->String
}


-- 6) собственно как реализовывать интерфейс для произвольного типа

instance UnitInterface Cruiser where {
    move c = "FLY FOR " ++ show (speed1 c)
}
instance UnitInterface Zergling where {
    move z = "rip grisha =(" ++ show (speed z)
}


-- 7) совсем не человеческий, но способа проще обеспечить полиморфизм в хаскеле нет, поэтому приходится смириться.
-- Тут два конструктора данных с разными названиями и разными параметрами, но они вернут вам юнита
-- Поэтому для расширения приложения и добавления юнитов можно писать новые конструкторы данных и добавлять варианты
-- для функции move

data Unit = Cruiser  Int Int Int | Zergling String deriving Show
move:: Unit-> String
(Cruiser _ _ _ ) = "cruiser"
(Zergling _) = "w"
-}
