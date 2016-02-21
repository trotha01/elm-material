module Material.Page (..) where


type alias Category =
  { title : String
  , pages : List Page
  }


type alias Page =
  { title : String
  }
