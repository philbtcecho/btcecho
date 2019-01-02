get_text <-
  function(data_line,sentiment,texte)
    gsub("ZZ",as.numeric(data_line$percent_change_24h),
         gsub("YY",abs(as.numeric(data_line$percent_change_24h)),
              gsub("XX",data_line$links,
                   sample(as.character(texte$description[texte$sentiment==sentiment]),1, replace = T))))
