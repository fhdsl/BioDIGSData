% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getdata.R
\name{BioDIGS_soil_data}
\alias{BioDIGS_soil_data}
\title{Produce soil testing data in a clean format for use in R.}
\usage{
BioDIGS_soil_data(info = TRUE)
}
\arguments{
\item{info}{if set to TRUE, print important information to the console. This can be disabled with \code{info = FALSE}.}
}
\value{
a \code{data.frame}
}
\description{
Produce soil testing data in a clean format for use in R.
}
\details{
\tabular{lll}{
   \strong{Field Name} \tab  \tab \strong{Description} \cr
   site_id \tab \verb{  } \tab Unique letter and number site name \cr
   full_name \tab \verb{  } \tab Full site name \cr
   As_EPA3051 \tab \verb{  } \tab Arsenic (mg/kg), EPA Method 3051A. Quantities < 3.0 are not detectable. \cr
   Cd_EPA3051 \tab \verb{  } \tab Cadmium (mg/kg), EPA Method 3051A. Quantities < 0.2 are not detectable. \cr
   Cr_EPA3051 \tab \verb{  } \tab Chromium (mg/kg), EPA Method 3051A \cr
   Cu_EPA3051 \tab \verb{  } \tab Copper (mg/kg), EPA Method 3051A \cr
   Ni_EPA3051 \tab \verb{  } \tab Nickel (mg/kg), EPA Method 3051A \cr
   Pb_EPA3051 \tab \verb{  } \tab Lead (mg/kg), EPA Method 3051A \cr
   Zn_EPA3051 \tab \verb{  } \tab Zinc (mg/kg), EPA Method 3051A \cr
   water_pH \tab \verb{  } \tab Water pH \cr
   A-E_Buffer_pH \tab \verb{  } \tab Buffer pH \cr
   OM_by_LOI_pct \tab \verb{  } \tab Organic Matter by Loss on Ignition \cr
   P_Mehlich3 \tab \verb{  } \tab Phosphorus (mg/kg), using the Mehlich 3 soil test extractant \cr
   K_Mehlich3 \tab \verb{  } \tab Potassium (mg/kg), using the Mehlich 3 soil test extractant \cr
   Ca_Mehlich3 \tab \verb{  } \tab Calcium (mg/kg), using the Mehlich 3 soil test extractant \cr
   Mg_Mehlich3 \tab \verb{  } \tab Magnesium (mg/kg), using the Mehlich 3 soil test extractant \cr
   Mn_Mehlich3 \tab \verb{  } \tab Manganese (mg/kg), using the Mehlich 3 soil test extractant \cr
   Zn_Mehlich3 \tab \verb{  } \tab Zinc (mg/kg), using the Mehlich 3 soil test extractant \cr
   Cu_Mehlich3 \tab \verb{  } \tab Copper (mg/kg), using the Mehlich 3 soil test extractant \cr
   Fe_Mehlich3 \tab \verb{  } \tab Iron (mg/kg), using the Mehlich 3 soil test extractant \cr
   B_Mehlich3 \tab \verb{  } \tab Boron (mg/kg), using the Mehlich 3 soil test extractant \cr
   S_Mehlich3 \tab \verb{  } \tab Sulfur (mg/kg), using the Mehlich 3 soil test extractant \cr
   Na_Mehlich3 \tab \verb{  } \tab Sodium (mg/kg), using the Mehlich 3 soil test extractant \cr
   Al_Mehlich3 \tab \verb{  } \tab Aluminum (mg/kg), using the Mehlich 3 soil test extractant \cr
   Est_CEC \tab \verb{  } \tab Cation Exchange Capacity (meq/100g) at pH 7.0 (CEC) \cr
   Base_Sat_pct \tab \verb{  } \tab Base saturation (BS). This represents the percentage of CEC occupied by bases (Ca2+, Mg2+, K+, and Na+). The \%BS increases with increasing soil pH (Figure 5). The availability of Ca2+, Mg2+, and K+ increases with increasing \%BS. \cr
   P_Sat_ratio \tab \verb{  } \tab Phosphorus saturation ratio. This is the ratio between the amount of phosphorus present in the soil and the total capacity of that soil to retain phosphorus. The ability of phosphorus to be bound in the soil is primary a function of iron (Fe) and aluminum (Al) content in that soil. \cr
}
}
\examples{
BioDIGS_soil_data()
}
