      *****************************************************************         
      * IMPORTER PRICE QUERY                                                    
      *****************************************************************         
       01   MFSPN01.                                                            
           02 MFSPN01L  COMP PIC S9(4)  VALUE +155.                             
           02 MFSPN01Z  COMP PIC S9(4).                                         
           02 FILLER REDEFINES MFSPN01Z.                                        
             03 MFSPN02      PIC X.                                             
             03 MFSPN03      PIC X.                                             
           02 FLD0102   PIC X.                                                  
           02 FLD0104   PIC X.                                                  
           02 FLD0106   PIC X.                                                  
           02 FLD0108   PIC X.                                                  
           02 TEMFSFEL  PIC X(40).                                              
           02 ARTIN     PIC X(9).                                               
           02 DISTIN    PIC X(4).                                               
           02 KUNDIN    PIC X(7).                                               
           02 ODKLIN    PIC X.                                                  
           02 KVANIN    PIC X(6).                                               
           02 ARTUT     PIC X(9).                                               
           02 DISTUT    PIC X(4).                                               
           02 KUNDUT    PIC X(7).                                               
           02 ODKLUT    PIC X.                                                  
           02 KVANUT    PIC X(6).                                               
           02 DESC      PIC X(25).                                              
           02 CURR      PIC X(3).                                               
           02 RETAIL    PIC X(10).                                              
           02 DLRNET    PIC X(10).                                              
           02 DISC      PIC X(5).                                               
      *****************************************************************         
      * IMPORTER PRICE QUERY                                                    
      *****************************************************************         
       01   MFSPN04.                                                            
           02 MFSPN04L  COMP PIC S9(4)  VALUE +52.                              
           02 MFSPN04Z  COMP PIC S9(4).                                         
           02 FILLER REDEFINES MFSPN04Z.                                        
             03 MFSPN05      PIC X.                                             
             03 MFSPN06      PIC X.                                             
           02 FLDPFK    PIC X(8).                                               
           02 MFSPN07   PIC X.                                                  
           02 FLD0102   PIC X.                                                  
           02 MFSPN08   PIC X.                                                  
           02 FLD0104   PIC X.                                                  
           02 FLD0106   PIC X.                                                  
           02 FLD0108   PIC X.                                                  
           02 MFSPN09   PIC X(2).                                               
           02 MFSPN10   PIC X(5).                                               
           02 ARTIN     PIC X(9).                                               
           02 DISTIN    PIC X(4).                                               
           02 KUNDIN    PIC X(7).                                               
           02 ODKLIN    PIC X.                                                  
           02 KVANIN    PIC X(6).                                               
