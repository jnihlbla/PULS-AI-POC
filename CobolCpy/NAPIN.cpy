000100*** EDIT ALLOWED                                                          
000200 01  NAP-IN.                                                              
000300*        AVTAL PRIS      FRÅN   NAP INKÖP                                 
000400*                        ANV I PGM  W09274                                
000400*                                   W09275                                
000600     03  NAP-HEADER.                                                      
000600         05  NAP-OBJECT-ID      PIC   X(10).                              
000500*                               TRANSACTION NO  AVTALSNR                  
000500*                                           00  44XXXXXXXX                
000500         05  FILLER  REDEFINES NAP-OBJECT-ID.                             
000500             07  NAP-OBJECT-PRE PIC   9(1).                               
000500             07  NAP-OBJECT-LNR PIC   9(6).                               
000500             07  NAP-OBJECT-SUF PIC   9(3).                               
000600         05  NAP-DESCRIPTION    PIC   X(40).                              
000500*                                                                         
000600         05  NAP-PUR-GROUP      PIC   X(03).                              
000500*                               PURCHASING GR   IDINK                     
000600         05  NAP-VENDOR         PIC   X(05).                              
000500*                               BUSIN.PART.NO   IDLEVNR                   
000600         05  NAP-VPER-START     PIC   9(08).                              
000500*                               DATE FROM       YYYYMMDD                  
000600         05  NAP-VPER-END       PIC   9(08).                              
000500*                               DATE TO         YYYYMMDD                  
000600         05  NAP-LOCATION       PIC   X(05).                              
000500*                               BUSIN.PART.NO   = BP2TW                   
000600         05  NAP-CURRENCY       PIC   X(03).                              
000500*                               CURRENCY KEY                              
000600         05  NAP-STATUS         PIC   X(01).                              
000500*                               STATUS   R/L/C  R=NYTT                    
000500*                                               L/C=ANN                   
000500*                                                                         
000600     03  NAP-COMMON-AREA.                                                 
000700         05 NAP-ORDERED-PROD    PIC   X(18).                              
000500*                               PRODUCT NAME     IDARTNR                  
               05  FILLER  REDEFINES NAP-ORDERED-PROD.                          
                   07  FILLER         PIC   X(9).                               
                   07  NAP-IDARTNR    PIC   9(9).                               
000700         05 NAP-DELIV-DAYS      PIC   9(03).                              
000700*                               DELIVERY TIME D  LEDTID DAG               
000700         05 NAP-PARTNER-PROD    PIC   X(40).                              
003800*                               PRODUCT NUMBER OF VENDOR                  
               05 FILLER  REDEFINES NAP-PARTNER-PROD.                           
                   07  FILLER         PIC   X(37).                              
                   07  FLSTART-TEST   PIC   X(1).                               
                   07  FLEND-TEST     PIC   X(1).                               
                   07  FLLARM-TEST    PIC   X(1).                               
000700         05 NAP-TDLINE          PIC   X(132).                             
000700*                               VENDOR TEXT                               
000700         05 NAP-PRICE           PIC   9(15).                              
000700*                               NET PRICE  HELTAL (2 DEC)                 
000700         05 NAP-PRICE-UNIT      PIC   9(05).                              
000700*                                                PRIS PER ?               
000700         05 NAP-UNIT            PIC   X(03).                              
000700*                               PURCHASE ORDER UNIT  ISO-KOD              
000700         05 NAP-ZZCONV          PIC   9(07).                              
000700*                               CONVERSION RULE FOR PACK.UNIT             
000700*                               FÖRPACKNINGSSTORLEK ?                     
000700         05 NAP-MIN-ORDER-QUAN  PIC   9(13)V999.                          
000700*                               QUANTITY IN ORDER UNIT                    
000700*                               MIN ORDER QTY                             
000700         05 NAP-NUMBER-INT      PIC   9(10).                              
000700*                               ITEM NUMBER        RADNR                  
000700         05 NAP-ITM-RELEASED    PIC   X(01).                              
003800*                               ITEM STATUS  X/ /C ?                      
003800*                               X=AKTIV, =ANNULL,C=CHECKED?               
000700         05 NAP-ZIM-MFG-NAME    PIC   X(40).                              
003800*                               MFG NAME                                  
000700         05 NAP-ZIM-MFG-PART-NO PIC   X(54).                              
003800*                               MFG PART NUMBER                           
000700*  LÄNGD = 427                                                            
