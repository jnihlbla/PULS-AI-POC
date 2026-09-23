000100 01  W3O12101.                                                            
000200*                                 COPYTEXT FÖR MOD                        
000300*                                 W3O12101                                
000400     03 IDTRANS              PIC X(4).                                    
000500*                                 BILDNUMMER                              
000600     03 TEMFSFEL             PIC X(40).                                   
000700*                                 MFS FELMEDDELANDE                       
000800     03 IDDISTR-IN-ATTR      PIC X(2).                                    
000900*                                 MFS ATTRIBUTFÄLT                        
001000     03 IDDISTR-IN           PIC Z(3)9.                                   
001100*                                 DISTRIKTNUMMER                          
001200     03 IDDISTR-UT           PIC Z(3)9.                                   
001300*                                 DISTRIKTNUMMER                          
001400     03 KDEXCHA-IN-ATTR      PIC X(2).                                    
001500*                                 MFS ATTRIBUTFÄLT                        
001600     03 KDEXCHA-IN           PIC 9(3).                                    
001700*                                 EXCHANGE ACCOUNT CODE                   
001800     03 KDEXCHA-UT           PIC 9(3).                                    
001900*                                 EXCHANGE ACCOUNT CODE                   
002000     03 IDDISTR-BET-ATTR     PIC X(2).                                    
002100*                                 MFS ATTRIBUTFÄLT                        
002200     03 IDDISTR-BET          PIC Z(4).                                    
002300*                                 BATALANDE DISTRIKT                      
002400     03 FLEXCRET-ATTR        PIC X(2).                                    
002500*                                 MFS ATTRIBUTFÄLT                        
002600     03 FLEXCRET             PIC X.                                       
002700*                                 EXCHANGE RETURN FLAG                    
002800     03 FLEXCREP-ATTR        PIC X(2).                                    
002900*                                 MFS ATTRIBUTFÄLT                        
003000     03 FLEXCREP             PIC X.                                       
003100*                                 EXCHANGE REPORT FLAG                    
003200     03 FLEXCDET-ATTR        PIC X(2).                                    
003300*                                 MFS ATTRIBUTFÄLT                        
003400     03 FLEXCDET             PIC X.                                       
003500*                                 EXCHANGE REPORT FLAG DET                
003600     03 IDMAIL-ATTR          PIC X(2).                                    
003700*                                 MFS ATTRIBUTFÄLT                        
003800     03 IDMAIL               PIC X(60).                                   
003900*                                 MAIL ADRESS                             
004000     03 FLFAKT-ATTR          PIC X(2).                                    
004100*                                 MFS ATTRIBUTFÄLT                        
004200     03 FLFAKT               PIC X.                                       
004300*                                 FAKTURERINGSFLAGGA                      
004400     03 FLRETREM-ATTR        PIC X(2).                                    
004500*                                 MFS ATTRIBUTFÄLT                        
004600     03 FLRETREM             PIC X.                                       
004700*                                 EXCHANGE REMIND FLAG                    
004800     03 KVVECKOR-BYFA-ATTR   PIC X(2).                                    
004900*                                 MFS ATTRIBUTFÄLT                        
005000     03 KVVECKOR-BYFA        PIC Z(3).                                    
005100*                                 ANTAL VECKOR FÖR BYTESFAKTURA           
005200     03 KVVECKOR-BYRE-ATTR   PIC X(2).                                    
005300*                                 MFS ATTRIBUTFÄLT                        
005400     03 KVVECKOR-BYRE        PIC Z(3).                                    
005500*                                 ANTAL VECKOR INNAN RENSING              
005600     03 FGRPS                OCCURS 16 TIMES.                             
005700        05 IDFKNGRP-FOM-ATTR PIC X(2).                                    
005800*                                 MFS ATTRIBUTFÄLT                        
005900        05 IDFKNGRP-FOM      PIC Z(4).                                    
006000*                                 FUNKTIONSGRUPP-FROM                     
006100        05 IDFKNGRP-TOM-ATTR PIC X(2).                                    
006200*                                 MFS ATTRIBUTFÄLT                        
006300        05 IDFKNGRP-TOM      PIC Z(4).                                    
006400*                                 FUNKTIONSGRUPP-TOM                      
006500     03 SUPOINT-BAL          PIC -(7)9.                                   
006600*                                 POINT VALUE                             
006700     03 SUPOINT-RIT          PIC -(7)9.                                   
006800*                                 POINT VALUE                             
006900     03 SUPOINT-PP           PIC -(7)9.                                   
007000*                                 PENDING VALUE                           
007100     03 TEMFSINF             PIC X(55).                                   
007200*                                 INFORMATIONSMEDDELANDE                  
007300*** END OF VILMAII-COPY LENGTH= 426 BYTES                                 
