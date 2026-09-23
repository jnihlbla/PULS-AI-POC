000100 01  RESP-W60119O1.                                                       
000200*                                 MODCOPYTEXT TILL W60119/W6W119          
000300     03 RESP-IDARTNR         PIC Z(7)9.                                   
000400*                                 ARTIKELNUMMER                           
000500*                                 PART NUMBER                             
000600     03 RESP-KVAVIS          PIC Z(5)9.                                   
000700*                                 AVISERAT ANTAL                          
000800*                                 QUANTITY NOTIFIED                       
000900     03 RESP-BEART           PIC X(25).                                   
001000*                                 ARTIKELBENÄMNING                        
001100*                                 PART DESCRIPTION                        
001200     03 RESP-IDLEVNR         PIC X(5).                                    
001300*                                 LEVERANTÖRNUMMER                        
001400*                                 SUPPLIER NUMBER (VENDOR NUMBER)         
001500     03 RESP-IDFS            PIC X(8).                                    
001600*                                 FÖLJESEDELSNUMMER ENL ODETTE            
001700*                                 ADVICE NOTE NUMBER ODETTE               
001800     03 RESP-TIAVIDAT        PIC 9(6).                                    
001900*                                 AVISERINGSDATUM (YYMMDD)                
002000*                                 ADVICE NOTE DATE                        
002100     03 RESP-FLMAK-ATTR      PIC X(2).                                    
002200*                                 MFS ATTRIBUTFÄLT                        
002300     03 RESP-FLMAK           PIC X.                                       
002400*                                 ALLMÄN FLAGGA                           
002500*                                 GENERAL FLAG                            
002600     03 RESP-FLBACK-ATTR     PIC X(2).                                    
002700*                                 MFS ATTRIBUTFÄLT                        
002800     03 RESP-FLBACK          PIC X.                                       
002900*                                 ALLMÄN FLAGGA                           
003000*                                 GENERAL FLAG                            
003100*** END OF VILMAII-COPY LENGTH= 64 BYTES                                  
