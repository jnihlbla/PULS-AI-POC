000100 01  REQU-WF0266I1.                                                       
000200*                                 REQUEST-COPYTEXT PGM WF0266             
000300*                                 SENDING COUNTRY MAINTENANCE             
000400     03 REQU-IDLEGSEL-KEY    PIC X(4).                                    
000500*                                 FAKTURERANDE FÖRETAG TEX VCCS           
000600*                                 LEGAL SELLER IDENTITY                   
000700     03 REQU-IDLANDX3-KEY    PIC X(3).                                    
000800*                                 3-STÄLLIG LANDSBETECKNINGSKOD           
000900*                                 3-LETTER CODE FOR COUNTRY.              
001000     03 REQU-KDSTATUS-KEY    PIC 9(3).                                    
001100*                                 STATUSKOD          KDSTATUS-002         
001200     03 REQU-FLCOMING        PIC X.                                       
001300*                                 ALLMÄN FLAGGA                           
001400*                                 GENERAL FLAG                            
001500     03 REQU-BELAND          PIC X(35).                                   
001600*                                 LANDSBETECKNING                         
001700*                                 NAME OF COUNTRY                         
001800     03 REQU-IDVAT           PIC X(17).                                   
001900*                                 MOMSREGISTRERINGSNUMMER                 
002000*                                 VAT REGISTRATION NUMBER                 
002100     03 REQU-BETEXT-1        PIC X(50).                                   
002200     03 REQU-BETEXT-2        PIC X(50).                                   
002300     03 REQU-BETEXT-3        PIC X(50).                                   
002400     03 REQU-BETEXT-4        PIC X(50).                                   
002500     03 REQU-KDVALISO        PIC X(3).                                    
002600*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
002700*                                 CURRENCY CODE BY ISO-STANDARD.          
002800     03 REQU-DAUPPDAT        PIC X(8).                                    
002900*                                 UPPDATERINGSDATUM  (ÅÅÅÅMMDD)           
003000*                                                                         
003100*                                 UPDATING DATE     (YYYYMMDD)            
003200*                                                                         
003300*** END OF VILMAII-COPY LENGTH= 274 BYTES                                 
