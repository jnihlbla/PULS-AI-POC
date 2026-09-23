000100 01  REQU-W60121I1.                                                       
000200*                                 COPYTEXT FÖR REQU                       
000300*                                 W60121I1                                
000400     03 REQU-GROUP.                                                       
000500*                                 LINES                                   
000600        05 REQU-IDARTNR-KEY  PIC X(9).                                    
000700*                                 ARTIKELNUMMER                           
000800*                                 PART NUMBER                             
000900        05 REQU-IDLOPNRM-KEY PIC X(8).                                    
001000*                                 LÖPNUMMER MOTTAGNINGSRAPPORT            
001100*                                 (0VVDLLLLK)                             
001200*                                 SERIAL NO RECEIVING REPORT              
001300*                                 (0WWDLLLLC)                             
001400        05 REQU-IDLEVNR-KEY  PIC X(5).                                    
001500*                                 LEVERANTÖRNUMMER                        
001600*                                 SUPPLIER NUMBER (VENDOR NUMBER)         
001700        05 REQU-IDFS-KEY     PIC X(8).                                    
001800*                                 FÖLJESEDELSNUMMER ENL ODETTE            
001900*                                 ADVICE NOTE NUMBER ODETTE               
002000        05 REQU-IDLBBET-KEY  PIC X(12).                                   
002100*                                 LASTBÄRARBETECKNING                     
002200*                                 TRAILER NUMBER                          
002300        05 REQU-IDDC-KEY     PIC X(2).                                    
002400*                                 IDENTIFIERARE LAGER                     
002500*                                 WAREHOUSE IDENTIFIER                    
002600        05 REQU-ADINLOMR-PRT PIC X(4).                                    
002700*                                 PRINTERPLACERING                        
002800*                                 PLACE OF A PRINTER                      
002900     03 REQU-KVRADER         PIC 9(5).                                    
003000*                                 ANTAL RADER                             
003100*                                 NUMBER OF LINES                         
003200     03 REQU-FLKLAR-TOT-KEY  PIC X.                                       
003300*                                 AVSLUTNINGSMARKERING                    
003400*                                 FINISHED FLAG                           
003500     03 REQU-IDARTNR-START   PIC 9(9).                                    
003600*                                 ARTIKELNUMMER                           
003700*                                 PART NUMBER                             
003800     03 REQU-IDLEVNR-START   PIC X(5).                                    
003900*                                 LEVERANTÖRNUMMER                        
004000*                                 SUPPLIER NUMBER (VENDOR NUMBER)         
004100     03 REQU-IDFS-START      PIC X(8).                                    
004200*                                 FÖLJESEDELSNUMMER ENL ODETTE            
004300*                                 ADVICE NOTE NUMBER ODETTE               
004400     03 REQU-START           PIC X.                                       
004500     03 REQU-FLKLAR-BIL      PIC X.                                       
004600*                                 AVSLUTNINGSMARKERING                    
004700*                                 FINISHED FLAG                           
004800     03 REQU-ADINLOMR        PIC X(4).                                    
004900*                                 INLEVERANSOMRÅDE                        
005000*                                 RECEIVING AREA                          
005100     03 REQU-TELOSSN1        PIC X(39).                                   
005200     03 REQU-TELOSSN2        PIC X(66).                                   
005300     03 REQU-LINE            OCCURS 500 TIMES.                            
005400*                                 LINES                                   
005500        05 REQU-KDCMDVAL-INPUT-LINE                                       
005600                             PIC X(3).                                    
005700*                                 GENERELL KOMMANDOKOD                    
005800*                                 GENERAL COMMAND-CODE                    
005900        05 REQU-IDARTNR-LINE PIC X(8).                                    
006000*                                 ARTIKELNUMMER                           
006100*                                 PART NUMBER                             
006200        05 REQU-IDLEVNR-LINE PIC X(5).                                    
006300*                                 LEVERANTÖRNUMMER                        
006400*                                 SUPPLIER NUMBER (VENDOR NUMBER)         
006500        05 REQU-IDFS-LINE    PIC X(8).                                    
006600*                                 FÖLJESEDELSNUMMER ENL ODETTE            
006700*                                 ADVICE NOTE NUMBER ODETTE               
006800*** END OF VILMAII-COPY LENGTH= 12187 BYTES                               
