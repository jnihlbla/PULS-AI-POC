000100 01  REQU-W60201I1.                                                       
000200*                                 COPYTEXT FOR REQU TILL W6020110         
000300     03 REQU-KDKRSTA-KEY     PIC X.                                       
000400*                                 KONTROLLRAPPORT STATUS                  
000500*                                 INSPECTION REPORT STATUS                
000600     03 REQU-KDPERSTYP-KEY   PIC X.                                       
000700     03 REQU-IDPERSON-KEY    PIC X(3).                                    
000800*                                 PERSONKOD                               
000900*                                 STAFF CODE                              
001000     03 REQU-KDBEHX-KEY      PIC X.                                       
001100*                                 BEHANDLINGSKOD-X                        
001200     03 REQU-IDTYP-KEY       PIC X.                                       
001300     03 REQU-IDARTNR-KEY     PIC X(9).                                    
001400*                                 ARTIKELNUMMER                           
001500*                                 PART NUMBER                             
001600     03 REQU-FLANNULL-KEY    PIC X.                                       
001700*                                 ANNULLATION                             
001800*                                 CANCELLATION                            
001900     03 REQU-IDDC-KEY        PIC X(2).                                    
002000*                                 IDENTIFIERARE LAGER                     
002100*                                 WAREHOUSE IDENTIFIER                    
002200     03 REQU-IDSPRAK         PIC X(2).                                    
002300*                                 2-STÄLLIG ISO SPRÅKKOD                  
002400*                                 2-LETTER ISO LANGUAGE CODE              
002500     03 REQU-IDDC-START      PIC X(2).                                    
002600*                                 IDENTIFIERARE LAGER                     
002700*                                 WAREHOUSE IDENTIFIER                    
002800     03 REQU-TIREGDAT-START  PIC 9(6).                                    
002900*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
003000*                                 REGISTRATION DATE (YYMMDD)              
003100     03 REQU-KDKRSTA-START   PIC X.                                       
003200*                                 KONTROLLRAPPORT STATUS                  
003300*                                 INSPECTION REPORT STATUS                
003400     03 REQU-IDLOPNRM-START  PIC X(9).                                    
003500*                                 LÖPNUMMER MOTTAGNINGSRAPPORT            
003600*                                 (0VVDLLLLK)                             
003700*                                 SERIAL NO RECEIVING REPORT              
003800*                                 (0WWDLLLLC)                             
003900     03 REQU-IDFTG-START     PIC 9(2).                                    
004000*                                 FÖRETAGSID EKONOM REDOVISNING           
004100*                                 COMPANY IDENTITY ACCOUNTING             
004200     03 REQU-IDKR-START      PIC 9(5).                                    
004300*                                 KONTROLLRAPPORT NUMMER                  
004400*                                 INSPECTION REPORT NUMBER                
004500     03 REQU-IDARTNR-START   PIC 9(9).                                    
004600*                                 ARTIKELNUMMER                           
004700*                                 PART NUMBER                             
004800     03 REQU-DAREGDAT-9KOMPL-START                                        
004900                             PIC 9(8).                                    
005000*                                 DATUMETS 9-KOMPLEMENT                   
005100*                                 DATES 9-COMPLEMENT                      
005200     03 REQU-IDLEVNR-START   PIC X(5).                                    
005300*                                 LEVERANTÖRNUMMER                        
005400*                                 SUPPLIER NUMBER (VENDOR NUMBER)         
005500     03 REQU-KVKRKNTR-START  PIC 9.                                       
005600*                                 REKNEVERK ANTAL/KVALITET AVV            
005700*                                 COUNTER QUANTITY/QUALITY DEV            
005800     03 REQU-KVRADER         PIC 9(5).                                    
005900*                                 ANTAL RADER                             
006000*                                 NUMBER OF LINES                         
006100     03 REQU-INPUT           OCCURS 500 TIMES.                            
006200*                                 INDATA FÖR UPPDATERING                  
006300        05 REQU-KDBEHX-UPDATE-LINE                                        
006400                             PIC X.                                       
006500*                                 BEHANDLINGSKOD-X                        
006600        05 REQU-IDKR-LINE    PIC 9(5).                                    
006700*                                 KONTROLLRAPPORT NUMMER                  
006800*                                 INSPECTION REPORT NUMBER                
006900*** END OF VILMAII-COPY LENGTH= 3074 BYTES                                
