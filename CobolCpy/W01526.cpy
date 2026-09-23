000100 01  W01526.                                                              
000200*                                 LISTA MED WEB LOG DATA                  
000300     03 IDDC                 PIC X(2)                                     
000400                             VALUE SPACES.                                
000500*                                 IDENTIFIERARE LAGER                     
000600*                                 WAREHOUSE IDENTIFIER                    
000700     03 HORIZTAB             PIC X                                        
000800                             VALUE X'05'.                                 
000900*                                 HORIZTAB                                
001000*                                 HORIZTAB                                
001100     03 TIREGDAT             PIC 9(6)                                     
001200                             VALUE ZEROS.                                 
001300*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
001400*                                 REGISTRATION DATE (YYMMDD)              
001500     03 HORIZTAB             PIC X                                        
001600                             VALUE X'05'.                                 
001700*                                 HORIZTAB                                
001800*                                 HORIZTAB                                
001900     03 TIREGTID             PIC 9(6)                                     
002000                             VALUE ZEROS.                                 
002100*                                 REGISTRERINGSTID                        
002200*                                 GENERAL REGISTRATION TIME               
002300     03 HORIZTAB             PIC X                                        
002400                             VALUE X'05'.                                 
002500*                                 HORIZTAB                                
002600*                                 HORIZTAB                                
002700     03 IDUSER               PIC X(8)                                     
002800                             VALUE SPACES.                                
002900*                                 ANVÄNDARENS SÄKERHETS ID                
003000*                                 USER SECURITY-IDENTITY                  
003100     03 HORIZTAB             PIC X                                        
003200                             VALUE X'05'.                                 
003300*                                 HORIZTAB                                
003400*                                 HORIZTAB                                
003500     03 IDLOPNR              PIC Z9                                       
003600                             VALUE ZEROS.                                 
003700*                                 LÖPNUMMER                               
003800*                                 SEQUENCE NUMBER                         
003900     03 HORIZTAB             PIC X                                        
004000                             VALUE X'05'.                                 
004100*                                 HORIZTAB                                
004200*                                 HORIZTAB                                
004300     03 KVMILSEC             PIC Z(5)9                                    
004400                             VALUE ZEROS.                                 
004500*                                 ANTAL MILLISEK FÖR WEBSVAR              
004600*                                 NO OF MILLISEC FOR WEB RESPONDE         
004700     03 HORIZTAB             PIC X                                        
004800                             VALUE X'05'.                                 
004900*                                 HORIZTAB                                
005000*                                 HORIZTAB                                
005100     03 BEWEBSCR             PIC X(50)                                    
005200                             VALUE SPACES.                                
005300*                                 VALD BILD I PULS WEBBEN                 
005400*                                 CHOSEN SCREEN ON PULS WEB               
005500     03 HORIZTAB             PIC X                                        
005600                             VALUE X'05'.                                 
005700*                                 HORIZTAB                                
005800*                                 HORIZTAB                                
005900     03 BEWEBURL             PIC X(50)                                    
006000                             VALUE SPACES.                                
006100*                                 LÄNK PULS WEB SIDA                      
006200*                                 LINK PULS WEBSITE                       
006300     03 HORIZTAB             PIC X                                        
006400                             VALUE X'05'.                                 
006500*                                 HORIZTAB                                
006600*                                 HORIZTAB                                
006700*** END OF VILMAII-COPY LENGTH= 138 BYTES                                 
