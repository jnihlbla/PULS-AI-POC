000100 01  W3714X.                                                              
000200*                                 REPORT OF BINNED CORES AND DEVI         
000300*                                 ATIONS                                  
000400*                                 FOR MAASTRICHT ORGANISATION             
000500*                                                                         
000600*                                                                         
000700     03 IDDC                 PIC X(2).                                    
000800*                                 IDENTIFIERARE LAGER                     
000900*                                 WAREHOUSE IDENTIFIER                    
001000     03 IDARTNR-OBJ          PIC S9(9)           COMP-3.                  
001100*                                 OBJEKTNUMMER                            
001200     03 IDDISTR              PIC S9(5)           COMP-3.                  
001300*                                 DISTRIKTNUMMER                          
001400*                                 DISTRICT NUMBER                         
001500     03 IDKUNDNR             PIC S9(7)           COMP-3.                  
001600*                                 KUNDNUMMER                              
001700*                                 CUSTOMER NO                             
001800     03 IDBYTRAP             PIC S9(7)           COMP-3.                  
001900*                                 RAPPORTNUMMER  BYTES                    
002000*                                 REPORTNUMBER   EXCHANGE                 
002100     03 DAREGDAT-GODK        PIC 9(8).                                    
002200*                                 REGISTRERINGSDATUM (ÅÅÅÅMMDD)           
002300*                                 REGISTRATION DATE (YYYYMMDD)            
002400     03 KVRETUR-URSP         PIC S9(7)           COMP-3.                  
002500*                                 ANTAL OBJEKT RETURER.                   
002600*                                 QUANTITY OBJECT RETURNS                 
002700     03 KVRETUR-GODK         PIC S9(7)           COMP-3.                  
002800*                                 ANTAL GODKÄNDA BYTESOBJEKT              
002900*                                 QUANTITY APPROVED OBJECT                
003000     03 KDBYTREF             PIC X(3).                                    
003100*                                 CENTRAL REFERENS                        
003200*                                 CENTRAL REFERENCE                       
003300     03 FLSKROT              PIC X.                                       
003400*                                 SKROTNINGSMARKERING                     
003500*                                 SCRAPPING FLAG                          
003600     03 IDUSER               PIC X(8).                                    
003700*                                 ANVÄNDARENS SÄKERHETS ID                
003800*                                 USER SECURITY-IDENTITY                  
003900*** END OF VILMAII-COPY LENGTH= 46 BYTES                                  
