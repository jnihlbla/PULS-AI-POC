000010*** EDIT ALLOWED                                                          
000100*     SENASTE UPPDATERING         74312      14.26.37.6                   
000200 01      V1206501.                                                        
000300   03    FILLER          PIC   X(16)                                      
000400                         VALUE '** CALL-AREA ** '.                        
000500   03    FCBKOMM         PIC   X(12).                                     
000600*                        KOMMUNIKATIONSAREA                               
000700   03    FCBIND          PIC   X(1).                                      
000800*                        INDIKATOR FOR OPTIONER                           
000900   03    FCBFEEDB        PIC   X(3).                                      
001000*                        SAVEAREA FÖR FEEDBACKADRESS                      
001100   03    FCBKONMO        PIC X(8).                                        
001200*                        NAMN PÅ KONTROLL-MODULEN.                        
001300   03    FCBDD           PIC X(8).                                        
001400*                        DD-NAMN FÖR DIREKTREGISTER                       
001500   03    FCBFUNCT        PIC X(2) VALUE SPACE.                            
001600*                        RO  READ ONLY      RU   READ UPDATE              
001700*                        WU  WRITE UPDATE   WA   WRITE ADD                
001800*                        DE  DELETE         RS   READ SEQUENTIAL          
001900*                        OP  OPEN           CL   CLOSE                    
002000   03    FCBSTKOD        PIC X(2) VALUE SPACE.                            
002100*                        FÖLJANDE STATUSKODER FINNS                       
002200*                            I/O OK                                       
002300*                        AD  WRITE UPDATE UTAN READ UPDATE                
002400*                        AU  TVÅ READ UPDATE I FÖLJD                      
002500*                        AK  FELAKTIG CALL.(DDNAMN,FCBFUNCT,KEY)          
002600*                        DJ DELETE RECORD NOT FOUND                       
002700*                        GB END OF FILE                                   
002800*                        GE  RECORD NOT FOUND                             
002900*** END COPY V1206501   LENGTH=0                                          
