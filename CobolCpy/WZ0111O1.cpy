000100 01  RESP-WZ0111O1.                                                       
000200*                                 DISP TABLE MAINT                        
000300     03 RESP-ADDISPABS       PIC X(50).                                   
000400*                                 ABSTRACT ADDRESS                        
000500     03 RESP-KDCOMTYPE       PIC X(10).                                   
000600*                                 COMMUNICATION METHOD                    
000700     03 RESP-FLCOMMDIR       PIC X.                                       
000800*                                 DIRECTION OF COMMUNICATION              
000900     03 RESP-FLCONN          PIC X.                                       
001000     03 RESP-KVDAGAR-RESEND  PIC 9(2).                                    
001100*                                 NO. OF DAYS TO KEEP FOR RESEND          
001200     03 RESP-API-METADATA.                                                
001300        05 RESP-IDAPI        PIC X(50).                                   
001400*                                 NAME OF THE API                         
001500        05 RESP-IDUSERKEY    PIC X(50).                                   
001600*                                 USER KEY                                
001700        05 RESP-IDPATH-API   PIC X(250).                                  
001800        05 RESP-IDPTYP-API   PIC X(3).                                    
001900        05 RESP-IDPROXY      PIC X(32).                                   
002000*                                 KEY TO AUTHENTICATE WITH VOLVO          
002100*                                 CARS GATEWAY FOR API                    
002200        05 RESP-IDAPPKEY     PIC X(75).                                   
002300*                                 KEY USED TO AUTHENTICATE TO DIF         
002400*                                 F APIS                                  
002500*                                 CAN BE MAPPED TO DIFFERENT HTTP         
002600*                                  HEADERS/QUERY PARM                     
002700*                                 DEPENDING ON THE API SPEC               
002800     03 RESP-MQ-METADATA.                                                 
002900        05 RESP-IDINTEGRATION                                             
003000                             PIC X(10).                                   
003100*                                 INTEGRATION ID                          
003200        05 RESP-IDCONTRACT-MQ                                             
003300                             PIC X(10).                                   
003400*                                 MQ CONTRACT ID                          
003500*                                                                         
003600        05 RESP-IDMQOBJ      PIC X(48).                                   
003700*                                 MQ OBJECT NAME                          
003800        05 RESP-KDMQOBJ      PIC X(12).                                   
003900*                                 MQ OBJECT TYPE                          
004000        05 RESP-KDMQFMT      PIC X(8).                                    
004100*                                 MQ MESSAGE FORMAT                       
004200*                                   MQFMT-NONE       = (SPACES)           
004300*                                   MQFMT-STRING     = MQSTR              
004400        05 RESP-KDDELIM-REC  PIC X(8).                                    
004500*                                 RECORD DELIMITER                        
004600        05 RESP-KVMSGLEN-MAX PIC 9(4).                                    
004700*                                 MAX MESSAGE LENGTH                      
004800*                                 ALSO CHECK KDMSGLEN                     
004900        05 RESP-KDMSGLEN     PIC X(2).                                    
005000*                                 MESSAGE LENGTH UNIT                     
005100*                                 B  - BYTES                              
005200*                                 KB - KILO BYTES                         
005300*                                 MB - MEGA BYTES                         
005400*                                 GB - GIGA BYTES                         
005500        05 RESP-FLMSGSPLIT   PIC X.                                       
005600*                                 SPLIT LONG MESSAGES AT THE END          
005700*                                 OF A RECORD. USED TOGETHER WITH         
005800*                                 FLMSGGROUP. PRIMARILY FOR MQ            
005900*                                 TRANSMISSIONS WHERE MESSAGE LEN         
006000*                                 EXCEEDS MAX MSG LEN                     
006100        05 RESP-FLMSGGROUP   PIC X.                                       
006200*                                 SEND LONG MESSAGES AS MULTIPLE          
006300*                                 SMALL MESSAGES PART OF A GROUP          
006400*                                 USED TOGETHER WITH FLMSGSPLIT           
006500*                                 PRIMARILY FOR MQ TRANSMISSIONS          
006600*                                 WHERE MESSAGE LENGTH EXCEEDS            
006700*                                 MAX MSG LENGTH                          
006800        05 RESP-IDQMGR       PIC X(4).                                    
006900*                                 MQ QUEUE MANAGER NAME                   
007000        05 RESP-KDCCSID      PIC 9(5).                                    
007100*                                 CCSID OF A MESSAGE                      
007200*                                 EX: 278 - SWEDISH EBCDIC                
007300        05 RESP-KDEOF        PIC X(8).                                    
007400*                                 END OF FILE INDICATOR                   
007500        05 RESP-KDLOG-LEVEL  PIC X.                                       
007600*                                 LOGGING LEVEL                           
007700*                                                                         
007800*                                 EX: T - TRACE                           
007900*                                     D - DEBUG                           
008000*                                     E - ERROR                           
008100*                                     W - WARN                            
008200*                                     I - INFO                            
008300*                                 BLANK - NONE                            
008400*** END OF VILMAII-COPY LENGTH= 646 BYTES                                 
