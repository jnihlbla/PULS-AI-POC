000100 01  REQU-WZ0111I1.                                                       
000200*                                 DISP TABLE MAINT                        
000300     03 REQU-KDCMD           PIC X.                                       
000400      88 REQU-KDCMD-INGENTING                                             
000500                             VALUE ' '.                                   
000600      88 REQU-KDCMD-DELETE   VALUE 'D'                                    
000700                             'B'.                                         
000800      88 REQU-KDCMD-REPLACE  VALUE 'R'                                    
000900                             'Ä'.                                         
001000      88 REQU-KDCMD-INSERT   VALUE 'I'                                    
001100                             'N'                                          
001200                             'A'.                                         
001300      88 REQU-KDCMD-SELECT   VALUE 'S'                                    
001400                             'V'.                                         
001500      88 REQU-KDCMD-PRINT    VALUE 'P'                                    
001600                             'P'.                                         
001700      88 REQU-KDCMD-COPY     VALUE 'C'                                    
001800                             'K'.                                         
001900*                                 LINE UPDATE COMMAND                     
002000     03 REQU-ADDISPABS       PIC X(50).                                   
002100*                                 ABSTRACT ADDRESS                        
002200     03 REQU-KDCOMTYPE       PIC X(10).                                   
002300*                                 COMMUNICATION METHOD                    
002400     03 REQU-FLCOMMDIR       PIC X.                                       
002500*                                 DIRECTION OF COMMUNICATION              
002600     03 REQU-FLCONN          PIC X.                                       
002700     03 REQU-KVDAGAR-RESEND  PIC 9(2).                                    
002800*                                 NO. OF DAYS TO KEEP FOR RESEND          
002900     03 REQU-API-METADATA.                                                
003000        05 REQU-IDAPI        PIC X(50).                                   
003100*                                 NAME OF THE API                         
003200        05 REQU-IDUSERKEY    PIC X(50).                                   
003300*                                 USER KEY                                
003400        05 REQU-IDPATH-API   PIC X(250).                                  
003500        05 REQU-IDPTYP-API   PIC X(3).                                    
003600        05 REQU-IDPROXY      PIC X(32).                                   
003700*                                 KEY TO AUTHENTICATE WITH VOLVO          
003800*                                 CARS GATEWAY FOR API                    
003900        05 REQU-IDAPPKEY     PIC X(75).                                   
004000*                                 KEY USED TO AUTHENTICATE TO DIF         
004100*                                 F APIS                                  
004200*                                 CAN BE MAPPED TO DIFFERENT HTTP         
004300*                                  HEADERS/QUERY PARM                     
004400*                                 DEPENDING ON THE API SPEC               
004500     03 REQU-MQ-METADATA.                                                 
004600        05 REQU-IDINTEGRATION                                             
004700                             PIC X(10).                                   
004800*                                 INTEGRATION ID                          
004900        05 REQU-IDCONTRACT-MQ                                             
005000                             PIC X(10).                                   
005100*                                 MQ CONTRACT ID                          
005200*                                                                         
005300        05 REQU-IDMQOBJ      PIC X(48).                                   
005400*                                 MQ OBJECT NAME                          
005500        05 REQU-KDMQOBJ      PIC X(12).                                   
005600*                                 MQ OBJECT TYPE                          
005700        05 REQU-KDMQFMT      PIC X(8).                                    
005800*                                 MQ MESSAGE FORMAT                       
005900*                                   MQFMT-NONE       = (SPACES)           
006000*                                   MQFMT-STRING     = MQSTR              
006100        05 REQU-KDDELIM-REC  PIC X(8).                                    
006200*                                 RECORD DELIMITER                        
006300        05 REQU-KVMSGLEN-MAX PIC 9(4).                                    
006400*                                 MAX MESSAGE LENGTH                      
006500*                                 ALSO CHECK KDMSGLEN                     
006600        05 REQU-KDMSGLEN     PIC X(2).                                    
006700*                                 MESSAGE LENGTH UNIT                     
006800*                                 B  - BYTES                              
006900*                                 KB - KILO BYTES                         
007000*                                 MB - MEGA BYTES                         
007100*                                 GB - GIGA BYTES                         
007200        05 REQU-FLMSGSPLIT   PIC X.                                       
007300*                                 SPLIT LONG MESSAGES AT THE END          
007400*                                 OF A RECORD. USED TOGETHER WITH         
007500*                                 FLMSGGROUP. PRIMARILY FOR MQ            
007600*                                 TRANSMISSIONS WHERE MESSAGE LEN         
007700*                                 EXCEEDS MAX MSG LEN                     
007800        05 REQU-FLMSGGROUP   PIC X.                                       
007900*                                 SEND LONG MESSAGES AS MULTIPLE          
008000*                                 SMALL MESSAGES PART OF A GROUP          
008100*                                 USED TOGETHER WITH FLMSGSPLIT           
008200*                                 PRIMARILY FOR MQ TRANSMISSIONS          
008300*                                 WHERE MESSAGE LENGTH EXCEEDS            
008400*                                 MAX MSG LENGTH                          
008500        05 REQU-IDQMGR       PIC X(4).                                    
008600*                                 MQ QUEUE MANAGER NAME                   
008700        05 REQU-KDCCSID      PIC 9(5).                                    
008800*                                 CCSID OF A MESSAGE                      
008900*                                 EX: 278 - SWEDISH EBCDIC                
009000        05 REQU-KDEOF        PIC X(8).                                    
009100*                                 END OF FILE INDICATOR                   
009200        05 REQU-KDLOG-LEVEL  PIC X.                                       
009300*                                 LOGGING LEVEL                           
009400*                                                                         
009500*                                 EX: T - TRACE                           
009600*                                     D - DEBUG                           
009700*                                     E - ERROR                           
009800*                                     W - WARN                            
009900*                                     I - INFO                            
010000*                                 BLANK - NONE                            
010100*** END OF VILMAII-COPY LENGTH= 647 BYTES                                 
