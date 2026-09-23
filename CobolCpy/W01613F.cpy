000010*** EDIT ALLOWED                                                          
000100******************************************************************        
000200*                                                                *        
000300*    VCOM APPLICATION PROGRAMMING INTERFACE (API) VERSION 1.0    *        
000400*    VCOM CONVERSATION AND DISTRIBUTION SERVER.                  *        
000500*                                                                *        
000600*    FÖR UTFÖRLIGARE BESKRIVNING SE VCOM API MANUAL  VD-0204     *        
000700*                                                                *        
000800*    ANVÄNDS AV:                                                 *        
000900*                                                                *        
001100*        W0161300        -   "  -     (MOTTAGANDE)               *        
001200*                                                                *        
001300******************************************************************        
001400*                                                                         
001500 01      VCOM.                                                            
001600  02     VCOM-RC             PIC S9(4)   VALUE ZERO COMP SYNC.            
001700  02     VCOM-DISTID         PIC X(34)   VALUE SPACE.                     
001800*                                                                         
001900*                                                                         
002000  02     CONS-AREA.                                                       
002100   03    CONS-SECUR.                                                      
002200    04   CONS-SECUR-LTH      PIC S9(4)   VALUE ZERO COMP SYNC.            
002300    04   CONS-SECUR-DATA     PIC X(99)   VALUE SPACE.                     
002400   03    CONS-TIMEOUT        PIC S9(9)   VALUE ZERO COMP SYNC.            
002500   03    CONS-SENDERTAG      PIC X(20)   VALUE SPACE.                     
002600   03    CONS-PARTNER        PIC X(8)    VALUE SPACE.                     
002700   03    CONS-RCPT.                                                       
002800    04   CONS-RCPT-DISTID    PIC X(34)   VALUE SPACE.                     
002900    04   CONS-RCPT-PARTNER   PIC X(8)    VALUE SPACE.                     
003000    04   CONS-RCPT-RC        PIC S9(4)   VALUE ZERO COMP SYNC.            
003100    04   CONS-RCPT-LTH       PIC S9(4)   VALUE ZERO COMP SYNC.            
003200    04   CONS-RCPT-DATA      PIC X(512)  VALUE SPACE.                     
003300   03    CONS-PRIO           PIC X(1)    VALUE SPACE.                     
003400*                                                                         
003500*                                                                         
003600  02     CONR-AREA.                                                       
003700   03    CONR-SECUR.                                                      
003800    04   CONR-SECUR-LTH      PIC S9(4)   VALUE ZERO COMP SYNC.            
003900    04   CONR-SECUR-DATA     PIC X(99)   VALUE SPACE.                     
004000   03    CONR-TIMEOUT        PIC S9(9)   VALUE ZERO COMP SYNC.            
004100   03    CONR-SENDERTAG      PIC X(20)   VALUE SPACE.                     
004200   03    CONR-EXPEDITER      PIC X(8)    VALUE SPACE.                     
004300   03    CONR-RECTYPE        PIC X(1)    VALUE SPACE.                     
004400*                                                                         
004500*                                                                         
004600  02     SEND-AREA.                                                       
004700   03    SEND-LTH            PIC S9(9)   VALUE ZERO COMP SYNC.            
004800   03    SEND-DATA           PIC X(4096) VALUE SPACE.                     
004900*                                                                         
005000*                                                                         
005100  02     RECV-AREA.                                                       
005200   03    RECV-MAXLTH         PIC S9(9)   VALUE +4096 COMP SYNC.           
005300   03    RECV-ACTLTH         PIC S9(9)   VALUE ZERO  COMP SYNC.           
005400   03    RECV-DATA           PIC X(4096) VALUE SPACE.                     
005500*                                                                         
005600   03    RECV-RCPT           REDEFINES RECV-DATA.                         
005700    04   RECV-RCPT-DISTID    PIC X(34).                                   
005800    04   RECV-RCPT-PARTNER   PIC X(8).                                    
005900    04   RECV-RCPT-RC        PIC S9(4)               COMP SYNC.           
006000    04   RECV-RCPT-LTH       PIC S9(4)               COMP SYNC.           
006100    04   RECV-RCPT-DATA      PIC X(512).                                  
006200    04   FILLER              PIC X(3538).                                 
006300*                                                                         
006400*                                                                         
006500  02     RLSE-AREA.                                                       
006600   03    RLSE-RVALUE         PIC S9(4)   VALUE ZERO  COMP SYNC.           
006700*** END OF VILMAII-COPY LENGTH= 9082 OLD LENGTH= 9082                     
