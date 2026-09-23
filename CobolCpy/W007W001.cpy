000010*** EDIT ALLOWED                                                          
000100 01  W007W001.                                                            
000200*                                *** COPYTEXT JCL-MALL FÖR W007           
000300     03  RAD10.                                                           
000400         05  FILLER              PIC X(2)    VALUE '//'.                  
000500         05  IDJOB               PIC X(8).                                
000600*                                *** JOBBNAMN                             
000700         05  FILLER              PIC X(6)    VALUE ' JOB ('.              
000800         05  KDDEBINFO           PIC X(11).                               
000900*                                *** DEBITERINGSINFO                      
001000         05  FILLER              PIC X(1)    VALUE ','.                   
001100         05  KDJROOM             PIC X(4).                                
001200*                                *** ROOM DESTINATION                     
001300         05  FILLER              PIC X(2)    VALUE '),'.                  
001400         05  FILLER              PIC X(1)    VALUE QUOTE.                 
001500         05  BEPGMNAMN           PIC X(20).                               
001600*                                *** PROGRAMMERS NAME                     
001700         05  FILLER              PIC X(1)    VALUE QUOTE.                 
001800         05  FILLER              PIC X(1)    VALUE ','.                   
001900         05  FILLER              PIC X(23)   VALUE SPACE.                 
002000*                                                                         
002100     03  RAD20.                                                           
002200         05  FILLER              PIC X(2)    VALUE '//'.                  
002300         05  FILLER              PIC X(13)   VALUE SPACE.                 
002400         05  FILLER              PIC X(9)    VALUE 'MSGCLASS='.           
002500         05  KDMSGCLASS          PIC X(1).                                
002600*                                *** MESSAGE CLASS                        
002700         05  FILLER              PIC X(11)   VALUE ',MSGLEVEL=('.         
002800         05  KDMSGLEVEL          PIC X(3).                                
002900*                                *** MESSAGE LEVEL                        
003000         05  FILLER              PIC X(2)    VALUE '),'.                  
003100         05  FILLER              PIC X(39)   VALUE SPACE.                 
003200*                                                                         
003300     03  RAD30.                                                           
003400         05  FILLER              PIC X(2)    VALUE '//'.                  
003500         05  FILLER              PIC X(13)   VALUE SPACE.                 
003600         05  FILLER              PIC X(6)    VALUE 'CLASS='.              
003700         05  KDJCLASS            PIC X(1).                                
003800*                                *** JOB CLASS                            
003900         05  FILLER              PIC X(8)    VALUE ',NOTIFY='.            
004000         05  IDNOTIFY            PIC X(8).                                
004100*                                *** NOTIFY DEST                          
004200         05  FILLER              PIC X(42)   VALUE SPACE.                 
004300*                                                                         
004400     03  RAD40.                                                           
004500         05  FILLER              PIC X(10)   VALUE '/*JOBPARM '.          
004600         05  FILLER              PIC X(5)    VALUE 'TIME='.               
004700         05  KVJTIME             PIC 9(4).                                
004800*                                *** EXEKVERINGSTID                       
004900         05  FILLER              PIC X(7)    VALUE ',LINES='.             
005000         05  KVJLINES            PIC 9(4).                                
005100*                                *** ANTAL 1000 RADER                     
005200         05  FILLER              PIC X(8)    VALUE ',CARDS=0'.            
005300         05  FILLER              PIC X(7)    VALUE ',FORMS='.             
005400         05  KDJFORMS            PIC X(8).                                
005500*                                *** FORMSNR FÖR JOBBET                   
005600         05  FILLER              PIC X(27)   VALUE SPACE.                 
005700*                                                                         
006500     03  RAD47.                                                           
006600         05  FILLER              PIC X(17)                                
006700                                 VALUE '/*JOBPARM LINECT='.               
006800         05  KVJCOUNT            PIC 9(3).                                
006900*                                *** ANTAL RADER/SIDA I SYSOUT            
007000         05  FILLER              PIC X(60)   VALUE SPACE.                 
007100*                                                                         
007200     03  RAD50.                                                           
007300         05  FILLER              PIC X(12)                                
007400                                 VALUE '/*ROUTE XEQ '.                    
007500         05  KDROUTEX            PIC X(8).                                
007600*                                *** EXEKVERINGSMASKIN                    
007700         05  FILLER              PIC X(60)   VALUE SPACE.                 
007800*                                                                         
007900     03  RAD60.                                                           
008000         05  FILLER              PIC X(14)                                
008100                                 VALUE '/*ROUTE PRINT '.                  
008200         05  KDROUTEP            PIC X(8).                                
008300*                                *** PRINT DESTINATION                    
008400         05  FILLER              PIC X(58)   VALUE SPACE.                 
008500*                                                                         
008600     03  RAD70.                                                           
008700         05  FILLER              PIC X(9)                                 
008800                                 VALUE '/*OUTPUT '.                       
008900         05  KDOUTPUT            PIC X(29).                               
009000*                                *** JES2 OUTPUT-KORT                     
009100         05  FILLER              PIC X(42)   VALUE SPACE.                 
009200*                                                                         
009210     03  RAD80.                                                           
009220         05  FILLER              PIC X(23)                                
009230                                 VALUE '//PROD JCLLIB ORDER=(W.'.         
009240         05  IDPROCDD            PIC X(4)    VALUE 'PROD'.                
009250*                                *** DDNAMN FÖR LOKALT PROCLIB            
009260         05  FILLER              PIC X(53)   VALUE '.PROCLIB)'.           
009270*                                                                         
009300*** END COPY W007W001C0  LENGTH=560   OLD LENGTH=                         
