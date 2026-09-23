000010*COMPOPT STDSUB=YES                                                       
000100 ID  DIVISION.                                                            
000200 PROGRAM-ID.    WNOTIFY.                                                  
000300 AUTHOR.        KJELL ANDRE                                               
000400 DATE-WRITTEN.  MARS 1985.                                                
000500     REMARKS.                                                             
000600*    PROGRAMMET SÄNDER GODTYCKLIGT NOTIFY-MEDELANDE TILL                  
000700*    TILL EN GODTYCKLIG TSO-USER.                                         
000800*                                                                         
000900 ENVIRONMENT DIVISION.                                                    
001000                                                                          
001100 DATA DIVISION.                                                           
001200                                                                          
001300     EJECT                                                                
001400 WORKING-STORAGE SECTION.                                                 
001500                                                                          
001501                                                                          
001510*    -- CHECKED BY WY2000                                                 
001600 77   IDPGM                      PIC X(8)    VALUE 'WNOTIFY'.             
001700                                                                          
001800 01  DYNAMISKA-SUBPROGRAM.                                                
001900     03  W009INTR                PIC X(8)    VALUE 'W009INTR'.            
002000     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
002100                                                                          
002200                                                                          
002300 01  KONSTANTER.                                                          
002400     03  OEPPNA                  PIC X       VALUE 'O'.                   
002500     03  SKRIV                   PIC X       VALUE ' '.                   
002600     03  STAENG                  PIC X       VALUE 'C'.                   
002700                                                                          
002800 01  RKOD                        PIC S9(4)  COMP VALUE +16.               
002900                                                                          
003000 01  TID.                                                                 
003100     03  HHMMSS                  PIC 9(6).                                
003200     03  FILLER                  PIC 99.                                  
003300                                                                          
003400 01  MEDD-TID                    PIC 99B99B99.                            
003500 01  FILLER REDEFINES MEDD-TID.                                           
003600     03  FILLER                  PIC XX.                                  
003700     03  KOLON-1                 PIC X.                                   
003800     03  FILLER                  PIC XX.                                  
003900     03  KOLON-2                 PIC X.                                   
004000     03  FILLER                  PIC XX.                                  
004100                                                                          
004200 01  SEND-KOMMANDO               PIC X(80).                               
004300                                                                          
004400 01  USERID                      PIC X(20).                               
004500 01  MEDDELANDE                  PIC X(90).                               
004600 01  MLENGD                      PIC S9(4)  COMP.                         
004700                                                                          
004800     SKIP3                                                                
004900 LINKAGE SECTION.                                                         
005000                                                                          
005100 01  EXEC-PARM.                                                           
005200     03  LENGD                   PIC S9(4)  COMP.                         
005300     03  PARM-VARDE              PIC X(70).                               
005400     EJECT                                                                
005500 PROCEDURE DIVISION USING EXEC-PARM.                                      
005600                                                                          
005700     MOVE SPACE TO USERID MEDDELANDE                                      
005800     UNSTRING PARM-VARDE DELIMITED BY '//'                                
005900              INTO USERID MEDDELANDE COUNT MLENGD                         
006000                                                                          
006100     IF MLENGD > 25                                                       
006200        DISPLAY 'ERROR - MESSAGE TOO LONG'                                
006300        CALL ABEND USING RKOD                                             
006400     END-IF                                                               
006500                                                                          
006600     ACCEPT TID FROM TIME                                                 
006700     MOVE HHMMSS TO MEDD-TID                                              
006800     MOVE ':' TO KOLON-1 KOLON-2                                          
006900                                                                          
007000     MOVE SPACE TO SEND-KOMMANDO                                          
007100     STRING '/*ÅVS,'  QUOTE  'SE ' QUOTE QUOTE                            
007200            MEDD-TID ' '    DELIMITED BY SIZE                             
007300            MEDDELANDE       DELIMITED BY '   '                           
007400            '.' QUOTE QUOTE                                               
007500            ',LOGON,USER=('  DELIMITED BY SIZE                            
007600            USERID           DELIMITED BY SPACE                           
007700            ')' QUOTE        DELIMITED BY SIZE                            
007800     INTO SEND-KOMMANDO                                                   
007900                                                                          
008000     CALL W009INTR USING OEPPNA                                           
008100     CALL W009INTR USING SKRIV SEND-KOMMANDO                              
008200     CALL W009INTR USING STAENG                                           
008300     GOBACK                                                               
008400     .                                                                    
