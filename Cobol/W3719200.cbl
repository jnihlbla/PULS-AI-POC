000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W3719200.                                                
000300 AUTHOR.         HÅKAN BOHLIN.                                            
000400 DATE-WRITTEN.   14/10/22.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700                                                                          
000800*    FUNCTION:                                                            
000900*        REMANUFACTOR ORDER IN NES.                                       
001000*        DELETE PROCESSED ORDER ON EVENT DATABASE.                        
001100*        SYMBOLIC PARAMETERS IDUSER/IDDISTR.                              
001200*                                                                         
001300*        PROGRAM    UPDATE   WDR2 (WDGX3152/WDGX3154)                     
001400*                                                                         
001500                                                                          
001600     SKIP3                                                                
001700 ENVIRONMENT DIVISION.                                                    
001800     SKIP2                                                                
001900 INPUT-OUTPUT SECTION.                                                    
002000                                                                          
002100 FILE-CONTROL.                                                            
002200     SKIP2                                                                
002300*          --- INPUT PARAMETERS FROM NES ORDER SCREEN (WEB)               
002400     SELECT INDATA                     ASSIGN TO W37192D1.                
002500                                                                          
002600 DATA DIVISION.                                                           
002700     SKIP3                                                                
002800 FILE SECTION.                                                            
002900     SKIP3                                                                
003000 FD  INDATA                                                               
003100     RECORDING       F                                                    
003200     BLOCK CONTAINS  0.                                                   
003300     SKIP2                                                                
003400 01  IN-RECORD            PIC X(80).                                      
003500     EJECT                                                                
003600 WORKING-STORAGE SECTION.                                                 
003700                                                                          
003800 77  IDPGM                       PIC X(8)    VALUE 'W3719200'.            
003900 77  YES                         PIC X       VALUE 'J'.                   
004000 77  NOO                         PIC X       VALUE 'N'.                   
004010 01  CHKP-VAR.                                                            
004020     03  CHKP-MSG-IO-AREA-LENGTH  PIC S9(9) VALUE +32 COMP SYNC.          
004030     03  CHKP-MSG-IO-AREA         PIC X(32) VALUE SPACE.                  
004040     03  CHKP-AREA-LENGTH         PIC S9(9) VALUE +32 COMP SYNC.          
004050     03  CHKP-AREA                PIC X(32) VALUE SPACE.                  
004100     SKIP2                                                                
004200 01  ERRTEXT.                                                             
004300     03  FILLER                  PIC X(8)    VALUE 'ERRTEXT'.             
004400     03  ERRTEXT-STR             PIC X(72)   VALUE SPACE.                 
004500     EJECT                                                                
004600                                                                          
004700 01  DYNAMISKA-SUBPROGRAM.                                                
004800*                                                                         
004900     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
005000     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
005100     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
005200     EJECT                                                                
005300*    --- PARAMETRAR TILL POSTSUM                                          
005400*                                                                         
005500*01  -COPY W0005   -PRE  POSTSUM-                                         
005600     EJECT                                                                
005700                                                                          
005800 01  IN-AREA-START               PIC X(16)   VALUE                        
005900                                 'IN-AREA-START  '.                       
006000     SKIP2                                                                
006100 01  IN-AREA-1.                                                           
006200     03  IN-IDUSER-OREG          PIC X(8).                                
006300     03  FILLER                  PIC X(72).                               
006400 01  IN-AREA-2.                                                           
006500     03  IN-IDDISTR              PIC 9(4).                                
006600     03  FILLER                  PIC X(76).                               
006700                                                                          
006800     EJECT                                                                
006900 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
007000     SKIP3                                                                
007100 01  KEYS-TO-DLI.                                                         
007200     03   W-WDGXKEY-3151-X.                                               
007300         05  W-IDHTYP            PIC X(4)   VALUE '3151'.                 
007400         05  FILLER              PIC X(26)  VALUE LOW-VALUE.              
007500                                                                          
007600     03  W-WDGXKEY-3152-X.                                                
007700         05 W-IDUSER-OREG        PIC X(8)  VALUE SPACE.                   
007800         05 W-IDDISTR            PIC S9(5) VALUE ZERO COMP-3.             
007900     EJECT                                                                
008000*    --- STATUS-CODE FROM IMS                                             
008100 01  STATUS-WS                   PIC XX.                                  
008200     88  SEGMENT-EXIST                       VALUE '  '.                  
008300     88  SEGMENT-MISSING                     VALUE 'GE'.                  
008400     SKIP2                                                                
008500 01  GOOD-STATUSCODES.                                                    
008600     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
008700     SKIP3                                                                
008800 01  SSA1                        PIC X(64).                               
008900 01  SSA2                        PIC X(64).                               
009000     EJECT                                                                
009100*    --- IMS RETURNCODES                                                  
009200*01  -COPY W0003                                                          
009300     EJECT                                                                
009400*    ---  DLI INPUT-OUTPUT AREA                                           
009500 01  FILLER                PIC X(16)   VALUE 'DLI-IO-3152'.               
009600 01  DLI-IO-3152.                                                         
009700*    03  -COPY WDGX3152                                                   
009800     EJECT                                                                
009900 LINKAGE SECTION.                                                         
010000                                                                          
010001*01  -COPY W0009  -PRE MSG-                                               
010010     EJECT                                                                
010100*01  -COPY W0008  -PRE 3151-                                              
010200     05  FILLER                  PIC X.                                   
010300     EJECT                                                                
010400 PROCEDURE DIVISION  USING MSG-PCB 3151-PCB.                              
010500 MAIN SECTION.                                                            
010600     ENTRY 'DLITCBL' USING MSG-PCB 3151-PCB.                              
010700                                                                          
010800     SKIP2                                                                
010900     PERFORM A-INIT                                                       
011000                                                                          
011100     MOVE IN-IDUSER-OREG    TO W-IDUSER-OREG                              
011200     MOVE IN-IDDISTR        TO W-IDDISTR                                  
011300     PERFORM IMS-GHU-WDGX3152                                             
011310     IF SEGMENT-EXIST                                                     
011400        PERFORM IMS-DLET-WDGX3152                                         
011410     END-IF                                                               
011500                                                                          
011600     MOVE ZERO TO RETURN-CODE                                             
011700     GOBACK                                                               
011800     .                                                                    
011900     EJECT                                                                
012000 A-INIT SECTION.                                                          
012100                                                                          
012101     PERFORM IMS-RESTART                                                  
012110                                                                          
012200     OPEN INPUT INDATA                                                    
012300     READ INDATA INTO IN-AREA-1                                           
012400     READ INDATA INTO IN-AREA-2                                           
012500     END-READ                                                             
012600     CLOSE INDATA                                                         
012700                                                                          
012800     MOVE IDPGM             TO POSTSUM-PROGNAMN                           
012900     .                                                                    
013000     EJECT                                                                
013100* --- IMS SECTIONS ---                                                    
013200                                                                          
013300 IMS-GHU-WDGX3152 SECTION.                                                
013400                                                                          
013500     STRING 'WDR201  (WDGXKEY  =' W-WDGXKEY-3151-X ')'                    
013600          DELIMITED BY SIZE INTO SSA1                                     
013700     STRING 'WDGX3152(KY3152   =' W-WDGXKEY-3152-X ')'                    
013800          DELIMITED BY SIZE INTO SSA2                                     
013900     MOVE '  GE' TO GOOD-STATUSCODES                                      
014000     CALL CBLTDLI USING GHU 3151-PCB DLI-IO-3152 SSA1 SSA2                
014100     MOVE 3151-STATUS-CODE TO STATUS-WS                                   
014200     PERFORM IMS-STATUSCHECK                                              
014300     .                                                                    
014400     SKIP2                                                                
014500 IMS-DLET-WDGX3152 SECTION.                                               
014600                                                                          
014700     MOVE '  ' TO GOOD-STATUSCODES                                        
014800     CALL CBLTDLI USING DLET 3151-PCB DLI-IO-3152                         
014900     MOVE 3151-STATUS-CODE TO STATUS-WS                                   
015000     PERFORM IMS-STATUSCHECK                                              
015100     .                                                                    
015110     EJECT                                                                
015120 IMS-RESTART SECTION.                                                     
015130                                                                          
015150     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
015160     MOVE '  ' TO GOOD-STATUSCODES                                        
015170     CALL CBLTDLI USING XRST MSG-PCB                                      
015180                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
015190                        CHKP-AREA-LENGTH CHKP-AREA                        
015191     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
015192     PERFORM IMS-STATUSCHECK                                              
015193     .                                                                    
015200     SKIP2                                                                
015300 IMS-STATUSCHECK SECTION.                                                 
015400     SKIP2                                                                
015500     SET STATUS-IX TO 1                                                   
015600     SEARCH GOOD-STATUS                                                   
015700       AT END                                                             
015800         STRING ' WRONG STATUS CODE FROM IMS: ' STATUS-WS                 
015900           DELIMITED BY SIZE INTO ERRTEXT-STR                             
016000         DISPLAY ERRTEXT                                                  
016100         CALL FELLOG                                                      
016200       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
016300         CONTINUE                                                         
016400     END-SEARCH                                                           
016500     .                                                                    
