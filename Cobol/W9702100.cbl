000100 ID  DIVISION.                                                            
000200     SKIP2                                                                
000300 PROGRAM-ID.  W9702100.                                                   
000400     SKIP2                                                                
000500 AUTHOR.        KARIN OLSSON.                                             
000600     DATE-WRITTEN.  JAN 1993.                                             
000700*                                                                         
000800*                                                                         
000900*                                                                         
001000     EJECT                                                                
001100 ENVIRONMENT DIVISION.                                                    
001200     SKIP2                                                                
001300 INPUT-OUTPUT  SECTION.                                                   
001400*                                                                         
001500 FILE-CONTROL.                                                            
001600*                                                                         
001700     SELECT INFIL           ASSIGN TO W97021D1.                           
001800*                                                                         
001900 DATA DIVISION.                                                           
002000                                                                          
002100 FILE  SECTION.                                                           
002200*                                                                         
002300 FD  INFIL                                                                
002400     LABEL RECORD   STANDARD                                              
002500     RECORDING      F                                                     
002600     BLOCK CONTAINS 0.                                                    
002700                                                                          
002800 01  FILLER                  PIC X(60).                                   
002900     EJECT                                                                
003000 WORKING-STORAGE  SECTION.                                                
003001                                                                          
003010*    -- CHECKED BY WY2000                                                 
003100*                                                                         
003200 01  W-IN-AREA.                                                           
003300*                                                                         
003400     03  IN-TRANS.                                                        
003410         05  IN-TRANS-1-2.                                                
003420             07  FILLER      PIC X(1).                                    
003421             07  IN-TRANS-2  PIC X(1).                                    
003430         05  FILLER          PIC X(1).                                    
003431         05  IN-TRANS-4-8    PIC X(5).                                    
003500     03  IN-TRANS-BESKR      PIC X(30).                                   
003600     03  IN-BOLAG            PIC X(3).                                    
003610     03  IN-LAND             PIC X(2).                                    
003700     03  IN-JOBFUNC          PIC X(2).                                    
003800     03  IN-DIVMISC          PIC X(3).                                    
003900     03  IN-ANSTNR           PIC X(5).                                    
004000     03  FILLER              PIC X(7).                                    
004100*                                                                         
004200 01  MEMPOST                     PIC X(80).                               
004300*                                                                         
004400 01  MEMPOST-1 REDEFINES MEMPOST.                                         
004500     03  MEM-TAG             PIC X(3).                                    
004600     03  MEM-RAD             PIC X(77).                                   
004700*                                                                         
004800     03  MEM-RAD-1 REDEFINES MEM-RAD.                                     
004900         05  MEM-JOBFUNC     PIC X(2).                                    
005000         05  FILLER          PIC X(2).                                    
005100         05  MEM-DIVMISC     PIC X(3).                                    
005200         05  FILLER          PIC X(2).                                    
005300         05  MEM-ANSTNR      PIC X(5).                                    
005400         05  FILLER          PIC X(58).                                   
005500*                                                                         
005600 01  GENERELLA-SUBPGM.                                                    
005700*                                                                         
005800     03  ABEND               PIC X(8)    VALUE 'ABEND'.                   
005900     03  ISPLINK             PIC X(8)    VALUE 'ISPLINK'.                 
006000*                                                                         
006100 01  RKOD-ABEND-UTAN-DUMP    PIC S9(4)   COMP VALUE +16.                  
006200 01  RKOD-ABEND-MED-DUMP     PIC S9(4)   COMP VALUE +1000.                
006300*                                                                         
006400 77  JA                      PIC X(1)    VALUE 'Y'.                       
006500 77  NEJ                     PIC X(1)    VALUE 'N'.                       
006510 77  DCF-FONT                PIC X(12)   VALUE ':font size=8'.            
006600 77  DCF-RDEF1               PIC X(80)                                    
006700       VALUE ':rdef hrules=no vrules=no id=col hp=''2 0 2 0'' '.          
006800 77  DCF-RDEF2               PIC X(80)                                    
006900       VALUE 'cwidths=''14mm 70mm 14mm 70mm''.'.                          
007000 77  DCF-TABLE                   PIC X(80)                                
007100       VALUE ':table frame=no refid=col page.'.                           
007200 77  DCF-ROW                 PIC X(4)    VALUE ':row'.                    
007300 77  DCF-COL                 PIC X(3)    VALUE ':c.'.                     
007420 77  DCF-ETABLE              PIC X(7)    VALUE ':etable'.                 
007430 77  DCF-EFONT               PIC X(6)    VALUE ':efont'.                  
007500 77  ISP-VGET                PIC X(8)    VALUE 'VGET    '.                
007600 77  ISP-VPUT                PIC X(8)    VALUE 'VPUT    '.                
007700 77  ISP-VDEFINE             PIC X(8)    VALUE 'VDEFINE '.                
007800 77  ISP-SHARED              PIC X(8)    VALUE 'SHARED  '.                
007900 77  ISP-PROFILE             PIC X(8)    VALUE 'PROFILE '.                
008000 77  CHAR                    PIC X(8)    VALUE 'CHAR    '.                
008010 77  FIXED                   PIC X(8)    VALUE 'FIXED   '.                
008100 77  VDEFINE-OPT             PIC X(16)                                    
008200                              VALUE '(COPY NOBSCAN)'.                     
008300 77  ISP-YES                 PIC X(3)    VALUE 'YES'.                     
008310 77  ISP-SHR                 PIC X(8)    VALUE 'SHRW    '.                
008320 77  ISP-SHRW                PIC X(8)    VALUE 'SHRW    '.                
008400 77  ISP-INPUT               PIC X(8)    VALUE 'INPUT'.                   
008410 77  ISP-OUTPUT              PIC X(8)    VALUE 'OUTPUT'.                  
008500 77  ISP-INVAR               PIC X(8)    VALUE 'INVAR'.                   
008600 77  ISP-DATALOC             PIC X(8)    VALUE 'MEMPOST'.                 
008700 77  ISP-DATALEN             PIC S9(9)   COMP SYNC.                       
008800 77  LMINIT                  PIC X(8)    VALUE 'LMINIT  '.                
008900 77  LMOPEN                  PIC X(8)    VALUE 'LMOPEN  '.                
009000 77  LMCLOSE                 PIC X(8)    VALUE 'LMCLOSE '.                
009100 77  LMFREE                  PIC X(8)    VALUE 'LMFREE  '.                
009300 77  LMMFIND                 PIC X(8)    VALUE 'LMMFIND '.                
009310 77  LMMREP                  PIC X(8)    VALUE 'LMMREP  '.                
009320 77  LMPUT                   PIC X(8)    VALUE 'LMPUT   '.                
009400                                                                          
009500 01  NOPARM                  PIC X       VALUE SPACE.                     
009600 01  ZLCDATE                 PIC X(8).                                    
009700 01  N-ZLCDATE               PIC X(8)    VALUE 'ZLCDATE'.                 
009800 01  L-ZLCDATE               PIC S9(9) COMP VALUE +8.                     
009900 01  ZLMDATE                 PIC X(8).                                    
010000 01  N-ZLMDATE               PIC X(8)    VALUE 'ZLMDATE'.                 
010100 01  L-ZLMDATE               PIC S9(9) COMP VALUE +8.                     
010110 01  ZLMTIME                 PIC X(5).                                    
010120 01  N-ZLMTIME               PIC X(8)    VALUE 'ZLMTIME'.                 
010130 01  L-ZLMTIME               PIC S9(9) COMP VALUE +5.                     
010140 01  ZLCNORC                 PIC S9(8) COMP.                              
010150 01  N-ZLCNORC               PIC X(8)    VALUE 'ZLCNORC'.                 
010160 01  L-ZLCNORC               PIC S9(9) COMP VALUE +4.                     
010170 01  ZLINORC                 PIC S9(8) COMP.                              
010180 01  N-ZLINORC               PIC X(8)    VALUE 'ZLINORC'.                 
010190 01  L-ZLINORC               PIC S9(9) COMP VALUE +4.                     
010200 01  ZLUSER                  PIC X(8).                                    
010300 01  N-ZLUSER                PIC X(8)    VALUE 'ZLUSER'.                  
010400 01  L-ZLUSER                PIC S9(9) COMP VALUE +8.                     
010500 01  DDVAR1                  PIC X(8).                                    
010600 01  N-DDVAR1                PIC X(8)    VALUE 'DDVAR1'.                  
010700 01  L-DDVAR1                PIC S9(9) COMP VALUE +8.                     
010710 01  DDVAR2                  PIC X(8).                                    
010720 01  N-DDVAR2                PIC X(8)    VALUE 'DDVAR2'.                  
010730 01  L-DDVAR2                PIC S9(9) COMP VALUE +8.                     
010800 01  N-MEMPOST               PIC X(8)    VALUE 'MEMPOST'.                 
010900 01  L-MEMPOST               PIC S9(9) COMP VALUE +80.                    
011000 01  WDSNAME                 PIC X(44)   VALUE SPACE.                     
011100 01  WMEMBER                 PIC X(8).                                    
011200*                                                                         
011300 01  INFIL-EOF               PIC X(1)    VALUE 'N'.                       
011400*                                                                         
011500 01  W-BOLAG                 PIC X(3).                                    
011501 01  W-JOBFUNC               PIC X(2).                                    
011510 01  W-LAND                  PIC X(2).                                    
011600 01  OLD-JOBFUNC             PIC X(2).                                    
011700 01  OLD-TRANS.                                                           
011710     03  OLD-TRANS-1-2       PIC X(2).                                    
011720     03  FILLER              PIC X(6).                                    
011800 01  OLD-BOLAG               PIC X(3).                                    
011810 01  OLD-LAND                PIC X(2).                                    
011900     EJECT                                                                
012000*                                                                         
012100     SKIP2                                                                
012200 01  RETURKODER.                                                          
012300*                                                                         
012400     03  RKOD                PIC S9(4)   COMP SYNC VALUE ZERO.            
012500     EJECT                                                                
012600 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
012700 01  FILLER REDEFINES DAGENS-DATUM.                                       
012800     03  DAGENS-DATUM-AAR        PIC X(2).                                
012900     03  DAGENS-DATUM-MAANAD     PIC X(2).                                
013000     03  DAGENS-DATUM-DAG        PIC X(2).                                
013001     SKIP2                                                                
013002 01  AKTUELL-TID.                                                         
013003     03  TID-TIMME               PIC X(2)    VALUE SPACE.                 
013004     03  TID-MINUT               PIC X(2)    VALUE SPACE.                 
013005     03  TID-SEKUND              PIC X(2)    VALUE SPACE.                 
013006     03  TID-100DELS-SEKUND      PIC X(2)    VALUE SPACE.                 
013007     SKIP2                                                                
013008 01  FOERSTA                     PIC X(1)    VALUE 'J'.                   
013100     EJECT                                                                
013110 LINKAGE SECTION.                                                         
013111 01  PARM.                                                                
013112     03  PARM-LAENGD             PIC S9(4)   COMP SYNC.                   
013113     03  PARM-VAERDE             PIC X(48).                               
013120     EJECT                                                                
013200 PROCEDURE DIVISION USING PARM.                                           
013300*                                                                         
013400     PERFORM A-INIT                                                       
013500     PERFORM S10-LAS-INFIL                                                
013510                                                                          
013600     PERFORM UNTIL INFIL-EOF = JA                                         
013700       PERFORM B-INITIERA-MEDLEM                                          
013710                                                                          
013800       PERFORM UNTIL INFIL-EOF = JA                                       
013900         OR IN-BOLAG NOT = OLD-BOLAG                                      
013910         OR IN-LAND  NOT = OLD-LAND                                       
014000         OR IN-JOBFUNC NOT = OLD-JOBFUNC                                  
014001         PERFORM C-INITIERA-TABELL                                        
014002                                                                          
014010         PERFORM UNTIL INFIL-EOF = JA                                     
014020           OR IN-BOLAG NOT = OLD-BOLAG                                    
014021           OR IN-LAND  NOT = OLD-LAND                                     
014030           OR IN-JOBFUNC NOT = OLD-JOBFUNC                                
014040           OR IN-TRANS-1-2 NOT = OLD-TRANS-1-2                            
014100           PERFORM D-SKRIV-MEMPOST                                        
014200           PERFORM S10-LAS-INFIL                                          
014300         END-PERFORM                                                      
014301                                                                          
014302         PERFORM E-AVSLUTA-TABELL                                         
014310       END-PERFORM                                                        
014320                                                                          
014400       PERFORM F-AVSLUTA-MEDLEM                                           
014500     END-PERFORM                                                          
014600                                                                          
014700     PERFORM Z-FINIT                                                      
014800     MOVE RKOD TO RETURN-CODE                                             
014900     GOBACK.                                                              
015000     EJECT                                                                
015100 A-INIT  SECTION.                                                         
015200     SKIP2                                                                
015210     IF PARM-LAENGD > 40                                                  
015220       DISPLAY 'NAMN PÅ TEXT-PDS LÄNGRE ÄN 40 TECKEN.'                    
015230       PERFORM S99-ABEND                                                  
015240     END-IF                                                               
015250                                                                          
015260     IF PARM-LAENGD = 0 OR 2                                              
015270       DISPLAY 'SAKNAR NAMN PÅ TEXT-PDS.'                                 
015280       PERFORM S99-ABEND                                                  
015290     END-IF                                                               
015291                                                                          
015295     MOVE PARM-VAERDE (1:PARM-LAENGD) TO WDSNAME                          
015400                                                                          
015410     ACCEPT DAGENS-DATUM  FROM DATE                                       
015420     ACCEPT AKTUELL-TID   FROM TIME                                       
015430                                                                          
015500     OPEN INPUT INFIL                                                     
015600                                                                          
015700     MOVE NEJ TO INFIL-EOF                                                
015800     MOVE '--' TO OLD-BOLAG OLD-LAND OLD-JOBFUNC                          
015900                                                                          
016000     CALL ISPLINK USING ISP-VDEFINE N-ZLCDATE ZLCDATE CHAR                
016100                         L-ZLCDATE VDEFINE-OPT                            
016200     IF RETURN-CODE > 8                                                   
016300       DISPLAY 'KAN INTE VDEFFA ZLCDATE'                                  
016400       PERFORM S99-ABEND                                                  
016500     END-IF                                                               
016600                                                                          
016700     CALL ISPLINK USING ISP-VDEFINE N-ZLMDATE ZLMDATE CHAR                
016800                         L-ZLMDATE VDEFINE-OPT                            
016900     IF RETURN-CODE > 8                                                   
017000       DISPLAY 'KAN INTE VDEFFA ZLMDATE'                                  
017100       PERFORM S99-ABEND                                                  
017200     END-IF                                                               
017300                                                                          
017310     CALL ISPLINK USING ISP-VDEFINE N-ZLMTIME ZLMTIME CHAR                
017320                         L-ZLMTIME VDEFINE-OPT                            
017330     IF RETURN-CODE > 8                                                   
017340       DISPLAY 'KAN INTE VDEFFA ZLMTIME'                                  
017350       PERFORM S99-ABEND                                                  
017360     END-IF                                                               
017370                                                                          
017380     CALL ISPLINK USING ISP-VDEFINE N-ZLCNORC ZLCNORC FIXED               
017390                         L-ZLCNORC VDEFINE-OPT                            
017391     IF RETURN-CODE > 8                                                   
017392       DISPLAY 'KAN INTE VDEFFA ZLCNORC'                                  
017393       PERFORM S99-ABEND                                                  
017394     END-IF                                                               
017395                                                                          
017396     CALL ISPLINK USING ISP-VDEFINE N-ZLINORC ZLINORC FIXED               
017397                         L-ZLINORC VDEFINE-OPT                            
017398     IF RETURN-CODE > 8                                                   
017399       DISPLAY 'KAN INTE VDEFFA ZLINORC'                                  
017400       PERFORM S99-ABEND                                                  
017401     END-IF                                                               
017402                                                                          
017410     CALL ISPLINK USING ISP-VDEFINE N-ZLUSER ZLUSER CHAR                  
017500                         L-ZLUSER VDEFINE-OPT                             
017600     IF RETURN-CODE > 8                                                   
017700       DISPLAY 'KAN INTE VDEFFA ZLUSER'                                   
017800       PERFORM S99-ABEND                                                  
017900     END-IF                                                               
018000                                                                          
018100     CALL ISPLINK USING ISP-VDEFINE N-DDVAR1 DDVAR1 CHAR                  
018200                         L-DDVAR1 VDEFINE-OPT                             
018300     IF RETURN-CODE > 8                                                   
018400       DISPLAY 'KAN INTE VDEFFA DDVAR1'                                   
018500       PERFORM S99-ABEND                                                  
018600     END-IF                                                               
018700                                                                          
018710     CALL ISPLINK USING ISP-VDEFINE N-DDVAR2 DDVAR2 CHAR                  
018720                         L-DDVAR2 VDEFINE-OPT                             
018730     IF RETURN-CODE > 8                                                   
018740       DISPLAY 'KAN INTE VDEFFA DDVAR2'                                   
018750       PERFORM S99-ABEND                                                  
018760     END-IF                                                               
018770                                                                          
018800     CALL ISPLINK USING ISP-VDEFINE N-MEMPOST MEMPOST CHAR                
018900                         L-MEMPOST VDEFINE-OPT                            
019000     IF RETURN-CODE > 8                                                   
019100       DISPLAY 'KAN INTE VDEFFA MEMPOST'                                  
019200       PERFORM S99-ABEND                                                  
019300     END-IF                                                               
019400                                                                          
019600                                                                          
019700     CALL ISPLINK USING LMINIT N-DDVAR1 NOPARM NOPARM NOPARM              
019800       NOPARM NOPARM NOPARM WDSNAME NOPARM NOPARM NOPARM ISP-SHRW         
019900     IF RETURN-CODE NOT = 0                                               
020000       DISPLAY 'KAN INTE INITIERA TEXT-PDS ' RETURN-CODE                  
020100       PERFORM S99-ABEND                                                  
020200     END-IF                                                               
020300                                                                          
020400     CALL ISPLINK USING LMOPEN DDVAR1 ISP-OUTPUT                          
020500     IF RETURN-CODE NOT = 0                                               
020600       DISPLAY 'KAN INTE ÖPPNA TEXT-PDS ' RETURN-CODE                     
020700       PERFORM S99-ABEND                                                  
020800     END-IF                                                               
020801                                                                          
020810     CALL ISPLINK USING LMINIT N-DDVAR2 NOPARM NOPARM NOPARM              
020820       NOPARM NOPARM NOPARM WDSNAME NOPARM NOPARM NOPARM ISP-SHR          
020830     IF RETURN-CODE NOT = 0                                               
020840       DISPLAY 'KAN INTE INITIERA TEXT-PDS ' RETURN-CODE                  
020850       PERFORM S99-ABEND                                                  
020860     END-IF                                                               
020870                                                                          
020880     CALL ISPLINK USING LMOPEN DDVAR2 ISP-INPUT                           
020890     IF RETURN-CODE NOT = 0                                               
020891       DISPLAY 'KAN INTE ÖPPNA TEXT-PDS ' RETURN-CODE                     
020892       PERFORM S99-ABEND                                                  
020893     END-IF                                                               
020900     .                                                                    
021000     EJECT                                                                
021100 B-INITIERA-MEDLEM  SECTION.                                              
021200     SKIP2                                                                
021300     MOVE SPACE TO WMEMBER                                                
021400                                                                          
021600     MOVE IN-BOLAG TO OLD-BOLAG W-BOLAG                                   
021610     MOVE IN-LAND TO OLD-LAND W-LAND                                      
021700     MOVE IN-JOBFUNC TO OLD-JOBFUNC W-JOBFUNC                             
021800                                                                          
021900     INSPECT W-BOLAG CONVERTING '*' TO 'Å'                                
021901     INSPECT W-LAND CONVERTING '*' TO 'Å'                                 
021910     INSPECT W-JOBFUNC CONVERTING '*' TO 'Å'                              
022000                                                                          
022100     STRING W-BOLAG DELIMITED BY SPACE                                    
022110            W-LAND   DELIMITED BY SPACE                                   
022200            W-JOBFUNC DELIMITED BY SPACE                                  
022300            INTO WMEMBER                                                  
022400                                                                          
022410     CALL ISPLINK USING LMMFIND DDVAR2 WMEMBER NOPARM NOPARM              
022420                           NOPARM NOPARM ISP-YES                          
022430     IF RETURN-CODE = 0 AND ZLCDATE NOT = SPACE                           
022431       STRING DAGENS-DATUM-AAR '/'                                        
022432              DAGENS-DATUM-MAANAD '/'                                     
022433              DAGENS-DATUM-DAG DELIMITED BY SIZE                          
022434              INTO ZLMDATE                                                
022453     ELSE                                                                 
022454       IF RETURN-CODE > 8                                                 
022455         DISPLAY 'FEL VID LMMFIND ' WMEMBER ' ' RETURN-CODE               
022456         PERFORM S99-ABEND                                                
022457       ELSE                                                               
022459         STRING DAGENS-DATUM-AAR '/'                                      
022460                DAGENS-DATUM-MAANAD '/'                                   
022461                DAGENS-DATUM-DAG DELIMITED BY SIZE                        
022462                INTO ZLCDATE                                              
022463         MOVE   ZLCDATE TO ZLMDATE                                        
022464       END-IF                                                             
022470     END-IF                                                               
022480                                                                          
022481     STRING TID-TIMME ':' TID-MINUT DELIMITED BY SIZE                     
022482            INTO ZLMTIME                                                  
022483                                                                          
022490     MOVE 'W970B9' TO ZLUSER                                              
022491     MOVE ZERO TO ZLCNORC ZLINORC                                         
022500                                                                          
022600     MOVE DCF-FONT  TO MEMPOST                                            
022700     PERFORM S20-SKRIV-MEMPOST                                            
022800                                                                          
022900     MOVE DCF-RDEF1 TO MEMPOST                                            
023000     PERFORM S20-SKRIV-MEMPOST                                            
023100                                                                          
023200     MOVE DCF-RDEF2 TO MEMPOST                                            
023300     PERFORM S20-SKRIV-MEMPOST                                            
023700     .                                                                    
023800     EJECT                                                                
023810 C-INITIERA-TABELL  SECTION.                                              
023820     SKIP2                                                                
023850     MOVE IN-TRANS TO OLD-TRANS                                           
023927                                                                          
023928     MOVE DCF-TABLE TO MEMPOST                                            
023930     PERFORM S20-SKRIV-MEMPOST                                            
023931                                                                          
023932     MOVE JA TO FOERSTA                                                   
023934     .                                                                    
023935     EJECT                                                                
023940 D-SKRIV-MEMPOST    SECTION.                                              
024000     SKIP2                                                                
024010     IF FOERSTA = JA                                                      
024100       MOVE DCF-ROW  TO MEMPOST                                           
024200       PERFORM S20-SKRIV-MEMPOST                                          
024210       MOVE NEJ TO FOERSTA                                                
024211     ELSE                                                                 
024212       MOVE JA TO FOERSTA                                                 
024220     END-IF                                                               
024300                                                                          
024400     MOVE SPACE TO MEMPOST                                                
024500     MOVE DCF-COL TO MEM-TAG                                              
024600                                                                          
024610     IF IN-ANSTNR = SPACE                                                 
024700       STRING IN-TRANS-2 IN-TRANS-4-8                                     
024710              DELIMITED BY SPACE INTO MEM-RAD                             
024720     ELSE                                                                 
024721       STRING IN-TRANS-2 IN-TRANS-4-8 '*'                                 
024722              DELIMITED BY SPACE INTO MEM-RAD                             
024730     END-IF                                                               
024800     PERFORM S20-SKRIV-MEMPOST                                            
024900                                                                          
025000     MOVE IN-TRANS-BESKR TO MEM-RAD                                       
025100     PERFORM S20-SKRIV-MEMPOST                                            
025500     .                                                                    
025600     EJECT                                                                
025700 E-AVSLUTA-TABELL   SECTION.                                              
025800     SKIP2                                                                
025900     MOVE DCF-ETABLE TO MEMPOST                                           
026000     PERFORM S20-SKRIV-MEMPOST                                            
026800     .                                                                    
026900     EJECT                                                                
026910 F-AVSLUTA-MEDLEM   SECTION.                                              
026920     SKIP2                                                                
026940     MOVE DCF-EFONT  TO MEMPOST                                           
026950     PERFORM S20-SKRIV-MEMPOST                                            
026951                                                                          
026952     MOVE ZLCNORC TO ZLINORC                                              
026953                                                                          
026960     CALL ISPLINK USING LMMREP DDVAR1 WMEMBER ISP-YES                     
026970                                                                          
026980     IF RETURN-CODE > 8                                                   
026990       DISPLAY 'KAN INTE REPLACA MEDLEM ' WMEMBER                         
026991       PERFORM S99-ABEND                                                  
026992     END-IF                                                               
026993     .                                                                    
026994     EJECT                                                                
027000 Z-FINIT  SECTION.                                                        
027100     SKIP2                                                                
027200     CLOSE INFIL                                                          
027300                                                                          
027400     CALL ISPLINK USING LMCLOSE DDVAR1                                    
027500     CALL ISPLINK USING LMFREE DDVAR1                                     
027510                                                                          
027520     CALL ISPLINK USING LMCLOSE DDVAR2                                    
027530     CALL ISPLINK USING LMFREE DDVAR2                                     
027600     .                                                                    
027700     EJECT                                                                
027800 S10-LAS-INFIL  SECTION.                                                  
027900     SKIP2                                                                
028000     READ INFIL INTO W-IN-AREA                                            
028100     AT END MOVE JA TO INFIL-EOF                                          
028200     .                                                                    
028300     SKIP3                                                                
028400 S20-SKRIV-MEMPOST  SECTION.                                              
028500     SKIP2                                                                
028600     MOVE +80 TO ISP-DATALEN                                              
028700     CALL ISPLINK USING LMPUT DDVAR1 ISP-INVAR ISP-DATALOC                
028800                           ISP-DATALEN                                    
028900     IF RETURN-CODE NOT = 0                                               
029000       DISPLAY 'KAN INTE SKRIVA MEDLEM ' WMEMBER ' ' RETURN-CODE          
029100       PERFORM S99-ABEND                                                  
029200     END-IF                                                               
029210     ADD 1 TO ZLCNORC                                                     
029300     .                                                                    
029400     SKIP3                                                                
029500 S99-ABEND  SECTION.                                                      
029600     SKIP2                                                                
029700     CALL ABEND USING RKOD-ABEND-UTAN-DUMP                                
029800     .                                                                    
