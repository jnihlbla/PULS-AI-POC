000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W6111700.                                                
000300 AUTHOR.         HÅKAN BOHLIN.                                            
000400 DATE-WRITTEN.   20/03/16.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*        UPDATES WDK611 WITH NETWEIGHT CHANGES                            
000900*        FROM KDP.                                                        
001000*                                                                         
001100                                                                          
001200 ENVIRONMENT DIVISION.                                                    
001300                                                                          
001400 INPUT-OUTPUT SECTION.                                                    
001500 FILE-CONTROL.                                                            
001600*          --- NETWEIGHT UPDATES FROM KDP                                 
001700     SELECT W61128                     ASSIGN TO W61117D1.                
001800     EJECT                                                                
001900                                                                          
002000 DATA DIVISION.                                                           
002100 FILE SECTION.                                                            
002200 FD  W61128                                                               
002300     RECORDING       F                                                    
002400     BLOCK CONTAINS  0.                                                   
002500                                                                          
002600*01  -COPY W61128      -L.                                                
002700     EJECT                                                                
002800                                                                          
002900 WORKING-STORAGE SECTION.                                                 
003000 77  IDPGM                       PIC X(8)    VALUE 'W6111700'.            
003100 01  CHKP-VAR.                                                            
003200     03 CHKP-MSG-IO-AREA-LENGTH  PIC S9(9)   VALUE +32 COMP SYNC.         
003300     03 CHKP-MSG-IO-AREA         PIC X(32)   VALUE SPACE.                 
003400     03 CHKP-AREA-LENGTH         PIC S9(9)   VALUE +32 COMP SYNC.         
003500     03 CHKP-AREA                PIC X(32)   VALUE SPACE.                 
003600     03 CHKP-ANT                 PIC S9(3)   VALUE +0   COMP-3.           
003700     03 CHKP-MAX                 PIC S9(3)   VALUE +800 COMP-3.           
003800     03 CHKP-TOT                 PIC S9(7)   VALUE ZERO.                  
003900 77  JA                          PIC X       VALUE 'J'.                   
004000 77  NEJ                         PIC X       VALUE 'N'.                   
004100 77  W61128-EOF-SW               PIC X       VALUE 'N'.                   
004200     88  END-OF-W61128                       VALUE 'J'.                   
004300                                                                          
004400 01  FELTEXT.                                                             
004500     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
004600     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
004700                                                                          
004800 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
004900 01  FILLER REDEFINES DAGENS-DATUM.                                       
005000     03  DAGENS-DATUM-AAR        PIC 9(2).                                
005100     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
005200     03  DAGENS-DATUM-DAG        PIC 9(2).                                
005300     EJECT                                                                
005400                                                                          
005500 01  DYNAMISKA-SUBPROGRAM.                                                
005600*                                                                         
005700     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
005800     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
005900     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
006000     EJECT                                                                
006100                                                                          
006200*    --- PARAMETRAR TILL POSTSUM                                          
006300*                                                                         
006400*01  -COPY W0005   -PRE  POSTSUM-                                         
006500     EJECT                                                                
006600                                                                          
006700 01  IN-AREA-START               PIC X(24)   VALUE                        
006800                                             'IN-AREA-START'.             
006900*01  AREA -COPY W61128     -PRE IN-                                       
007000*                                                                         
007100     EJECT                                                                
007200                                                                          
007300 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
007400 01  NYCKLAR-TILL-DLI.                                                    
007500     03  W-IDARTNR-X.                                                     
007600         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
007700     03  W-KDSEGKEY-X.                                                    
007800         05  FILLER              PIC X(1)    VALUE '1'.                   
008000                                                                          
008100*    --- STATUS-KOD FRÅN IMS                                              
008200 01  STATUS-WS                   PIC XX.                                  
008300     88  SEGMENT-FINNS                       VALUE '  '.                  
008400     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
008500     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
008600     88  SEGMENT-SLUT                        VALUE 'GB'.                  
008700     88  IMS-EJ-OK                           VALUE 'XD'.                  
008800                                                                          
008900 01  GODK-STATUSKODER.                                                    
009000     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
009100                                                                          
009200 01  SSA1                        PIC X(64).                               
009300 01  SSA2                        PIC X(64).                               
009400     EJECT                                                                
009500                                                                          
009600*    --- IMS FUNKTIONSKODER                                               
009700*01  -COPY W0003                                                          
009800     EJECT                                                                
009900                                                                          
010000*    ---  DLI INPUT-OUTPUT AREA                                           
010100 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK601'.                      
010200 01  DLI-IO-WDK601.                                                       
010300*    03  -COPY WDK601                                                     
010400     EJECT                                                                
010500                                                                          
010600 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK611'.                      
010700 01  DLI-IO-WDK611.                                                       
010800*    03  -COPY WDK611                                                     
010900     EJECT                                                                
011000                                                                          
011100 LINKAGE SECTION.                                                         
011200*01  -COPY W0009   -PRE MSG-                                              
011300     EJECT                                                                
011400                                                                          
011500*01  -COPY W0008   -PRE WDK6-                                             
011600     05  FILLER                  PIC X.                                   
011700     EJECT                                                                
011800                                                                          
011900 PROCEDURE DIVISION  USING MSG-PCB                                        
012000                           WDK6-PCB.                                      
012100 MAIN SECTION.                                                            
012200     ENTRY 'DLITCBL' USING MSG-PCB                                        
012300                           WDK6-PCB.                                      
012400                                                                          
012500     PERFORM A-INIT                                                       
012600                                                                          
012700     PERFORM S01-LAS-W61128                                               
012800     PERFORM UNTIL END-OF-W61128                                          
012900       IF CHKP-ANT > CHKP-MAX                                             
013000         PERFORM X-TAG-CHECKPOINT                                         
013100       END-IF                                                             
013200                                                                          
013300       PERFORM B-BEARBETA                                                 
013400       PERFORM S01-LAS-W61128                                             
013500     END-PERFORM                                                          
013600                                                                          
013700     PERFORM Z-FINIT                                                      
013800                                                                          
013900     MOVE ZERO TO RETURN-CODE                                             
014000     GOBACK                                                               
014100     .                                                                    
014200     EJECT                                                                
014300                                                                          
014400 A-INIT SECTION.                                                          
014500     PERFORM IMS-RESTART                                                  
014600     OPEN INPUT W61128                                                    
014700     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
014800     ACCEPT DAGENS-DATUM FROM DATE                                        
014900     .                                                                    
015000     EJECT                                                                
015100                                                                          
015200 B-BEARBETA SECTION.                                                      
015300     MOVE IN-IDARTNR                    TO W-IDARTNR                      
015400     PERFORM IMS-GHU-WDK611                                               
015410     IF (IN-VKART-NTO > CLAG-VKART AND                                    
015411         CLAG-VKART > ZERO) OR                                            
015412        (IN-VKART-NTO = CLAG-VKART-NTO AND                                
015413         CLAG-KDUVKNTO = '3') OR                                          
015414        (CLAG-KDUVKNTO = '1' OR '2')                                      
015415        CONTINUE                                                          
015420     ELSE                                                                 
015421        MOVE '3'          TO CLAG-KDUVKNTO                                
015424        MOVE 'KDP'        TO CLAG-IDUSER-VUPD                             
015427        MOVE IN-VKART-NTO TO CLAG-VKART-NTO                               
015428        IF CLAG-VKART = ZERO                                              
015429          MOVE IN-VKART-NTO TO CLAG-VKART                                 
015430        END-IF                                                            
015431        MOVE DAGENS-DATUM TO CLAG-TIUPPDAT-VUPD                           
015432        PERFORM IMS-REPL-WDK611                                           
015433        ADD +1            TO CHKP-ANT                                     
015440     END-IF                                                               
018400     .                                                                    
018500     EJECT                                                                
018600                                                                          
018700 Z-FINIT SECTION.                                                         
018800     CLOSE W61128                                                         
018900                                                                          
019000     MOVE 'S' TO POSTSUM-OPKOD                                            
019100     CALL POSTSUM USING POSTSUM-PARM                                      
019200     .                                                                    
019300     EJECT                                                                
019400                                                                          
019500 S01-LAS-W61128  SECTION.                                                 
019600     READ W61128 INTO IN-AREA                                             
019700     AT END                                                               
019800        MOVE HIGH-VALUE   TO IN-W61128                                    
019900        SET END-OF-W61128 TO TRUE                                         
020000                                                                          
020100     NOT AT END                                                           
020200        MOVE 'W61128'     TO POSTSUM-FDNAMN                               
020300        MOVE 'W61117D1'   TO POSTSUM-DDNAMN2                              
020400        MOVE 'IN-'        TO POSTSUM-TRANSTYP                             
020500        CALL POSTSUM USING POSTSUM-PARM                                   
020600     END-READ                                                             
020700     .                                                                    
020800     EJECT                                                                
020900                                                                          
021000 X-TAG-CHECKPOINT   SECTION.                                              
021100     PERFORM IMS-CHECKPOINT                                               
021200     MOVE ZERO TO CHKP-ANT                                                
021300     .                                                                    
021400     EJECT                                                                
021500                                                                          
021600* --- IMS SEKTIONER ---                                                   
021700     EJECT                                                                
021800                                                                          
021900 IMS-GHU-WDK611 SECTION.                                                  
022000     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
022100          DELIMITED BY SIZE INTO SSA1                                     
022110     STRING 'WDK611  (KDSEGKEY =' W-KDSEGKEY-X ')'                        
022120          DELIMITED BY SIZE INTO SSA2                                     
022200     MOVE '  '             TO GODK-STATUSKODER                            
022300     CALL CBLTDLI USING GHU WDK6-PCB DLI-IO-WDK611 SSA1 SSA2              
022400     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
022500     PERFORM IMS-STATUSKONTROLL                                           
022600     .                                                                    
025300                                                                          
025400 IMS-REPL-WDK611 SECTION.                                                 
025500     MOVE '  '             TO GODK-STATUSKODER                            
025600     CALL CBLTDLI USING REPL WDK6-PCB DLI-IO-WDK611                       
025700     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
025800     PERFORM IMS-STATUSKONTROLL                                           
025900     .                                                                    
026000     EJECT                                                                
026100                                                                          
026200 IMS-RESTART SECTION.                                                     
026300     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
026400     MOVE '  ' TO GODK-STATUSKODER                                        
026500     CALL CBLTDLI USING XRST MSG-PCB                                      
026600                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
026700                        CHKP-AREA-LENGTH CHKP-AREA                        
026800     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
026900     PERFORM IMS-STATUSKONTROLL                                           
027000     .                                                                    
027100                                                                          
027200 IMS-CHECKPOINT SECTION.                                                  
027300     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
027400     MOVE '  XD' TO GODK-STATUSKODER                                      
027500     CALL CBLTDLI USING CHKP MSG-PCB                                      
027600                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
027700                        CHKP-AREA-LENGTH CHKP-AREA                        
027800     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
027900     PERFORM IMS-STATUSKONTROLL                                           
028000                                                                          
028100     IF IMS-EJ-OK                                                         
028200       MOVE 'IMS-KONTROLLREGION EJ TILLGÄNGLIG' TO FELTEXT-STR            
028300       DISPLAY FELTEXT                                                    
028400       CALL FELLOG                                                        
028500     END-IF                                                               
028600     .                                                                    
028700     EJECT                                                                
028800                                                                          
028900 IMS-STATUSKONTROLL SECTION.                                              
029000     SET STATUS-IX TO 1                                                   
029100     SEARCH GODK-STATUS                                                   
029200       AT END                                                             
029300         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
029400           DELIMITED BY SIZE INTO FELTEXT                                 
029500         DISPLAY FELTEXT                                                  
029600         CALL FELLOG                                                      
029700       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
029800         CONTINUE                                                         
029900     END-SEARCH                                                           
030000     .                                                                    
