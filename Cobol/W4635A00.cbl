000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W4635A00.                                                
000300 AUTHOR.         GÖRAN KJELLSSON.                                         
000400 DATE-WRITTEN.   HÖSTEN 2019                                              
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*        KONTROLLERA ATT BRUTTOVIKTEN I DESADV                            
000900*        INTE ÄR MINDRE ÄN NETTOVIKTEN                                    
001000*                                                                         
001100*        PROGRAMMET LÄSER      WDK6                                       
001200*                                                                         
001300*    ABENDKODER:                                                          
001400*        U0016 -  . . . .                                                 
001500*        U1000 -  . . . .                                                 
001600*                                                                         
001700                                                                          
001800                                                                          
001900 ENVIRONMENT DIVISION.                                                    
002000 INPUT-OUTPUT SECTION.                                                    
002100                                                                          
002200 FILE-CONTROL.                                                            
002300                                                                          
002400*          --- DESADV-POSTER                                              
002500     SELECT W46356                     ASSIGN TO W4635AD1.                
002600                                                                          
002700*          --- VIKTJUSTERADE KOLLI                                        
002800     SELECT W4635J                     ASSIGN TO W4635AD2.                
003200                                                                          
003300                                                                          
003400 DATA DIVISION.                                                           
003500 FILE SECTION.                                                            
003600                                                                          
003700 FD  W46356                                                               
003800     RECORDING       F                                                    
003900     BLOCK CONTAINS  0.                                                   
004000                                                                          
004100*01  -COPY W46356      -L.                                                
004300                                                                          
004400 FD  W4635J                                                               
004500     RECORDING       F                                                    
004600     BLOCK CONTAINS  0.                                                   
004700                                                                          
004800*01  POST -COPY W4635J -PRE  JUST-  -L.                                   
005800                                                                          
005900 WORKING-STORAGE SECTION.                                                 
006000                                                                          
006100 77  IDPGM                       PIC X(8)    VALUE 'W4635A00'.            
006200 77  JA                          PIC X       VALUE 'J'.                   
006300 77  NEJ                         PIC X       VALUE 'N'.                   
006400 77  CURRENT-SECTION             PIC X(16)   VALUE SPACE.                 
006500 77  CURRENT-IMS-SECTION         PIC X(16)   VALUE SPACE.                 
006600                                                                          
006700 77  W46356-EOF-SW               PIC X       VALUE 'N'.                   
006800     88  END-OF-W46356                       VALUE 'J'.                   
006900                                                                          
007000 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
007100 01  FILLER REDEFINES DAGENS-DATUM.                                       
007200     03  DAGENS-DATUM-AAR        PIC 9(2).                                
007300     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
007400     03  DAGENS-DATUM-DAG        PIC 9(2).                                
007500                                                                          
007600 01  DYNAMISKA-SUBPROGRAM.                                                
007700*                                                                         
007800     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
007900     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
008000     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
008100     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
008200                                                                          
008300*    --- PARAMETRAR TILL ABEND                                            
008400                                                                          
008500 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
008600 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
008700 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
008800                                                                          
008810 01  RAD-IX                      PIC 9(3).                                
008820 01  RAD-IX-MAX                  PIC 9(3)    VALUE 150.                   
008900 01  RADPOSTER.                                                           
008910     03  RADPOST OCCURS 150.                                              
009000         05  RAD-IDRADNR         PIC 9(4).                                
009010         05  RAD-IDARTNR         PIC 9(8).                                
009020         05  RAD-KVLEVART        PIC 9(6).                                
009030         05  RAD-VKART           PIC 9(7).                                
009040                                                                          
009100 01  W-VKORDBTO-GRAM             PIC 9(7)      VALUE ZERO.                
009200 01  W-VKART-SUM                 PIC 9(7)      VALUE ZERO.                
009300                                                                          
009400 01  FELTEXT.                                                             
009500     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
009600     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
010000*                                                                         
010100*01  -COPY W0005   -PRE  POSTSUM-                                         
010300                                                                          
010400 01  IN-AREA-START               PIC X(24)   VALUE                        
010500                                 'IN-AREA-START  '.                       
010600                                                                          
010700*01  AREA -COPY W46356     -PRE IN-                                       
010800                                                                          
010900                                                                          
011000 01  JUST-AREA-START             PIC X(24)   VALUE                        
011100                                 'JUST-AREA-START  '.                     
011200                                                                          
011300*01  AREA -COPY W4635J     -PRE JUST-                                     
011500                                                                          
012100*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
012300                                                                          
012400 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
012500                                                                          
012600 01  NYCKLAR-TILL-DLI.                                                    
012700     03  W-IDARTNR-X.                                                     
012800         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
012900     03  W-KDSEGKEY-X.                                                    
013000         05  W-KDSEGKEY          PIC X(1)    VALUE '1'.                   
013100                                                                          
013200*    --- STATUS-KOD FRÅN IMS                                              
013300 01  STATUS-WS                   PIC XX.                                  
013400     88  SEGMENT-FINNS                       VALUE '  '.                  
013500     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
013600                                                                          
013700 01  GODK-STATUSKODER.                                                    
013800     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
013900                                                                          
014000 01  SSA1                        PIC X(64).                               
014100 01  SSA2                        PIC X(64).                               
014200                                                                          
014300*    --- IMS FUNKTIONSKODER                                               
014400*01  -COPY W0003                                                          
014600                                                                          
014700*    ---  DLI INPUT-OUTPUT AREA                                           
014800 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK611'.                      
014900 01  DLI-IO-WDK611.                                                       
015000*    03  -COPY WDK611                                                     
015200                                                                          
015300 LINKAGE SECTION.                                                         
015400                                                                          
015500*01  -COPY W0008  -PRE WDK6-                                              
015600     05  FILLER                  PIC X.                                   
015800                                                                          
015900 PROCEDURE DIVISION  USING WDK6-PCB.                                      
016000 MAIN SECTION.                                                            
016100     ENTRY 'DLITCBL' USING WDK6-PCB.                                      
016300                                                                          
016400     PERFORM A-INIT                                                       
016500                                                                          
016600     PERFORM S01-LAES-W46356                                              
016700     PERFORM UNTIL END-OF-W46356                                          
016800        IF IN-IDPRODNR       = JUST-IDPRODNR AND                          
016900           IN-IDKOLLI        = JUST-IDKOLLI                               
017100           PERFORM B-SUMMERA-VKART                                        
017200        ELSE                                                              
017300           IF JUST-IDPRODNR = ZERO AND                                    
017400              JUST-IDKOLLI  = ZERO                                        
017410              CONTINUE                                                    
017600           ELSE                                                           
017700              IF W-VKORDBTO-GRAM < W-VKART-SUM                            
017800                 PERFORM C-SKRIV-JUSTERINGSFIL                            
018000              END-IF                                                      
019200           END-IF                                                         
019201           PERFORM S10-NOLLA-RADPOSTER                                    
019210           MOVE IN-IDPRODNR             TO JUST-IDPRODNR                  
019220           MOVE IN-IDSUPREF             TO JUST-IDSUPREF                  
019221           MOVE IN-IDLEVNR              TO JUST-IDLEVNR                   
019230           MOVE IN-IDDC                 TO JUST-IDDC                      
019240           MOVE IN-IDDISTR              TO JUST-IDDISTR                   
019250           MOVE IN-IDKUNDNR             TO JUST-IDKUNDNR                  
019260           MOVE IN-IDORDNR7             TO JUST-IDORDNR7                  
019270           MOVE IN-IDKOLLI              TO JUST-IDKOLLI                   
019271           MOVE IN-VKORDBTO-KOLLI       TO JUST-VKORDBTO-KOLLI            
019280           MOVE ZERO                    TO JUST-VKORDBTO-KOLLI-ADJ        
019290                                        W-VKART-SUM                       
019291           COMPUTE W-VKORDBTO-GRAM =                                      
019292                  IN-VKORDBTO-KOLLI * 1000                                
019293           PERFORM B-SUMMERA-VKART                                        
019500        END-IF                                                            
019600                                                                          
019700        PERFORM S01-LAES-W46356                                           
019800     END-PERFORM                                                          
019900*    OM SISTA KOLLIT HAR FELAKTIG VIKT                                    
020000     IF W-VKORDBTO-GRAM < W-VKART-SUM                                     
020100        PERFORM C-SKRIV-JUSTERINGSFIL                                     
020200     END-IF                                                               
020300                                                                          
020400                                                                          
020500     PERFORM Z-FINIT                                                      
020600                                                                          
020700     MOVE ZERO TO RETURN-CODE                                             
020800     GOBACK                                                               
020900     .                                                                    
021000                                                                          
021100                                                                          
021200 A-INIT SECTION.                                                          
021300                                                                          
021400     OPEN INPUT  W46356                                                   
021500                                                                          
021600     OPEN OUTPUT W4635J                                                   
021800                                                                          
021900     ACCEPT DAGENS-DATUM  FROM DATE                                       
022000     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
022010                                                                          
022020     MOVE ZERO  TO JUST-IDPRODNR                                          
022030                   JUST-IDKOLLI                                           
022040                   JUST-IDARTNR                                           
022050                   JUST-VKART                                             
022051                   JUST-KVLEVART                                          
022060                   JUST-IDRADNR                                           
022080                                                                          
022090     MOVE DAGENS-DATUM TO JUST-DAREGDAT                                   
022091     ADD 20000000      TO JUST-DAREGDAT                                   
022092                                                                          
022093     PERFORM S10-NOLLA-RADPOSTER                                          
022100     .                                                                    
022200                                                                          
022300                                                                          
022400 B-SUMMERA-VKART SECTION.                                                 
022500     MOVE 'B-SUMMERA-VKART'  TO CURRENT-SECTION                           
022600                                                                          
022700     MOVE IN-IDARTNR  TO W-IDARTNR                                        
022710                         RAD-IDARTNR  (RAD-IX)                            
022720     MOVE IN-IDRADNR  TO RAD-IDRADNR  (RAD-IX)                            
022730     MOVE IN-KVLEVART TO RAD-KVLEVART (RAD-IX)                            
022740     MOVE ZERO        TO RAD-VKART    (RAD-IX)                            
022800     PERFORM IMS-GU-WDK611                                                
022900     IF SEGMENT-FINNS                                                     
023001        COMPUTE W-VKART-SUM = W-VKART-SUM +                               
023002                             (CLAG-VKART * IN-KVLEVART)                   
023010        MOVE CLAG-VKART TO RAD-VKART (RAD-IX)                             
023100     END-IF                                                               
023110     ADD 1 TO RAD-IX                                                      
023200                                                                          
023300     .                                                                    
023400                                                                          
023500                                                                          
023600 C-SKRIV-JUSTERINGSFIL SECTION.                                           
023700     MOVE 'C-SKRIV-JUSTFIL'  TO CURRENT-SECTION                           
023800                                                                          
023900     COMPUTE JUST-VKORDBTO-KOLLI-ADJ = W-VKART-SUM / 1000                 
023910     MOVE 1 TO RAD-IX                                                     
023920     PERFORM UNTIL RAD-IDARTNR (RAD-IX) = ZERO OR                         
023930                   RAD-IX > RAD-IX-MAX                                    
023940        MOVE RAD-IDRADNR  (RAD-IX) TO JUST-IDRADNR                        
023950        MOVE RAD-IDARTNR  (RAD-IX) TO JUST-IDARTNR                        
023960        MOVE RAD-KVLEVART (RAD-IX) TO JUST-KVLEVART                       
023970        MOVE RAD-VKART    (RAD-IX) TO JUST-VKART                          
024000        PERFORM S11-SKRIV-W4635J                                          
024010        ADD 1 TO RAD-IX                                                   
024020     END-PERFORM                                                          
024100                                                                          
024200     .                                                                    
024300                                                                          
024400                                                                          
025480 Z-FINIT SECTION.                                                         
025500     CLOSE W46356                                                         
025600           W4635J                                                         
025800                                                                          
025900     MOVE 'S' TO POSTSUM-OPKOD                                            
026000     CALL POSTSUM USING POSTSUM-PARM                                      
026100     .                                                                    
026200                                                                          
026300                                                                          
026400 S01-LAES-W46356  SECTION.                                                
026500     READ W46356 INTO IN-AREA                                             
026600     AT END                                                               
026700        MOVE HIGH-VALUE TO IN-AREA                                        
026800        SET END-OF-W46356 TO TRUE                                         
026900                                                                          
027000     NOT AT END                                                           
027100        MOVE 'W46356'   TO POSTSUM-FDNAMN                                 
027200        MOVE 'W4635AD1' TO POSTSUM-DDNAMN2                                
027300        MOVE 'IN'       TO POSTSUM-TRANSTYP                               
027400        CALL POSTSUM USING POSTSUM-PARM                                   
027500     END-READ                                                             
027600     .                                                                    
027700                                                                          
027800                                                                          
027810 S10-NOLLA-RADPOSTER SECTION.                                             
027820                                                                          
027830     MOVE 1 TO RAD-IX                                                     
027840     PERFORM UNTIL RAD-IX > RAD-IX-MAX                                    
027841     MOVE ZERO TO  RAD-IDRADNR  (RAD-IX)                                  
027842                   RAD-IDARTNR  (RAD-IX)                                  
027843                   RAD-KVLEVART (RAD-IX)                                  
027844                   RAD-VKART    (RAD-IX)                                  
027850        ADD 1 TO RAD-IX                                                   
027893     END-PERFORM                                                          
027894                                                                          
027895     MOVE 1 TO RAD-IX                                                     
027896     .                                                                    
027897                                                                          
027898                                                                          
027900 S11-SKRIV-W4635J SECTION.                                                
028000                                                                          
028100     WRITE JUST-POST FROM JUST-AREA                                       
028200                                                                          
028300     MOVE 'ADJ'       TO POSTSUM-TRANSTYP                                 
028400     MOVE 'W4635J'    TO POSTSUM-FDNAMN                                   
028500     MOVE 'W4635AD2'  TO POSTSUM-DDNAMN2                                  
028600     CALL POSTSUM USING POSTSUM-PARM                                      
028700     .                                                                    
028800                                                                          
028900                                                                          
030100 S99-ABEND SECTION.                                                       
030200                                                                          
030300     SKIP2                                                                
030400     MOVE 'S' TO POSTSUM-OPKOD                                            
030500     CALL POSTSUM USING POSTSUM-PARM                                      
030600     CALL ABEND USING RKOD-ABEND                                          
030700     .                                                                    
030800                                                                          
030900                                                                          
031000* --- IMS SEKTIONER ---                                                   
031100                                                                          
031200 IMS-GU-WDK611 SECTION.                                                   
031300                                                                          
031400     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
031500          DELIMITED BY SIZE INTO SSA1                                     
031600     STRING 'WDK611  (KDSEGKEY =' W-KDSEGKEY-X ')'                        
031700          DELIMITED BY SIZE INTO SSA2                                     
031800     MOVE '  GE' TO GODK-STATUSKODER                                      
031900     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-WDK611 SSA1 SSA2               
032000     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
032100     PERFORM IMS-STATUSKONTROLL                                           
032200     .                                                                    
032300                                                                          
032400 IMS-STATUSKONTROLL SECTION.                                              
032500                                                                          
032600     SET STATUS-IX TO 1                                                   
032700     SEARCH GODK-STATUS                                                   
032800       AT END                                                             
032900         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
033000           DELIMITED BY SIZE INTO FELTEXT                                 
033100         DISPLAY FELTEXT                                                  
033200         CALL FELLOG                                                      
033300       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
033400         CONTINUE                                                         
033500     END-SEARCH                                                           
033600     .                                                                    
