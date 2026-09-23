001100 ID DIVISION.                                                             
001200 PROGRAM-ID.     W3718900.                                                
001300 AUTHOR.         BO HAMMARIN.                                             
001400 DATE-WRITTEN.   00/04/28.                                                
001500 DATE-COMPILED.                                                           
001700                                                                          
001800*    FUNKTION:                                                            
001900*        PGM UPPDATERAR UPPFÖLJNINGSREGISTER WDA9                         
002000*        MED NYINKOMNA RETURER                                            
002100*                                                                         
002400                                                                          
002600 ENVIRONMENT DIVISION.                                                    
002700                                                                          
002800 INPUT-OUTPUT SECTION.                                                    
003000 FILE-CONTROL.                                                            
003102*          --- NYINKOMNA RETURER                                          
003110     SELECT W37188                     ASSIGN TO W37189D1.                
003300     EJECT                                                                
003310                                                                          
003400 DATA DIVISION.                                                           
003600 FILE SECTION.                                                            
003702 FD  W37188                                                               
003703     RECORDING       F                                                    
003704     BLOCK CONTAINS  0.                                                   
003705                                                                          
003710*01  -COPY W37188      -L.                                                
003800     EJECT                                                                
003810                                                                          
003900 WORKING-STORAGE SECTION.                                                 
004100 77  IDPGM                       PIC X(8)    VALUE 'W3718900'.            
004200 01  CHKP-VAR.                                                            
004300     03 CHKP-MSG-IO-AREA-LENGTH  PIC S9(9)   VALUE +32 COMP SYNC.         
004400     03 CHKP-MSG-IO-AREA         PIC X(32)   VALUE SPACE.                 
004500     03 CHKP-AREA-LENGTH         PIC S9(9)   VALUE +32 COMP SYNC.         
004600     03 CHKP-AREA                PIC X(32)   VALUE SPACE.                 
004700     03 CHKP-ANT                 PIC S9(3)   VALUE +0   COMP-3.           
004800     03 CHKP-MAX                 PIC S9(3)   VALUE +500 COMP-3.           
004810     03 CHKP-TOT                 PIC S9(7)   VALUE ZERO.                  
004900 77  JA                          PIC X       VALUE 'J'.                   
005000 77  NEJ                         PIC X       VALUE 'N'.                   
005030 77  W-KVPOST-3146               PIC S9(7)   VALUE ZERO COMP-3.           
005040 77  INDX                        PIC S9(9)   VALUE ZERO COMP SYNC.        
005050 77  W37188-EOF-SW               PIC X       VALUE 'N'.                   
005060     88  END-OF-W37188                       VALUE 'J'.                   
005100                                                                          
005200 01  FELTEXT.                                                             
005300     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
005400     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
005601                                                                          
006000 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
006100 01  FILLER REDEFINES DAGENS-DATUM.                                       
006200     03  DAGENS-DATUM-AAR        PIC 9(2).                                
006300     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
006400     03  DAGENS-DATUM-DAG        PIC 9(2).                                
006401                                                                          
006410 01  W-W37188-KVPOST-IN          PIC S9(9)   VALUE ZERO.                  
006500     EJECT                                                                
006510                                                                          
006600 01  DYNAMISKA-SUBPROGRAM.                                                
006700*                                                                         
006800     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
006900     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
007010     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
007101     EJECT                                                                
007102                                                                          
007103*    --- PARAMETRAR TILL POSTSUM                                          
007104*                                                                         
007110*01  -COPY W0005   -PRE  POSTSUM-                                         
007401     EJECT                                                                
007402                                                                          
007403 01  IN-AREA-START               PIC X(24)   VALUE                        
007404                                             'IN-AREA-START'.             
007410*01  AREA -COPY W37188     -PRE IN-                                       
007500*                                                                         
007600     EJECT                                                                
007610                                                                          
007700 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
007900 01  NYCKLAR-TILL-DLI.                                                    
008000     03  W-WDGXKEY-X.                                                     
008001         05  FILLER              PIC X(4)    VALUE '3145'.                
008002         05  FILLER              PIC X(26)   VALUE LOW-VALUE.             
008003     03  W-IDARTNR-X.                                                     
008004         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
008005     03  W-DAAAPP-X.                                                      
008010         05  W-DAAAPP            PIC  9(6)   VALUE ZERO.                  
008100                                                                          
008200*    --- STATUS-KOD FRÅN IMS                                              
008300 01  STATUS-WS                   PIC XX.                                  
008400     88  SEGMENT-FINNS                       VALUE '  '.                  
008500     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
008600     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
008700     88  SEGMENT-SLUT                        VALUE 'GB'.                  
008800     88  IMS-EJ-OK                           VALUE 'XD'.                  
008910                                                                          
009000 01  GODK-STATUSKODER.                                                    
009100     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
009200                                                                          
009300 01  SSA1                        PIC X(64).                               
009400 01  SSA2                        PIC X(64).                               
009500     EJECT                                                                
009510                                                                          
009600*    --- IMS FUNKTIONSKODER                                               
009700*01  -COPY W0003                                                          
009800     EJECT                                                                
009900                                                                          
010000*    ---  DLI INPUT-OUTPUT AREA                                           
010201 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX3145'.                    
010202 01  DLI-IO-WDGX3145.                                                     
010203*    03  -COPY WDGX01                                                     
010204     EJECT                                                                
010205                                                                          
010206 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX3146'.                    
010207 01  DLI-IO-WDGX3146.                                                     
010208*    03  -COPY WDGX3146                                                   
010209     EJECT                                                                
010210                                                                          
010211 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDA901'.                      
010212 01  DLI-IO-WDA901.                                                       
010213*    03  -COPY WDA901                                                     
010214     EJECT                                                                
010215                                                                          
010216 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDA912'.                      
010217 01  DLI-IO-WDA912.                                                       
010220*    03  -COPY WDA912                                                     
010700     EJECT                                                                
010710                                                                          
010800 LINKAGE SECTION.                                                         
011000*01  -COPY W0009   -PRE MSG-                                              
011100     EJECT                                                                
011101                                                                          
011102*01  -COPY W0008  -PRE WDA9-                                              
011110     05  FILLER                  PIC X.                                   
011400     EJECT                                                                
011500                                                                          
011501*01  -COPY W0008  -PRE 3145-                                              
011502     05  FILLER                  PIC X.                                   
011503     EJECT                                                                
011504                                                                          
011505 PROCEDURE DIVISION  USING MSG-PCB                                        
011506                           3145-PCB WDA9-PCB.                             
011507 MAIN SECTION.                                                            
011510     ENTRY 'DLITCBL' USING MSG-PCB                                        
011520                           3145-PCB WDA9-PCB.                             
011600                                                                          
011900     PERFORM A-INIT                                                       
011910     PERFORM B-KOLLA-CHECKPOINT                                           
012000                                                                          
012010     PERFORM S01-LAS-W37188                                               
012100     PERFORM UNTIL END-OF-W37188                                          
012200       IF CHKP-ANT > CHKP-MAX                                             
012300         PERFORM X-TAG-CHECKPOINT                                         
012400       END-IF                                                             
012500                                                                          
012600       PERFORM C-BEARBETA                                                 
013110       PERFORM S01-LAS-W37188                                             
013200     END-PERFORM                                                          
013400                                                                          
013500     PERFORM Z-FINIT                                                      
013600                                                                          
013700     MOVE ZERO TO RETURN-CODE                                             
013800     GOBACK                                                               
013900     .                                                                    
014000     EJECT                                                                
014010                                                                          
014100 A-INIT SECTION.                                                          
014400     PERFORM IMS-RESTART                                                  
014601                                                                          
014610     OPEN INPUT W37188                                                    
015200                                                                          
015310     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
015600     .                                                                    
015800     EJECT                                                                
015810                                                                          
015820 B-KOLLA-CHECKPOINT SECTION.                                              
015840     PERFORM IMS-GHU-3145                                                 
015850     PERFORM IMS-GHU-3146                                                 
015851                                                                          
015860*----DET FÖRSTA SEGMENTET ----------------------------------------        
015870     MOVE 3146-KVPOST     TO W-KVPOST-3146                                
015880     IF W-KVPOST-3146 = ZERO                                              
015890       CONTINUE                                                           
015891     ELSE                                                                 
015892*-------PGM HAR ABENDAT OCH SKA NU OMSTARTAS                              
015893       PERFORM BA-FELHANTERING-INFIL                                      
015894       DISPLAY 'OMSTART AV PGM > POSTER ' W-KVPOST-3146                   
015895     END-IF                                                               
015896     .                                                                    
015897                                                                          
015898 BA-FELHANTERING-INFIL SECTION.                                           
015901     MOVE +0 TO INDX                                                      
015902                                                                          
015903     PERFORM UNTIL INDX = W-KVPOST-3146                                   
015904       PERFORM S01-LAS-W37188                                             
015905       ADD +1 TO INDX                                                     
015906     END-PERFORM                                                          
015907     .                                                                    
015908     EJECT                                                                
015909                                                                          
015910 C-BEARBETA SECTION.                                                      
016000     MOVE IN-IDARTNR                    TO W-IDARTNR                      
016010     PERFORM IMS-GHU-WDA901                                               
016020                                                                          
016100     IF SEGMENT-SAKNAS                                                    
016200       MOVE W-IDARTNR                   TO UPB-IDARTNR                    
016300       MOVE IN-IDFKNGRP                 TO UPB-IDFKNGRP                   
016400       PERFORM IMS-ISRT-WDA901                                            
016410       ADD +1                           TO CHKP-ANT                       
016500     END-IF                                                               
016600                                                                          
016610     MOVE IN-DAAAPP                     TO W-DAAAPP                       
016700     PERFORM IMS-GHU-WDA912                                               
016701                                                                          
016710     IF SEGMENT-SAKNAS                                                    
016720       PERFORM IMS-GHU-WDA901                                             
016730       MOVE IN-DAAAPP                   TO UPA-DAAAPP                     
016740       MOVE ZERO                        TO UPA-SUINVEST-DC                
016750                                           UPA-SULEVANT-DC                
016780                                           UPA-SUSKROT-DC                 
016781       MOVE IN-KVRETUR                  TO UPA-SUMOTT-CP                  
016790       PERFORM IMS-ISRT-WDA912                                            
016792     ELSE                                                                 
016793       ADD IN-KVRETUR                   TO UPA-SUMOTT-CP                  
016794       PERFORM IMS-REPL-WDA912                                            
016795     END-IF                                                               
016796                                                                          
016797     ADD +1                             TO CHKP-ANT                       
016800     .                                                                    
016901     EJECT                                                                
016902                                                                          
016903 Z-FINIT SECTION.                                                         
016904     PERFORM IMS-GHU-3145                                                 
016905     PERFORM IMS-GHU-3146                                                 
016906     MOVE ZERO     TO 3146-KVPOST                                         
016907     PERFORM IMS-REPL-3146                                                
016908                                                                          
016909     CLOSE W37188                                                         
016910                                                                          
016911     MOVE 'S' TO POSTSUM-OPKOD                                            
016912     CALL POSTSUM USING POSTSUM-PARM                                      
016913     .                                                                    
016914     EJECT                                                                
016915                                                                          
016916 S01-LAS-W37188  SECTION.                                                 
016917     READ W37188 INTO IN-AREA                                             
016918     AT END                                                               
016919        MOVE HIGH-VALUE   TO IN-W37188                                    
016920        SET END-OF-W37188 TO TRUE                                         
016921                                                                          
016922     NOT AT END                                                           
016923        MOVE 'W37188'     TO POSTSUM-FDNAMN                               
016924        MOVE 'W37189D1'   TO POSTSUM-DDNAMN2                              
016925        MOVE 'IN-'        TO POSTSUM-TRANSTYP                             
016926        CALL POSTSUM USING POSTSUM-PARM                                   
016927                                                                          
016928        ADD 1             TO W-W37188-KVPOST-IN                           
016929     END-READ                                                             
016930     .                                                                    
017200     EJECT                                                                
017210                                                                          
017300 X-TAG-CHECKPOINT   SECTION.                                              
017700     ADD CHKP-ANT TO CHKP-TOT                                             
017800     PERFORM IMS-GHU-3145                                                 
017900     PERFORM IMS-GHU-3146                                                 
017910     MOVE CHKP-TOT TO 3146-KVPOST                                         
017920     PERFORM IMS-REPL-3146                                                
018010                                                                          
018020     PERFORM IMS-CHECKPOINT                                               
018100     MOVE ZERO TO CHKP-ANT                                                
018300     .                                                                    
018400     EJECT                                                                
018410                                                                          
018500* --- IMS SEKTIONER ---                                                   
018600 IMS-GHU-3145 SECTION.                                                    
018701     STRING 'WDR501  (WDGXKEY  =' W-WDGXKEY-X ')'                         
018702          DELIMITED BY SIZE INTO SSA1                                     
018703     MOVE '    '           TO GODK-STATUSKODER                            
018704     CALL CBLTDLI USING GU 3145-PCB DLI-IO-WDGX3145 SSA1                  
018705     MOVE 3145-STATUS-CODE TO STATUS-WS                                   
018706     PERFORM IMS-STATUSKONTROLL                                           
018707     .                                                                    
018708                                                                          
018709 IMS-GHU-3146 SECTION.                                                    
018710     MOVE  'WDR540'        TO SSA1                                        
018711     MOVE '    '           TO GODK-STATUSKODER                            
018712     CALL CBLTDLI USING GHNP 3145-PCB DLI-IO-WDGX3146 SSA1                
018713     MOVE 3145-STATUS-CODE TO STATUS-WS                                   
018714     PERFORM IMS-STATUSKONTROLL                                           
018715     .                                                                    
018716                                                                          
018717 IMS-REPL-3146 SECTION.                                                   
018720     MOVE '  '             TO GODK-STATUSKODER                            
018721     CALL CBLTDLI USING REPL 3145-PCB DLI-IO-WDGX3146                     
018722     MOVE 3145-STATUS-CODE TO STATUS-WS                                   
018723     PERFORM IMS-STATUSKONTROLL                                           
018724     .                                                                    
018725     EJECT                                                                
018726                                                                          
018727 IMS-GHU-WDA901 SECTION.                                                  
018728     STRING 'WDA901  (IDARTNR  =' W-IDARTNR-X ')'                         
018729          DELIMITED BY SIZE INTO SSA1                                     
018730     MOVE '  GE'           TO GODK-STATUSKODER                            
018731     CALL CBLTDLI USING GHU WDA9-PCB DLI-IO-WDA901 SSA1                   
018732     MOVE WDA9-STATUS-CODE TO STATUS-WS                                   
018733     PERFORM IMS-STATUSKONTROLL                                           
018734     .                                                                    
018735                                                                          
018736 IMS-ISRT-WDA901 SECTION.                                                 
018737     MOVE 'WDA901 '        TO SSA1                                        
018738     MOVE '  '             TO GODK-STATUSKODER                            
018739     CALL CBLTDLI USING ISRT WDA9-PCB DLI-IO-WDA901 SSA1                  
018740     MOVE WDA9-STATUS-CODE TO STATUS-WS                                   
018741     PERFORM IMS-STATUSKONTROLL                                           
018742     .                                                                    
018743                                                                          
018744 IMS-GHU-WDA912 SECTION.                                                  
018745     STRING 'WDA901  (IDARTNR  =' W-IDARTNR-X ')'                         
018746          DELIMITED BY SIZE INTO SSA1                                     
018747     STRING 'WDA912  (DAAAPPR  =' W-DAAAPP-X ')'                          
018748          DELIMITED BY SIZE INTO SSA2                                     
018749     MOVE '  GE'           TO GODK-STATUSKODER                            
018750     CALL CBLTDLI USING GHU WDA9-PCB DLI-IO-WDA912 SSA1 SSA2              
018751     MOVE WDA9-STATUS-CODE TO STATUS-WS                                   
018752     PERFORM IMS-STATUSKONTROLL                                           
018753     .                                                                    
018754                                                                          
018755 IMS-ISRT-WDA912 SECTION.                                                 
018756     MOVE 'WDA912 '        TO SSA1                                        
018757     MOVE '  '             TO GODK-STATUSKODER                            
018758     CALL CBLTDLI USING ISRT WDA9-PCB DLI-IO-WDA912 SSA1                  
018759     MOVE WDA9-STATUS-CODE TO STATUS-WS                                   
018760     PERFORM IMS-STATUSKONTROLL                                           
018761     .                                                                    
018762                                                                          
018763 IMS-REPL-WDA912 SECTION.                                                 
018764     MOVE '  '             TO GODK-STATUSKODER                            
018765     CALL CBLTDLI USING REPL WDA9-PCB DLI-IO-WDA912                       
018766     MOVE WDA9-STATUS-CODE TO STATUS-WS                                   
018767     PERFORM IMS-STATUSKONTROLL                                           
018770     .                                                                    
018800     EJECT                                                                
018810                                                                          
018900 IMS-RESTART SECTION.                                                     
019100     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
019200     MOVE '  ' TO GODK-STATUSKODER                                        
019300     CALL CBLTDLI USING XRST MSG-PCB                                      
019400                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
019500                        CHKP-AREA-LENGTH CHKP-AREA                        
019600     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
019700     PERFORM IMS-STATUSKONTROLL                                           
019800     .                                                                    
019900                                                                          
020000 IMS-CHECKPOINT SECTION.                                                  
020200     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
020300     MOVE '  XD' TO GODK-STATUSKODER                                      
020400     CALL CBLTDLI USING CHKP MSG-PCB                                      
020500                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
020600                        CHKP-AREA-LENGTH CHKP-AREA                        
020700     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
020800     PERFORM IMS-STATUSKONTROLL                                           
020900                                                                          
021000     IF IMS-EJ-OK                                                         
021100       MOVE 'IMS-KONTROLLREGION EJ TILLGÄNGLIG' TO FELTEXT-STR            
021200       DISPLAY FELTEXT                                                    
021300       CALL FELLOG                                                        
021400     END-IF                                                               
021500     .                                                                    
021600     EJECT                                                                
021610                                                                          
021700 IMS-STATUSKONTROLL SECTION.                                              
021900     SET STATUS-IX TO 1                                                   
022000     SEARCH GODK-STATUS                                                   
022100       AT END                                                             
022200         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
022300           DELIMITED BY SIZE INTO FELTEXT                                 
022400         DISPLAY FELTEXT                                                  
022500         CALL FELLOG                                                      
022600       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
022700         CONTINUE                                                         
022800     END-SEARCH                                                           
022900     .                                                                    
