001000 ID DIVISION.                                                             
001100 PROGRAM-ID.     W6121400.                                                
001200 AUTHOR.         TOMMIE JIVARP.                                           
001300 DATE-WRITTEN.   98/01/08.                                                
001400 DATE-COMPILED.                                                           
001500                                                                          
001600*    FUNKTION:                                                            
001700*        PROGRAMMET KOLLAR LAGERPLATSHISTORIKEN (WDL6) FÖR                
001710*        ARTIKLAR SOM FALLIT FÖR 'RELOCATION RULES', SAMT LÄSER           
001900*        BENÄMNINGSREGISTRET (WDD3).                                      
002000*                                                                         
002101*        PROGRAMMET LÄSER      WLINLC (WDL6)                              
002110*        PROGRAMMET LÄSER      WLBENA (WDD3)                              
002200*                                                                         
002300*    ABENDKODER:                                                          
002400*        U0016 -  . . . .                                                 
002500*        U1000 -  . . . .                                                 
002600*                                                                         
002700                                                                          
002800     SKIP3                                                                
002900 ENVIRONMENT DIVISION.                                                    
003000     SKIP2                                                                
003100 INPUT-OUTPUT SECTION.                                                    
003200                                                                          
003300 FILE-CONTROL.                                                            
003401     SKIP2                                                                
003402*          --- INFIL MED LAGERPLATSFREKVENS                               
003403     SELECT W61213                     ASSIGN TO W61214D1.                
003404     SKIP2                                                                
003405*          --- UTFIL MED KOMPL. UPPG. IFRÅN WDD3 & WDL6                   
003410     SELECT W61217                     ASSIGN TO W61214D2.                
003600     EJECT                                                                
003700 DATA DIVISION.                                                           
003800     SKIP2                                                                
003900 FILE SECTION.                                                            
004001     SKIP3                                                                
004002 FD  W61213                                                               
004003     RECORDING       F                                                    
004004     BLOCK CONTAINS  0.                                                   
004005                                                                          
004006*01  -COPY W61211      -L.                                                
004007     SKIP3                                                                
004008 FD  W61217                                                               
004009     RECORDING       F                                                    
004010     BLOCK CONTAINS  0.                                                   
004011                                                                          
004020*01  POST -COPY W61211 -PRE  UT1-  -L.                                    
004100     EJECT                                                                
004110                                                                          
004200 WORKING-STORAGE SECTION.                                                 
004300                                                                          
004301                                                                          
004302*    -COPY WY2000W1                                                       
004303     SKIP3                                                                
004400 77  IDPGM                       PIC X(8)    VALUE 'W6121400'.            
004500 77  JA                          PIC X       VALUE 'J'.                   
004600 77  NEJ                         PIC X       VALUE 'N'.                   
004801                                                                          
004802 77  W61213-EOF-SW               PIC X       VALUE 'N'.                   
004810     88  END-OF-W61213                       VALUE 'J'.                   
004900     EJECT                                                                
005000 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
005100 01  FILLER REDEFINES DAGENS-DATUM.                                       
005200     03  DAGENS-DATUM-AAR        PIC 9(2).                                
005300     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
005400     03  DAGENS-DATUM-DAG        PIC 9(2).                                
005500     EJECT                                                                
005600 01  DYNAMISKA-SUBPROGRAM.                                                
005700*                                                                         
005800     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
005900     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
006000     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
006110     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
006120     03  WDAGKONV                PIC X(8)    VALUE 'WDAGKONV'.            
006200     SKIP2                                                                
006210*    -COPY WDAGAREA                                                       
006220                                                                          
006300*    --- PARAMETRAR TILL ABEND                                            
006400                                                                          
006500 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
006600 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
006700 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
006800     SKIP2                                                                
006900 01  FELTEXT.                                                             
007000     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
007100     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
007201     EJECT                                                                
007202*    --- PARAMETRAR TILL POSTSUM                                          
007203*                                                                         
007210*01  -COPY W0005   -PRE  POSTSUM-                                         
007401     EJECT                                                                
007403 01  IN1-AREA-START              PIC X(24)   VALUE                        
007404                                 'IN1-AREA-START  '.                      
007405     SKIP2                                                                
007406                                                                          
007407*01  AREA -COPY W61211     -PRE IN1-                                      
007408     EJECT                                                                
007409 01  UT1-AREA-START              PIC X(24)   VALUE                        
007410                                 'UT1-AREA-START  '.                      
007411     SKIP2                                                                
007412                                                                          
007420*01  AREA -COPY W61211     -PRE UT1-                                      
007500     EJECT                                                                
007600*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
007700*                                                                         
007800     EJECT                                                                
007900 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
008000     SKIP3                                                                
008100 01  NYCKLAR-TILL-DLI.                                                    
008201     03  W-IDARTNR-X.                                                     
008202         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
008203     03  W-DAINLEV-X.                                                     
008204         05  W-DAINLEV           PIC 9(16)   VALUE ZERO.                  
008207     03  W-IDSKYLT-X.                                                     
008210         05  W-IDSKYLT           PIC X(3)    VALUE SPACE.                 
008300     SKIP2                                                                
008400*    --- STATUS-KOD FRÅN IMS                                              
008500 01  STATUS-WS                   PIC XX.                                  
008600     88  SEGMENT-FINNS                       VALUE '  '.                  
008700     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
008800     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
008900     SKIP2                                                                
008910                                                                          
009000 01  GODK-STATUSKODER.                                                    
009100     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
009200     SKIP3                                                                
009300 01  SSA1                        PIC X(64).                               
009400 01  SSA2                        PIC X(64).                               
009500     EJECT                                                                
009600*    --- IMS FUNKTIONSKODER                                               
009700*01  -COPY W0003                                                          
009800     EJECT                                                                
010000*    ---  DLI INPUT-OUTPUT AREA                                           
010101 01  FILLER         PIC X(16) VALUE 'DLI-IO-WLINLC01'.                    
010102 01  DLI-IO-WLINLC01.                                                     
010103*    03  -COPY WDL601  -PRE INLC-                                         
010104     EJECT                                                                
010105 01  FILLER         PIC X(16) VALUE 'DLI-IO-WLINLC11'.                    
010106 01  DLI-IO-WLINLC11.                                                     
010107*    03  -COPY WDL611  -PRE INLC-                                         
010112 01  FILLER         PIC X(16) VALUE 'DLI-IO-WLBENA11'.                    
010113 01  DLI-IO-WLBENA11.                                                     
010120*    03  -COPY WDD311  -PRE BENA-                                         
010400     EJECT                                                                
010410                                                                          
010500 LINKAGE SECTION.                                                         
010600                                                                          
010701     EJECT                                                                
010702*01  -COPY W0008  -PRE INLC-                                              
010703     05  FILLER                  PIC X.                                   
010704     EJECT                                                                
010705*01  -COPY W0008  -PRE BENA-                                              
010710     05  FILLER                  PIC X.                                   
010800     EJECT                                                                
010900                                                                          
010901 PROCEDURE DIVISION  USING INLC-PCB BENA-PCB.                             
010902 MAIN SECTION.                                                            
010910     ENTRY 'DLITCBL' USING INLC-PCB BENA-PCB.                             
011000                                                                          
011200                                                                          
011300     PERFORM A-INIT                                                       
011400                                                                          
011510     PERFORM S01-LAES-W61213                                              
011600     PERFORM UNTIL END-OF-W61213                                          
011601     MOVE IN1-IDARTNR TO W-IDARTNR-X                                      
011602       PERFORM B-TILLDELA-KLAR-DATA                                       
011603       PERFORM IMS-GU-BENA01-BSEQ                                         
011604       IF SEGMENT-FINNS                                                   
011605         MOVE BENA-TEXT-BEART TO UT1-BEART-ENG                            
011606       ELSE                                                               
011607         CONTINUE                                                         
011608       END-IF                                                             
011700       PERFORM IMS-GU-INLC01                                              
011800       IF SEGMENT-FINNS                                                   
011900         PERFORM IMS-GNP-INLC11                                           
011912         MOVE INLC-INL-TIINLINL   TO TMP1-YYMMDD                          
011913         MOVE DAG-TIAAMMDD-FOM    TO TMP2-YYMMDD                          
011914         PERFORM WY2000P1                                                 
011915*****    KOLL OM LAGERPLACERING ÄNDRATS SENASTE ÅRET    *****             
012000         PERFORM UNTIL SEGMENT-SAKNAS OR                                  
012100           (TMP1-YYMMDD < TMP2-YYMMDD AND                                 
012101           INLC-INL-TIINLINL NOT = ZERO) OR                               
012110           (INLC-INL-IDDC = IN1-IDDC AND                                  
012200            NOT (INLC-INL-ADLAGOMR = IN1-ADLAGOMR                         
012300            AND  INLC-INL-ADGANG   = IN1-ADGANG                           
012301            AND  INLC-INL-ADPLATS  = IN1-ADPLATS))                        
012312           PERFORM IMS-GNP-INLC11                                         
012313           MOVE INLC-INL-TIINLINL   TO TMP1-YYMMDD                        
012314           MOVE DAG-TIAAMMDD-FOM    TO TMP2-YYMMDD                        
012315           PERFORM WY2000P1                                               
012316         END-PERFORM                                                      
012317         IF SEGMENT-FINNS                                                 
012318           PERFORM C-TESTA-WDL6-DATA                                      
012319         END-IF                                                           
012329         PERFORM S11-SKRIV-W61217                                         
012330       END-IF                                                             
012340       PERFORM S01-LAES-W61213                                            
012400     END-PERFORM                                                          
012500                                                                          
012600                                                                          
012700     PERFORM Z-FINIT                                                      
012800                                                                          
012900     MOVE ZERO TO RETURN-CODE                                             
013000     GOBACK                                                               
013100     .                                                                    
013200     EJECT                                                                
013210                                                                          
013300 A-INIT SECTION.                                                          
013401                                                                          
013410     OPEN INPUT  W61213                                                   
013501                                                                          
013510     OPEN OUTPUT W61217                                                   
013600                                                                          
013700     ACCEPT DAGENS-DATUM  FROM DATE                                       
013810     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
013811     MOVE 'GB' TO W-IDSKYLT                                               
013812                                                                          
013813     MOVE 003 TO DAG-KDCALL                                               
013814     MOVE 366 TO DAG-KVKALDAG                                             
013815     MOVE DAGENS-DATUM TO DAG-TIAAMMDD-TOM                                
013820     CALL WDAGKONV USING DAG-KDCALL                                       
013830                         DAG-DATUM-AREA                                   
013840                         DAG-KDSVAR                                       
014000     .                                                                    
014100     EJECT                                                                
014110                                                                          
014200 B-TILLDELA-KLAR-DATA SECTION.                                            
014210                                                                          
014220     MOVE IN1-IDARTNR     TO UT1-IDARTNR                                  
014230     MOVE IN1-IDDC        TO UT1-IDDC                                     
014240     MOVE IN1-ADLAGOMR    TO UT1-ADLAGOMR                                 
014250     MOVE IN1-ADGANG      TO UT1-ADGANG                                   
014260     MOVE IN1-ADPLATS     TO UT1-ADPLATS                                  
014270     MOVE IN1-KDSTOR      TO UT1-KDSTOR                                   
014280     MOVE IN1-KDFREQ      TO UT1-KDFREQ                                   
014293     MOVE IN1-KVPB-REF    TO UT1-KVPB-REF                                 
014294     MOVE IN1-FLSEASON    TO UT1-FLSEASON                                 
014295     MOVE IN1-KDTECKEN    TO UT1-KDTECKEN                                 
014296                                                                          
014297*   AKUT ÄNDRING 030207   JOHAN L                                         
014298     MOVE ZERO            TO UT1-TIINLINL-OLD                             
014299                             UT1-ADLAGOMR-OLD                             
014300                             UT1-ADGANG-OLD                               
014301                             UT1-ADPLATS-OLD                              
014302     .                                                                    
014303     EJECT                                                                
014304                                                                          
014305 C-TESTA-WDL6-DATA SECTION.                                               
014306                                                                          
014307     MOVE INLC-INL-TIINLINL   TO TMP1-YYMMDD                              
014308     MOVE DAG-TIAAMMDD-FOM    TO TMP2-YYMMDD                              
014309     PERFORM WY2000P1                                                     
014310     IF TMP1-YYMMDD >= TMP2-YYMMDD AND                                    
014311       (INLC-INL-IDDC = IN1-IDDC AND                                      
014312        NOT (INLC-INL-ADLAGOMR = IN1-ADLAGOMR                             
014313        AND  INLC-INL-ADGANG   = IN1-ADGANG                               
014314        AND  INLC-INL-ADPLATS  = IN1-ADPLATS))                            
014315       MOVE INLC-INL-TIINLINL  TO UT1-TIINLINL-OLD                        
014316       MOVE INLC-INL-ADLAGOMR  TO UT1-ADLAGOMR-OLD                        
014317       MOVE INLC-INL-ADGANG    TO UT1-ADGANG-OLD                          
014318       MOVE INLC-INL-ADPLATS   TO UT1-ADPLATS-OLD                         
014319     ELSE                                                                 
014320       MOVE ZERO               TO UT1-TIINLINL-OLD                        
014321                                  UT1-ADLAGOMR-OLD                        
014322                                  UT1-ADGANG-OLD                          
014323                                  UT1-ADPLATS-OLD                         
014324     END-IF                                                               
014325     .                                                                    
014326     EJECT                                                                
014327                                                                          
014328 Z-FINIT SECTION.                                                         
014330     CLOSE W61213                                                         
014340           W61217                                                         
014401     SKIP2                                                                
014402     MOVE 'S' TO POSTSUM-OPKOD                                            
014410     CALL POSTSUM USING POSTSUM-PARM                                      
014500     .                                                                    
014601     EJECT                                                                
014602                                                                          
014603 S01-LAES-W61213  SECTION.                                                
014604     READ W61213 INTO IN1-AREA                                            
014605     AT END                                                               
014606        MOVE HIGH-VALUE TO IN1-AREA                                       
014607        SET END-OF-W61213 TO TRUE                                         
014608                                                                          
014609     NOT AT END                                                           
014610        MOVE 'W61213' TO POSTSUM-FDNAMN                                   
014611        MOVE 'W61214D1' TO POSTSUM-DDNAMN2                                
014613        MOVE SPACE TO POSTSUM-TRANSTYP                                    
014614        CALL POSTSUM USING POSTSUM-PARM                                   
014615     END-READ                                                             
014620     .                                                                    
014701     EJECT                                                                
014702                                                                          
014703 S11-SKRIV-W61217 SECTION.                                                
014704                                                                          
014708     WRITE UT1-POST FROM UT1-AREA                                         
014710     MOVE SPACE TO POSTSUM-TRANSTYP                                       
014711     MOVE 'W61214' TO POSTSUM-FDNAMN                                      
014712     MOVE 'W61214D2' TO POSTSUM-DDNAMN2                                   
014713     CALL POSTSUM USING POSTSUM-PARM                                      
014720     .                                                                    
014900     EJECT                                                                
014910                                                                          
015000 S99-ABEND SECTION.                                                       
015100                                                                          
015201     SKIP2                                                                
015202     MOVE 'S' TO POSTSUM-OPKOD                                            
015210     CALL POSTSUM USING POSTSUM-PARM                                      
015300     CALL ABEND USING RKOD-ABEND                                          
015400     .                                                                    
015500     EJECT                                                                
015600* --- IMS SEKTIONER ---                                                   
015700     SKIP3                                                                
015801     EJECT                                                                
015802                                                                          
015803 IMS-GU-INLC01 SECTION.                                                   
015804                                                                          
015805     STRING 'WLINLC01(IDARTNR  =' W-IDARTNR-X ')'                         
015806          DELIMITED BY SIZE INTO SSA1                                     
015807     MOVE '  GE' TO GODK-STATUSKODER                                      
015808     CALL CBLTDLI USING GU INLC-PCB DLI-IO-WLINLC01 SSA1                  
015809     MOVE INLC-STATUS-CODE TO STATUS-WS                                   
015810     PERFORM IMS-STATUSKONTROLL                                           
015811     .                                                                    
015812     EJECT                                                                
015813                                                                          
015814 IMS-GNP-INLC11 SECTION.                                                  
015815                                                                          
015816     MOVE 'WLINLC11 ' TO SSA1                                             
015818     MOVE '  GE' TO GODK-STATUSKODER                                      
015819     CALL CBLTDLI USING GNP INLC-PCB DLI-IO-WLINLC11 SSA1                 
015820     MOVE INLC-STATUS-CODE TO STATUS-WS                                   
015821     PERFORM IMS-STATUSKONTROLL                                           
015822     .                                                                    
015823     EJECT                                                                
015824                                                                          
015825 IMS-GU-BENA01-BSEQ SECTION.                                              
015826                                                                          
015827     STRING 'WLBENA01(WDD3BSEQ =' W-IDARTNR-X ')'                         
015828          DELIMITED BY SIZE INTO SSA1                                     
015831     STRING 'WLBENA11(IDSKYLT  =' W-IDSKYLT-X ')'                         
015832          DELIMITED BY SIZE INTO SSA2                                     
015833     MOVE '  GE'               TO GODK-STATUSKODER                        
015834     CALL CBLTDLI USING GU BENA-PCB DLI-IO-WLBENA11 SSA1 SSA2             
015839     MOVE BENA-STATUS-CODE     TO STATUS-WS                               
015840     PERFORM IMS-STATUSKONTROLL                                           
015841     .                                                                    
015842     EJECT                                                                
015843                                                                          
015870                                                                          
016000 IMS-STATUSKONTROLL SECTION.                                              
016100                                                                          
016200     SET STATUS-IX TO 1                                                   
016300     SEARCH GODK-STATUS                                                   
016400       AT END                                                             
016500         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
016600           DELIMITED BY SIZE INTO FELTEXT                                 
016700         DISPLAY FELTEXT                                                  
016800         CALL FELLOG                                                      
016900       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
017000         CONTINUE                                                         
017100     END-SEARCH                                                           
017200     .                                                                    
017210     EJECT                                                                
017300*    -COPY WY2000P1                                                       
