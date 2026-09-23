000100 ID DIVISION.                                                             
000200*                                                                         
000301 PROGRAM-ID.             W2218100.                                        
000400 AUTHOR.                 P-A HELGEGREN.                                   
000500 DATE-WRITTEN.           OKTOBER  1993.                                   
000600                                                                          
000700     REMARKS.                                                             
000800*                                                                         
000905*    KOPIA PÅ W2218000 (SUBPGM W2218110 KOPIA PÅ W2218010)                
001001*              HÄNDELSE-ROT 2217 PÅ XXBL ÄR HÄR                           
001101*              ERSATT MED HÄNDELSEROT 2217J PÅ XXBL                       
001201*              LOGIKEN ISTYRSEKTIONEN ÄR OCKSÅ ÄNDRAD                     
001300*                                                                         
001401*              IN-PARAMETER TILLAGD   (DAG/PER)                           
001501*                                                                         
001600*    FUNKTION: PROGRAMMET LÄSER EN LEVERANTÖR PÅ WLXXBK. MED DENNA        
001700*              SOM NYCKEL HÄMTAS SAMTLIGA ARTIKLAR MED DENNA LEV-         
001800*              RANTÖR PÅ WLXXBL.FÖR VARJE ARTIKEL SKAPAS EN UTPOST        
001900*              OCH SEGMENTET WLXXBL11 DELETAS (I NÄSTA PGM).              
002014*              LEVERANTÖRENS IDOVERFNR UPPDATERAS                         
002114*              (EV NOLLAS TISEND-PER OM EJ > DAGENS-DAT)                  
002200*              OCH REPLACE SKER AV SEGMENTET.(WLXXBK11).                  
002314*              (TISEND-SEN UPPDATERAS I PGM W22160.)                      
002414*                                                                         
002500*    SUBPROGRAM:                                                          
002606*            W2218110   - SKÖTER ALLA ANROP MOT IMS                       
002700*                                                                         
002800     EJECT                                                                
002900 ENVIRONMENT DIVISION.                                                    
003000                                                                          
003100 INPUT-OUTPUT SECTION.                                                    
003200                                                                          
003300 FILE-CONTROL.                                                            
003400                                                                          
003500*- - - - - - - - - - - - - - UTFILER:                                     
003600                                                                          
003702     SELECT  PARMIN                   ASSIGN TO W22181D1.                 
003801     SELECT  W22181                   ASSIGN TO W22181D2.                 
003901     SELECT  W22182                   ASSIGN TO W22181D3.                 
004024     SELECT  W22184                   ASSIGN TO W22181D4.                 
004124     EJECT                                                                
004224 DATA DIVISION.                                                           
004324     SKIP2                                                                
004424 FILE SECTION.                                                            
004524     SKIP3                                                                
004624                                                                          
004724 FD  PARMIN                                                               
004824     LABEL RECORD STANDARD                                                
004924     RECORDING F                                                          
005024     BLOCK CONTAINS 0.                                                    
005124     SKIP2                                                                
005224 01  FILLER                     PIC X(80).                                
005324     SKIP3                                                                
005424 FD  W22181                                                               
005524     LABEL RECORD STANDARD                                                
005624     RECORDING F                                                          
005724     BLOCK CONTAINS 0.                                                    
005824     SKIP2                                                                
005924*01  UTPOST2 -COPY W22181      -L.                                        
006000     EJECT                                                                
006100 FD  W22182                                                               
006200     LABEL RECORD STANDARD                                                
006300     RECORDING F                                                          
006400     BLOCK CONTAINS 0.                                                    
006500     SKIP2                                                                
006600*01  UTPOST3 -COPY W22182      -L.                                        
006700     EJECT                                                                
006824 FD  W22184                                                               
006924     LABEL RECORD STANDARD                                                
007024     RECORDING F                                                          
007124     BLOCK CONTAINS 0.                                                    
007224     SKIP2                                                                
007324*01  UTPOST4 -COPY W22184      -L.                                        
007424     EJECT                                                                
007500 WORKING-STORAGE SECTION.                                                 
007600     SKIP2                                                                
007720*    -COPY WY2000W1                                                       
007820     SKIP3                                                                
007900*- - - - - - - - - - - - - -  GENERERAT PROGRAM-NAMN                      
008001 77   PROGRAM-NAMN           VALUE 'W2218100'                             
008100                                 PIC X(8).                                
008200     SKIP2                                                                
008300*- - - - - - - - - - - - - -  GENERELLA KONSTANTER                        
008400                                                                          
008500 77  JA                          PIC X       VALUE 'J'.                   
008600 77  NEJ                         PIC X       VALUE 'N'.                   
008708                                                                          
008808 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
008908 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
009000                                                                          
009100 01  DATUMKORT-ID                PIC X(6)    VALUE 'WDATUM'.              
009200                                                                          
009300 01  SPAR-FALT.                                                           
009400     05 SPAR-TISEND-SEN-AAR-VV.                                           
009500        10  SPAR-TISEND-SEN-AAR  PIC 9(02).                               
009600        10  SPAR-TISEND-SEN-VV   PIC 9(02).                               
009700                                                                          
009800 01  ARBETS-FALT.                                                         
009900   03  WS-IDLPLAN                PIC 9(7)    VALUE ZERO.                  
010000   03  WS-IDAAVVLLL.                                                      
010100     05 WS-IDAAVV                PIC 9(4).                                
010200     05 WS-LLL                   PIC 9(3).                                
010300     EJECT                                                                
010400 01  DYNAMISKA-SUBPROGRAM.                                                
010500   03  POSTSUM                   PIC X(8)    VALUE 'POSTSUM '.            
010606   03  W2218110                  PIC X(8)    VALUE 'W2218110'.            
010700   03  DATKORT                   PIC X(8)    VALUE 'DATKORT '.            
010800   03  WDATKONV                  PIC X(8)    VALUE 'WDATKONV'.            
010901   03  ABEND                     PIC X(8)    VALUE 'ABEND   '.            
011000     SKIP3                                                                
011100 01  WS-DAGENS-DATUM.                                                     
011200     03  DAGENS-AA               PIC 9(02).                               
011300     03  DAGENS-MM               PIC 9(02).                               
011400     03  DAGENS-DD               PIC 9(02).                               
011500                                                                          
011600 01  DAGENS-DATUM                PIC 9(06).                               
011700                                                                          
011701 01  DAGENS-DAGNR                PIC 9(1) VALUE ZERO.                     
011720                                                                          
011800 01  DAGENS-AAR-VV.                                                       
011900     05  DAGENS-AAR              PIC 9(02).                               
012000     05  DAGENS-VV               PIC 9(02).                               
012102     SKIP3                                                                
012202 01  PARM-AREA.                                                           
012302     03  KORTYP                  PIC X(3)  VALUE SPACE.                   
012402     03  FILLER                  PIC X(73) VALUE SPACE.                   
012502     EJECT                                                                
012619*01  WS           -COPY W22181       -PRE UTPOST2-.                       
012700     EJECT                                                                
012800*01  WS           -COPY W22182       -PRE UTPOST3-.                       
012900     EJECT                                                                
013024*01  WS           -COPY W22184       -PRE UTPOST4-.                       
013124     EJECT                                                                
013200*- - - - - - - - - - - - - -  PARAMETRAR TILL POSTSUM                     
013300                                                                          
013419*01  -COPY W0005       -PRE POSTSUM-.                                     
013500     EJECT                                                                
013600*- - - - - - - - - - - - - -  PARAMETRAR TILL DATKORT                     
013700                                                                          
013800*01  -COPY WDATKORTC0  -PRE DATKORT-.                                     
013900     EJECT                                                                
014000*- - - - - - - - - - - - - -  PARAMETRAR TILL WDATKONV                    
014100                                                                          
014200*01  -COPY WDATAREA.                                                      
014300     EJECT                                                                
014400*- - - - - - - - - - - - - - PARAMETER-AREOR TILL IMS-SUBPGM              
014500*                                                                         
014600 01  FILLER                      PIC X(24)    VALUE                       
014700                                              '0-AREA-START'.             
014800                                                                          
014900*01  0-AREA    -COPY W221L800 -PRE CALL                                   
015000     EJECT                                                                
015100 01  FILLER                      PIC X(24)    VALUE                       
015200                                              '1-AREA-START'.             
015300                                                                          
015400*01  1-AREA    -COPY W221L801 -PRE CALL                                   
015500     EJECT                                                                
015600 01  FILLER                      PIC X(24)    VALUE                       
015700                                              '2-AREA-START'.             
015800                                                                          
015900*01  2-AREA    -COPY W221L802 -PRE CALL                                   
016000     EJECT                                                                
016100 01  FILLER                      PIC X(24)    VALUE                       
016200                                              '3-AREA-START'.             
016300                                                                          
016400*01  3-AREA    -COPY W221L803 -PRE CALL                                   
016800     EJECT                                                                
016900 LINKAGE SECTION.                                                         
017000     SKIP3                                                                
017100*01  -COPY  W0009  -PRE MSG-                                              
017200     SKIP3                                                                
017300 01  WLXXBL-PCB              PIC X(36).                                   
017400 01  WLXXBK-PCB              PIC X(36).                                   
017500 01  WLARTC-PCB              PIC X(36).                                   
017600     EJECT                                                                
017700 PROCEDURE DIVISION USING  MSG-PCB                                        
017800                        WLXXBL-PCB WLXXBK-PCB WLARTC-PCB.                 
017900     ENTRY 'DLITCBL' USING MSG-PCB                                        
018000                        WLXXBL-PCB WLXXBK-PCB WLARTC-PCB.                 
018100                                                                          
018200 STYR SECTION.                                                            
018300     SKIP3                                                                
018400     PERFORM A-INIT                                                       
018500                                                                          
018600     PERFORM B-LAES-WLXXBK01                                              
018700     PERFORM C-LAES-WLXXBK11                                              
018800                                                                          
018900     PERFORM UNTIL CALL0-KDSVAR NOT = CALL0-KDSVAR-OK                     
019013                                                                          
019113***     KOLLA OM PERIODSLUTSKÖRNING (DE SOM ÄR MÄRKTA SKALL               
019213***     DÅ MED) ELLER OM DATUM FINNS = DAGENS DATUM                       
019300                                                                          
019420        MOVE CALL1-TISEND-PER   TO TMP1-YYMMDD                            
019520        MOVE DAGENS-DATUM       TO TMP2-YYMMDD                            
019620        PERFORM WY2000P1                                                  
019721        IF (KORTYP         = 'PER' AND                                    
019830            (CALL1-FLLEVPLP = JA                                          
019930           OR CALL1-FLLEVVB = JA))                                        
020030                                                                          
020427                                                                          
020521        OR (TMP1-YYMMDD     > ZERO AND                                    
021021            TMP1-YYMMDD NOT > TMP2-YYMMDD)                                
021900                                                                          
022000           PERFORM D-LAES-WLXXBL-SKRIV-UTPOST                             
022113                                                                          
022200        END-IF                                                            
022210                                                                          
022211        IF (KORTYP         = 'VEC' AND                                    
022212            CALL1-FLLEVVB  = JA )                                         
022220                                                                          
022230           IF CALL1-KDVECKOSL  = 'D'                                      
022240             IF DAGENS-DAGNR = 1                                          
022242*--- LEV. MED DAGLIG SÄNDNING SKALL HA VECKOBATCH PÅ MÅNDAG.              
022243               PERFORM D-LAES-WLXXBL-SKRIV-UTPOST                         
022244             ELSE                                                         
022245               CONTINUE                                                   
022246             END-IF                                                       
022247           ELSE                                                           
022248             IF DAGENS-DAGNR = 1                                          
022249               CONTINUE                                                   
022250             ELSE                                                         
022251*--- ALLA ANDRA LEVERANTÖRER SKALL HA VECKOBATCH PÅ TORSDAG.              
022252               PERFORM D-LAES-WLXXBL-SKRIV-UTPOST                         
022253             END-IF                                                       
022254           END-IF                                                         
022255        END-IF                                                            
022260                                                                          
022300                                                                          
022400        PERFORM C-LAES-WLXXBK11                                           
022500     END-PERFORM                                                          
022600                                                                          
022700     PERFORM Z-FINIT                                                      
022800     MOVE ZERO TO RETURN-CODE                                             
022900     GOBACK                                                               
023000     .                                                                    
023100     EJECT                                                                
023200 A-INIT SECTION.                                                          
023300     SKIP3                                                                
023411     OPEN INPUT  PARMIN                                                   
023511     OPEN OUTPUT W22181                                                   
023600                 W22182                                                   
023724                 W22184                                                   
023824                                                                          
023924     MOVE PROGRAM-NAMN TO POSTSUM-PROGNAMN                                
024024                                                                          
024124*    ACCEPT DAGENS-DATUM FROM DATE                                        
024224                                                                          
024225*--  TAG REDA PÅ DAGENS DAGNUMMER, EJ FRÅN DATKORTET.                     
024226*--  W221V2 KÖRS PÅ MÅNDAG MED FREDAGENS DATUM I DATKORT.                 
024227*--  W221V2 KÖRS PÅ TORSDAG MED ONSDAGENS DATUM I DATKORT.                
024228                                                                          
024229     MOVE 'IDAG  '    TO DAT-KDDATFORM                                    
024230     CALL WDATKONV USING DAT-KDDATFORM                                    
024240                         DAT-I-TIDATUM                                    
024250                         DAT-O-TIDATUM                                    
024260                         DAT-KDSVAR                                       
024270                                                                          
024280     MOVE DAT-TID     TO DAGENS-DAGNR                                     
024290                                                                          
024324*    HÄMTA DAGENS-DATUM FRÅN DATKORT                                      
024424                                                                          
024524     CALL DATKORT USING PROGRAM-NAMN                                      
024624                        DATUMKORT-ID                                      
024724                        DATKORT-DATUMKORT                                 
024824                                                                          
024924     MOVE DATKORT-D-AAR       TO DAGENS-AAR                               
025024                                 DAGENS-AA                                
025124     MOVE DATKORT-D-VECKA     TO DAGENS-VV                                
025224     MOVE DATKORT-D-MAANAD    TO DAGENS-MM                                
025324     MOVE DATKORT-D-DAG       TO DAGENS-DD                                
025424     MOVE WS-DAGENS-DATUM     TO DAGENS-DATUM                             
025524                                                                          
025624     PERFORM S01-LAS-PARMIN                                               
025724     .                                                                    
025824     EJECT                                                                
025924 B-LAES-WLXXBK01 SECTION.                                                 
026024     SKIP3                                                                
026124     MOVE CALL0-LAES-WLXXBK01 TO CALL0-KDCALL                             
026224                                                                          
026324     CALL W2218110 USING CALL0-AREA                                       
026424                         CALL1-AREA                                       
026524                         WLXXBL-PCB                                       
026624                         WLXXBK-PCB                                       
026724                         WLARTC-PCB                                       
026824     .                                                                    
026924     EJECT                                                                
027024 C-LAES-WLXXBK11 SECTION.                                                 
027124     SKIP3                                                                
027224     MOVE CALL0-LAES-WLXXBK11 TO CALL0-KDCALL                             
027324                                                                          
027424     CALL W2218110 USING CALL0-AREA                                       
027524                         CALL1-AREA                                       
027624                         WLXXBL-PCB                                       
027724                         WLXXBK-PCB                                       
027824                         WLARTC-PCB                                       
027924                                                                          
028024***  FIX                                                                  
028124     IF CALL1-TISEND-PER NOT NUMERIC                                      
028224        MOVE ZERO TO CALL1-TISEND-PER                                     
028324     END-IF                                                               
028424     IF CALL1-FLLEVPLP NOT = JA                                           
028524        MOVE NEJ TO CALL1-FLLEVPLP                                        
028624     END-IF                                                               
028727     IF CALL1-FLLEVVB  NOT = JA                                           
028827        MOVE NEJ TO CALL1-FLLEVVB                                         
028927     END-IF                                                               
029024***  FIX                                                                  
029126                                                                          
029226     MOVE CALL1-IDLEVNR       TO UTPOST4-IDLEVNR                          
029326     MOVE CALL1-IDOVERFNR     TO UTPOST4-IDOVERFNR                        
029426     MOVE CALL1-TISEND-PER    TO UTPOST4-TISEND-PER                       
029526     MOVE CALL1-FLLEVPLP      TO UTPOST4-FLLEVPLP                         
029629     MOVE CALL1-FLLEVVB       TO UTPOST4-FLLEVVB                          
029729     .                                                                    
029829     EJECT                                                                
029929 D-LAES-WLXXBL-SKRIV-UTPOST SECTION.                                      
030029     SKIP3                                                                
030129     PERFORM DA-LAES-WLXXBL01                                             
030229     MOVE CALL1-IDLEVNR          TO CALL0-IDLEVNR                         
030329                                                                          
030429     PERFORM DB-LAES-WLXXBL11-KVAL                                        
030529                                                                          
030629     IF CALL0-KDSVAR = CALL0-KDSVAR-OK                                    
030729        PERFORM UNTIL CALL0-KDSVAR NOT = CALL0-KDSVAR-OK                  
030829           IF CALL1-KDEDI = 'O' OR 'F' OR 'E'                             
030929              MOVE CALL2-IDARTNR TO UTPOST2-IDARTNR                       
031029              MOVE CALL2-IDLEVNR TO UTPOST2-IDLEVNR                       
031129              MOVE CALL2-IDARTNR TO CALL0-IDARTNR                         
031229              PERFORM DC-LAES-WLARTC01                                    
031329              IF CALL0-KDSVAR = CALL0-KDSVAR-OK                           
031429                 MOVE CALL3-IDFTG TO UTPOST2-IDFTG                        
031529                 MOVE CALL3-KDPRODSL TO UTPOST2-KDPRODSL                  
031629              ELSE                                                        
031729                 MOVE ZERO TO UTPOST2-IDFTG                               
031829                              UTPOST2-KDPRODSL                            
031929              END-IF                                                      
032029                                                                          
032129              PERFORM DD-SKRIV-W22181-UTPOST                              
032229           END-IF                                                         
032329***        KDEDI = O , V , T , F   ????                                   
032429           PERFORM DE-DELETE-WLXXBL11                                     
032529           PERFORM DB-LAES-WLXXBL11-KVAL                                  
032629        END-PERFORM                                                       
032729                                                                          
032829        ADD +1 TO UTPOST4-IDOVERFNR                                       
032929        MOVE CALL1-TISEND-PER   TO TMP1-YYMMDD                            
033029        MOVE DAGENS-DATUM       TO TMP2-YYMMDD                            
033129        PERFORM WY2000P1                                                  
033229        IF TMP1-YYMMDD <= TMP2-YYMMDD                                     
033329           MOVE ZERO TO UTPOST4-TISEND-PER                                
033429           PERFORM DG-KOLL-EJ-STOPP                                       
033529        END-IF                                                            
033629        PERFORM DF-REPL-WLXXBK11                                          
033729     END-IF                                                               
033829                                                                          
033929     .                                                                    
034029     EJECT                                                                
034129 DA-LAES-WLXXBL01 SECTION.                                                
034229     SKIP3                                                                
034329     MOVE CALL0-LAES-WLXXBL01 TO CALL0-KDCALL                             
034429                                                                          
034529     CALL W2218110 USING CALL0-AREA                                       
034629                         CALL2-AREA                                       
034729                         WLXXBL-PCB                                       
034829                         WLXXBK-PCB                                       
034929                         WLARTC-PCB                                       
035029     .                                                                    
035129     EJECT                                                                
035229 DB-LAES-WLXXBL11-KVAL SECTION.                                           
035329     SKIP3                                                                
035429     MOVE CALL0-LAES-WLXXBL11-KVAL TO CALL0-KDCALL                        
035529                                                                          
035629     CALL W2218110 USING CALL0-AREA                                       
035729                         CALL2-AREA                                       
035829                         WLXXBL-PCB                                       
035929                         WLXXBK-PCB                                       
036029                         WLARTC-PCB                                       
036129     .                                                                    
036229     EJECT                                                                
036329 DE-DELETE-WLXXBL11 SECTION.                                              
036429     SKIP3                                                                
036529     MOVE CALL2-IDLEVNR  TO UTPOST3-IDLEVNR                               
036629     MOVE CALL2-IDARTNR  TO UTPOST3-IDARTNR                               
036729                                                                          
036829     PERFORM DEA-SKRIV-W22182-UTPOST                                      
036929                                                                          
037029*    DELETE SKER AV KÖRNINGSTEKNISKA SKÄL I EFTERFÖLJANDE                 
037129*    PROGRAM W22183                                                       
037229     .                                                                    
037329     SKIP3                                                                
037429 DEA-SKRIV-W22182-UTPOST SECTION.                                         
037529     SKIP3                                                                
037629     WRITE UTPOST3 FROM UTPOST3-WS                                        
037729                                                                          
037829     MOVE 'DLET'     TO POSTSUM-TRANSTYP                                  
037929     MOVE 'W22182'   TO POSTSUM-FDNAMN                                    
038029     MOVE 'W22181D3' TO POSTSUM-DDNAMN2                                   
038129     CALL POSTSUM USING POSTSUM-PARM                                      
038229     .                                                                    
038329     EJECT                                                                
038429 DF-REPL-WLXXBK11 SECTION.                                                
038529     SKIP3                                                                
038629     WRITE UTPOST4 FROM UTPOST4-WS                                        
038729                                                                          
038829     MOVE 'REPL'     TO POSTSUM-TRANSTYP                                  
038929     MOVE 'W22184'   TO POSTSUM-FDNAMN                                    
039029     MOVE 'W22181D4' TO POSTSUM-DDNAMN2                                   
039129     CALL POSTSUM USING POSTSUM-PARM                                      
039800     .                                                                    
039900     EJECT                                                                
040000 DC-LAES-WLARTC01 SECTION.                                                
040100     SKIP3                                                                
040200     MOVE CALL0-LAES-WLARTC01 TO CALL0-KDCALL                             
040300                                                                          
040406     CALL W2218110 USING CALL0-AREA                                       
040500                         CALL3-AREA                                       
040600                         WLXXBL-PCB                                       
040700                         WLXXBK-PCB                                       
040800                         WLARTC-PCB                                       
040900     .                                                                    
041000     EJECT                                                                
041100 DD-SKRIV-W22181-UTPOST SECTION.                                          
041200     SKIP3                                                                
041313*    POSTER SOM SKALL BLI ODETTE-LEVERANSPLANER                           
041413                                                                          
041500     WRITE UTPOST2 FROM UTPOST2-WS                                        
041600                                                                          
041712     MOVE 'ODET'     TO POSTSUM-TRANSTYP                                  
041800     MOVE 'W22181'   TO POSTSUM-FDNAMN                                    
041901     MOVE 'W22181D2' TO POSTSUM-DDNAMN2                                   
042000     CALL POSTSUM USING POSTSUM-PARM                                      
042115     .                                                                    
042215     EJECT                                                                
042315 DG-KOLL-EJ-STOPP       SECTION.                                          
042415     SKIP3                                                                
042515*    KONTROLL SÅ ATT INTE ALLA LEVPLANER BLIR STOPPADE                    
042615                                                                          
042715     IF CALL1-KDVECKOSL = 'P' AND                                         
042815        CALL1-FLLEVPLP  = NEJ                                             
042926        MOVE JA TO UTPOST4-FLLEVPLP                                       
043015     END-IF                                                               
043100     .                                                                    
043200     EJECT                                                                
043300 Z-FINIT   SECTION.                                                       
043400     SKIP3                                                                
043502     CLOSE  PARMIN                                                        
043600            W22181                                                        
043700            W22182                                                        
043824            W22184                                                        
043924*- - - - - - - - - - - - - - -  SKRIV UT ANTAL LÄSTA OCH                  
044024*                               SKRIVNA POSTER                            
044124     MOVE 'S' TO POSTSUM-OPKOD                                            
044224     CALL POSTSUM USING POSTSUM-PARM                                      
044324     .                                                                    
044424     EJECT                                                                
044524 S01-LAS-PARMIN   SECTION.                                                
044624     SKIP2                                                                
044724*    HÄR LÄSER VI IN PARAMETER, SÅ ATT VI VET OM                          
044824*    DETTA ÄR EN DAGLIG ELLER PERIODSLUTS-KÖRNING                         
044924                                                                          
045024     READ PARMIN INTO PARM-AREA                                           
045124                                                                          
045224     AT END                                                               
045324        DISPLAY '***   PARM SAKNAS   ***'                                 
045424        PERFORM S99-ABEND                                                 
045524     NOT AT END                                                           
045624        IF KORTYP NOT = 'DAG' AND                                         
045728           KORTYP NOT = 'PER' AND                                         
045828           KORTYP NOT = 'VEC'                                             
045928           DISPLAY '***   PARM FELAKTIG ***'                              
046028           PERFORM S99-ABEND                                              
046128        END-IF                                                            
046228                                                                          
046328        MOVE 'PARM'     TO POSTSUM-TRANSTYP                               
046428        MOVE 'PARMIN'   TO POSTSUM-FDNAMN                                 
046528        MOVE 'W22181D1' TO POSTSUM-DDNAMN2                                
046628        CALL POSTSUM USING POSTSUM-PARM                                   
046728     END-READ                                                             
046828     .                                                                    
046928     EJECT                                                                
047028 S99-ABEND SECTION.                                                       
047128     SKIP2                                                                
047228     SKIP2                                                                
047328     MOVE 'S' TO POSTSUM-OPKOD                                            
047428     CALL POSTSUM USING POSTSUM-PARM                                      
048001     CALL ABEND USING RKOD-ABEND-UTAN-DUMP                                
050000     .                                                                    
051020     EJECT                                                                
060020*    -COPY WY2000P1                                                       
