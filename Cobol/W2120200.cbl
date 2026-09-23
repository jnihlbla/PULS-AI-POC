000100 ID DIVISION.                                                             
000200 PROGRAM-ID.         W2120200.                                            
000300 AUTHOR.             BARBRO SWAHNBERG, DATA LOGIC AB.                     
000400 DATE-WRITTEN        OKT 1978.                                            
000500*    REMARKS.                                                             
000600*    FUNKTION:                                                            
000700*        PROGRAMMET LÄSER TRANSAKTIONSFILEN W21201 INNEHÅLLANDE           
000800*        NYUPPLÄGG OCH UPPDATERING AV BESTÄLLNINGAR OCH AVTAL.            
000900*        (R22 OCH R23:OR)                                                 
001000*        DESSA UPPDATERAR ARTIKELREGISTRET WLARTC.                        
001100*        KONTROLL AV LEVERANTÖRSNR SKER MOT LEVERANTÖRSREGISTRET          
001200*                                                                         
001300*        FELMEDDELANDEN OCH ÖVRIGA MEDDELANDE SKRIVS PÅ FILEN             
001400*        W21205.                                                          
001500*        INFO TILL NYPON OM NYA AVTAL OCH BESTÄLLNINGAR (R22/R23)         
001600*        SKRIVS PÅ FIL W21211                                             
001700*        POSTER FÖR UPPDATERING I EFTERFÖLJANDE BMP                       
001800*        SKRIVS PÅ FIL W21203                                             
001900*        TRANSAR FÖR BYTE AV HUVUDLEVERANTÖR (R01/2222)                   
002000*        SKRIVS PÅ FIL W21231                                             
002100*        TRANSAR FÖR OMSPEC AV LEVERANSPLAN (2204 OCH 2213)               
002200*        SKRIVS PÅ FIL W22232                                             
002300*                                                                         
002400*        OKT-04 ÄNDRINGAR GJORDA I BCD-ANNU-BESTID FÖR ATT                
002500*               SE TILL ATT KDAVT INTE BLIR 0 FÖRRÄN ALLA                 
002600*               AVTAL ÄR BORTA// JOHAN NIHLBLAD                           
002700*                                                                         
002800*        150422 E'TRACKER 10130993                                        
002900*               REDUCE NUMBER OF DELIVERY SCHEDULES                       
003000*                                                                         
003100*        151228 E'TRACKER 10243132                                        
003200*               CHINA EXPORT PROJECT 2015                                 
003300*                                                                         
003400     EJECT                                                                
003500 ENVIRONMENT DIVISION.                                                    
003600 INPUT-OUTPUT SECTION.                                                    
003700 FILE-CONTROL.                                                            
003800*                            *** UPPDATERINGAR TILL BESTÄLLN ***          
003900*                            *** OCH AVTAL PÅ ARTIKELREG     ***          
004000*                            *** FRÅN TRATTEN OCH ONLINE     ***          
004100*                            *** INPUT                       ***          
004200     SELECT W092XX ASSIGN W21202D1.                                       
004300*                                                                         
004400*                            *** FELPOSTER TILL TRATTEN      ***          
004500*                            *** OUTPUT                      ***          
004600     SELECT W21205 ASSIGN W21202D3.                                       
004700*                                                                         
004800*                            *** NYA AVTAL OCH BESTÄLLNINGAR ***          
004900*                            *** TILL NYPON (R22)            ***          
005000*                            *** OUTPUT                      ***          
005100     SELECT W21211 ASSIGN W21202D4.                                       
005200*                                                                         
005300*                            *** UPPDATERINGSPOSTER FÖR ARTC ***          
005400*                            *** TILL EFTERFÖLJANDE BMP      ***          
005500*                            *** OUTPUT                      ***          
005600     SELECT W21203 ASSIGN W21202D5.                                       
005700*                                                                         
005800*                            *** R01/2222 TILL W213          ***          
005900*                            *** OUTPUT                      ***          
006000     SELECT W21231 ASSIGN W21202D6.                                       
006100*                                                                         
006200*                            *** 2204 OCH 2213 FÖR UPPLÄGG   ***          
006300*                            *** TILL EFTERFÖLJANDE BMP      ***          
006400*                            *** OUTPUT                      ***          
006500     SELECT W21232 ASSIGN W21202D7.                                       
006600*                                                                         
006700     EJECT                                                                
006800 DATA DIVISION.                                                           
006900 FILE SECTION.                                                            
007000     SKIP2                                                                
007100 FD  W092XX                                                               
007200     RECORDING F                                                          
007300     BLOCK 0                                                              
007400     LABEL RECORD STANDARD.                                               
007500*01  -COPY W212R22      -L.                                               
007600*01  -COPY W212R23      -L.                                               
007700     SKIP3                                                                
007800 FD  W21205                                                               
007900     RECORDING V                                                          
008000     BLOCK 0                                                              
008100     LABEL RECORD STANDARD.                                               
008200*01  W092W001-POST  -COPY W092W001   -L.                                  
008300     EJECT                                                                
008400 FD  W21211                                                               
008500     RECORDING F                                                          
008600     BLOCK 0                                                              
008700     LABEL RECORD STANDARD.                                               
008800*01  POST -COPY W212R22      -L -PRE NYPON-R22-                           
008900*01  POST -COPY W212R23      -L -PRE NYPON-R23-                           
009000     EJECT                                                                
009100 FD  W21203                                                               
009200     RECORDING V                                                          
009300     BLOCK 0                                                              
009400     LABEL RECORD STANDARD.                                               
009500*01  BEST-POST  -COPY W2120301     -L -PRE DBUP-                          
009600*01  AVT-POST   -COPY W2120302     -L -PRE DBUP-                          
009700*01  LEVPL-POST -COPY W2120303     -L -PRE DBUP-                          
009800*01  MTRLF-POST -COPY W2120304     -L -PRE DBUP-                          
009900     EJECT                                                                
010000 FD  W21231                                                               
010100     RECORDING F                                                          
010200     BLOCK 0                                                              
010300     LABEL RECORD STANDARD.                                               
010400*01  POST -COPY W213R01      -L -PRE R01-                                 
010500     EJECT                                                                
010600 FD  W21232                                                               
010700     RECORDING F                                                          
010800     BLOCK 0                                                              
010900     LABEL RECORD STANDARD.                                               
011000*01  POST -COPY W2132204     -L -PRE 2204-                                
011100*01  POST -COPY W2132213     -L -PRE 2213-                                
011200     EJECT                                                                
011300 WORKING-STORAGE SECTION.                                                 
011400     SKIP2                                                                
011500*    -- CHECKED BY WY2000                                                 
011600     SKIP3                                                                
011700 77  TRAEFF                  PIC X.                                       
011800     SKIP2                                                                
011900                                                                          
012000 01  RKOD                    PIC S9(4)   VALUE +0    COMP SYNC.           
012100     SKIP2                                                                
012200 01  KONSTANTER.                                                          
012300     03  JA                  PIC X       VALUE 'J'.                       
012400     03  NEJ                 PIC X       VALUE 'N'.                       
012500     03  USA                 PIC 999     VALUE 111.                       
012600     03  CAN                 PIC 999     VALUE 122.                       
012700     SKIP2                                                                
012800 01  EOF-SWITCHAR.                                                        
012900     03  W092XX-EOF          PIC X       VALUE 'N'.                       
013000     SKIP2                                                                
013100 01  BEST-INDEX.                                                          
013200     03  BEST-IX             PIC S9      VALUE +0.                        
013300     03  HELP-IX             PIC S9      VALUE +0.                        
013400     SKIP2                                                                
013500 01  DYNAMISKA-SUBPROGRAM.                                                
013600     03  W2120210            PIC X(8)    VALUE 'W2120210'.                
013700     03  POSTSUM             PIC X(8)    VALUE 'POSTSUM '.                
013800     03  ABEND               PIC X(8)    VALUE 'ABEND   '.                
013900     03  DATKORT             PIC X(8)    VALUE 'DATKORT '.                
014000*                                                                         
014100 01  TIAVV.                                                               
014200     03  AAR                 PIC 9.                                       
014300     03  VECKA               PIC 9(2).                                    
014400                                                                          
014500 01  SW-AVT-BORT             PIC X       VALUE 'N'.                       
014600*                                                                         
014700 01  DAGENS-DATUM            PIC 9(6).                                    
014800 01    FILLER  REDEFINES DAGENS-DATUM.                                    
014900     03  DAGENS-AAR          PIC 9(2).                                    
015000     03  DAGENS-MAANAD       PIC 9(2).                                    
015100     03  DAGENS-DAGNR        PIC 9(2).                                    
015200*                                                                         
015300*    AREOR FÖR ATT AVGÖRA OM BESTID/AVTALSID FINNS                        
015400*                                                                         
015500 01  IDBEST-AREOR.                                                        
015600     03  IDBEST-FINNS        PIC X.                                       
015700     03  IDBEST-JFR          PIC S9(13)              COMP-3.              
015800     03  IDBEST-IX           PIC 9.                                       
015900     SKIP3                                                                
016000 01  W-IDAVTAL-RED           PIC 9(13).                                   
016100 01  W-IDAVTAL REDEFINES W-IDAVTAL-RED.                                   
016200     03  FILLER              PIC X(1).                                    
016300     03  W-PREFIX            PIC X(3).                                    
016400     03  W-AVTALNR           PIC X(6).                                    
016500     03  W-SUFFIX            PIC X(3).                                    
016600     SKIP3                                                                
016700 01  W-IDARTNR-8             PIC 9(8)  VALUE ZERO.                        
016800 01  W-IDLEVNR-AVT           PIC X(5)  VALUE SPACE.                       
016900 01  WS-IDLEVNR-MOTSV        PIC X(5)  VALUE SPACE.                       
017000     EJECT                                                                
017100*    AREA FÖR ATT AVGÖRA OM BYTE AV                                       
017200*    HUVUDLEVERANTÖR                                                      
017300*                                                                         
017400 01  BEST-ID                 PIC 9(13).                                   
017500 01  BYTE-IDBEST     REDEFINES BEST-ID.                                   
017600     03    FILLER            PIC X.                                       
017700     03  BYTE-IDINK          PIC 9(3).                                    
017800     03  BYTE-BESTNR         PIC 9(6).                                    
017900     03  BYTE-SUFFIX         PIC 9(3).                                    
018000*                                                                         
018100*  KDCALL-VÄRDEN                                                          
018200*                                                                         
018300*    -COPY W212CALL                                                       
018400     EJECT                                                                
018500*                                                                         
018600*    BEHANDLINGSKODER I R22                                               
018700*                                                                         
018800 01  BEHANDLINGSKODER.                                                    
018900     03  NYTT-BEST           PIC S9(1)   VALUE +1    COMP-3.              
019000     03  BEKR-BEST           PIC S9(1)   VALUE +2    COMP-3.              
019100     03  JUSTE-UPP           PIC S9(1)   VALUE +3    COMP-3.              
019200     03  JUSTE-NED           PIC S9(1)   VALUE +4    COMP-3.              
019300     03  NYTT-ANNU           PIC S9(1)   VALUE +5    COMP-3.              
019400     03  BEKR-ANNU           PIC S9(1)   VALUE +6    COMP-3.              
019500     EJECT                                                                
019600*    PARAMETRAR TILL POSTSUM                                              
019700*                                                                         
019800*01  -COPY W0005        -PRE POSTSUM-.                                    
019900     EJECT                                                                
020000                                                                          
020100*01  -COPY WWPRODSL                                                       
020200                                                                          
020300*01  -COPY WWDCKONS                                                       
020400     SKIP2                                                                
020500*                                                                         
020600*    ARBETS-AREA INPUT                                                    
020700*                                                                         
020800*01  AREA -COPY W212R22    -PRE I01R22-.                                  
020900     EJECT                                                                
021000*01  AREA -COPY W212R23    -RED I01R22-AREA -PRE I01R23-.                 
021100     EJECT                                                                
021200*    MELLANLAGRINGSFÄLT KORTTYP U61                                       
021300*                                                                         
021400     EJECT                                                                
021500*                                                                         
021600*    ARBETSAREA OUTPUT DATABASUPPDATERINGAR                               
021700*                                                                         
021800 01  FILLER                 PIC X(16)  VALUE 'W21203-BEST     '.          
021900*01  BEST-AREA -COPY W2120301   -PRE DBUP-                                
022000     EJECT                                                                
022100 01  FILLER                 PIC X(16)  VALUE 'W21203-AVT      '.          
022200*01  AVT-AREA  -COPY W2120302   -PRE DBUP-                                
022300     EJECT                                                                
022400 01  FILLER                 PIC X(16)  VALUE 'W21203-LEVPL    '.          
022500*01  LEVPL-AREA -COPY W2120303   -PRE DBUP-                               
022600     EJECT                                                                
022700 01  FILLER                 PIC X(16)  VALUE 'W21203-MTRLF    '.          
022800*01  MTRLF-AREA -COPY W2120304   -PRE DBUP-                               
022900     EJECT                                                                
023000*                                                                         
023100*    ARBETSAREA OUTPUT R01:OR (HTYP 2222) TILL W213                       
023200*                                                                         
023300 01  FILLER                 PIC X(16)  VALUE 'W21213-AREA'.               
023400*01  AREA -COPY W213R01    -PRE R01-                                      
023500     EJECT                                                                
023600*                                                                         
023700*    ARBETSAREA OUTPUT HTYP 2204 TILL BMP-UPPLÄGG                         
023800*                                                                         
023900 01  FILLER                 PIC X(16)  VALUE 'W21232-2204     '.          
024000*01  AREA -COPY W2132204   -PRE 2204-                                     
024100     EJECT                                                                
024200*                                                                         
024300*    ARBETSAREA OUTPUT HTYP 2213 TILL BMP-UPPLÄGG                         
024400*                                                                         
024500*01  AREA -COPY W2132213   -PRE 2213-                                     
024600                                                                          
024700     EJECT                                                                
024800                                                                          
024900*                                                                         
025000*    ARBETSAREA OUTPUT FELMEDDELANDE                                      
025100*                                                                         
025200*01  AREA -COPY W092W001   -PRE U05W001-.                                 
025300     EJECT                                                                
025400*                                                                         
025500*    LÄNKAREOR TILL IMS-MODULEN                                           
025600*                                                                         
025700*    -COPY W212L001   -PRE L001-.                                         
025800     EJECT                                                                
025900*    -COPY W212L002   -PRE L002-.                                         
026000     EJECT                                                                
026100*    -COPY W212L003   -PRE L003-.                                         
026200     EJECT                                                                
026300*    -COPY W212L004   -PRE L004-.                                         
026400     EJECT                                                                
026500*    -COPY W212L005   -PRE L005-.                                         
026600     EJECT                                                                
026700*    -COPY W212L006   -PRE L006-.                                         
026800     EJECT                                                                
026900*    -COPY W212L008   -PRE L008-.                                         
027000     EJECT                                                                
027100*    -COPY W212L009   -PRE L009-.                                         
027200     EJECT                                                                
027300 01  PROGRAM-NAMN            PIC X(6)    VALUE 'W21202'.                  
027400 01  DATUMKORT-ID            PIC X(6)    VALUE 'WDATUM'.                  
027500*    -COPY WDATKORT                                                       
027600     EJECT                                                                
027700 LINKAGE SECTION.                                                         
027800*                                                                         
027900*                                                                         
028000*01  -COPY W0008      -PRE ARTC-.                                         
028100     05    FILLER            PIC X(14).                                   
028200     EJECT                                                                
028300*01  -COPY W0008      -PRE INLB-.                                         
028400     05    FILLER            PIC X(14).                                   
028500     EJECT                                                                
028600*01  -COPY W0008      -PRE LEVA-.                                         
028700     05    FILLER            PIC X(3).                                    
028800     EJECT                                                                
028900*01  -COPY W0008      -PRE WDK6-.                                         
029000     05    FILLER            PIC X(14).                                   
029100     EJECT                                                                
029200 PROCEDURE DIVISION USING ARTC-PCB INLB-PCB LEVA-PCB WDK6-PCB.            
029300 MAIN SECTION.                                                            
029400     ENTRY 'DLITCBL' USING ARTC-PCB INLB-PCB LEVA-PCB WDK6-PCB.           
029500*                                                                         
029600     PERFORM A-HOUSEKEEPING                                               
029700     PERFORM S01-LAS-POST                                                 
029800*                                                                         
029900     PERFORM UNTIL W092XX-EOF = JA                                        
030000                                                                          
030100         MOVE SPACE TO U05W001-IDFELKODX                                  
030200         MOVE HAMTA-ARTIKEL TO L001-KDCALL                                
030300         MOVE SPACE TO L001-FLJANEJ-ARTIKEL                               
030400         MOVE I01R22-IDARTNR TO L001-IDARTNR                              
030500*                                                                         
030600         CALL W2120210 USING L001-W212L001                                
030700                             ARTC-PCB INLB-PCB LEVA-PCB WDK6-PCB          
030800*                                                                         
030900         IF L001-ARTIKEL-SAKNAS                                           
031000           MOVE '200' TO U05W001-IDFELKODX                                
031100           PERFORM S02-FELMEDDELANDE                                      
031200         ELSE                                                             
031300           PERFORM S03-HAMTA-MTRLF-INFO                                   
031400           MOVE L002-IDLEVNR-MOTSV TO WS-IDLEVNR-MOTSV                    
031500*                                                                         
031600           IF  I01R22-IDPTYP = 'R22'                                      
031700           AND (I01R22-KDBEH-BEST = JUSTE-UPP OR JUSTE-NED)               
031800           AND I01R22-KVBEST = 0                                          
031900             MOVE '213' TO U05W001-IDFELKODX                              
032000             PERFORM S02-FELMEDDELANDE                                    
032100           ELSE                                                           
032200             IF  I01R22-IDPTYP = 'R22'                                    
032300             AND (I01R22-KDBEH-BEST = NYTT-BEST OR BEKR-BEST)             
032400             AND I01R22-KVBEST = 0                                        
032500               MOVE '214' TO U05W001-IDFELKODX                            
032600               PERFORM S02-FELMEDDELANDE                                  
032700             ELSE                                                         
032800               IF L002-LEVNR-FANNS                                        
032900               OR (I01R22-IDPTYP = 'R22' AND                              
033000                  (I01R22-KDBEH-BEST = JUSTE-UPP OR JUSTE-NED))           
033100               OR (I01R22-IDPTYP       = 'R22'     AND                    
033200                   I01R22-KDBEH-BEST   = NYTT-ANNU AND                    
033300                   L002-PARMA-OCH-KONV = JA)                              
033400                 IF I01R22-IDPTYP = 'R22'                                 
033500                     PERFORM B-BEARBETA-BEST                              
033600                 ELSE                                                     
033700                     PERFORM C-BEARBETA-AVTAL                             
033800                 END-IF                                                   
033900                 IF (I01R22-IDPTYP = 'R22' AND                            
034000                    (I01R22-KDBEH-BEST = NYTT-BEST OR BEKR-BEST))         
034100                 OR                                                       
034200                    (I01R23-IDPTYP = 'R23' AND                            
034300                    (I01R23-KDBEH-AVT  = NYTT-BEST OR BEKR-BEST))         
034400                     PERFORM S13-SKRIV-NYPON-POST                         
034500                 END-IF                                                   
034600               ELSE                                                       
034700                 MOVE '203' TO U05W001-IDFELKODX                          
034800                 PERFORM S02-FELMEDDELANDE                                
034900               END-IF                                                     
035000*                                                                         
035100             END-IF                                                       
035200           END-IF                                                         
035300         END-IF                                                           
035400         PERFORM S01-LAS-POST                                             
035500     END-PERFORM                                                          
035600*                                                                         
035700     PERFORM D-CLOSE                                                      
035800                                                                          
035900     IF RKOD > ZERO                                                       
036000         CALL ABEND USING RKOD                                            
036100     ELSE                                                                 
036200         MOVE ZERO TO RETURN-CODE                                         
036300         GOBACK                                                           
036400     END-IF                                                               
036500     .                                                                    
036600     EJECT                                                                
036700******************************************************************        
036800*                                                                *        
036900*    ÖPPNAR FILER, INITIERAR DIV AREOR                           *        
037000*                                                                *        
037100******************************************************************        
037200     SKIP2                                                                
037300 A-HOUSEKEEPING SECTION.                                                  
037400     SKIP1                                                                
037500     OPEN INPUT  W092XX                                                   
037600          OUTPUT W21205                                                   
037700                 W21211                                                   
037800                 W21203                                                   
037900                 W21231                                                   
038000                 W21232                                                   
038100*                                                                         
038200     MOVE ZERO TO U05W001-KDCLAGER                                        
038300     MOVE ZERO TO U05W001-KDFRAKT                                         
038400     MOVE ZERO TO U05W001-IDORDNR                                         
038500     MOVE ZERO TO U05W001-KDORDKL                                         
038600     MOVE ZERO TO U05W001-KDFELMRK                                        
038700*                                                                         
038800     CALL DATKORT USING PROGRAM-NAMN DATUMKORT-ID DATUMKORT               
038900*                                                                         
039000     MOVE K-AAR TO AAR                                                    
039100     MOVE K-VECKA TO VECKA                                                
039200*                                                                         
039300     MOVE 'W21202' TO POSTSUM-PROGNAMN                                    
039400*                                                                         
039500     MOVE D-AAR    TO DAGENS-AAR                                          
039600     MOVE D-MAANAD TO DAGENS-MAANAD                                       
039700     MOVE D-DAG    TO DAGENS-DAGNR                                        
039800     .                                                                    
039900     EJECT                                                                
040000******************************************************************        
040100*                                                                *        
040200*    STYR BEARBETNINGEN AV RT R22, NYUPPLÄGG/UPPDATERING/        *        
040300*    ANNULATION AV BESTÄLLNING                                   *        
040400*                                                                *        
040500******************************************************************        
040600     SKIP2                                                                
040700 B-BEARBETA-BEST SECTION.                                                 
040800*                                                                         
040900     IF I01R22-TIBEST = ZERO                                              
041000         MOVE DAGENS-DATUM TO I01R22-TIBEST                               
041100     END-IF                                                               
041200     MOVE HAMTA-BEST-INFO-KEY TO L003-KDCALL                              
041300     MOVE I01R22-IDARTNR      TO L003-IDARTNR                             
041400     MOVE LOW-VALUE           TO L003-IO-AREA                             
041500     CALL W2120210 USING L003-W212L003                                    
041600                         ARTC-PCB INLB-PCB LEVA-PCB WDK6-PCB              
041700*                                                                         
041800     MOVE NEJ TO IDBEST-FINNS                                             
041900     IF I01R22-KDBEH-BEST = JUSTE-UPP OR JUSTE-NED                        
042000         MOVE I01R22-TIBEST TO IDBEST-JFR                                 
042100     ELSE                                                                 
042200         MOVE I01R22-IDBEST TO IDBEST-JFR                                 
042300     END-IF                                                               
042400*                                                                         
042500     PERFORM BA-FINNS-IDBEST                                              
042600*                                                                         
042700     IF I01R22-KDBEH-BEST = BEKR-BEST OR BEKR-ANNU                        
042800         IF IDBEST-FINNS = NEJ                                            
042900             MOVE '202' TO U05W001-IDFELKODX                              
043000             PERFORM S02-FELMEDDELANDE                                    
043100         ELSE                                                             
043200             PERFORM BB-BEARBETA-BEKRAFTELSE                              
043300         END-IF                                                           
043400     ELSE                                                                 
043500         IF IDBEST-FINNS = JA                                             
043600         AND I01R22-KDBEH-BEST NOT = NYTT-ANNU                            
043700             MOVE '201' TO U05W001-IDFELKODX                              
043800             PERFORM S02-FELMEDDELANDE                                    
043900         ELSE                                                             
044000             PERFORM BC-BEARBETA-NY-BEST                                  
044100         END-IF                                                           
044200     END-IF                                                               
044300     .                                                                    
044400     EJECT                                                                
044500******************************************************************        
044600*                                                                *        
044700*    AVGÖR OM TRANSENS BEST-ID FINNS BLAND DE BEFINTLIGA         *        
044800*                                                                *        
044900******************************************************************        
045000     SKIP2                                                                
045100 BA-FINNS-IDBEST SECTION.                                                 
045200*                                                                         
045300     MOVE 1 TO IDBEST-IX                                                  
045400*                                                                         
045500     PERFORM UNTIL IDBEST-IX > L003-ANTAL-BEST                            
045600               OR  IDBEST-FINNS = JA                                      
045700         IF IDBEST-JFR = L003-TAB-IDBEST (IDBEST-IX)                      
045800         AND I01R22-IDLEVNR-BEST = L003-TAB-IDLEVNR (IDBEST-IX)           
045900             IF I01R22-KDBEH-BEST = BEKR-ANNU                             
046000                 IF I01R22-TIBEST = L003-TAB-TIBEST (IDBEST-IX)           
046100                     MOVE JA TO IDBEST-FINNS                              
046200                 ELSE                                                     
046300                     ADD 1 TO IDBEST-IX                                   
046400                 END-IF                                                   
046500             ELSE                                                         
046600                 MOVE JA TO IDBEST-FINNS                                  
046700             END-IF                                                       
046800         ELSE                                                             
046900             ADD 1 TO IDBEST-IX                                           
047000         END-IF                                                           
047100     END-PERFORM                                                          
047200     .                                                                    
047300     EJECT                                                                
047400******************************************************************        
047500*                                                                *        
047600*    STYR BEARBETNINGEN RT R22 BEKRÄFTELSE AV NYUPPLÄGG          *        
047700*    ALT. AV ANNULATION.                                         *        
047800*                                                                *        
047900******************************************************************        
048000     SKIP2                                                                
048100 BB-BEARBETA-BEKRAFTELSE SECTION.                                         
048200*                                                                         
048300     MOVE HAMTA-BEST-INFO     TO L004-KDCALL                              
048400     MOVE I01R22-IDARTNR      TO L004-IDARTNR                             
048500     MOVE I01R22-IDLEVNR-BEST TO L004-IDLEVNR                             
048600     MOVE I01R22-IDBEST       TO L004-IDBEST                              
048700**** MOVE L002-FLAVRART       TO L004-FLAVRART                            
048800     MOVE LOW-VALUE           TO L004-IO-AREA                             
048900*                                                                         
049000     CALL W2120210 USING L004-W212L004                                    
049100                         ARTC-PCB INLB-PCB LEVA-PCB WDK6-PCB              
049200*                                                                         
049300     MOVE ZERO TO L006-KDCALL                                             
049400     MOVE ZERO TO L006-TILEVPL                                            
049500     SKIP1                                                                
049600     MOVE NEJ TO TRAEFF                                                   
049700     PERFORM UNTIL TRAEFF = JA OR L004-BESTINFO-SAKNAS                    
049800        PERFORM BBD-HAEMTA-RAETT-BEST                                     
049900        IF TRAEFF = NEJ                                                   
050000          MOVE HAMTA-NEXT-BEST-INFO TO L004-KDCALL                        
050100          MOVE LOW-VALUE TO L004-IO-AREA                                  
050200          CALL W2120210 USING L004-W212L004                               
050300                             ARTC-PCB INLB-PCB LEVA-PCB WDK6-PCB          
050400        END-IF                                                            
050500     END-PERFORM                                                          
050600*                                                                         
050700     IF TRAEFF = JA                                                       
050800*                                                                         
050900         IF I01R22-KDBEH-BEST = BEKR-BEST                                 
051000             PERFORM BBA-UPPDATERA-BEST-BEKRAFT                           
051100         ELSE                                                             
051200             PERFORM BBB-UPPDATERA-ANN-BEKRAFT                            
051300         END-IF                                                           
051400*                                                                         
051500         PERFORM S14-UPPDAT-BEST-INFO                                     
051600                                                                          
051700         IF L006-KDCALL NOT = ZERO                                        
051800           PERFORM S16-AENDRA-LEVPLAN-INFO                                
051900         END-IF                                                           
052000     ELSE                                                                 
052100         PERFORM BBC-BEKR-FEL                                             
052200     END-IF                                                               
052300     .                                                                    
052400     EJECT                                                                
052500******************************************************************        
052600*                                                                *        
052700*    UTFÖR UPPDAT AV BEKRÄFT BEST, SKAPAR EV TRANS FÖR OMSPEC    *        
052800*                                                                *        
052900******************************************************************        
053000     SKIP2                                                                
053100 BBA-UPPDATERA-BEST-BEKRAFT SECTION.                                      
053200*                                                                         
053300     MOVE I01R22-KVBEST TO L004-KVBEST-BEKR                               
053400     MOVE +2  TO  L004-KDBEH-BEST                                         
053500*                                                                         
053600     IF L004-KVBEST NOT = L004-KVBEST-BEKR                                
053700*                                                                         
053800*        BEST-REST = BEST.REST + (BEKRÄFTAT - BESTÄLLT)                   
053900*                                                                         
054000         SUBTRACT L004-KVBEST FROM L004-KVBEST-BEKR                       
054100               GIVING L006-KVBR                                           
054200         IF L002-LEVPLAN-FANNS                                            
054300             ADD L002-KVBR TO L006-KVBR                                   
054400             MOVE UPPDAT-LEVPLAN-INFO TO L006-KDCALL                      
054500         ELSE                                                             
054600             MOVE NYUPPL-LEVPLAN-INFO TO L006-KDCALL                      
054700         END-IF                                                           
054800*                                                                         
054900         IF L006-KVBR < 0                                                 
055000             MOVE ZERO TO L006-KVBR                                       
055100         END-IF                                                           
055200         MOVE L004-IDARTNR        TO L006-IDARTNR  L009-IDARTNR           
055300         MOVE L004-IDLEVNR        TO L006-IDLEVNR  L009-IDLEVNR           
055400     END-IF                                                               
055500     .                                                                    
055600     EJECT                                                                
055700******************************************************************        
055800*                                                                *        
055900*    UTFÖR UPPDAT AV BEKRÄFTELSE ANNULERING, SKAPAR EV TRANS     *        
056000*    FÖR BEGÄRAN AV OMSPEC LEVPLAN                               *        
056100*                                                                *        
056200******************************************************************        
056300     SKIP2                                                                
056400 BBB-UPPDATERA-ANN-BEKRAFT SECTION.                                       
056500*                                                                         
056600     MOVE I01R22-KVBEST TO L004-KVBEST-BEKR                               
056700     MOVE +6 TO L004-KDBEH-BEST                                           
056800*                                                                         
056900     IF L004-KVBEST NOT = L004-KVBEST-BEKR                                
057000*                                                                         
057100*    BEST.REST = BEST.REST + (ANNULERAT - BEKRÄFTAT)                      
057200*                                                                         
057300         SUBTRACT L004-KVBEST-BEKR FROM L004-KVBEST                       
057400               GIVING L006-KVBR                                           
057500         IF L002-LEVPLAN-FANNS                                            
057600             ADD L002-KVBR TO L006-KVBR                                   
057700             MOVE UPPDAT-LEVPLAN-INFO TO L006-KDCALL                      
057800         ELSE                                                             
057900             MOVE NYUPPL-LEVPLAN-INFO TO L006-KDCALL                      
058000         END-IF                                                           
058100*                                                                         
058200         IF L006-KVBR < 0                                                 
058300             MOVE ZERO TO L006-KVBR                                       
058400         END-IF                                                           
058500         MOVE L004-IDARTNR        TO L006-IDARTNR  L009-IDARTNR           
058600         MOVE L004-IDLEVNR        TO L006-IDLEVNR  L009-IDLEVNR           
058700*                                                                         
058800         IF L006-KVBR NOT = ZERO                                          
058900             MOVE '208' TO U05W001-IDFELKODX                              
059000             PERFORM S02-FELMEDDELANDE                                    
059100         END-IF                                                           
059200     END-IF                                                               
059300     .                                                                    
059400     EJECT                                                                
059500******************************************************************        
059600*                                                                *        
059700*    AVGÖR TYP AV FEL NÄR BEKRÄFTELSEN MATCHAR REG               *        
059800*                                                                *        
059900******************************************************************        
060000     SKIP3                                                                
060100 BBC-BEKR-FEL SECTION.                                                    
060200*                                                                         
060300     IF I01R22-KDBEH-BEST = BEKR-BEST                                     
060400        MOVE '212' TO U05W001-IDFELKODX                                   
060500     ELSE                                                                 
060600        MOVE '211' TO U05W001-IDFELKODX                                   
060700     END-IF                                                               
060800*                                                                         
060900     PERFORM S02-FELMEDDELANDE                                            
061000     .                                                                    
061100     EJECT                                                                
061200 BBD-HAEMTA-RAETT-BEST SECTION.                                           
061300                                                                          
061400     IF I01R22-IDLEVNR-BEST = L004-IDLEVNR-BEST                           
061500        IF I01R22-KDBEH-BEST = BEKR-BEST                                  
061600        AND  L004-KDBEH-BEST = NYTT-BEST                                  
061700           MOVE JA TO TRAEFF                                              
061800        ELSE                                                              
061900           IF I01R22-KDBEH-BEST = BEKR-ANNU                               
062000           AND  L004-KDBEH-BEST = NYTT-ANNU                               
062100           AND  I01R22-TIBEST = L004-TIBEST                               
062200                MOVE JA TO TRAEFF                                         
062300           END-IF                                                         
062400        END-IF                                                            
062500     END-IF                                                               
062600     .                                                                    
062700     EJECT                                                                
062800******************************************************************        
062900*                                                                *        
063000*    STYR BEARBETNINGEN RT R22 NYUPPLÄGG BESTÄLLNING/JUSTERING/  *        
063100*    /ANNULERING                                                 *        
063200*                                                                *        
063300******************************************************************        
063400     SKIP2                                                                
063500 BC-BEARBETA-NY-BEST SECTION.                                             
063600*                                                                         
063700     MOVE ZERO TO L004-KDCALL                                             
063800     MOVE ZERO TO L006-KDCALL                                             
063900     MOVE ZERO TO L006-TILEVPL                                            
064000*                                                                         
064100     IF L002-LEVPLAN-SAKNAS                                               
064200*        FÖR ATT INDIKERA NYUPPLÄGG AV LEVPLAN-SEGM                       
064300         MOVE NYUPPL-LEVPLAN-INFO TO L006-KDCALL                          
064400         MOVE ZERO TO L002-KVBR                                           
064500                      L006-KVBR                                           
064600         MOVE ZERO TO L006-TILEVPL                                        
064700         MOVE I01R22-IDARTNR TO L006-IDARTNR                              
064800         MOVE L002-IDLEVNR TO L006-IDLEVNR                                
064900     END-IF                                                               
065000     IF L003-ANTAL-BEST = 7                                               
065100         MOVE BORTTAG-BEST   TO L005-KDCALL                               
065200         MOVE I01R22-IDARTNR TO L005-IDARTNR                              
065300***      MOVE L002-FLAVRART  TO L005-FLAVRART                             
065400         MOVE ZERO           TO L005-IDAVTAL                              
065500         MOVE +7             TO BEST-IX                                   
065600         MOVE NEJ            TO TRAEFF                                    
065700         PERFORM UNTIL BEST-IX = 0 OR TRAEFF = JA                         
065800             IF L003-TAB-KDBEH-BEST(BEST-IX) = NYTT-BEST                  
065900                 MOVE BEST-IX TO HELP-IX                                  
066000                 SUBTRACT 1 FROM HELP-IX                                  
066100                 PERFORM UNTIL HELP-IX = 0 OR TRAEFF = JA                 
066200                     IF L003-TAB-KDBEH-BEST(HELP-IX) = NYTT-BEST          
066300                         MOVE JA TO TRAEFF                                
066400                         MOVE L003-TAB-IDBEST(BEST-IX) TO                 
066500                                       L005-IDBEST                        
066600                         MOVE L003-TAB-TIBEST(BEST-IX) TO                 
066700                                       L005-TIBEST                        
066800                     ELSE                                                 
066900                         SUBTRACT 1 FROM HELP-IX                          
067000                     END-IF                                               
067100                 END-PERFORM                                              
067200                 IF TRAEFF = NEJ                                          
067300                     SUBTRACT 1 FROM BEST-IX                              
067400                 END-IF                                                   
067500             ELSE                                                         
067600                 IF L003-TAB-KDBEH-BEST(BEST-IX) = NYTT-ANNU              
067700                     MOVE BEST-IX TO HELP-IX                              
067800                     SUBTRACT 1 FROM HELP-IX                              
067900                     PERFORM UNTIL HELP-IX = 0 OR TRAEFF = JA             
068000                       IF L003-TAB-KDBEH-BEST(HELP-IX) = NYTT-ANNU        
068100                           MOVE JA TO TRAEFF                              
068200                           MOVE L003-TAB-IDBEST(BEST-IX) TO               
068300                                         L005-IDBEST                      
068400                           MOVE L003-TAB-TIBEST(BEST-IX) TO               
068500                                         L005-TIBEST                      
068600                       ELSE                                               
068700                           SUBTRACT 1 FROM HELP-IX                        
068800                       END-IF                                             
068900                     END-PERFORM                                          
069000                     IF TRAEFF = NEJ                                      
069100                         SUBTRACT 1 FROM BEST-IX                          
069200                     END-IF                                               
069300                 ELSE                                                     
069400                     MOVE L003-TAB-IDBEST(BEST-IX) TO L005-IDBEST         
069500                     MOVE L003-TAB-TIBEST(BEST-IX) TO L005-TIBEST         
069600                     MOVE JA TO TRAEFF                                    
069700                 END-IF                                                   
069800             END-IF                                                       
069900         END-PERFORM                                                      
070000*                                                                         
070100         IF BEST-IX > 0                                                   
070200           PERFORM S15-BORTTAG-BEST-INFO                                  
070300         END-IF                                                           
070400     END-IF                                                               
070500*                                                                         
070600     EVALUATE I01R22-KDBEH-BEST                                           
070700        WHEN NYTT-BEST                                                    
070800           PERFORM  BCB-NYTT-BESTID                                       
070900        WHEN JUSTE-UPP                                                    
071000           PERFORM  BCC-JUST-BESTID                                       
071100        WHEN JUSTE-NED                                                    
071200           PERFORM  BCC-JUST-BESTID                                       
071300        WHEN NYTT-ANNU                                                    
071400           PERFORM  BCD-ANNU-BESTID                                       
071500     END-EVALUATE                                                         
071600*                                                                         
071700     IF L006-KDCALL NOT = ZERO                                            
071800         PERFORM S16-AENDRA-LEVPLAN-INFO                                  
071900     END-IF                                                               
072000*                                                                         
072100     IF L004-KDCALL NOT = ZERO                                            
072200*        -- NYUPPL-BEST-INFO (I BCC)                                      
072300*        -- TIDIGARE OCKSÅ NYUPPL-BEST-NOT (I S05 VIA BCB)                
072400         PERFORM S14-NYUPPL-BEST-INFO                                     
072500     END-IF                                                               
072600     .                                                                    
072700******************************************************************        
072800*                                                                *        
072900*    LÄGGER UPP NY BESTÄLLNING, ÄNDRAR BEST.REST, SKAPAR TRANS   *        
073000*    BEGÄRAN OMSPEC LEVPLAN                                      *        
073100*                                                                *        
073200******************************************************************        
073300     SKIP2                                                                
073400 BCB-NYTT-BESTID SECTION.                                                 
073500*                                                                         
073600     PERFORM BCBA-UPPDAT-MTRLF-INFO-NYTT                                  
073700*                                                                         
073800     PERFORM S05-REDIGERA-BEST-ANNULERING                                 
073900*                                                                         
074000     IF L006-KDCALL = ZERO                                                
074100         MOVE UPPDAT-LEVPLAN-INFO TO L006-KDCALL                          
074200     END-IF                                                               
074300*                                                                         
074400     MOVE L002-IDARTNR  TO L006-IDARTNR  L009-IDARTNR                     
074500     MOVE L002-IDLEVNR  TO L006-IDLEVNR  L009-IDLEVNR                     
074600     MOVE L002-KVBR     TO L006-KVBR                                      
074700     ADD  I01R22-KVBEST TO L006-KVBR                                      
074800*                                                                         
074900     IF L002-KDKSP = 1                                                    
075000         MOVE '204' TO U05W001-IDFELKODX                                  
075100         PERFORM S02-FELMEDDELANDE                                        
075200     END-IF                                                               
075300     .                                                                    
075400     EJECT                                                                
075500******************************************************************        
075600*                                                                *        
075700*    UTFÖR UPPDATERING AV HUVUDLEVERANTÖR, AVTALSANMÄRKNING,     *        
075800*    KÖPSPÄRR, INKÖPARNR I SAMBAND MED UPPLÄGGNING AV BESTÄLLN   *        
075900******************************************************************        
076000     SKIP2                                                                
076100 BCBA-UPPDAT-MTRLF-INFO-NYTT SECTION.                                     
076200*                                                                         
076300     MOVE ZERO          TO L008-KDCALL                                    
076400     MOVE L002-KDAVT    TO L008-KDAVT                                     
076500     MOVE L002-KDKSP    TO L008-KDKSP                                     
076600     MOVE L002-IDINK    TO L008-IDINK                                     
076700     MOVE L002-IDARTNR  TO L008-IDARTNR  L009-IDARTNR                     
076800     MOVE L002-IDLEVNR  TO L008-IDLEVNR  L009-IDLEVNR                     
076900     MOVE I01R22-IDBEST TO BEST-ID                                        
077000*                                                                         
077100*    BYTE AV HUVUDLEVERANTÖR                                              
077200*                                                                         
077300     IF (BYTE-SUFFIX  = 099 OR 129 OR USA OR CAN) AND                     
077400        (BYTE-IDINK NOT = 004)                                            
077500        CONTINUE                                                          
077600     ELSE                                                                 
077700        IF I01R22-IDLEVNR-BEST NOT = L002-IDLEVNR-REG                     
077800          PERFORM S06-BYTE-HUVLEV                                         
077900        END-IF                                                            
078000     END-IF                                                               
078100*                                                                         
078200     IF L002-KDAVT > ZERO AND L002-KDAVT < 5                              
078300         MOVE UPPDAT-MTRLF-INFO TO L008-KDCALL                            
078400         MOVE L002-KDPRODSL     TO TEST-KDPRODSL                          
078500         IF ((BYTE-SUFFIX = 099  OR 129) OR                               
078600             ((BYTE-SUFFIX = USA OR CAN) AND                              
078700                     KDPRODSL-VOLVO-BIMA )) AND                           
078800            (BYTE-IDINK NOT = 004)                                        
078900           CONTINUE                                                       
079000         ELSE                                                             
079100           MOVE ZERO          TO L008-KDAVT                               
079200         END-IF                                                           
079300     END-IF                                                               
079400*                                                                         
079500     IF L002-KDKSP > 1                                                    
079600         MOVE UPPDAT-MTRLF-INFO TO L008-KDCALL                            
079700         MOVE ZERO              TO L008-KDKSP                             
079800     END-IF                                                               
079900*                                                                         
080000     IF L008-KDCALL NOT = ZERO                                            
080100         PERFORM S18-UPPDAT-MTRLF-INFO                                    
080200     END-IF                                                               
080300*                                                                         
080400     IF L002-IDINK NOT = L008-IDINK OR                                    
080500        L002-KDAVT NOT = L008-KDAVT                                       
080600         PERFORM S08-NY-TID                                               
080700     END-IF                                                               
080800     .                                                                    
080900     EJECT                                                                
081000******************************************************************        
081100*                                                                *        
081200*    LÄGGER UPP JUSTERING ÄNDRAR BESTÄLLNINGSREST                *        
081300*                                                                *        
081400******************************************************************        
081500     SKIP2                                                                
081600 BCC-JUST-BESTID SECTION.                                                 
081700*                                                                         
081800***  MOVE L002-FLAVRART TO L004-FLAVRART                                  
081900     IF I01R22-IDLEVNR-BEST = SPACE OR L002-LEVNR-SAKNAS                  
082000         PERFORM BCCA-IDLEVNR-REG-ANVANDS                                 
082100     ELSE                                                                 
082200         MOVE I01R22-IDLEVNR-BEST TO L004-IDLEVNR                         
082300         MOVE I01R22-IDLEVNR-BEST TO L004-IDLEVNR-BEST                    
082400     END-IF                                                               
082500*                                                                         
082600     MOVE NYUPPL-BEST-INFO  TO L004-KDCALL                                
082700     MOVE I01R22-TIBEST     TO L004-IDBEST                                
082800     MOVE I01R22-TIBEST     TO L004-TIBEST                                
082900     MOVE I01R22-TIBEST     TO L004-IDBEST-REG                            
083000     MOVE I01R22-KDBEH-BEST TO L004-KDBEH-BEST                            
083100     MOVE I01R22-KVBEST     TO L004-KVBEST                                
083200     MOVE I01R22-IDARTNR    TO L004-IDARTNR                               
083300     MOVE ZERO              TO L004-KVBEST-BEKR                           
083400*                                                                         
083500     MOVE L004-IDARTNR TO L006-IDARTNR                                    
083600     MOVE L004-IDLEVNR TO L006-IDLEVNR                                    
083700     MOVE L002-KVBR TO L006-KVBR                                          
083800*                                                                         
083900     IF I01R22-KDBEH-BEST = JUSTE-UPP                                     
084000         ADD I01R22-KVBEST TO L006-KVBR                                   
084100     ELSE                                                                 
084200         SUBTRACT I01R22-KVBEST FROM L006-KVBR                            
084300     END-IF                                                               
084400     IF L006-KVBR < 0                                                     
084500          MOVE ZERO TO L006-KVBR                                          
084600     END-IF                                                               
084700*                                                                         
084800     IF L006-KDCALL = ZERO                                                
084900         MOVE UPPDAT-LEVPLAN-INFO TO L006-KDCALL                          
085000     END-IF                                                               
085100     .                                                                    
085200     EJECT                                                                
085300******************************************************************        
085400*                                                                *        
085500*    HÄMTAR BEST.REST FÖR IDLVENR-REG (JUST MED LEVNR=0 EL FEL)  *        
085600*                                                                *        
085700******************************************************************        
085800     SKIP2                                                                
085900 BCCA-IDLEVNR-REG-ANVANDS SECTION.                                        
086000*                                                                         
086100     MOVE L002-IDLEVNR-REG TO L004-IDLEVNR                                
086200     MOVE L002-IDLEVNR-REG TO L004-IDLEVNR-BEST                           
086300     MOVE L002-IDLEVNR-REG TO I01R22-IDLEVNR-BEST                         
086400*                                                                         
086500     PERFORM S03-HAMTA-MTRLF-INFO                                         
086600*                                                                         
086700     MOVE ZERO TO L006-KDCALL                                             
086800*                                                                         
086900     IF L002-LEVPLAN-SAKNAS                                               
087000          MOVE NYUPPL-LEVPLAN-INFO TO L006-KDCALL                         
087100          MOVE ZERO                TO L002-KVBR                           
087200     END-IF                                                               
087300     .                                                                    
087400     EJECT                                                                
087500******************************************************************        
087600*                                                                *        
087700*    LÄGGER UPP ANNULERING, ÄNDRAR BEST.REST, SKAPAR TRANS       *        
087800*    BEGÄRAN OMSPEC LEVPLAN                                      *        
087900*                                                                *        
088000******************************************************************        
088100     SKIP2                                                                
088200 BCD-ANNU-BESTID SECTION.                                                 
088300*                                                                         
088400     MOVE ZERO         TO L008-KDCALL                                     
088500     MOVE L002-KDAVT   TO L008-KDAVT                                      
088600     MOVE L002-KDKSP   TO L008-KDKSP                                      
088700     MOVE L002-IDINK   TO L008-IDINK                                      
088800     MOVE L002-IDARTNR TO L008-IDARTNR  L009-IDARTNR L006-IDARTNR         
088900     MOVE L002-IDLEVNR TO L008-IDLEVNR  L009-IDLEVNR L006-IDLEVNR         
089000     MOVE I01R22-IDBEST   TO BEST-ID                                      
089100*                                                                         
089200     SUBTRACT I01R22-KVBEST FROM L002-KVBR                                
089300           GIVING L006-KVBR                                               
089400                                                                          
089500     IF L006-KVBR < 0                                                     
089600         MOVE ZERO TO L006-KVBR                                           
089700     END-IF                                                               
089800                                                                          
089900     IF L006-KVBR NOT = ZERO                                              
090000         MOVE '206' TO U05W001-IDFELKODX                                  
090100         PERFORM S02-FELMEDDELANDE                                        
090200     END-IF                                                               
090300     IF L002-KDAVT = 1                                                    
090400       IF I01R22-IDLEVNR-BEST = L002-IDLEVNR-REG                          
090500         MOVE L002-KDPRODSL      TO TEST-KDPRODSL                         
090600         IF  (BYTE-SUFFIX = USA OR CAN) AND                               
090700             (KDPRODSL-VOLVO-BIMA) AND                                    
090800             (BYTE-IDINK NOT = 004)                                       
090900           CONTINUE                                                       
091000         ELSE                                                             
091100****ÄT OKT-04 ETRACKER 1382465                                            
091200           MOVE HAMTA-ANTAL-AVTAL TO L001-KDCALL                          
091300           MOVE SPACE TO L001-FLJANEJ-ARTIKEL                             
091400           MOVE NEJ   TO L001-FLAGGA-AVTAL                                
091500           MOVE I01R22-IDARTNR TO L001-IDARTNR                            
091600                                                                          
091700           CALL W2120210 USING L001-W212L001                              
091800                               ARTC-PCB INLB-PCB LEVA-PCB WDK6-PCB        
091900           IF L001-FLAGGA-AVTAL = JA                                      
092000             CONTINUE                                                     
092100           ELSE                                                           
092200             MOVE ZERO            TO L008-KDAVT                           
092300             MOVE UPPDAT-MTRLF-INFO TO L008-KDCALL                        
092400           END-IF                                                         
092500****ÄT                                                                    
092600         END-IF                                                           
092700       END-IF                                                             
092800       PERFORM S08-NY-TID                                                 
092900     END-IF                                                               
093000                                                                          
093100     PERFORM BCDA-UPPDAT-MTRLF-INFO-ANNU                                  
093200     PERFORM S05-REDIGERA-BEST-ANNULERING                                 
093300     IF L006-KDCALL = ZERO                                                
093400         MOVE UPPDAT-LEVPLAN-INFO TO L006-KDCALL                          
093500     END-IF                                                               
093600                                                                          
093700*TRANS BEGÄRAN OMSPEC                                                     
093800                                                                          
093900     MOVE +4 TO L009-KDLPORS                                              
094000     PERFORM S07-OMSPEC-LEVPLAN                                           
094100     .                                                                    
094200     EJECT                                                                
094300******************************************************************        
094400*                                                                *        
094500*        UTFÖR KONTROLL AV KÖPSPÄRR OCH INKÖPARNR VID ANNULERING *        
094600*                                                                *        
094700******************************************************************        
094800     SKIP2                                                                
094900 BCDA-UPPDAT-MTRLF-INFO-ANNU SECTION.                                     
095000*                                                                         
095100     EVALUATE L002-KDKSP                                                  
095200      WHEN 1                                                              
095300             MOVE '207' TO U05W001-IDFELKODX                              
095400             PERFORM S02-FELMEDDELANDE                                    
095500      WHEN 3                                                              
095600             MOVE UPPDAT-MTRLF-INFO TO L008-KDCALL                        
095700             MOVE ZERO TO L008-KDKSP                                      
095800      WHEN 4                                                              
095900             MOVE UPPDAT-MTRLF-INFO TO L008-KDCALL                        
096000             MOVE 1 TO L008-KDKSP                                         
096100     END-EVALUATE                                                         
096200*                                                                         
096300     MOVE I01R22-IDBEST TO BEST-ID                                        
096400*                                                                         
096500     IF L008-KDCALL NOT = ZERO                                            
096600       PERFORM S18-UPPDAT-MTRLF-INFO                                      
096700     END-IF                                                               
096800     .                                                                    
096900     EJECT                                                                
097000******************************************************************        
097100*                                                                *        
097200*    STYR BEARBETNINGEN AV RT R23, NYUPPLÄGGNING AV AVTAL        *        
097300*                                                                *        
097400******************************************************************        
097500     SKIP2                                                                
097600 C-BEARBETA-AVTAL SECTION.                                                
097700                                                                          
097800     IF L002-LEVNR-FANNS                                                  
097900         MOVE SPACE              TO W-IDLEVNR-AVT                         
098000         MOVE HAMTA-AVT-INFO-KEY TO L003-KDCALL                           
098100         MOVE I01R23-IDARTNR     TO L003-IDARTNR                          
098200         MOVE LOW-VALUE          TO L003-IO-AREA                          
098300         CALL W2120210 USING L003-W212L003                                
098400                             ARTC-PCB INLB-PCB LEVA-PCB WDK6-PCB          
098500                                                                          
098600         MOVE +1                 TO IDBEST-IX                             
098700         MOVE NEJ                TO SW-AVT-BORT                           
098800         PERFORM UNTIL SW-AVT-BORT = JA                                   
098900                       OR IDBEST-IX > 5                                   
099000                       OR IDBEST-IX > L003-ANTAL-BEST                     
099100            IF L003-TAB-IDLEVNR (IDBEST-IX) = I01R23-IDLEVNR-AVT          
099200               MOVE L003-TAB-IDBEST (IDBEST-IX) TO L005-IDAVTAL           
099300               MOVE L003-TAB-IDLEVNR(IDBEST-IX) TO W-IDLEVNR-AVT          
099400               MOVE JA                          TO SW-AVT-BORT            
099500            END-IF                                                        
099600            IF SW-AVT-BORT = NEJ                                          
099700               ADD +1 TO IDBEST-IX                                        
099800            END-IF                                                        
099900         END-PERFORM                                                      
100000         IF IDBEST-IX = 6                                                 
100100            MOVE L003-TAB-IDBEST (5) TO L005-IDAVTAL                      
100200            MOVE L003-TAB-IDLEVNR(5) TO W-IDLEVNR-AVT                     
100300            MOVE JA                  TO SW-AVT-BORT                       
100400         END-IF                                                           
100500         IF SW-AVT-BORT = JA                                              
100600            MOVE BORTTAG-AVTAL       TO L005-KDCALL                       
100700            MOVE I01R23-IDARTNR      TO L005-IDARTNR                      
100800            PERFORM S15-BORTTAG-AVTAL-INFO                                
100900         END-IF                                                           
101000                                                                          
101100         PERFORM CA-UPPDAT-MTRLF-INFO-AVTAL                               
101200         PERFORM CB-REDIGERA-AVTAL                                        
101300     ELSE                                                                 
101400         MOVE '203' TO U05W001-IDFELKODX                                  
101500         PERFORM S02-FELMEDDELANDE                                        
101600     END-IF                                                               
101700     .                                                                    
101800     EJECT                                                                
101900******************************************************************        
102000*                                                                *        
102100*    KONTROLLERAR OCH EV UPPDATERAR INFO I MATERIALFÖRSÖRJNINGS- *        
102200*    SEGMENTET, SKRIVER EV FELMEDDELANDE                         *        
102300*                                                                *        
102400******************************************************************        
102500     SKIP2                                                                
102600 CA-UPPDAT-MTRLF-INFO-AVTAL SECTION.                                      
102700*                                                                         
102800     MOVE ZERO TO L008-KDCALL                                             
102900     MOVE L002-KDAVT   TO L008-KDAVT                                      
103000     MOVE L002-KDKSP   TO L008-KDKSP                                      
103100     MOVE L002-IDINK   TO L008-IDINK                                      
103200     MOVE L002-IDARTNR TO L008-IDARTNR  L009-IDARTNR                      
103300     MOVE L002-IDLEVNR TO L008-IDLEVNR  L009-IDLEVNR                      
103400*                                                                         
103500     MOVE I01R23-IDAVTAL TO BEST-ID                                       
103600*                                                                         
103700*    IF (BYTE-IDINK >  99 AND < 400) OR                                   
103800*       (BYTE-IDINK > 539 AND < 580) OR                                   
103900*       (BYTE-IDINK > 699 AND < 725)                                      
104000*        CONTINUE                                                         
104100*     ELSE                                                                
104200*         IF BYTE-IDINK NOT = L002-IDINK                                  
104300*              MOVE UPPDAT-MTRLF-INFO TO L008-KDCALL                      
104400*              MOVE BYTE-IDINK        TO L008-IDINK                       
104500*         END-IF                                                          
104600*    END-IF                                                               
104700*                                                                         
104800     IF I01R23-IDLEVNR-AVT NOT = L002-IDLEVNR-REG OR                      
104900        I01R23-IDLEVNR-SHIP NOT = L002-IDLEVNR-SHIP                       
105000        IF (BYTE-SUFFIX  = USA OR CAN) AND                                
105100           (BYTE-IDINK NOT = 004)                                         
105200           CONTINUE                                                       
105300        ELSE                                                              
105400           PERFORM S06-BYTE-HUVLEV                                        
105500        END-IF                                                            
105600     END-IF                                                               
105700     IF L002-KDKSP = 1 OR 3 OR 4                                          
105800         MOVE '209' TO U05W001-IDFELKODX                                  
105900         PERFORM S02-FELMEDDELANDE                                        
106000         MOVE UPPDAT-MTRLF-INFO TO L008-KDCALL                            
106100     END-IF                                                               
106200*                                                                         
106300     IF L002-KDKSP = 2                                                    
106400         MOVE UPPDAT-MTRLF-INFO TO L008-KDCALL                            
106500         MOVE ZERO              TO L008-KDKSP                             
106600     END-IF                                                               
106700*                                                                         
106800     IF L002-KDAVT = ZERO OR 1 OR 3                                       
106900         MOVE L002-KDPRODSL      TO TEST-KDPRODSL                         
107000         IF  (BYTE-SUFFIX = USA OR CAN) AND                               
107100             (KDPRODSL-VOLVO-BIMA) AND                                    
107200             (BYTE-IDINK NOT = 004)                                       
107300           CONTINUE                                                       
107400         ELSE                                                             
107500           MOVE UPPDAT-MTRLF-INFO TO L008-KDCALL                          
107600           MOVE +1                TO L008-KDAVT                           
107700         END-IF                                                           
107800     ELSE                                                                 
107900         IF L002-KDAVT = 4                                                
108000             MOVE '210' TO U05W001-IDFELKODX                              
108100             PERFORM S02-FELMEDDELANDE                                    
108200         END-IF                                                           
108300     END-IF                                                               
108400*                                                                         
108500     IF L008-KDCALL NOT = ZERO                                            
108600         PERFORM S18-UPPDAT-MTRLF-INFO                                    
108700     END-IF                                                               
108800*                                                                         
108900     IF L002-IDINK NOT = L008-IDINK OR                                    
109000        L002-KDAVT NOT = L008-KDAVT                                       
109100         PERFORM S08-NY-TID                                               
109200     END-IF                                                               
109300     .                                                                    
109400     EJECT                                                                
109500******************************************************************        
109600*                                                                *        
109700*    SKAPAR AVTALS-SEGMENT (U05W001-IDFELKODX KAN SÄTTAS I CA-.) *        
109800*                                                                *        
109900******************************************************************        
110000     SKIP2                                                                
110100 CB-REDIGERA-AVTAL SECTION.                                               
110200*                                                                         
110300     IF U05W001-IDFELKODX = SPACE OR '209' OR '210'                       
110400         MOVE L002-IDARTNR     TO L004-IDARTNR                            
110500         MOVE L002-IDLEVNR     TO L004-IDLEVNR                            
110600         MOVE L002-IDLEVNR     TO L004-IDLEVNR-AVT                        
110700         MOVE I01R23-IDAVTAL   TO L004-IDAVTAL                            
110800         MOVE I01R23-IDAVTAL   TO L004-IDAVTAL-REG                        
110900         MOVE I01R23-KDBEH-AVT TO L004-KDBEH-AVT                          
111000         MOVE I01R23-KVAVTANT  TO L004-KVAVTANT                           
111100         MOVE I01R23-TIAVTAL   TO L004-TIAVTAL                            
111200*                                                                         
111300*         -- BORTTAGET 96-10-16 (KA). NYTTJAS EJ (!?)                     
111400*         IF I01R23-TENOT-AVTPRIS NOT = SPACE                             
111500*            MOVE NYUPPL-AVT-NOT       TO L004-KDCALL                     
111600*            MOVE I01R23-TENOT-AVTPRIS TO L004-TENOT-INTRANS              
111700*            MOVE +1                   TO L004-KDNOTTYP-INTRANS           
111800*        ELSE                                                             
111900             MOVE NYUPPL-AVT-INFO TO L004-KDCALL                          
112000*        END-IF                                                           
112100*                                                                         
112200         PERFORM S14-NYUPPL-AVT-INFO                                      
112300     END-IF                                                               
112400     .                                                                    
112500     EJECT                                                                
112600******************************************************************        
112700*                                                                *        
112800*    STÄNGER SAMTLIGA FILER                                      *        
112900*                                                                *        
113000******************************************************************        
113100     SKIP2                                                                
113200 D-CLOSE SECTION.                                                         
113300*                                                                         
113400     MOVE 'S' TO POSTSUM-OPKOD                                            
113500     CALL POSTSUM USING POSTSUM-PARM                                      
113600*                                                                         
113700     CLOSE  W092XX                                                        
113800            W21205                                                        
113900            W21211                                                        
114000            W21203                                                        
114100            W21231                                                        
114200            W21232                                                        
114300     .                                                                    
114400     EJECT                                                                
114500******************************************************************        
114600*                                                                *        
114700*    LÄSER TRANSAKTIONSFILEN W092XX OCH ÖKAR POSTRÄKNAREN        *        
114800*                                                                *        
114900******************************************************************        
115000     SKIP2                                                                
115100 S01-LAS-POST SECTION.                                                    
115200     SKIP1                                                                
115300     READ W092XX INTO I01R22-AREA                                         
115400     AT END                                                               
115500             MOVE JA TO W092XX-EOF                                        
115600     END-READ                                                             
115700*                                                                         
115800     IF W092XX-EOF = NEJ                                                  
115900         MOVE 'W092XX'      TO POSTSUM-FDNAMN                             
116000         MOVE 'W21202D1'    TO POSTSUM-DDNAMN2                            
116100         MOVE I01R22-IDPTYP TO POSTSUM-TRANSTYP                           
116200         CALL POSTSUM USING POSTSUM-PARM                                  
116300     END-IF                                                               
116400     .                                                                    
116500     EJECT                                                                
116600******************************************************************        
116700*                                                                *        
116800*    REDIGERAR FELMEDDELANDE TILL FILEN W21205, ÖKAR POSTRÄKNARE *        
116900*                                                                *        
117000******************************************************************        
117100     SKIP2                                                                
117200 S02-FELMEDDELANDE SECTION.                                               
117300*                                                                         
117400     MOVE I01R22-IDPTYP  TO U05W001-IDPTYP                                
117500     MOVE I01R22-IDARTNR TO U05W001-SORTBGP                               
117600     MOVE ZERO TO U05W001-IDDISTR                                         
117700*    SKIP1                                                                
117800     IF U05W001-IDFELKODX = '201' OR '204'                                
117900             MOVE L002-IDANSK TO U05W001-IDKUNDNR                         
118000     ELSE                                                                 
118100             MOVE ZERO TO U05W001-IDKUNDNR                                
118200     END-IF                                                               
118300                                                                          
118400     WRITE W092W001-POST FROM U05W001-AREA                                
118500*                                                                         
118600     MOVE 'W21205'       TO POSTSUM-FDNAMN                                
118700     MOVE 'W21202D2'     TO POSTSUM-DDNAMN2                               
118800     MOVE U05W001-IDPTYP TO POSTSUM-TRANSTYP                              
118900*                                                                         
119000     CALL POSTSUM USING POSTSUM-PARM                                      
119100     .                                                                    
119200     EJECT                                                                
119300******************************************************************        
119400*                                                                *        
119500*    INITIERAR LÄNKAREA OCH HÄMTAR MATERIAL-INFO                 *        
119600*                                                                *        
119700******************************************************************        
119800     SKIP2                                                                
119900 S03-HAMTA-MTRLF-INFO SECTION.                                            
120000*                                                                         
120100     MOVE HAMTA-MTRLF-INFO    TO L002-KDCALL                              
120200     MOVE SPACE               TO L002-FLJANEJ-LEVNR                       
120300     MOVE SPACE               TO L002-FLJANEJ-LEVPLAN                     
120400     MOVE LOW-VALUE           TO L002-IO-AREA                             
120500     MOVE I01R22-IDARTNR      TO L002-IDARTNR                             
120600     MOVE I01R22-IDLEVNR-BEST TO L002-IDLEVNR                             
120700*                                                                         
120800     CALL W2120210 USING L002-W212L002                                    
120900                         ARTC-PCB INLB-PCB LEVA-PCB WDK6-PCB              
121000     .                                                                    
121100     EJECT                                                                
121200******************************************************************        
121300*                                                                *        
121400*    SKAPAR BESTÄLLNINGS/ANNULERINGS-SEGMENT                     *        
121500*                                                                *        
121600******************************************************************        
121700     SKIP2                                                                
121800 S05-REDIGERA-BEST-ANNULERING SECTION.                                    
121900*                                                                         
122000***  MOVE L002-FLAVRART     TO L004-FLAVRART                              
122100     MOVE L002-IDARTNR      TO L004-IDARTNR                               
122200     MOVE L002-IDLEVNR      TO L004-IDLEVNR L004-IDLEVNR-BEST             
122300     MOVE I01R22-IDBEST     TO L004-IDBEST  L004-IDBEST-REG               
122400     MOVE I01R22-KDBEH-BEST TO L004-KDBEH-BEST                            
122500     MOVE I01R22-KVBEST     TO L004-KVBEST                                
122600     MOVE I01R22-TIBEST     TO L004-TIBEST                                
122700     MOVE ZERO              TO L004-KVBEST-BEKR                           
122800*                                                                         
122900***  -- BORTTAGET 96-10-16 (KA). NYTTJAS EJ (!?)                          
123000***  IF I01R22-TENOT-BESTPRIS NOT = SPACE                                 
123100***      MOVE NYUPPL-BEST-NOT       TO L004-KDCALL                        
123200***      MOVE I01R22-TENOT-BESTPRIS TO L004-TENOT-INTRANS                 
123300***      MOVE +1                    TO L004-KDNOTTYP-INTRANS              
123400***  ELSE                                                                 
123500         MOVE NYUPPL-BEST-INFO TO L004-KDCALL                             
123600***  END-IF                                                               
123700     .                                                                    
123800     EJECT                                                                
123900******************************************************************        
124000*                                                                *        
124100*    SKRIVER TRANSAKTION FÖR UPPLÄGG PÅ HÄNDELSEREGISTRET        *        
124200*    BYTE AV HUVUDLEVERANTÖR (HT 2222)                           *        
124300*                                                                *        
124400******************************************************************        
124500     SKIP2                                                                
124600 S06-BYTE-HUVLEV SECTION.                                                 
124700                                                                          
124800     MOVE 'R01'               TO R01-IDPTYP  POSTSUM-TRANSTYP             
124900     MOVE L009-IDARTNR        TO R01-IDARTNR                              
125000     MOVE L009-IDLEVNR        TO R01-IDLEVNR                              
125100     MOVE I01R23-IDLEVNR-SHIP TO R01-IDLEVNR-SHIP                         
125200     MOVE 'W212'              TO R01-IDSYSTEM                             
125300                                                                          
125400     WRITE R01-POST FROM R01-AREA                                         
125500                                                                          
125600     MOVE 'W21231'       TO POSTSUM-FDNAMN                                
125700     MOVE 'W21202D6'     TO POSTSUM-DDNAMN2                               
125800     CALL POSTSUM USING POSTSUM-PARM                                      
125900     .                                                                    
126000     EJECT                                                                
126100******************************************************************        
126200*                                                                *        
126300*    SKRIVER TRANSAKTION FÖR UPPLÄGG PÅ HÄNDELSERERGISTRET       *        
126400*    OMSPEC AV LEVERANSPLAN (HT 2204)                            *        
126500*                                                                *        
126600******************************************************************        
126700     SKIP2                                                                
126800 S07-OMSPEC-LEVPLAN SECTION.                                              
126900                                                                          
127000     INITIALIZE 2204-W2132204                                             
127100                                                                          
127200     MOVE '2204'         TO 2204-IDHTYP  POSTSUM-TRANSTYP                 
127300     MOVE L009-IDARTNR   TO 2204-IDARTNR                                  
127400     MOVE L009-KDLPORS   TO 2204-KDLPORS                                  
127500     MOVE WC-CDC-SE      TO 2204-IDDC                                     
127600                                                                          
127700     WRITE 2204-POST FROM 2204-AREA                                       
127800                                                                          
127900     MOVE 'W21232'       TO POSTSUM-FDNAMN                                
128000     MOVE 'W21202D7'     TO POSTSUM-DDNAMN2                               
128100     CALL POSTSUM USING POSTSUM-PARM                                      
128200     .                                                                    
128300     EJECT                                                                
128400******************************************************************        
128500*                                                                *        
128600*    SKRIVER TRANSAKTION FÖR UPPLÄGG PÅ HÄNDELSERERGISTRET       *        
128700*    OMRÄKNING AV TID (HT 2213)                                  *        
128800*                                                                *        
128900******************************************************************        
129000     SKIP2                                                                
129100 S08-NY-TID SECTION.                                                      
129200                                                                          
129300     MOVE '2213'         TO 2213-IDHTYP  POSTSUM-TRANSTYP                 
129400     MOVE L009-IDARTNR   TO 2213-IDARTNR                                  
129500                                                                          
129600     WRITE 2213-POST FROM 2213-AREA                                       
129700                                                                          
129800     MOVE 'W21232'       TO POSTSUM-FDNAMN                                
129900     MOVE 'W21202D7'     TO POSTSUM-DDNAMN2                               
130000     CALL POSTSUM USING POSTSUM-PARM                                      
130100     .                                                                    
130200     EJECT                                                                
130300 S13-SKRIV-NYPON-POST SECTION.                                            
130400     SKIP2                                                                
130500     IF I01R22-IDPTYP = 'R22'                                             
130600       WRITE NYPON-R22-POST FROM I01R22-AREA                              
130700     ELSE                                                                 
130800       WRITE NYPON-R23-POST FROM I01R23-AREA                              
130900     END-IF                                                               
131000     MOVE 'W21211' TO POSTSUM-FDNAMN                                      
131100     MOVE 'W21202D4' TO POSTSUM-DDNAMN2                                   
131200     MOVE I01R22-IDPTYP TO POSTSUM-TRANSTYP                               
131300     CALL POSTSUM USING POSTSUM-PARM                                      
131400     .                                                                    
131500     EJECT                                                                
131600 S14-UPPDAT-BEST-INFO  SECTION.                                           
131700                                                                          
131800     MOVE LOW-VALUE           TO DBUP-BEST-AREA                           
131900     MOVE 'UBE'               TO DBUP-BEST-IDPTYP                         
132000                                 POSTSUM-TRANSTYP                         
132100     MOVE L004-IDARTNR        TO DBUP-BEST-IDARTNR                        
132200     MOVE L004-IDBEST         TO DBUP-BEST-IDBEST                         
132300     MOVE L004-KVBEST-BEKR    TO DBUP-BEST-KVBEST-BEKR                    
132400     MOVE L004-KDBEH-BEST     TO DBUP-BEST-KDBEH-BEST                     
132500                                                                          
132600     WRITE DBUP-BEST-POST FROM DBUP-BEST-AREA                             
132700                                                                          
132800     MOVE 'W21203'          TO POSTSUM-FDNAMN                             
132900     MOVE 'W21202D5'        TO POSTSUM-DDNAMN2                            
133000     CALL POSTSUM USING POSTSUM-PARM                                      
133100     .                                                                    
133200     EJECT                                                                
133300 S14-NYUPPL-BEST-INFO SECTION.                                            
133400                                                                          
133500     MOVE 'NBE'               TO DBUP-BEST-IDPTYP                         
133600                                 POSTSUM-TRANSTYP                         
133700     MOVE L004-IDARTNR        TO DBUP-BEST-IDARTNR                        
133800     MOVE L004-IDBEST-REG     TO DBUP-BEST-IDBEST                         
133900     MOVE L004-IDLEVNR-BEST   TO DBUP-BEST-IDLEVNR-BEST                   
134000     MOVE L004-KDBEH-BEST     TO DBUP-BEST-KDBEH-BEST                     
134100     MOVE L004-KVBEST         TO DBUP-BEST-KVBEST                         
134200     MOVE L004-KVBEST-BEKR    TO DBUP-BEST-KVBEST-BEKR                    
134300     MOVE L004-TIBEST         TO DBUP-BEST-TIBEST                         
134400                                                                          
134500     WRITE DBUP-BEST-POST FROM DBUP-BEST-AREA                             
134600                                                                          
134700     MOVE 'W21203'          TO POSTSUM-FDNAMN                             
134800     MOVE 'W21202D5'        TO POSTSUM-DDNAMN2                            
134900     CALL POSTSUM USING POSTSUM-PARM                                      
135000                                                                          
135100     IF WS-IDLEVNR-MOTSV > SPACE         AND                              
135200        (I01R22-IDPTYP       = 'R22'     AND                              
135300         I01R22-KDBEH-BEST   = NYTT-ANNU)                                 
135400*       OM LEV = GSDB FÖRSÖKER VI TA BORT AVT MED NUM LEVNR               
135500        MOVE LOW-VALUE           TO DBUP-AVT-AREA                         
135600        MOVE 'BAV'               TO DBUP-AVT-IDPTYP                       
135700        MOVE L004-IDARTNR        TO DBUP-AVT-IDARTNR                      
135800        MOVE L004-IDBEST-REG     TO DBUP-AVT-IDAVTAL                      
135900        MOVE L004-TIBEST         TO DBUP-AVT-TIAVTAL                      
136000        MOVE I01R23-IDLEVNR-SHIP TO DBUP-AVT-IDLEVNR-SHIP                 
136100        MOVE WS-IDLEVNR-MOTSV    TO DBUP-AVT-IDLEVNR-AVT                  
136200        WRITE DBUP-AVT-POST FROM DBUP-AVT-AREA                            
136300        MOVE 'BAVN'           TO POSTSUM-TRANSTYP                         
136400        MOVE 'W21203'         TO POSTSUM-FDNAMN                           
136500        MOVE 'W21202D5'       TO POSTSUM-DDNAMN2                          
136600        CALL POSTSUM USING POSTSUM-PARM                                   
136700     END-IF                                                               
136800     .                                                                    
136900     EJECT                                                                
137000 S14-NYUPPL-AVT-INFO SECTION.                                             
137100                                                                          
137200     MOVE 'NAV'               TO DBUP-AVT-IDPTYP                          
137300                                 POSTSUM-TRANSTYP                         
137400     MOVE L004-IDARTNR        TO DBUP-AVT-IDARTNR                         
137500     MOVE L004-IDAVTAL-REG    TO DBUP-AVT-IDAVTAL                         
137600     MOVE L004-IDLEVNR-AVT    TO DBUP-AVT-IDLEVNR-AVT                     
137700     MOVE I01R23-IDLEVNR-SHIP TO DBUP-AVT-IDLEVNR-SHIP                    
137800     MOVE L004-KDBEH-AVT      TO DBUP-AVT-KDBEH-AVT                       
137900     MOVE L004-KVAVTANT       TO DBUP-AVT-KVAVTANT                        
138000     MOVE L004-TIAVTAL        TO DBUP-AVT-TIAVTAL                         
138100                                                                          
138200     WRITE DBUP-AVT-POST FROM DBUP-AVT-AREA                               
138300                                                                          
138400     MOVE 'W21203'          TO POSTSUM-FDNAMN                             
138500     MOVE 'W21202D5'        TO POSTSUM-DDNAMN2                            
138600     CALL POSTSUM USING POSTSUM-PARM                                      
138700     .                                                                    
138800     EJECT                                                                
138900 S15-BORTTAG-BEST-INFO SECTION.                                           
139000     SKIP3                                                                
139100*                  SKRIV POST FÖR BORTTAG AV BESTÄLLNINGS-SEGM            
139200*                  MOTTAGANDE PROGRAM KONTROLLERAR OM ÄVEN                
139300*                  ETT ELLER FLERA AVTAL MED SAMMA ID FINNS.              
139400*                  I SÅ FALL TAS ÄVEN DESSA BORT.                         
139500                                                                          
139600     MOVE LOW-VALUE           TO DBUP-BEST-AREA                           
139700     MOVE 'BBE'               TO DBUP-BEST-IDPTYP                         
139800                                POSTSUM-TRANSTYP                          
139900     MOVE L005-IDARTNR        TO DBUP-BEST-IDARTNR                        
140000     MOVE L005-IDBEST         TO DBUP-BEST-IDBEST                         
140100     MOVE L005-TIBEST         TO DBUP-BEST-TIBEST                         
140200                                                                          
140300     WRITE DBUP-BEST-POST FROM DBUP-BEST-AREA                             
140400                                                                          
140500     MOVE 'W21203'          TO POSTSUM-FDNAMN                             
140600     MOVE 'W21202D5'        TO POSTSUM-DDNAMN2                            
140700     CALL POSTSUM USING POSTSUM-PARM                                      
140800                                                                          
140900     IF WS-IDLEVNR-MOTSV > SPACE         AND                              
141000        (I01R22-IDPTYP       = 'R22'     AND                              
141100         I01R22-KDBEH-BEST   = NYTT-ANNU)                                 
141200*       OM LEV = GSDB FÖRSÖKER VI TA BORT AVT MED NUM LEVNR               
141300        MOVE LOW-VALUE           TO DBUP-AVT-AREA                         
141400        MOVE 'BAV'               TO DBUP-AVT-IDPTYP                       
141500        MOVE L005-IDARTNR        TO DBUP-AVT-IDARTNR                      
141600        MOVE L005-IDBEST         TO DBUP-AVT-IDAVTAL                      
141700        MOVE L005-TIBEST         TO DBUP-AVT-TIAVTAL                      
141800        MOVE I01R23-IDLEVNR-SHIP TO DBUP-AVT-IDLEVNR-SHIP                 
141900        MOVE WS-IDLEVNR-MOTSV    TO DBUP-AVT-IDLEVNR-AVT                  
142000        WRITE DBUP-AVT-POST FROM DBUP-AVT-AREA                            
142100        MOVE 'BAVB'              TO POSTSUM-TRANSTYP                      
142200        MOVE 'W21203'            TO POSTSUM-FDNAMN                        
142300        MOVE 'W21202D5'          TO POSTSUM-DDNAMN2                       
142400        CALL POSTSUM USING POSTSUM-PARM                                   
142500     END-IF                                                               
142600     .                                                                    
142700     EJECT                                                                
142800 S15-BORTTAG-AVTAL-INFO  SECTION.                                         
142900                                                                          
143000     MOVE LOW-VALUE           TO DBUP-AVT-AREA                            
143100     MOVE 'BAV'               TO DBUP-AVT-IDPTYP                          
143200                                 POSTSUM-TRANSTYP                         
143300     MOVE L005-IDARTNR        TO DBUP-AVT-IDARTNR                         
143400     MOVE L005-IDAVTAL        TO DBUP-AVT-IDAVTAL                         
143500     MOVE I01R23-IDLEVNR-SHIP TO DBUP-AVT-IDLEVNR-SHIP                    
143600     MOVE W-IDLEVNR-AVT       TO DBUP-AVT-IDLEVNR-AVT                     
143700                                                                          
143800     WRITE DBUP-AVT-POST FROM DBUP-AVT-AREA                               
143900                                                                          
144000     MOVE 'W21203'          TO POSTSUM-FDNAMN                             
144100     MOVE 'W21202D5'        TO POSTSUM-DDNAMN2                            
144200     CALL POSTSUM USING POSTSUM-PARM                                      
144300     .                                                                    
144400     EJECT                                                                
144500 S16-AENDRA-LEVPLAN-INFO SECTION.                                         
144600                                                                          
144700     EVALUATE L006-KDCALL                                                 
144800     WHEN UPPDAT-LEVPLAN-INFO                                             
144900       MOVE 'ULE'             TO DBUP-LEVPL-IDPTYP                        
145000     WHEN NYUPPL-LEVPLAN-INFO                                             
145100       MOVE 'NLE'             TO DBUP-LEVPL-IDPTYP                        
145200     END-EVALUATE                                                         
145300     MOVE DBUP-LEVPL-IDPTYP   TO POSTSUM-TRANSTYP                         
145400                                                                          
145500     MOVE L006-IDARTNR        TO DBUP-LEVPL-IDARTNR                       
145600     MOVE L006-IDLEVNR        TO DBUP-LEVPL-IDLEVNR                       
145700     MOVE I01R23-IDLEVNR-SHIP TO DBUP-LEVPL-IDLEVNR-SHIP                  
145800     MOVE L006-KVBR           TO DBUP-LEVPL-KVBR                          
145900     MOVE L006-TILEVPL        TO DBUP-LEVPL-TILEVPL                       
146000                                                                          
146100     WRITE DBUP-LEVPL-POST FROM DBUP-LEVPL-AREA                           
146200                                                                          
146300     MOVE 'W21203'          TO POSTSUM-FDNAMN                             
146400     MOVE 'W21202D5'        TO POSTSUM-DDNAMN2                            
146500     CALL POSTSUM USING POSTSUM-PARM                                      
146600     .                                                                    
146700     EJECT                                                                
146800 S18-UPPDAT-MTRLF-INFO SECTION.                                           
146900                                                                          
147000     MOVE 'UMF'             TO DBUP-MTRLF-IDPTYP POSTSUM-TRANSTYP         
147100     MOVE L008-IDARTNR      TO DBUP-MTRLF-IDARTNR                         
147200     MOVE L008-KDAVT        TO DBUP-MTRLF-KDAVT                           
147300     MOVE L008-KDKSP        TO DBUP-MTRLF-KDKSP                           
147400     MOVE L008-IDINK        TO DBUP-MTRLF-IDINK                           
147500                                                                          
147600     WRITE DBUP-MTRLF-POST FROM DBUP-MTRLF-AREA                           
147700                                                                          
147800     MOVE 'W21203'          TO POSTSUM-FDNAMN                             
147900     MOVE 'W21202D5'        TO POSTSUM-DDNAMN2                            
148000     CALL POSTSUM USING POSTSUM-PARM                                      
148100     .                                                                    
