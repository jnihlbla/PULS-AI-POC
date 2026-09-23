001102 ID DIVISION.                                                             
001103 PROGRAM-ID.     W6125900.                                                
001104 AUTHOR.         BODIL LINDAHL.                                           
001105 DATE-WRITTEN.   JANUARI 2007.                                            
001106 DATE-COMPILED.                                                           
001107                                                                          
001108*                                                                         
001109*    FUNCTION:                                                            
001110*        LÄSER FIL W61249 (AVVIKELSE VID REFILL INLEVERANS)               
001111*        SKAPADE UNDER VECKAN OCH SKAPAR LISTA                            
001133*                                                                         
001135*    ABENDCODES:                                                          
001137*        U1000 - D&P ERROR                                                
001138*                                                                         
001140                                                                          
001141     SKIP3                                                                
001142 ENVIRONMENT DIVISION.                                                    
001143     SKIP2                                                                
001144 INPUT-OUTPUT SECTION.                                                    
001145                                                                          
001146 FILE-CONTROL.                                                            
001147     SKIP2                                                                
001148*          --- INFIL                                                      
001149     SELECT W61249                     ASSIGN TO W61259D1.                
001150     SKIP2                                                                
001156 DATA DIVISION.                                                           
001157     SKIP3                                                                
001158 FILE SECTION.                                                            
001159     SKIP3                                                                
001160 FD  W61249                                                               
001161     RECORDING       F                                                    
001162     BLOCK CONTAINS  0.                                                   
001163                                                                          
001164*01  -COPY W6124A    -L.                                                  
001165     SKIP3                                                                
001178 WORKING-STORAGE SECTION.                                                 
001179                                                                          
001180 77  IDPGM                       PIC X(8)    VALUE 'W6125900'.            
001181 77  JA                          PIC X       VALUE 'J'.                   
001182 77  NEJ                         PIC X       VALUE 'N'.                   
001183 77  WS-PRINT                    PIC X       VALUE 'N'.                   
001184 77  SPAR-IDDC                   PIC X(2)    VALUE SPACE.                 
001185 77  SPAR-IDFAKT                 PIC 9(7)    VALUE ZERO.                  
001186 77  SPAR-IDKUNDRF               PIC X(10)   VALUE SPACE.                 
001187 77  SPAR-IDKUNDNR               PIC 9(7)    VALUE ZERO.                  
001188 77  SPAR-IDKOLLI                PIC 9(5)    VALUE ZERO.                  
001189 77  SPAR-KDSORT1                PIC 9(1)    VALUE ZERO.                  
001190 77  WS-IDDC                     PIC X(2)    VALUE SPACE.                 
001191 77  INDX                        PIC S9(3)   VALUE +000 COMP SYNC.        
001192 77  KDRC-DISPLAY                PIC Z(5).                                
001193                                                                          
001194 77  WS-ANTAL-OVERLEV            PIC 9(5).                                
001195 77  WS-ANTAL-UNDERLEV           PIC 9(5).                                
001196 77  WS-ANTAL-DAM                PIC 9(5).                                
001197 77  WS-ANTAL-LOST               PIC 9(5).                                
001198 77  WS-ANTAL-FOUND              PIC 9(5).                                
001199 77  WS-ANTAL-NY                 PIC 9(5).                                
001200                                                                          
001201 77  WS-TOT-SUM-OVERLEV          PIC 9(5)V9(2).                           
001202 77  WS-TOT-SUM-UNDERLEV         PIC 9(5)V9(2).                           
001203 77  WS-TOT-SUM-DAM              PIC 9(5)V9(2).                           
001204 77  WS-TOT-SUM-LOST             PIC 9(5)V9(2).                           
001205 77  WS-TOT-SUM-FOUND            PIC 9(5)V9(2).                           
001206 77  WS-TOT-SUM-NY               PIC 9(5)V9(2).                           
001207 77  WS-SUMMA                    PIC 9(5)V9(2).                           
001208                                                                          
001209 77  W61249-EOF-SW               PIC X       VALUE 'N'.                   
001210     88  END-OF-W61249                       VALUE 'Y'.                   
001211     EJECT                                                                
001212*01  -COPY WDATAREA                                                       
001213     EJECT                                                                
001214 01  HDR-AREA.                                                            
001215*    03  -COPY WZ01REQU                                                   
001216*    03  -COPY WZ04HDR                                                    
001217     EJECT                                                                
001218 01  FILLER                      PIC X(16)   VALUE 'SEND-AREA'.           
001219 01  SEND-AREA.                                                           
001220*    03  -COPY WZ01SEND                                                   
001221     EJECT                                                                
001222 01  SEND-RAD-STYRTECKEN.                                                 
001223     03  STYRTECKEN-RAD          PIC X.                                   
001224     03  SEND-RAD                PIC X(120)  VALUE SPACE.                 
001225*    --- CONTROL CHARACTERS                                               
001226 01  WS-SKIP1                    PIC X       VALUE ' '.                   
001227 01  WS-SKIP2                    PIC X       VALUE '0'.                   
001228 01  WS-SKIP3                    PIC X       VALUE '-'.                   
001229 01  WS-PAGESKIP                 PIC X       VALUE '1'.                   
001230     EJECT                                                                
001231                                                                          
001232 01  FILLER                      PIC X(16)   VALUE 'BOLIST'.              
001233                                                                          
001234     EJECT                                                                
001235*    --- LISTLAYOUT                                                       
001236 01  LISTA.                                                               
001237     03  RUBRIK-1.                                                        
001238         05  FILLER     PIC X       VALUE SPACE.                          
001239         05  FILLER     PIC X(15)   VALUE 'VCCS W61259-001'.              
001240         05  FILLER     PIC X(3)    VALUE SPACE.                          
001241         05  FILLER     PIC X(2)    VALUE SPACE.                          
001242         05  FILLER     PIC X       VALUE SPACE.                          
001243         05  RUB1-IDDC  PIC X(2)    VALUE SPACE.                          
001244         05  FILLER     PIC X(3)    VALUE SPACE.                          
001245         05  FILLER     PIC X(6)    VALUE SPACE.                          
001246         05  FILLER     PIC X(2)    VALUE SPACE.                          
001250         05  RUB1-DAT   PIC X(6)    VALUE SPACE.                          
001263     03  RUBRIK-2.                                                        
001264         05  FILLER           PIC X       VALUE SPACE.                    
001265         05  RUB2-DC          PIC X(2)    VALUE 'DC'.                     
001266         05  FILLER           PIC X       VALUE SPACE.                    
001267         05  RUB2-IDDC        PIC X(2)    VALUE SPACE.                    
001268         05  FILLER           PIC X(2)    VALUE SPACE.                    
001269         05  FILLER           PIC X(34)   VALUE                           
001270                  'AVVIKELSE REFILL INLEVERANS VECKA '.                   
001271         05  RUB2-AAVV        PIC X(4).                                   
001296     03  RUBRIK-3.                                                        
001297         05  FILLER           PIC X(26)   VALUE SPACE.                    
001298         05  FILLER           PIC X(5)    VALUE 'ANTAL'.                  
001299         05  FILLER           PIC X(7)    VALUE SPACE.                    
001300         05  FILLER           PIC X(34)   VALUE 'TOT VÄRDE'.              
001305     03  RAD-1.                                                           
001306         05  FILLER           PIC X       VALUE SPACE.                    
001307         05  FILLER           PIC X(12)   VALUE 'ÖVERLEVERANS'.           
001308         05  FILLER           PIC X(13)   VALUE SPACE.                    
001309         05  RAD1-ANTAL       PIC ZZZZ9.                                  
001311         05  FILLER           PIC X(8)    VALUE SPACE.                    
001312         05  RAD1-TOT-SUMMA   PIC ZZZZ9V,99.                              
001313     03  RAD-2.                                                           
001314         05  FILLER           PIC X       VALUE SPACE.                    
001315         05  FILLER           PIC X(13)   VALUE 'UNDERLEVERANS'.          
001316         05  FILLER           PIC X(12)   VALUE SPACE.                    
001317         05  RAD2-ANTAL       PIC ZZZZ9.                                  
001318         05  FILLER           PIC X(8)    VALUE SPACE.                    
001319         05  RAD2-TOT-SUMMA   PIC ZZZZ9V,99.                              
001320     03  RAD-3.                                                           
001321         05  FILLER           PIC X       VALUE SPACE.                    
001322         05  FILLER           PIC X(5)    VALUE 'SKROT'.                  
001323         05  FILLER           PIC X(20)   VALUE SPACE.                    
001324         05  RAD3-ANTAL       PIC ZZZZ9.                                  
001325         05  FILLER           PIC X(8)    VALUE SPACE.                    
001326         05  RAD3-TOT-SUMMA   PIC ZZZZ9V,99.                              
001327     03  RAD-4.                                                           
001328         05  FILLER           PIC X       VALUE SPACE.                    
001329         05  FILLER           PIC X(14)   VALUE 'FÖRLORAT KOLLI'.         
001330         05  FILLER           PIC X(11)   VALUE SPACE.                    
001331         05  RAD4-ANTAL       PIC ZZZZ9.                                  
001332         05  FILLER           PIC X(8)    VALUE SPACE.                    
001333         05  RAD4-TOT-SUMMA   PIC ZZZZ9V,99.                              
001334     03  RAD-5.                                                           
001335         05  FILLER           PIC X       VALUE SPACE.                    
001336         05  FILLER           PIC X(16)   VALUE                           
001337               'ÅTERFUNNET KOLLI'.                                        
001338         05  FILLER           PIC X(9)    VALUE SPACE.                    
001339         05  RAD5-ANTAL       PIC ZZZZ9.                                  
001340         05  FILLER           PIC X(8)    VALUE SPACE.                    
001341         05  RAD5-TOT-SUMMA   PIC ZZZZ9V,99.                              
001342     03  RAD-6.                                                           
001343         05  FILLER           PIC X       VALUE SPACE.                    
001344         05  FILLER           PIC X(19)   VALUE                           
001345               'EJ BESTÄLLD ARTIKEL'.                                     
001346         05  FILLER           PIC X(6)    VALUE SPACE.                    
001347         05  RAD6-ANTAL       PIC ZZZZ9.                                  
001348         05  FILLER           PIC X(8)    VALUE SPACE.                    
001349         05  RAD6-TOT-SUMMA   PIC ZZZZ9V,99.                              
001370                                                                          
001371                                                                          
001372 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
001373 01  FILLER REDEFINES DAGENS-DATUM.                                       
001374     03  DAGENS-AA               PIC 9(2).                                
001375     03  DAGENS-MM               PIC 9(2).                                
001376     03  DAGENS-DD               PIC 9(2).                                
001377     EJECT                                                                
001378 01  GENERAL-SUBPROGRAMS.                                                 
001379*                                                                         
001380     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
001381     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
001382     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
001383     03  WZ01SEND                PIC X(8)    VALUE 'WZ01SEND'.            
001384     03  WZ01RECV                PIC X(8)    VALUE 'WZ01RECV'.            
001385     SKIP2                                                                
001386*    --- PARAMETERS TO ABEND                                              
001387                                                                          
001388 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
001389 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
001390 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
001391     SKIP2                                                                
001392 01  ERRTEXT.                                                             
001393     03  FILLER                  PIC X(8)    VALUE 'ERRTEXT'.             
001394     03  ERRTEXT-STR             PIC X(72)   VALUE SPACE.                 
001395     EJECT                                                                
001396*    --- PARAMETRAR TILL POSTSUM                                          
001397*                                                                         
001398*01  -COPY W0005   -PRE  POSTSUM-                                         
001399     EJECT                                                                
001400 01  IN-AREA-START               PIC X(24)   VALUE                        
001401                                 'IN-AREA-START  '.                       
001402     SKIP2                                                                
001403*01  AREA -COPY W6124A      -PRE IN-                                      
001404     EJECT                                                                
001405                                                                          
001406 LINKAGE SECTION.                                                         
001407                                                                          
001408*01  -COPY W0009            -PRE MSG-                                     
001409                                                                          
001410*01  -COPY W0009            -PRE DISTRDOC-                                
001411     EJECT                                                                
001412 PROCEDURE DIVISION  USING MSG-PCB DISTRDOC-PCB.                          
001413 MAIN SECTION.                                                            
001414     ENTRY 'DLITCBL' USING MSG-PCB DISTRDOC-PCB.                          
001415                                                                          
001416     PERFORM A-INIT                                                       
001417                                                                          
001418     PERFORM S01-LAS-W61249                                               
001419     PERFORM UNTIL END-OF-W61249                                          
001421       MOVE IN-IDDC-REC TO SPAR-IDDC                                      
001422                           WS-IDDC                                        
001428       PERFORM S90-SEND-OPEN                                              
001429       PERFORM S90-PUT-DAP-START                                          
001430                                                                          
001431       PERFORM B-INIT-DC                                                  
001432       PERFORM C-PRINT-HEAD                                               
001433       PERFORM UNTIL END-OF-W61249 OR                                     
001434         (SPAR-IDDC NOT = IN-IDDC-REC)                                    
001437           PERFORM D-RAD-DATA                                             
001441         PERFORM S01-LAS-W61249                                           
001442       END-PERFORM                                                        
001443       PERFORM E-SKAPA-UTRAD                                              
001444       PERFORM S90-SEND-CLOSE                                             
001445       PERFORM S02-NOLLSTALL                                              
001446     END-PERFORM                                                          
001450                                                                          
001700     PERFORM Z-FINIT                                                      
001800                                                                          
001900     MOVE ZERO TO RETURN-CODE                                             
002000     GOBACK                                                               
002100     .                                                                    
002200     EJECT                                                                
002300 A-INIT SECTION.                                                          
002400                                                                          
002500     OPEN INPUT W61249                                                    
002800     ACCEPT DAGENS-DATUM FROM DATE                                        
002801     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
002810                                                                          
002900     MOVE 'IDAG  ' TO DAT-KDDATFORM                                       
002910     CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                      
002920                     DAT-O-TIDATUM DAT-KDSVAR                             
002940     IF DAT-KDSVAR-OK                                                     
002950       MOVE DAT-TIAAVV-GRP TO RUB2-AAVV                                   
002960     END-IF                                                               
002970                                                                          
002980     PERFORM S02-NOLLSTALL                                                
005500     .                                                                    
010000     EJECT                                                                
010100 B-INIT-DC SECTION.                                                       
010200                                                                          
010300     MOVE IN-IDDC-REC                TO RUB2-IDDC                         
010400     .                                                                    
010500     EJECT                                                                
018600 C-PRINT-HEAD SECTION.                                                    
018610                                                                          
018620     MOVE SPACE                      TO SEND-RAD-STYRTECKEN               
018660     MOVE WS-SKIP2                   TO STYRTECKEN-RAD                    
018661     MOVE SPACE                      TO SEND-RAD                          
018670     PERFORM S90-PUT-DOC-LINE                                             
018690     MOVE RUBRIK-1                   TO SEND-RAD                          
018691     PERFORM S90-PUT-DOC-LINE                                             
018692                                                                          
018693     MOVE WS-SKIP3                   TO STYRTECKEN-RAD                    
018694     MOVE SPACE                      TO SEND-RAD                          
018695     PERFORM S90-PUT-DOC-LINE                                             
018696     MOVE IN-IDDC-REC                TO RUB2-IDDC                         
018697     MOVE DAT-TIAAVV-GRP             TO RUB2-AAVV                         
018698     MOVE RUBRIK-2                   TO SEND-RAD                          
018699     PERFORM S90-PUT-DOC-LINE                                             
018700                                                                          
018701     MOVE WS-SKIP2                   TO STYRTECKEN-RAD                    
018702     MOVE SPACE                      TO SEND-RAD                          
018703     PERFORM S90-PUT-DOC-LINE                                             
018704     .                                                                    
018705     EJECT                                                                
018706 D-RAD-DATA SECTION.                                                      
018707                                                                          
018708     IF IN-AVVIKELSETYP = 'ÖVERLEV.'                                      
018709        ADD +1 TO WS-ANTAL-OVERLEV                                        
018710        COMPUTE WS-SUMMA = IN-KVANTAL * IN-PRARTSTD                       
018711        ADD WS-SUMMA TO WS-TOT-SUM-OVERLEV                                
018712        PERFORM S03-SPARA-SORT                                            
018713     ELSE                                                                 
018714      IF IN-AVVIKELSETYP = 'UNDERLEV.'                                    
018715         ADD +1 TO WS-ANTAL-UNDERLEV                                      
018716         COMPUTE WS-SUMMA = IN-KVANTAL * IN-PRARTSTD                      
018717         ADD WS-SUMMA TO WS-TOT-SUM-UNDERLEV                              
018719         PERFORM S03-SPARA-SORT                                           
018720      ELSE                                                                
018721       IF IN-AVVIKELSETYP = 'DAM'                                         
018722          ADD +1 TO WS-ANTAL-DAM                                          
018723          COMPUTE WS-SUMMA = IN-KVANTAL * IN-PRARTSTD                     
018724          ADD WS-SUMMA TO WS-TOT-SUM-DAM                                  
018726          PERFORM S03-SPARA-SORT                                          
018727       ELSE                                                               
018728        IF IN-AVVIKELSETYP = 'NY'                                         
018729          ADD +1 TO WS-ANTAL-NY                                           
018730          COMPUTE WS-SUMMA = IN-KVANTAL * IN-PRARTSTD                     
018731          ADD WS-SUMMA TO WS-TOT-SUM-NY                                   
018733          PERFORM S03-SPARA-SORT                                          
018734        END-IF                                                            
018735       END-IF                                                             
018736      END-IF                                                              
018737     END-IF                                                               
018738                                                                          
018739     IF IN-AVVIKELSETYP = 'LOST'                                          
018740        IF SPAR-IDFAKT    = IN-IDFAKT                                     
018741        AND SPAR-IDKUNDRF = IN-IDKUNDRF                                   
018742        AND SPAR-IDKUNDNR = IN-IDKUNDNR                                   
018743        AND SPAR-IDKOLLI  = IN-IDKOLLI                                    
018744        AND SPAR-KDSORT1  = IN-KDSORT1                                    
018745           COMPUTE WS-SUMMA = IN-KVANTAL * IN-PRARTSTD                    
018746           ADD WS-SUMMA TO  WS-TOT-SUM-LOST                               
018748           PERFORM S03-SPARA-SORT                                         
018751        ELSE                                                              
018752           COMPUTE WS-SUMMA = IN-KVANTAL * IN-PRARTSTD                    
018753           ADD WS-SUMMA TO  WS-TOT-SUM-LOST                               
018754           ADD +1 TO WS-ANTAL-LOST                                        
018755           PERFORM S03-SPARA-SORT                                         
018759        END-IF                                                            
018760     ELSE                                                                 
018761        IF IN-AVVIKELSETYP = 'FOUND'                                      
018762         IF SPAR-IDFAKT    = IN-IDFAKT                                    
018763            AND SPAR-IDKUNDRF = IN-IDKUNDRF                               
018764            AND SPAR-IDKUNDNR = IN-IDKUNDNR                               
018765            AND SPAR-IDKOLLI  = IN-IDKOLLI                                
018766            AND SPAR-KDSORT1  = IN-KDSORT1                                
018767              COMPUTE WS-SUMMA = IN-KVANTAL * IN-PRARTSTD                 
018768              ADD WS-SUMMA TO WS-TOT-SUM-FOUND                            
018769              PERFORM S03-SPARA-SORT                                      
018770          ELSE                                                            
018771              COMPUTE WS-SUMMA = IN-KVANTAL * IN-PRARTSTD                 
018772              ADD WS-SUMMA TO WS-TOT-SUM-FOUND                            
018773              ADD +1 TO WS-ANTAL-FOUND                                    
018774              PERFORM S03-SPARA-SORT                                      
018775          END-IF                                                          
018776        END-IF                                                            
018777     END-IF                                                               
018778     .                                                                    
018779     EJECT                                                                
018780 E-SKAPA-UTRAD SECTION.                                                   
018781                                                                          
018782     MOVE WS-ANTAL-OVERLEV      TO RAD1-ANTAL                             
018783     MOVE WS-TOT-SUM-OVERLEV    TO RAD1-TOT-SUMMA                         
018784     MOVE RAD-1                 TO SEND-RAD                               
018785     PERFORM S90-PUT-DOC-LINE                                             
018786                                                                          
018787     MOVE WS-ANTAL-UNDERLEV     TO RAD2-ANTAL                             
018788     MOVE WS-TOT-SUM-UNDERLEV   TO RAD2-TOT-SUMMA                         
018789     MOVE RAD-2                 TO SEND-RAD                               
018790     PERFORM S90-PUT-DOC-LINE                                             
018791                                                                          
018792     MOVE WS-ANTAL-DAM          TO RAD3-ANTAL                             
018793     MOVE WS-TOT-SUM-DAM        TO RAD3-TOT-SUMMA                         
018794     MOVE RAD-3                 TO SEND-RAD                               
018795     PERFORM S90-PUT-DOC-LINE                                             
018796                                                                          
018797     MOVE WS-ANTAL-LOST         TO RAD4-ANTAL                             
018798     MOVE WS-TOT-SUM-LOST       TO RAD4-TOT-SUMMA                         
018799     MOVE RAD-4                 TO SEND-RAD                               
018800     PERFORM S90-PUT-DOC-LINE                                             
018801                                                                          
018802     MOVE WS-ANTAL-FOUND        TO RAD5-ANTAL                             
018803     MOVE WS-TOT-SUM-FOUND      TO RAD5-TOT-SUMMA                         
018804     MOVE RAD-5                 TO SEND-RAD                               
018805     PERFORM S90-PUT-DOC-LINE                                             
018806                                                                          
018807     MOVE WS-ANTAL-NY           TO RAD6-ANTAL                             
018808     MOVE WS-TOT-SUM-NY         TO RAD6-TOT-SUMMA                         
018809     MOVE RAD-6                 TO SEND-RAD                               
018810     PERFORM S90-PUT-DOC-LINE                                             
018811     .                                                                    
018812     EJECT                                                                
018813 Z-FINIT SECTION.                                                         
018820                                                                          
018900     CLOSE W61249                                                         
019200     MOVE 'S' TO POSTSUM-OPKOD                                            
019300     CALL POSTSUM USING POSTSUM-PARM                                      
019400     .                                                                    
019500     EJECT                                                                
019600 S01-LAS-W61249 SECTION.                                                  
019610                                                                          
019700     READ W61249 INTO IN-AREA                                             
019800     AT END                                                               
019900        MOVE HIGH-VALUE TO IN-AREA                                        
020000        SET END-OF-W61249 TO TRUE                                         
020200     NOT AT END                                                           
020300        MOVE 'W61249'   TO POSTSUM-FDNAMN                                 
020400        MOVE 'W61259D1' TO POSTSUM-DDNAMN2                                
020500        MOVE SPACE      TO POSTSUM-TRANSTYP                               
020600        CALL POSTSUM USING POSTSUM-PARM                                   
020700     END-READ                                                             
020800     .                                                                    
020900     EJECT                                                                
020910 S02-NOLLSTALL SECTION.                                                   
020920                                                                          
021000     MOVE ZERO TO WS-ANTAL-OVERLEV                                        
021100                  WS-ANTAL-UNDERLEV                                       
021200                  WS-ANTAL-DAM                                            
021300                  WS-ANTAL-LOST                                           
021400                  WS-ANTAL-FOUND                                          
021500                  WS-ANTAL-NY                                             
021700                  WS-TOT-SUM-OVERLEV                                      
021800                  WS-TOT-SUM-UNDERLEV                                     
021900                  WS-TOT-SUM-DAM                                          
022000                  WS-TOT-SUM-LOST                                         
022100                  WS-TOT-SUM-FOUND                                        
022200                  WS-TOT-SUM-NY                                           
022300     .                                                                    
022400     EJECT                                                                
022500 S03-SPARA-SORT SECTION.                                                  
022510                                                                          
022520     MOVE IN-IDFAKT   TO SPAR-IDFAKT                                      
022530     MOVE IN-IDKUNDRF TO SPAR-IDKUNDRF                                    
022540     MOVE IN-IDKUNDNR TO SPAR-IDKUNDNR                                    
022550     MOVE IN-IDKOLLI  TO SPAR-IDKOLLI                                     
022560     MOVE IN-KDSORT1  TO SPAR-KDSORT1                                     
022600     .                                                                    
022700     EJECT                                                                
053700 S90-SEND-OPEN SECTION.                                                   
053800                                                                          
053900     MOVE 'OPEN'                        TO SEND-KDFUNC                    
054000     MOVE 'CARPARTS.DAP.DISTRDOC'       TO SEND-ADDISPABS                 
054100     CALL WZ01SEND USING SEND-CONTROL-AREA                                
054200                         SEND-OPEN-AREA                                   
054300     IF SEND-KDRC > 0                                                     
054400       MOVE SEND-KDRC TO KDRC-DISPLAY                                     
054500       STRING 'WZ01SEND OPEN ERROR RC=' KDRC-DISPLAY                      
054600       DELIMITED BY SIZE INTO ERRTEXT                                     
054700       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
054800     END-IF                                                               
054900     .                                                                    
055000     EJECT                                                                
055100 S90-PUT-DAP-START SECTION.                                               
055200                                                                          
055300     MOVE 1                       TO REQU-IDMSGVER                        
055400     MOVE 'R'                     TO REQU-KDPGMACT                        
055500     MOVE IDPGM                   TO REQU-IDUSER                          
055600     MOVE 'W61259'                TO HDR-IDOUTTYPE                        
055700     MOVE SPACE                   TO HDR-IDOUTREC                         
055800                                     HDR-IDLIST                           
055900     MOVE WS-IDDC                 TO HDR-IDOUTREC (1:2)                   
056000                                     HDR-IDLIST                           
056010     MOVE 'PUT'                   TO SEND-KDFUNC                          
056100     MOVE LENGTH OF HDR-AREA      TO SEND-KVDLEN                          
056200     CALL WZ01SEND USING SEND-CONTROL-AREA                                
056300                         SEND-KVDLEN                                      
056400                         HDR-AREA                                         
056500     IF SEND-KDRC > ZERO                                                  
056600       MOVE SEND-KDRC             TO KDRC-DISPLAY                         
056700       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
056800       DELIMITED BY SIZE INTO ERRTEXT-STR                                 
056900       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
057000     END-IF                                                               
057100     .                                                                    
057200     EJECT                                                                
057300 S90-PUT-DOC-LINE SECTION.                                                
057400                                                                          
057500     MOVE 'PUT'                           TO SEND-KDFUNC                  
057510*                     -- UTAN STYRTECKEN:                                 
057600     MOVE LENGTH OF SEND-RAD              TO SEND-KVDLEN                  
057700     CALL WZ01SEND USING SEND-CONTROL-AREA                                
057800                         SEND-KVDLEN                                      
057810*                     -- UTAN STYRTECKEN:                                 
057900                         SEND-RAD                                         
058000     IF SEND-KDRC > ZERO                                                  
058100       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
058200       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
058300       DELIMITED BY SIZE INTO ERRTEXT-STR                                 
058400       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
058500     END-IF                                                               
058600     .                                                                    
058700     EJECT                                                                
058800 S90-SEND-CLOSE SECTION.                                                  
058900                                                                          
059000     MOVE 'CLOSE'                    TO SEND-KDFUNC                       
059100     CALL WZ01SEND USING SEND-CONTROL-AREA                                
059200                                                                          
059300     IF SEND-KDRC > 0                                                     
059400       MOVE SEND-KDRC TO KDRC-DISPLAY                                     
059500       STRING 'WZ01SEND CLOSE ERROR RC=' KDRC-DISPLAY                     
059600       DELIMITED BY SIZE INTO ERRTEXT                                     
059700       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
059800     END-IF                                                               
059900     .                                                                    
060000     EJECT                                                                
