000100 ID DIVISION.                                                             
000201 PROGRAM-ID.    W6016110.                                                 
000301 AUTHOR.        ERIK RINGQVIST / ARCHANA BHAT.                            
000401 DATE-WRITTEN.  MAJ 1984 / JUNE 2012.                                     
000501*    REMARKS.                                                             
000601*    FUNKTION.                                                            
000701*        TP-PROGRAM FÖR REGISTRERING AV FÄLTET KDARTHNT                   
000801*        I SEGMENT 13 I ARTIKELREGISTRET.                                 
000901*                                                                         
001001*     +  STARTAR EV DISPATCH FÖR UPPDATERING AV URSPRUNG PÅ               
001101*        INLEVERANSREGISTRET.                                             
001201*                                                                         
001301*    INDATA.                                                              
001401*        REQU:        W60161I1                                            
001501*    UTDATA.                                                              
001601*        RESP:        W60161O1                                            
001701*    SUBPROGRAM.                                                          
001801*        FELLOG                                                           
001901*        W006KOM  (DISPATCH)                                              
002003*        W005WDK7                                                         
002103*    SKIP3                                                                
002203 ENVIRONMENT DIVISION.                                                    
002303*                                                                         
002403 DATA DIVISION.                                                           
002503     EJECT                                                                
002603 WORKING-STORAGE SECTION.                                                 
002703                                                                          
002803                                                                          
002903*    -- CHECKED BY WY2000                                                 
003003 77    PROGRAM-NAMN          PIC X(8)    VALUE 'W6016110'.                
003103 77    JA                    PIC X       VALUE 'J'.                       
003203 77    NEJ                   PIC X       VALUE 'N'.                       
003303 77  LNG-P-TO-P-PREFIX           PIC S9(4) COMP SYNC   VALUE +17.         
003403 77  DAGENS-DATUM                PIC 9(6)   VALUE ZERO.                   
003503 77  DAGENS-TID                  PIC 9(8)   VALUE ZERO.                   
003504 77  DADATTID                    PIC 9(14).                               
003505 77  TODAYS-DATE                 PIC S9(8)             VALUE ZERO.        
003603 77  WS-IDARTNR-NUM9             PIC 9(9)   VALUE ZERO.                   
003703*                                                                         
003803                                                                          
003903 77  FEL-FINNS-SW            PIC X       VALUE 'N'.                       
004003     88  FEL-FINNS                       VALUE 'J'.                       
004103     88  FEL-SAKNAS                      VALUE 'N'.                       
004207                                                                          
004303 77  INDATA-FINNS-SW         PIC X       VALUE 'J'.                       
004403     88  INDATA-SAKNAS                   VALUE 'N'.                       
004503     88  INDATA-FINNS                    VALUE 'J'.                       
004603                                                                          
004703 77    RAD-IX                PIC S9(9)   VALUE +1    COMP SYNC.           
004803 77    SPRAK-IX              PIC S9(9)   VALUE +1    COMP SYNC.           
004903     SKIP3                                                                
005003*      --- VALID IDDC CODES                                               
005103*                                                                         
005203*01    -COPY WWDCKONS                                                     
005303       EJECT                                                              
005403*01    -COPY WWLNDKON                                                     
005503       EJECT                                                              
005603*01    -COPY WWDC99                                                       
005703       EJECT                                                              
005803*01    -COPY WWDC99          -PRE REF-                                    
005903       EJECT                                                              
006003 01    DYNAMISKA-SUBPROGRAM.                                              
006103   03    CBLTDLI             PIC X(8)    VALUE 'CBLTDLI'.                 
006203   03    FELLOG              PIC X(8)    VALUE 'FELLOG'.                  
006303   03    W400ARTU            PIC X(8)    VALUE 'W400ARTU'.                
006403   03    W006KOM             PIC X(8)    VALUE 'W006KOM '.                
006503   03    W005WDK7            PIC X(8)    VALUE 'W005WDK7'.                
006603*                                                                         
006703 01  ALL-SPACE.                                                           
006803     03 FILLER                   PIC X(50)   VALUE SPACE.                 
006903*                                                                         
007003 01  ALL-PLUS.                                                            
007103     03 FILLER                   PIC X(50)   VALUE ALL '+'.               
007203*                                                                         
007303* - - - - - - - - - - - - - - - - - - -  NYCKLAR TILL ARTU                
007403 01  FILLER                      PIC X(16)  VALUE 'STARTARTU'.            
007503*    -COPY W400ARTU                                                       
007603     EJECT                                                                
007703* - - - - - - - - - - - - - - - - - - -  NYCKLAR TILL DLI                 
007803 01    NYCKLAR-TILL-DLI.                                                  
007903   03    W-IDARTNR-X.                                                     
008003     05    W-IDARTNR         PIC S9(9)   VALUE ZERO  COMP-3.              
008103   03    W-KDCLAGER-X.                                                    
008203     05    W-KDCLAGER        PIC S9(1)   VALUE ZERO  COMP-3.              
008303   03    W-IDLAND-X.                                                      
008403     05    W-IDLAND          PIC X(2)    VALUE SPACE.                     
008503   03    W-IDDC-X.                                                        
008603     05    W-IDDC            PIC X(2)    VALUE SPACE.                     
008703                                                                          
008803   03  W-IDDC-MIN-X.                                                      
008903       05  W-IDDC-MIN-1        PIC X(2)    VALUE LOW-VALUE.               
009003                                                                          
009103   03  W-IDDC-MAX-X.                                                      
009203       05  W-IDDC-MAX-1        PIC X(2)    VALUE HIGH-VALUE.              
009303                                                                          
009403   03  W-IDDC-REF-MIN-X.                                                  
010001       05  W-IDDC-REF-MIN      PIC X(2)    VALUE LOW-VALUE.               
020001                                                                          
030001   03  W-IDDC-REF-MAX-X.                                                  
040001       05  W-IDDC-REF-MAX      PIC X(2)    VALUE HIGH-VALUE.              
040101                                                                          
040200     SKIP3                                                                
040300 01    W-KDARTHNT            PIC 9(6).                                    
040400 01    FILLER REDEFINES W-KDARTHNT.                                       
040500   03    W-KDARTHNT-V        PIC 9(3).                                    
040600   03    W-KDARTHNT-H        PIC 9(3).                                    
040700     EJECT                                                                
040800 01 MESSAGE-CODES.                                                        
040900    03 ERR-PART-MISSING          PIC X(3) VALUE '025'.                    
041000    03 ERR-CORR-HILITE-FLDS      PIC X(3) VALUE '020'.                    
041100    03 ERR-ORIGIN-EXISTS         PIC X(3) VALUE '380'.                    
041200    03 ERR-UPDATE-NOT-VALID      PIC X(3) VALUE '007'.                    
041300                                                                          
041400    03 INF-REGIST-COMPLTD        PIC X(3) VALUE '381'.                    
041500****************************************************************          
041600*                                                                         
041700*                AREOR FÖR MFS OCH SKÄRMHANTERING                         
041800*                                                                         
041900 01    FILLER                PIC X(8)    VALUE 'MFS-WS  '.                
042000                                                                          
042100*01    MID -COPY W6I16101                                                 
042200     EJECT                                                                
042300*01    -COPY WMSGAREA                                                     
042400     EJECT                                                                
042500*  03    W6O16101 -COPY W6O16101         -RED MSG-AREA.                   
042600     EJECT                                                                
042700*01      -COPY WMFSAREA                                                   
042800     EJECT                                                                
042900 01  FILLER                   PIC X(16)  VALUE 'KOM-MSG-IO-AREA '.        
043000 01  KOM-MSG-IO-AREA.                                                     
043100*03  -COPY WMSGKOM                                                        
043200     EJECT                                                                
043303 01  FILLER                      PIC X(16)   VALUE 'W005WDK7'.            
043403*01 -COPY W005WDK7                                                        
043503     EJECT                                                                
043600                                                                          
043700 01  P-TO-P-SW.                                                           
043800     02     P-TO-P-KVLL             PIC S9(4) COMP SYNC.                  
043900     02     P-TO-P-KDZ1             PIC X(1)  VALUE LOW-VALUE.            
044000     02     P-TO-P-KDZ2             PIC X(1)  VALUE LOW-VALUE.            
044100     02     P-TO-P-KDTRANS          PIC X(8).                             
044200     02     P-TO-P-IDTRANS          PIC X(4).                             
044300     02     P-TO-P-KDMFSFOR         PIC X(1).                             
044400     02     P-TO-P-DATA             PIC X(1000).                          
044500     EJECT                                                                
044600                                                                          
044700 01      FILLER                  PIC X(24)   VALUE                        
044800                                 'MOD619B-MID-W6I19B01'.                  
044900     SKIP2                                                                
045000     -COPY W6I19B01 -PRE MOD619B-                                         
045100     EJECT                                                                
045200*-----------------------------------------------------------              
045300 01    IMS-WS.                                                            
045400   03    FILLER              PIC X(8)    VALUE 'IMS-WS  '.                
045500                                                                          
045600*                            *** STATUSKOD FRÅN IMS                       
045700   03    STATUS-WS           PIC XX.                                      
045800     88    SEGMENT-FINNS               VALUE '  '.                        
045900     88    SEGMENT-SAKNAS              VALUE 'GE'.                        
046000                                                                          
046100   03    GODK-STATUSKODER.                                                
046200     05    GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.              
046300                                                                          
046400 01    SSA1                  PIC X(32).                                   
046500 01    SSA2                  PIC X(64).                                   
046600 01    SSA3                  PIC X(32).                                   
046700                                                                          
046800*                            *** IMS FUNKTIONSKODER                       
046900*01      -COPY W0003                                                      
047000     EJECT                                                                
047100*                            *** DLI INPUT-OUTPUT AREA                    
047200 01  DLI-IO-AREA.                                                         
047300   03  FILLER              PIC X(16)   VALUE 'IO-AREA-C'.                 
047400   03  IO-AREA-C           PIC X(900)  VALUE SPACE.                       
047500                                                                          
047600*  03  WLARTC11  -COPY WDK611 -PRE ARTC- -RED IO-AREA-C.                  
047700     EJECT                                                                
047900 01  FILLER                 PIC X(16)   VALUE 'DLI-IO-WDK601'.            
048000 01  DLI-IO-WDK601.                                                       
048100*    03  -COPY WDK601                                                     
048200     EJECT                                                                
047800*                                                                         
047900 01  FILLER                 PIC X(16)   VALUE 'DLI-IO-WDK711'.            
048000 01  DLI-IO-WDK711.                                                       
048100*    03  -COPY WDK711                                                     
048200     EJECT                                                                
048300 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK712'.                      
048400 01  DLI-IO-WDK712.                                                       
048500*    03  -COPY WDK712                                                     
048600     EJECT                                                                
048601 01  FILLER                 PIC X(16)   VALUE 'DLI-IO-WDT501'.            
048602 01  DLI-IO-WDT501.                                                       
048603*    03  -COPY WDT501                                                     
048604     EJECT                                                                
048605 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDT511'.                      
048606 01  DLI-IO-WDT511.                                                       
048607*    03  -COPY WDT511                                                     
048608     EJECT                                                                
048700*    --- FÖR TEST AV BYTESNR                                              
048800*                                                                         
048900 01  FILLER                      PIC  X(16)  VALUE 'BYTES-TEST'.          
049000 01  TEST-IDARTNR                PIC  9(9)   COMP-3.                      
049100*01  FILLER  -COPY WWBYT02     -RED TEST-IDARTNR.                         
049200                                                                          
049300*01  FILLER  -COPY WWBYT16     -RED TEST-IDARTNR.                         
049400                                                                          
049500 LINKAGE SECTION.                                                         
049600                                                                          
049700 01  REQU-AREA.                                                           
049800*    03 -COPY WZ01REQU                                                    
049900*    03 -COPY W60161I1                                                    
050000     EJECT                                                                
050100 01  RESP-AREA.                                                           
050200*    03 -COPY WZ01RESP                                                    
050300*    03 -COPY W60161O1                                                    
050400     EJECT                                                                
050500 01  MAX-KVRADER                 PIC S9(4) COMP.                          
050600*01      -COPY W0009     -PRE MSG-                                        
050700     EJECT                                                                
050800*01      -COPY W0009     -PRE DISP-                                       
050900     EJECT                                                                
051000*01      -COPY W0008     -PRE ARTC-                                       
051100      05 FILLER              PIC X.                                       
051200     EJECT                                                                
051300*    PCB'ER FÖR SUBPGM                                                    
051400 01 KOM-KOMA-PCB         PIC X.                                           
051500     EJECT                                                                
051600*01      -COPY W0008     -PRE WDK7-                                       
051700      05 FILLER              PIC X.                                       
051701*01      -COPY W0008     -PRE WDT5-                                       
051702      05 FILLER              PIC X.                                       
051803     EJECT                                                                
051903 01  W005K7-WDB6-PCB     PIC X.                                           
052003 01  W005K7-WDK6-PCB     PIC X.                                           
052103 01  W005K7-WDK7-PCB     PIC X.                                           
052203     EJECT                                                                
052300 PROCEDURE DIVISION USING REQU-AREA RESP-AREA MAX-KVRADER                 
052400                          MSG-PCB DISP-PCB ARTC-PCB KOM-KOMA-PCB          
052503                          WDK7-PCB WDT5-PCB                               
052504                          W005K7-WDB6-PCB                                 
052603                          W005K7-WDK6-PCB W005K7-WDK7-PCB.                
052703                                                                          
052803 STYR SECTION.                                                            
052903     PERFORM A-INIT-SPARA-INPUT                                           
053003     PERFORM B-KONTROLLERA-INPUT                                          
053103                                                                          
053203     IF INDATA-FINNS  AND  FEL-SAKNAS                                     
053303       PERFORM C-KONTROLLERA-MOT-ARTC                                     
053403                                                                          
053503       IF FEL-SAKNAS                                                      
053603         PERFORM F-UPPDATERA-ARTC                                         
053703         PERFORM MFS-RENSA-FAELT-MODTAB                                   
053803       ELSE                                                               
053903         PERFORM MFS-ROER-EJ-FAELT-MODTAB                                 
054003       END-IF                                                             
054103     ELSE                                                                 
054203       MOVE ERR-CORR-HILITE-FLDS  TO RESP-IDMSG-ERROR                     
054303       PERFORM MFS-ROER-EJ-FAELT-MODTAB                                   
054403     END-IF                                                               
054503                                                                          
054603     GOBACK                                                               
054703                                                                          
054803     .                                                                    
054903     EJECT                                                                
055003 A-INIT-SPARA-INPUT SECTION.                                              
055103                                                                          
055203     MOVE ALL '+'      TO RESP-W60161O1                                   
055303                                                                          
055403     MOVE 001          TO RESP-IDMSGVER                                   
055503     MOVE SPACE        TO RESP-IDMSG-ERROR                                
055603                          RESP-IDMSG-INFO                                 
055703                          RESP-IDELMT-ERROR                               
055803                                                                          
055903     MOVE REQU-KVRADER TO RESP-KVRADER                                    
056003     MOVE REQU-IDDC    TO WS-IDDC                                         
056103                                                                          
056203     MOVE MSG-KDTRTYP     TO MFS-KDTRTYP                                  
056303                                                                          
056403     MOVE LOW-VALUE       TO MSG-AREA                                     
056503     MOVE 'W6O161N1'      TO MFS-IDMOD                                    
056603     MOVE '6161'          TO MOD-IDTRANS                                  
056703     MOVE SPACE           TO MOD-MESSAGE-RAD1                             
056803                             MOD-MESSAGE-RAD23                            
056903                                                                          
057003     ACCEPT DAGENS-DATUM       FROM DATE                                  
057103     ACCEPT DAGENS-TID         FROM TIME                                  
057203                                                                          
057303     .                                                                    
057403     EJECT                                                                
057503 B-KONTROLLERA-INPUT SECTION.                                             
057603                                                                          
057703     MOVE +1            TO RAD-IX                                         
057803     SET  FEL-SAKNAS    TO TRUE                                           
057903     SET  INDATA-SAKNAS TO TRUE                                           
058003     MOVE REQU-KDMFSFOR TO SPRAK-IX                                       
058103                           W-KDCLAGER                                     
058203                                                                          
058303     PERFORM UNTIL RAD-IX > REQU-KVRADER                                  
058403       IF REQU-IDARTNR  (RAD-IX) NOT = ALL '+'                            
058503         IF REQU-IDARTNR (RAD-IX) NUMERIC                                 
058603           MOVE MFS-NUM-FAELT-RAETT TO RESP-IDARTNR-ATTR (RAD-IX)         
058703         ELSE                                                             
058803           MOVE MFS-NUM-FAELT-FEL   TO RESP-IDARTNR-ATTR (RAD-IX)         
058903           SET  FEL-FINNS TO TRUE                                         
059003         END-IF                                                           
059103       END-IF                                                             
059203                                                                          
059303       IF REQU-KDARTHNT-V (RAD-IX) NOT = ALL '+'                          
059403         IF  REQU-KDARTHNT-V (RAD-IX) NUMERIC                             
059503         AND REQU-IDARTNR (RAD-IX)   NUMERIC                              
059603           MOVE MFS-NUM-FAELT-RAETT                                       
059703                              TO RESP-KDARTHNT-V-ATTR (RAD-IX)            
059803           SET  INDATA-FINNS  TO TRUE                                     
059903         ELSE                                                             
060003           MOVE MFS-NUM-FAELT-FEL                                         
060103                              TO RESP-KDARTHNT-V-ATTR (RAD-IX)            
060203           SET  FEL-FINNS     TO TRUE                                     
060303         END-IF                                                           
060403       END-IF                                                             
060503                                                                          
060603       IF REQU-KDARTHNT-H (RAD-IX) NOT = ALL '+'                          
060703         IF REQU-KDARTHNT-H (RAD-IX) NUMERIC                              
060803         AND REQU-IDARTNR (RAD-IX)  NUMERIC                               
060903           MOVE MFS-NUM-FAELT-RAETT                                       
061003                              TO RESP-KDARTHNT-H-ATTR (RAD-IX)            
061103           SET  INDATA-FINNS  TO TRUE                                     
061203         ELSE                                                             
061303           MOVE MFS-NUM-FAELT-FEL                                         
061403                              TO RESP-KDARTHNT-H-ATTR (RAD-IX)            
061503           SET  FEL-FINNS     TO TRUE                                     
061603         END-IF                                                           
061703       END-IF                                                             
061803                                                                          
061903       IF REQU-KDARTURS(RAD-IX) NOT = ALL '+'                             
062003          AND REQU-IDARTNR (RAD-IX) NUMERIC                               
062103          MOVE REQU-KDARTURS(RAD-IX) TO ARTU-KDARTURS                     
062203          MOVE SPACE        TO ARTU-IDDC                                  
062303          MOVE ZERO         TO ARTU-IDDISTR                               
062403          CALL W400ARTU USING ARTU-W400ARTU                               
062503          IF ARTU-KDARTURS = SPACE OR                                     
062603             ARTU-KDARTURS-NUM = ZERO                                     
062703             MOVE MFS-ALFA-FAELT-FEL TO                                   
062803                RESP-KDARTURS-ATTR(RAD-IX)                                
062903             SET  FEL-FINNS     TO TRUE                                   
063003          ELSE                                                            
063103             MOVE MFS-ALFA-FAELT-RAETT TO                                 
063203                RESP-KDARTURS-ATTR(RAD-IX)                                
063303             SET  INDATA-FINNS  TO TRUE                                   
063403          END-IF                                                          
063503       END-IF                                                             
063603                                                                          
063703       ADD +1 TO RAD-IX                                                   
063803     END-PERFORM                                                          
063903                                                                          
064003     .                                                                    
064103     EJECT                                                                
064203 C-KONTROLLERA-MOT-ARTC      SECTION.                                     
064303                                                                          
064403     MOVE +1 TO RAD-IX                                                    
064503     PERFORM UNTIL RAD-IX > REQU-KVRADER                                  
064603                                                                          
064703       IF REQU-IDARTNR (RAD-IX) NUMERIC                                   
064803         MOVE REQU-IDARTNR (RAD-IX) TO W-IDARTNR                          
064903*                                                                         
065003         PERFORM IMS-GHU-ARTC                                             
065103         IF SEGMENT-SAKNAS                                                
065203           MOVE MFS-NUM-FAELT-FEL TO RESP-IDARTNR-ATTR (RAD-IX)           
065303           SET  FEL-FINNS         TO TRUE                                 
065403           MOVE ERR-PART-MISSING  TO RESP-IDMSG-ERROR                     
065503           MOVE 'IDARTNR'         TO RESP-IDELMT-ERROR                    
065603** FÅR BARA UPPDATERA URSPRUNG OM URSPRUNG ÄR SPACE                       
065703** FÖRUTOM MED PF11                                                       
065803** FÅR INTE UPPDATERA OM KDPCOO ÄR SATT                                   
065903         ELSE                                                             
066003           MOVE ARTC-CLAG-IDDC-REF TO REF-WS-IDDC                         
066103           IF NOT REQU-UPDATE                                             
066203             IF REQU-KDARTURS(RAD-IX) NOT = ALL '+'                       
066303               IF ARTC-CLAG-KDARTURS NOT = SPACE                          
066403                 MOVE MFS-NUM-FAELT-FEL                                   
066503                                    TO RESP-KDARTURS-ATTR(RAD-IX)         
066603                 MOVE ERR-ORIGIN-EXISTS                                   
066703                                    TO RESP-IDMSG-ERROR                   
066803                 SET  FEL-FINNS     TO TRUE                               
066903               END-IF                                                     
067003             END-IF                                                       
067103           ELSE                                                           
067203             IF NDC-CN OR NDC-US                                          
067204               MOVE WS-IDDC    TO W-IDDC                                  
067205               PERFORM IMS-GU-WDK711                                      
067206               IF SEGMENT-FINNS                                           
067207                 IF SLAG-IDDC-REF = SPACE                                 
067303                   IF NDC-CN                                              
067403                     MOVE WC-LAND-CN    TO W-IDLAND                       
067503                   ELSE                                                   
067603                     MOVE WC-LAND-US    TO W-IDLAND                       
067703                   END-IF                                                 
067803                   PERFORM IMS-GU-WDK712                                  
067903                   IF SEGMENT-SAKNAS                                      
068003                     MOVE MFS-NUM-FAELT-FEL                               
068103                                   TO RESP-IDARTNR-ATTR (RAD-IX)          
068203                     MOVE ERR-UPDATE-NOT-VALID                            
068303                                   TO RESP-IDMSG-ERROR                    
068403                     SET FEL-FINNS     TO TRUE                            
068404                   END-IF                                                 
068405                 ELSE                                                     
068406                     MOVE MFS-NUM-FAELT-FEL                               
068407                                   TO RESP-IDARTNR-ATTR (RAD-IX)          
068408                     MOVE ERR-UPDATE-NOT-VALID                            
068409                                   TO RESP-IDMSG-ERROR                    
068410                     SET FEL-FINNS     TO TRUE                            
068420                 END-IF                                                   
068503               ELSE                                                       
068504                 MOVE MFS-NUM-FAELT-FEL                                   
068505                                   TO RESP-IDARTNR-ATTR (RAD-IX)          
068506                 MOVE ERR-UPDATE-NOT-VALID                                
068507                                   TO RESP-IDMSG-ERROR                    
068508                 SET FEL-FINNS     TO TRUE                                
068509               END-IF                                                     
068603             ELSE                                                         
068703               IF REF-NDC-CN OR REF-NDC-US                                
068803                 MOVE ERR-UPDATE-NOT-VALID                                
068903                                   TO RESP-IDMSG-ERROR                    
069003                 SET FEL-FINNS     TO TRUE                                
069103               END-IF                                                     
069203               IF REQU-KDARTURS(RAD-IX) = ARTC-CLAG-KDARTURS              
069303                 CONTINUE                                                 
069403               ELSE                                                       
069503                 IF ARTC-CLAG-KDPCOO = SPACE                              
069603                   CONTINUE                                               
069703                 ELSE                                                     
069803                   IF DAGENS-DATUM > ARTC-CLAG-TIGILTIG-PCOO              
069903                     CONTINUE                                             
070003                   ELSE                                                   
070103                     CONTINUE                                             
070203*                    MOVE MFS-NUM-FAELT-FEL                               
070303*                                    TO RESP-KDARTURS-ATTR(RAD-IX)        
070403*                    SET FEL-FINNS    TO TRUE                             
070503                   END-IF                                                 
070603                 END-IF                                                   
070703               END-IF                                                     
070803             END-IF                                                       
070903           END-IF                                                         
071003         END-IF                                                           
071103       END-IF                                                             
071203                                                                          
071303       ADD +1 TO RAD-IX                                                   
071403     END-PERFORM                                                          
071503                                                                          
071603     .                                                                    
071703     EJECT                                                                
071803 F-UPPDATERA-ARTC    SECTION.                                             
071903                                                                          
072003     MOVE +1 TO RAD-IX                                                    
072103     PERFORM UNTIL RAD-IX > REQU-KVRADER                                  
072203                                                                          
072303       IF REQU-IDARTNR (RAD-IX) NUMERIC                                   
072403         MOVE REQU-IDARTNR (RAD-IX) TO W-IDARTNR                          
072503                                      WS-IDARTNR-NUM9                     
               PERFORM IMS-GU-WDK601                                            
072603         PERFORM IMS-GHU-ARTC                                             
072703         IF SEGMENT-FINNS                                                 
072803           MOVE ARTC-CLAG-IDDC-REF TO REF-WS-IDDC                         
072903           MOVE ARTC-CLAG-KDARTHNT TO W-KDARTHNT                          
073003           IF REQU-KDARTHNT-V    (RAD-IX) NUMERIC                         
073103             MOVE REQU-KDARTHNT-V (RAD-IX) TO W-KDARTHNT-V                
073203           END-IF                                                         
073303           IF REQU-KDARTHNT-H    (RAD-IX) NUMERIC                         
073403             MOVE REQU-KDARTHNT-H (RAD-IX) TO W-KDARTHNT-H                
073503           END-IF                                                         
073603           MOVE  W-KDARTHNT TO ARTC-CLAG-KDARTHNT                         
073703           PERFORM FA-UPDATE-KDARTURS                                     
073803           PERFORM IMS-REPLACE-ARTC                                       
073804           PERFORM FB-UPDATE-WDT5                                         
073903         ELSE                                                             
074003           CALL FELLOG                                                    
074103         END-IF                                                           
074203         IF REQU-KDARTURS (RAD-IX) NOT = ALL '+'                          
074303           PERFORM S01-STARTA-DISPATCHEN                                  
074403           PERFORM S02-CHECK-BYTES                                        
074503         END-IF                                                           
074603                                                                          
074703       END-IF                                                             
074803       ADD +1 TO RAD-IX                                                   
074903     END-PERFORM                                                          
075003     MOVE INF-REGIST-COMPLTD TO RESP-IDMSG-INFO                           
075103     .                                                                    
075203     EJECT                                                                
075303 FA-UPDATE-KDARTURS SECTION.                                              
075403     IF REQU-KDARTURS (RAD-IX) NOT = ALL '+'                              
075503       IF NDC-CN OR NDC-US                                                
075603         IF NDC-US                                                        
075703            MOVE WC-LAND-US     TO LART-IDLANDX2                          
075803                                   W-IDLAND                               
075903         ELSE                                                             
076001            MOVE WC-LAND-CN     TO LART-IDLANDX2                          
077001                                   W-IDLAND                               
078001         END-IF                                                           
079001         PERFORM IMS-GHU-WDK712                                           
080001         MOVE REQU-KDARTURS (RAD-IX) TO LART-KDARTURS                     
090001         PERFORM IMS-REPL-WDK712                                          
100001                                                                          
110001** UPDATE COUNTRY SEGMENT - START                                         
120001         IF NDC-CN                                                        
130001            MOVE '40'        TO W-IDDC-MIN-1                              
140001                                W-IDDC-MAX-1                              
150001            MOVE 'A'         TO W-IDDC-MIN-1(2:1)                         
160001            MOVE '9'         TO W-IDDC-MAX-1(2:1)                         
170001                                                                          
180001            IF W-IDDC-REF-MIN(1:1) = '4'                                  
190001               MOVE 51  TO W-IDDC-MAX-1                                   
200001            END-IF                                                        
210001                                                                          
220001            MOVE '70'        TO W-IDDC-REF-MIN                            
230001                                W-IDDC-REF-MAX                            
240001            MOVE 'A'         TO W-IDDC-REF-MIN(2:1)                       
250001            MOVE '9'         TO W-IDDC-REF-MAX(2:1)                       
260001         END-IF                                                           
270001                                                                          
280001         IF NDC-US                                                        
290001               MOVE '70'     TO W-IDDC-MIN-1                              
300001                                W-IDDC-MAX-1                              
310001               MOVE 'A'      TO W-IDDC-MIN-1(2:1)                         
320001               MOVE '9'      TO W-IDDC-MAX-1(2:1)                         
330001                                                                          
340001               MOVE '40'     TO W-IDDC-REF-MIN                            
350001                                W-IDDC-REF-MAX                            
360001               MOVE 'A'      TO W-IDDC-REF-MIN(2:1)                       
370001               MOVE '9'      TO W-IDDC-REF-MAX(2:1)                       
380001            IF W-IDDC-REF-MIN(1:1) = '4'                                  
390001               MOVE 51  TO W-IDDC-REF-MAX                                 
400001            END-IF                                                        
410001         END-IF                                                           
420001                                                                          
430001         PERFORM IMS-GU-WDK711-REF                                        
440001         IF SEGMENT-FINNS                                                 
450001            IF NDC-US                                                     
460001               MOVE WC-LAND-CN     TO LART-IDLANDX2                       
470001                                      W-IDLAND                            
480001            ELSE                                                          
490001               MOVE WC-LAND-US     TO LART-IDLANDX2                       
500001                                      W-IDLAND                            
510001            END-IF                                                        
520001            PERFORM IMS-GHU-WDK712                                        
530001            IF REQU-KDARTURS (RAD-IX) NOT = ALL '+'                       
540001              MOVE REQU-KDARTURS (RAD-IX) TO LART-KDARTURS                
550001            END-IF                                                        
560001            PERFORM IMS-REPL-WDK712                                       
570001         END-IF                                                           
570101** UPDATE COUNTRY SEGMENT - END                                           
570201                                                                          
570300         IF ((REF-NDC-CN AND NDC-CN)                                      
570400          OR (REF-NDC-US AND NDC-US))                                     
570500           MOVE  REQU-KDARTURS(RAD-IX)                                    
570600                                     TO ARTC-CLAG-KDARTURS                
570700         END-IF                                                           
570800       ELSE                                                               
570900         PERFORM IMS-GHU-ARTC                                             
571000         IF SEGMENT-FINNS                                                 
571100           IF ARTC-CLAG-KDPCOO = SPACE                                    
571201             MOVE  REQU-KDARTURS(RAD-IX) TO ARTC-CLAG-KDARTURS            
571301           ELSE                                                           
571401             IF DAGENS-DATUM > ARTC-CLAG-TIGILTIG-PCOO                    
571500               MOVE  REQU-KDARTURS(RAD-IX) TO ARTC-CLAG-KDARTURS          
571601             ELSE                                                         
571701               MOVE  REQU-KDARTURS(RAD-IX) TO ARTC-CLAG-KDARTURS          
571801               MOVE  SPACE                 TO ARTC-CLAG-KDPCOO            
571901               MOVE  ZERO           TO ARTC-CLAG-TIGILTIG-PCOO            
572001             END-IF                                                       
572100           END-IF                                                         
572200         END-IF                                                           
572300       END-IF                                                             
572400     END-IF                                                               
572500     .                                                                    
572600     EJECT                                                                
572700                                                                          
572800 FB-UPDATE-WDT5 SECTION.                                                  
572900                                                                          
573000     PERFORM IMS-GU-WDT501                                                
           IF SEGMENT-FINNS                                                     
            PERFORM IMS-GHNP-WDT511                                             
            MOVE ART-IDLEVNR TO GLO-IDLEVNR                                     
            PERFORM IMS-REPL-WDT511                                             
           END-IF                                                               
573100     IF SEGMENT-SAKNAS                                                    
573200        MOVE W-IDARTNR TO ARTU-IDARTNR                                    
573300        PERFORM IMS-ISRT-WDT501                                           
573400     END-IF                                                               
573500                                                                          
573600     MOVE REQU-KDARTURS(RAD-IX) TO GLO-KDARTURS                           
573700     MOVE MSG-SIGNON-USERID     TO GLO-IDUSER                             
           MOVE SPACES                TO GLO-IDLEVNR                            
573800     MOVE FUNCTION CURRENT-DATE(1:14) TO DADATTID                         
573900     MOVE FUNCTION CURRENT-DATE(1:8)  TO TODAYS-DATE                      
574000     COMPUTE GLO-DADATTID-9KOMPL =                                        
574100             99999999999999 - DADATTID                                    
574200                                                                          
574300     PERFORM IMS-ISRT-WDT511                                              
574311     .                                                                    
574312     EJECT                                                                
574313                                                                          
574314 S01-STARTA-DISPATCHEN  SECTION.                                          
574315     MOVE SPACE                  TO MSG-KOM-WMSGKOM                       
574316     COMPUTE MSG-KOM-KVLL = LENGTH OF MSG-KOM-WMSGKOM                     
574317     MOVE LOW-VALUE              TO MSG-KOM-KDZ1                          
574318     MOVE LOW-VALUE              TO MSG-KOM-KDZ2                          
574319     MOVE SPACE                  TO MSG-KOM-KDTRANS                       
574320     MOVE 'W6I19B01'             TO MSG-KOM-IDCPYTXT                      
574321     MOVE 'INLEV   '             TO MSG-KOM-IDSNDNOD                      
574322     MOVE 'W6016100'             TO MSG-KOM-IDSNDJOB                      
574323     MOVE DAGENS-DATUM           TO MSG-KOM-TIREGDAT                      
574324     MOVE DAGENS-TID             TO MSG-KOM-TIKLOCK                       
574325     MOVE SPACE                  TO MSG-KOM-IDMFSMED                      
574326                                                                          
574327     MOVE ALL '+'                TO MOD619B-MID-W6I19B01                  
574328     MOVE WS-IDARTNR-NUM9        TO MOD619B-MID-IDARTNR                   
574329     MOVE REQU-IDDC              TO MOD619B-MID-IDDC                      
574400     MOVE REQU-KDARTURS(RAD-IX)  TO MOD619B-MID-KDARTURS                  
574500     COMPUTE P-TO-P-KVLL       =  LNG-P-TO-P-PREFIX + 87                  
574600     MOVE 'W6T19BX '             TO P-TO-P-KDTRANS                        
574700     MOVE '6161'                 TO P-TO-P-IDTRANS                        
574800     MOVE REQU-KDMFSFOR          TO P-TO-P-KDMFSFOR                       
574900     MOVE MOD619B-MID-W6I19B01   TO P-TO-P-DATA                           
575000                                                                          
575100     CALL W006KOM USING MSG-PCB                                           
575200                        DISP-PCB                                          
575300                        KOM-KOMA-PCB                                      
575400                        MSG-KOM-WMSGKOM                                   
575500                        P-TO-P-SW                                         
575600     .                                                                    
575700     EJECT                                                                
575800 S02-CHECK-BYTES SECTION.                                                 
575900     MOVE REQU-IDARTNR (RAD-IX) TO TEST-IDARTNR                           
576000     IF BYT02-RENOV                                                       
576100        IF BYT16-BYTES                                                    
576200           COMPUTE TEST-IDARTNR = TEST-IDARTNR +                          
576300                                  6000                                    
576400           END-COMPUTE                                                    
576500        ELSE                                                              
576600           COMPUTE TEST-IDARTNR = TEST-IDARTNR +                          
576700                                  1000                                    
576800           END-COMPUTE                                                    
576900        END-IF                                                            
577000        MOVE TEST-IDARTNR  TO W-IDARTNR                                   
578001        IF NDC-CN OR NDC-US                                               
578103           MOVE WS-IDDC    TO W-IDDC                                      
578203           PERFORM IMS-GU-WDK711                                          
578303           IF SEGMENT-FINNS                                               
578403             CONTINUE                                                     
578503           ELSE                                                           
578603             MOVE ALL '+'          TO WDK7-W005WDK7                       
578703             MOVE 'WDK711'         TO WDK7-IDSEGM                         
578803             MOVE W-IDARTNR        TO WDK7-IDARTNR-KFB                    
579003             MOVE W-IDDC           TO WDK7-IDDC-KFB                       
579103                                      WDK7-IDDC                           
579104             MOVE NEJ              TO WDK7-FLREFILL                       
579203                                                                          
579303             CALL W005WDK7 USING WDK7-W005WDK7 W005K7-WDB6-PCB            
579403             W005K7-WDK6-PCB W005K7-WDK7-PCB                              
579603           END-IF                                                         
579701        END-IF                                                            
579800        PERFORM FA-UPDATE-KDARTURS                                        
579900     END-IF                                                               
580000                                                                          
580100     .                                                                    
580200     EJECT                                                                
580300* MFS SECTIONER                                                           
580400     SKIP3                                                                
580500                                                                          
580600 MFS-ROER-EJ-FAELT-MODTAB   SECTION.                                      
580700                                                                          
580800     MOVE +1 TO RAD-IX                                                    
580900     PERFORM  UNTIL RAD-IX > REQU-KVRADER                                 
581000       MOVE ALL-PLUS          TO RESP-IDARTNR   (RAD-IX)                  
581100                                 RESP-KDARTHNT-V (RAD-IX)                 
581200                                 RESP-KDARTHNT-H (RAD-IX)                 
581300                                 RESP-KDARTURS  (RAD-IX)                  
581400       ADD 1 TO RAD-IX                                                    
581500     END-PERFORM                                                          
581600     SKIP3                                                                
581700                                                                          
581800     .                                                                    
581900 MFS-RENSA-FAELT-MODTAB      SECTION.                                     
582000                                                                          
582100     MOVE +1 TO RAD-IX                                                    
582200     PERFORM  UNTIL RAD-IX > REQU-KVRADER                                 
582300       MOVE ALL-SPACE       TO RESP-IDARTNR    (RAD-IX)                   
582400                               RESP-KDARTHNT-V (RAD-IX)                   
582500                               RESP-KDARTHNT-H (RAD-IX)                   
582600                               RESP-KDARTURS   (RAD-IX)                   
582700       ADD +1 TO RAD-IX                                                   
582800     END-PERFORM                                                          
582900     EJECT                                                                
583000     .                                                                    
583100* IMS SECTIONER                                                           
583200 IMS-GHU-ARTC        SECTION.                                             
583300     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
583400             DELIMITED BY SIZE    INTO SSA1                               
583500     MOVE 'WLARTC11 ' TO SSA2                                             
583600     MOVE '  GE' TO GODK-STATUSKODER                                      
583700     CALL CBLTDLI USING GHU ARTC-PCB IO-AREA-C SSA1 SSA2                  
583800     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
583900     PERFORM IMS-STATUS-KONTROLL                                          
584000     .                                                                    
584100     SKIP3                                                                
       IMS-GU-WDK601 SECTION.                                                   
                                                                                
           STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
                DELIMITED BY SIZE INTO SSA1                                     
           MOVE SPACES                 TO GODK-STATUSKODER                      
           CALL CBLTDLI USING GU ARTC-PCB DLI-IO-WDK601 SSA1                    
           MOVE ARTC-STATUS-CODE       TO STATUS-WS                             
           PERFORM IMS-STATUS-KONTROLL                                          
           .                                                                    
           EJECT                                                                
584200 IMS-REPLACE-ARTC SECTION.                                                
584300     MOVE '  ' TO GODK-STATUSKODER                                        
584400     CALL CBLTDLI USING REPL ARTC-PCB IO-AREA-C                           
584500     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
584600     PERFORM IMS-STATUS-KONTROLL                                          
584700     .                                                                    
584800     EJECT                                                                
584903 IMS-GU-WDK711 SECTION.                                                   
585000     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
585100          DELIMITED BY SIZE INTO SSA1                                     
585203     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
585300          DELIMITED BY SIZE INTO SSA2                                     
585400     MOVE '  GE' TO GODK-STATUSKODER                                      
585500     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-WDK711 SSA1 SSA2               
585600     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
585700     PERFORM IMS-STATUS-KONTROLL                                          
585800     .                                                                    
585903     EJECT                                                                
586003 IMS-GU-WDK711-REF SECTION.                                               
586103     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
586203          DELIMITED BY SIZE INTO SSA1                                     
586303     STRING 'WDK711  (IDDC    >=' W-IDDC-MIN-X                            
586403                    '&IDDC    <=' W-IDDC-MAX-X                            
586503                    '&IDDCREF >=' W-IDDC-REF-MIN-X                        
586603                    '&IDDCREF <=' W-IDDC-REF-MAX-X ')'                    
586703          DELIMITED BY SIZE INTO SSA2                                     
586803     MOVE '  GE' TO GODK-STATUSKODER                                      
586903     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-WDK711 SSA1 SSA2               
587003     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
587103     PERFORM IMS-STATUS-KONTROLL                                          
587203     .                                                                    
587303     EJECT                                                                
587403 IMS-GU-WDK712 SECTION.                                                   
587503     STRING 'WDK701  (IDARTNR = ' W-IDARTNR-X ')'                         
587603          DELIMITED BY SIZE INTO SSA1                                     
587703     STRING 'WDK712  (IDLAND  = ' W-IDLAND-X ')'                          
587803          DELIMITED BY SIZE INTO SSA2                                     
587903     MOVE '  GE' TO GODK-STATUSKODER                                      
588003     CALL CBLTDLI USING GHU WDK7-PCB DLI-IO-WDK712 SSA1 SSA2              
588103     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
588203     PERFORM IMS-STATUS-KONTROLL                                          
588303     .                                                                    
588304     EJECT                                                                
588305 IMS-GHU-WDK712 SECTION.                                                  
588306     STRING 'WDK701  (IDARTNR = ' W-IDARTNR-X ')'                         
588307          DELIMITED BY SIZE INTO SSA1                                     
588308     STRING 'WDK712  (IDLAND  = ' W-IDLAND-X ')'                          
588309          DELIMITED BY SIZE INTO SSA2                                     
588310     MOVE '  ' TO GODK-STATUSKODER                                        
588320     CALL CBLTDLI USING GHU WDK7-PCB DLI-IO-WDK712 SSA1 SSA2              
588330     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
588340     PERFORM IMS-STATUS-KONTROLL                                          
588350     .                                                                    
588403     EJECT                                                                
588503 IMS-REPL-WDK712 SECTION.                                                 
588603     MOVE '  ' TO GODK-STATUSKODER                                        
588703     CALL CBLTDLI USING REPL WDK7-PCB DLI-IO-WDK712                       
588803     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
588903     PERFORM IMS-STATUS-KONTROLL                                          
589003     .                                                                    
589103     EJECT                                                                
589104 IMS-GU-WDT501 SECTION.                                                   
589105     STRING 'WDT501  (IDARTNR = ' W-IDARTNR-X ')'                         
589106          DELIMITED BY SIZE INTO SSA1                                     
589107     MOVE '  GE' TO GODK-STATUSKODER                                      
589108     CALL CBLTDLI USING GU  WDT5-PCB DLI-IO-WDT501 SSA1                   
589109     MOVE WDT5-STATUS-CODE TO STATUS-WS                                   
589110     PERFORM IMS-STATUS-KONTROLL                                          
589111     .                                                                    
589112     EJECT                                                                
589113 IMS-ISRT-WDT501 SECTION.                                                 
589114     MOVE 'WDT501 ' TO SSA1                                               
589115     MOVE '  II' TO GODK-STATUSKODER                                      
589116     CALL CBLTDLI USING ISRT WDT5-PCB DLI-IO-WDT501 SSA1                  
589117     MOVE WDT5-STATUS-CODE TO STATUS-WS                                   
589118     PERFORM IMS-STATUS-KONTROLL                                          
589119     .                                                                    
       IMS-GHNP-WDT511 SECTION.                                                 
           MOVE   'WDT511  *F' TO SSA1                                          
           MOVE '  ' TO GODK-STATUSKODER                                        
           CALL CBLTDLI USING GHNP  WDT5-PCB DLI-IO-WDT511 SSA1                 
           MOVE WDT5-STATUS-CODE TO STATUS-WS                                   
           PERFORM IMS-STATUS-KONTROLL                                          
           .                                                                    
           EJECT                                                                
       IMS-REPL-WDT511 SECTION.                                                 
           MOVE '  ' TO GODK-STATUSKODER                                        
           CALL CBLTDLI USING REPL  WDT5-PCB DLI-IO-WDT511                      
           MOVE WDT5-STATUS-CODE TO STATUS-WS                                   
           PERFORM IMS-STATUS-KONTROLL                                          
           .                                                                    
           EJECT                                                                
589120 IMS-ISRT-WDT511 SECTION.                                                 
589121     STRING 'WDT501  (IDARTNR  =' W-IDARTNR-X ')'                         
589122            DELIMITED BY SIZE INTO SSA1                                   
589123     MOVE 'WDT511 ' TO SSA2                                               
589160     MOVE '  II' TO GODK-STATUSKODER                                      
589125     CALL CBLTDLI USING ISRT WDT5-PCB DLI-IO-WDT511 SSA1 SSA2             
589126     MOVE WDT5-STATUS-CODE TO STATUS-WS                                   
589127     PERFORM IMS-STATUS-KONTROLL                                          
589128     .                                                                    
589203 IMS-STATUS-KONTROLL SECTION.                                             
589303     SET STATUS-IX TO 1                                                   
589403     SEARCH GODK-STATUS AT END CALL FELLOG                                
589503       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
589603     END-SEARCH                                                           
589703     CONTINUE                                                             
590003     .                                                                    
