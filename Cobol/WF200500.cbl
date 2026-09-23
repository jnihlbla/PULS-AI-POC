000100 PROCESS DYNAM                                                            
000200*        - THE OPTION ABOVE IS NEEDED TO LINK A BATCH-DB2-PGM             
000400*                                                                         
001150 ID DIVISION.                                                             
001200 PROGRAM-ID.     WF200500.                                                
001300 AUTHOR.         BO HAMMARIN.                                             
001400 DATE-WRITTEN.   AUG 2002.                                                
001500 DATE-COMPILED.                                                           
001700                                                                          
001800*   PGM                                                                   
001900*   CREATES COMPLETE APPENDIX                                             
002410*                                                                         
002423*   PGM INSERTS                                                           
002424*   - ROWS IN TABLE T01DAPP                                               
002425*                                                                         
002430*   PGM READS                                                             
002440*   - ROWS IN TABLE T01PROC                                               
002460*   - ROWS IN TABLE T01FCUS                                               
002470*   - ROWS IN TABLE T01CUGR                                               
002471*   - ROWS IN TABLE T01DLIN                                               
002480*   - ROWS IN TABLE T01LSEL                                               
002490*   - ROWS IN TABLE T01VAT                                                
002491*   - ROWS IN TABLE T01INRE                                               
002492*   - ROWS IN TABLE T01BURE                                               
002500                                                                          
002700 ENVIRONMENT DIVISION.                                                    
002800                                                                          
002900 INPUT-OUTPUT SECTION.                                                    
003000                                                                          
003011 FILE-CONTROL.                                                            
003012                                                                          
003013 DATA DIVISION.                                                           
003014                                                                          
003015 FILE SECTION.                                                            
003016                                                                          
004000 WORKING-STORAGE SECTION.                                                 
004110*    -- CHECKED BY WY2000                                                 
004200 77  IDPGM                        PIC X(8)   VALUE 'WF200300'.            
004201                                                                          
004210 77  JA                           PIC X      VALUE 'J'.                   
004220 77  NEJ                          PIC X      VALUE 'N'.                   
004300                                                                          
004400 01  WS-MISCELLANEOUS.                                                    
004413     03  WS-KDVAT                 PIC X(2).                               
004414     03  WS-IDEXCUST-1            PIC X(15).                              
004415     03  WS-KDAPPEND              PIC X(4).                               
004417     03  WS-IDAPPEND              PIC X(8).                               
004418     03  WS-IDAPPEND-X.                                                   
004419         05 WS-IDAPPEND-NUM       PIC Z(3)9.99.                           
004420         05 WS-IDAPPEND-PROC      PIC X(1)   VALUE '%'.                   
004421     03  WS-CURRENT-VERSION       PIC S9(3)  VALUE +001 COMP-3.           
004422     03  WS-ACTIVE                PIC X(8)   VALUE '00000000'.            
004423     03  WS-SPACE                 PIC X(4)   VALUE SPACE.                 
004430                                                                          
004431 01  WS-DIVERSE-MULTIFETCH.                                               
004432     03 WS-MX                    PIC S9(3)  COMP-3.                       
004433     03 WS-MULTIFETCH            PIC S9(3)  COMP-3.                       
004434                                                                          
004435     03 WS-SUNTO-APP             PIC S9(11)V9(2)      COMP-3.             
004436     03 WS-SUVAT-BILLIT-APP      PIC S9(11)V9(2)      COMP-3.             
004437     03 WS-SUBTO-APP             PIC S9(11)V9(2)      COMP-3.             
004438     03 WS-DLIN-KDAPPEND   OCCURS 100  PIC X(4).                          
004439     03 WS-DLIN-IDAPPEND   OCCURS 100  PIC X(8).                          
004440     03 WS-DLIN-KDVAT      OCCURS 100  PIC X(2).                          
004441     03 WS-DLIN-IDEXCUST-1 OCCURS 100  PIC X(2).                          
004450                                                                          
004451     03 WS-IDLEGSEL                 PIC X(4).                             
004452     03 WS-DAEXDAT                  PIC X(8).                             
004453     03 WS-TIEXTID                  PIC S9(7) COMP-3.                     
004454     03 WS-KDVALISO                 PIC X(3).                             
004455     03 WS-IDLANDX3-SEND            PIC X(3).                             
004456     03 WS-IDLEVNR                  PIC X(5).                             
004457     03 WS-IDPARTNR                 PIC X(9).                             
004458     03 WS-KDFINDOC                 PIC X(4).                             
004459     03 WS-FLSOFT                   PIC X(1).                             
004460     03 WS-FLFREE                   PIC X(1).                             
004461     03 WS-FLPRIV                   PIC X(1).                             
004462     03 WS-IDBREAK-1                PIC X(8).                             
004463     03 WS-IDBREAK-2                PIC X(8).                             
004464     03 WS-IDDC                     PIC X(2).                             
004470                                                                          
004480*01  -COPY WWLANDX2                                                       
004490                                                                          
006000 01  DYNAMISKA-SUBPROGRAM.                                                
006100*                                                                         
006300     03  ABEND                    PIC X(8)   VALUE 'ABEND   '.            
006310                                                                          
006400 01  RKOD-ABEND-DB2               PIC S9(4)  VALUE +998 COMP SYNC.        
006501     EJECT                                                                
006550*                                                                         
010602*        WORK-AREAS FOR DB2-SECTIONS                                      
010603*                                                                         
010604 01  FILLER                       PIC X(16)  VALUE 'PROC-TAB   '.         
010605*01  -COPY T01PROC    -PRE PROC-                                          
010606                                                                          
010619 01  FILLER                       PIC X(16)  VALUE 'FCUS-TAB   '.         
010620*01  -COPY T01FCUS    -PRE FCUS-                                          
010621                                                                          
010622 01  FILLER                       PIC X(16)  VALUE 'CUGR-TAB   '.         
010623*01  -COPY T01CUGR    -PRE CUGR-                                          
010624                                                                          
010625 01  FILLER                       PIC X(16)  VALUE 'LSEL-TAB   '.         
010626*01  -COPY T01LSEL    -PRE LSEL-                                          
010627                                                                          
010628 01  FILLER                       PIC X(16)  VALUE 'INRE-TAB   '.         
010629*01  -COPY T01INRE    -PRE INRE-                                          
010630                                                                          
010631 01  FILLER                       PIC X(16)  VALUE 'VAT-TAB   '.          
010632*01  -COPY T01VAT     -PRE VAT-                                           
010633                                                                          
010634 01  FILLER                       PIC X(16)  VALUE 'BURE-TAB   '.         
010635*01  -COPY T01BURE    -PRE BURE-                                          
010636                                                                          
010637 01  FILLER                       PIC X(16)  VALUE 'DLIN-TAB   '.         
010638*01  -COPY T01DLINT   -PRE DLIN-                                          
010639                                                                          
010640 01  FILLER                       PIC X(16)  VALUE 'DAPP-TAB   '.         
010641*01  -COPY T01DAPPT   -PRE DAPP-                                          
010642                                                                          
010643 01  FILLER                       PIC X(16)  VALUE 'DHEA-TAB   '.         
010644*01  -COPY T01DHEA    -PRE DHEA-                                          
010645     EJECT                                                                
010646                                                                          
010647 01  FILLER                       PIC X(16)  VALUE 'PROC-AREA'.           
010648       EXEC SQL INCLUDE T01PROC  END-EXEC.                                
010649                                                                          
010650 01  FILLER                       PIC X(16)  VALUE 'FCUS-AREA'.           
010651       EXEC SQL INCLUDE T01FCUS  END-EXEC.                                
010652                                                                          
010653 01  FILLER                       PIC X(16)  VALUE 'CUGR-AREA'.           
010654       EXEC SQL INCLUDE T01CUGR  END-EXEC.                                
010655                                                                          
010656 01  FILLER                       PIC X(16)  VALUE 'LSEL-AREA'.           
010657       EXEC SQL INCLUDE T01LSEL  END-EXEC.                                
010658                                                                          
010659 01  FILLER                       PIC X(16)  VALUE 'INRE-AREA'.           
010660       EXEC SQL INCLUDE T01INRE  END-EXEC.                                
010661                                                                          
010662 01  FILLER                       PIC X(16)  VALUE 'VAT-AREA'.            
010663       EXEC SQL INCLUDE T01VAT   END-EXEC.                                
010664                                                                          
010665 01  FILLER                       PIC X(16)  VALUE 'BURE-AREA'.           
010666       EXEC SQL INCLUDE T01BURE  END-EXEC.                                
010667                                                                          
010668 01  FILLER                       PIC X(16)  VALUE 'DLIN-AREA'.           
010669       EXEC SQL INCLUDE T01DLIN  END-EXEC.                                
010670                                                                          
010671 01  FILLER                       PIC X(16)  VALUE 'DAPP-AREA'.           
010672       EXEC SQL INCLUDE T01DAPP  END-EXEC.                                
010673                                                                          
010674 01  FILLER                       PIC X(16)  VALUE 'DHEA-AREA'.           
010675       EXEC SQL INCLUDE T01DHEA  END-EXEC.                                
010676     EJECT                                                                
010677                                                                          
010678 01  FILLER                       PIC X(16)  VALUE 'SQLCA-AREA'.          
010679       EXEC SQL INCLUDE SQLCA END-EXEC.                                   
010680*                        **** STATUS-CODE FROM DB2                        
010681                                                                          
010682 01  FILLER                       PIC X(16)  VALUE 'SQLCODE-WS'.          
010683 01  DB2-WS.                                                              
010684   03  SQLCODE-WS                 PIC S9(3)  VALUE ZERO.                  
010685     88  ROW-FOUND                           VALUE +000.                  
010686     88  ROW-MISSING                         VALUE +100.                  
010687     88  VAT-MISSING                         VALUE +100.                  
010688   03  GOOD-SQLCODES.                                                     
010689     05  GOOD-SQLCODE OCCURS 5                                            
010690         INDEXED BY SQLCODE-IX    PIC 999.                                
010691     EJECT                                                                
010700                                                                          
010717 PROCEDURE DIVISION.                                                      
010718 MAIN SECTION.                                                            
011600     PERFORM B-EXECUTE                                                    
011700     PERFORM C-EXECUTE                                                    
011900                                                                          
012600     MOVE ZERO TO RETURN-CODE                                             
012700     GOBACK                                                               
012800     .                                                                    
012900     EJECT                                                                
012910                                                                          
014985 B-EXECUTE SECTION.                                                       
014986     PERFORM DB2-OPEN-CRS-MISC1                                           
014988                                                                          
014989     PERFORM DB2-FETCH-CRS-MISC1                                          
014990     IF SQLERRD(3) > 0                                                    
014991       MOVE 000     TO SQLCODE-WS                                         
014992     END-IF                                                               
014993     PERFORM UNTIL ROW-MISSING                                            
014994       MOVE SQLERRD(3) TO WS-MULTIFETCH                                   
014995       MOVE ZERO       TO WS-MX                                           
014996       PERFORM UNTIL WS-MX = WS-MULTIFETCH                                
014997         ADD +1        TO WS-MX                                           
014998         MOVE DLIN-IDLEGSEL(WS-MX)      TO WS-IDLEGSEL                    
014999         MOVE DLIN-DAEXDAT(WS-MX)       TO WS-DAEXDAT                     
015000         MOVE DLIN-TIEXTID(WS-MX)       TO WS-TIEXTID                     
015001         MOVE DLIN-KDVALISO(WS-MX)      TO WS-KDVALISO                    
015002         MOVE DLIN-IDLANDX3-SEND(WS-MX) TO WS-IDLANDX3-SEND               
015003         MOVE DLIN-IDLEVNR(WS-MX)       TO WS-IDLEVNR                     
015004         MOVE DLIN-IDPARTNR(WS-MX)      TO WS-IDPARTNR                    
015005         MOVE DLIN-KDFINDOC(WS-MX)      TO WS-KDFINDOC                    
015006         MOVE DLIN-FLSOFT(WS-MX)        TO WS-FLSOFT                      
015007         MOVE DLIN-FLFREE(WS-MX)        TO WS-FLFREE                      
015008         MOVE DLIN-FLPRIV(WS-MX)        TO WS-FLPRIV                      
015009         MOVE DLIN-IDBREAK-1(WS-MX)     TO WS-IDBREAK-1                   
015010         MOVE DLIN-IDBREAK-2(WS-MX)     TO WS-IDBREAK-2                   
015011         MOVE DLIN-IDAPPEND(WS-MX)      TO WS-IDAPPEND                    
015013         PERFORM BA-INIT-AMOUNTS                                          
015014         PERFORM DB2-SELECT-DLIN1                                         
015015         MOVE WS-SUNTO-APP        TO DAPP-SUNTO-APP(WS-MX)                
015016         MOVE WS-SUVAT-BILLIT-APP TO DAPP-SUVAT-BILLIT-APP(WS-MX)         
015017         MOVE WS-SUBTO-APP        TO DAPP-SUBTO-APP(WS-MX)                
015018       END-PERFORM                                                        
015019       PERFORM DB2-INSERT-DAPP1                                           
015020       IF WS-MULTIFETCH = 100                                             
015021         PERFORM DB2-FETCH-CRS-MISC1                                      
015022         IF SQLERRD(3) > 0                                                
015023           MOVE 000     TO SQLCODE-WS                                     
015024         END-IF                                                           
015025       ELSE                                                               
015026         MOVE 100     TO SQLCODE-WS                                       
015027       END-IF                                                             
015028     END-PERFORM                                                          
015029                                                                          
015030     PERFORM DB2-CLOSE-CRS-MISC1                                          
015103     .                                                                    
015104     EJECT                                                                
015106                                                                          
015107 BA-INIT-AMOUNTS SECTION.                                                 
015108     MOVE ZERO TO DAPP-SUNTO-APP(WS-MX)                                   
015109                  DAPP-SUVAT-BILLIT-APP(WS-MX)                            
015110                  DAPP-SUBTO-APP(WS-MX)                                   
015111     .                                                                    
015112     EJECT                                                                
015113                                                                          
015114 C-EXECUTE SECTION.                                                       
015115     PERFORM DB2-OPEN-CRS-MISC2                                           
015116                                                                          
015117     PERFORM DB2-FETCH-CRS-MISC2                                          
015118     IF SQLERRD(3) > 0                                                    
015119       MOVE 000     TO SQLCODE-WS                                         
015120     END-IF                                                               
015121     PERFORM UNTIL ROW-MISSING                                            
015122       MOVE SQLERRD(3) TO WS-MULTIFETCH                                   
015123       MOVE ZERO       TO WS-MX                                           
015124       PERFORM UNTIL WS-MX = WS-MULTIFETCH                                
015125         ADD +1        TO WS-MX                                           
015126         MOVE DLIN-IDLEGSEL(WS-MX)      TO WS-IDLEGSEL                    
015127         MOVE DLIN-DAEXDAT(WS-MX)       TO WS-DAEXDAT                     
015128         MOVE DLIN-TIEXTID(WS-MX)       TO WS-TIEXTID                     
015129         MOVE DLIN-KDVALISO(WS-MX)      TO WS-KDVALISO                    
015130         MOVE DLIN-IDLANDX3-SEND(WS-MX) TO WS-IDLANDX3-SEND               
015131         MOVE DLIN-IDLEVNR(WS-MX)       TO WS-IDLEVNR                     
015132         MOVE DLIN-IDPARTNR(WS-MX)      TO WS-IDPARTNR                    
015133         MOVE DLIN-KDFINDOC(WS-MX)      TO WS-KDFINDOC                    
015134         MOVE DLIN-FLSOFT(WS-MX)        TO WS-FLSOFT                      
015135         MOVE DLIN-FLFREE(WS-MX)        TO WS-FLFREE                      
015136         MOVE DLIN-FLPRIV(WS-MX)        TO WS-FLPRIV                      
015137         MOVE DLIN-IDBREAK-1(WS-MX)     TO WS-IDBREAK-1                   
015138         MOVE DLIN-IDBREAK-2(WS-MX)     TO WS-IDBREAK-2                   
015139         MOVE DLIN-IDDC(WS-MX)          TO WS-IDDC                        
015140         MOVE DLIN-KDVAT(WS-MX)         TO WS-KDVAT                       
015141         MOVE DLIN-IDEXCUST-1(WS-MX)    TO WS-IDEXCUST-1                  
015142         PERFORM CA-INIT-AMOUNTS                                          
015143         PERFORM DB2-SELECT-DLIN2                                         
015144         MOVE WS-SUNTO-APP        TO DAPP-SUNTO-APP(WS-MX)                
015145         MOVE WS-SUVAT-BILLIT-APP TO DAPP-SUVAT-BILLIT-APP(WS-MX)         
015146         MOVE WS-SUBTO-APP        TO DAPP-SUBTO-APP(WS-MX)                
015147         PERFORM DB2-SELECT-BUSINESS                                      
015148**** DIRTY FIX IPT                                                        
015149*        IF  WS-IDBREAK-1 = 'SERVEXT'                                     
015150*        AND WS-IDEXCUST-1 = '2278'                                       
015151*          PERFORM DB2-SELECT-VAT-VERS2                                   
015152*        ELSE                                                             
015153**** HERE WE CHECK IF IT'S A DDGS FROM A EU COUNTRY                       
015154           MOVE WS-IDDC         TO LANDX2-IDLANDX2                        
015155           IF LANDX2-EU-IDLANDX2                                          
015156             IF FCUS-FLDIRVAT = 'J'                                       
015157               PERFORM DB2-SELECT-VAT-VERS2                               
015158             ELSE                                                         
015159               IF BURE-FLVATCHK = 'J'                                     
015160                 PERFORM DB2-SELECT-VAT-VERS1                             
015161               ELSE                                                       
015162                 PERFORM DB2-SELECT-VAT-VERS2                             
015163               END-IF                                                     
015164             END-IF                                                       
015165           ELSE                                                           
015166             IF BURE-FLVATCHK = 'J'                                       
015167               PERFORM DB2-SELECT-VAT-VERS1                               
015168             ELSE                                                         
015169               PERFORM DB2-SELECT-VAT-VERS2                               
015171             END-IF                                                       
015172           END-IF                                                         
015173*        END-IF                                                           
015174****                                                                      
015178         PERFORM CB-CALCULATE-VAT                                         
015180         MOVE VAT-REVAT     TO WS-IDAPPEND-NUM                            
015181         MOVE WS-IDAPPEND-X TO WS-IDAPPEND                                
015182         MOVE WS-IDAPPEND   TO WS-DLIN-IDAPPEND(WS-MX)                    
015187         MOVE DLIN-KDVAT(WS-MX) TO WS-DLIN-KDAPPEND(WS-MX)                
015189       END-PERFORM                                                        
015190       PERFORM DB2-INSERT-DAPP2                                           
015191       IF WS-MULTIFETCH = 100                                             
015192         PERFORM DB2-FETCH-CRS-MISC2                                      
015193         IF SQLERRD(3) > 0                                                
015194           MOVE 000     TO SQLCODE-WS                                     
015195         END-IF                                                           
015196       ELSE                                                               
015197         MOVE 100     TO SQLCODE-WS                                       
015198       END-IF                                                             
015199     END-PERFORM                                                          
015200                                                                          
015201     PERFORM DB2-CLOSE-CRS-MISC2                                          
015202     .                                                                    
015203     EJECT                                                                
015204                                                                          
015205 CA-INIT-AMOUNTS SECTION.                                                 
015206     MOVE ZERO TO DAPP-SUNTO-APP(WS-MX)                                   
015207                  DAPP-SUVAT-BILLIT-APP(WS-MX)                            
015208                  DAPP-SUBTO-APP(WS-MX)                                   
015209     .                                                                    
015210     EJECT                                                                
015211                                                                          
015212 CB-CALCULATE-VAT SECTION.                                                
015213     IF VAT-MISSING                                                       
015214       IF WS-FLSOFT = 'J'                                                 
015215       OR WS-FLSOFT = 'Y'                                                 
015216         PERFORM DB2-SELECT-VAT-VERS2                                     
015217         IF VAT-REVAT > ZERO                                              
015218           COMPUTE DAPP-SUVAT-BILLIT-APP(WS-MX) ROUNDED =                 
015219                  (DAPP-SUNTO-APP(WS-MX) *                                
015220                   VAT-REVAT)   /                                         
015221                   100                                                    
015222           END-COMPUTE                                                    
015223           COMPUTE DAPP-SUBTO-APP(WS-MX)       =                          
015224                   DAPP-SUNTO-APP(WS-MX) +                                
015225                   DAPP-SUVAT-BILLIT-APP(WS-MX)                           
015226           END-COMPUTE                                                    
015227         ELSE                                                             
015228           MOVE ZERO               TO DAPP-SUVAT-BILLIT-APP(WS-MX)        
015229                                      VAT-REVAT                           
015230           MOVE DAPP-SUNTO-APP(WS-MX) TO DAPP-SUBTO-APP(WS-MX)            
015231         END-IF                                                           
015232       ELSE                                                               
015233         MOVE ZERO                 TO DAPP-SUVAT-BILLIT-APP(WS-MX)        
015234                                      VAT-REVAT                           
015235         MOVE DAPP-SUNTO-APP(WS-MX) TO DAPP-SUBTO-APP(WS-MX)              
015236       END-IF                                                             
015237     ELSE                                                                 
015238*    RULES FOR CALCULATION IF THE BUSINESSRELATION DOESN'T                
015239*    INDICATE VAT-CODE DETERMINATION IN BILLIT                            
015240       IF BURE-FLVATCHK = NEJ                                             
015241         IF VAT-REVAT > ZERO                                              
015242           COMPUTE DAPP-SUVAT-BILLIT-APP(WS-MX) ROUNDED =                 
015243                  (DAPP-SUNTO-APP(WS-MX) *                                
015244                   VAT-REVAT)     /                                       
015245                   100                                                    
015246           END-COMPUTE                                                    
015247           COMPUTE DAPP-SUBTO-APP(WS-MX)         =                        
015248                   DAPP-SUNTO-APP(WS-MX) +                                
015249                   DAPP-SUVAT-BILLIT-APP(WS-MX)                           
015250           END-COMPUTE                                                    
015251         ELSE                                                             
015252           MOVE ZERO               TO DAPP-SUVAT-BILLIT-APP(WS-MX)        
015253                                         VAT-REVAT                        
015254           MOVE DAPP-SUNTO-APP(WS-MX) TO DAPP-SUBTO-APP(WS-MX)            
015255         END-IF                                                           
015256       ELSE                                                               
015257*    RULES FOR CALCULATION IF THE BUSINESSRELATION                        
015258*    INDICATES VAT-CODE DETERMINATION IN BILLIT                           
015259         PERFORM DB2-SELECT-CUGR-INRE                                     
015260         IF WS-FLPRIV = NEJ                                               
015261           IF CUGR-FLVAT = NEJ                                            
015262           OR INRE-FLVAT = NEJ                                            
015263**** IPT FIX                                                              
015264*            IF  WS-IDBREAK-1 = 'SERVEXT'                                 
015265*            AND WS-IDEXCUST-1 = '2278'                                   
015266*              COMPUTE DAPP-SUVAT-BILLIT-APP(WS-MX) ROUNDED =             
015267*                     (DAPP-SUNTO-APP(WS-MX) *                            
015268*                      VAT-REVAT) /                                       
015269*                      100                                                
015270*              END-COMPUTE                                                
015271*              COMPUTE DAPP-SUBTO-APP(WS-MX)     =                        
015272*                      DAPP-SUNTO-APP(WS-MX) +                            
015273*                      DAPP-SUVAT-BILLIT-APP(WS-MX)                       
015274*              END-COMPUTE                                                
015275*            ELSE                                                         
015276**** HERE WE CHECK IF IT'S A DDGS FROM A EU COUNTRY                       
015277               MOVE WS-IDDC         TO LANDX2-IDLANDX2                    
015278               IF LANDX2-EU-IDLANDX2                                      
015279                 IF FCUS-FLDIRVAT = 'J'                                   
015280                   COMPUTE DAPP-SUVAT-BILLIT-APP(WS-MX) ROUNDED =         
015281                          (DAPP-SUNTO-APP(WS-MX) *                        
015282                           VAT-REVAT) /                                   
015283                           100                                            
015284                   END-COMPUTE                                            
015285                   COMPUTE DAPP-SUBTO-APP(WS-MX)   =                      
015286                           DAPP-SUNTO-APP(WS-MX) +                        
015287                           DAPP-SUVAT-BILLIT-APP(WS-MX)                   
015288                   END-COMPUTE                                            
015289                 ELSE                                                     
015290                   MOVE ZERO TO DAPP-SUVAT-BILLIT-APP(WS-MX)              
015291                                VAT-REVAT                                 
015292                   MOVE DAPP-SUNTO-APP(WS-MX)                             
015293                             TO DAPP-SUBTO-APP(WS-MX)                     
015294                 END-IF                                                   
015295               ELSE                                                       
015296                 MOVE ZERO   TO DAPP-SUVAT-BILLIT-APP(WS-MX)              
015297                                VAT-REVAT                                 
015298                 MOVE DAPP-SUNTO-APP(WS-MX)                               
015299                             TO DAPP-SUBTO-APP(WS-MX)                     
015300               END-IF                                                     
015301*            END-IF                                                       
015302           ELSE                                                           
015303             COMPUTE DAPP-SUVAT-BILLIT-APP(WS-MX) ROUNDED =               
015304                    (DAPP-SUNTO-APP(WS-MX) *                              
015305                     VAT-REVAT)     /                                     
015306                     100                                                  
015307             END-COMPUTE                                                  
015308             COMPUTE DAPP-SUBTO-APP(WS-MX)         =                      
015309                     DAPP-SUNTO-APP(WS-MX) +                              
015310                     DAPP-SUVAT-BILLIT-APP(WS-MX)                         
015311             END-COMPUTE                                                  
015312           END-IF                                                         
015313         ELSE                                                             
015314           IF CUGR-FLVAT      = NEJ                                       
015315           OR INRE-FLVAT-PRIV = NEJ                                       
015316**** IPT FIX                                                              
015317*            IF  WS-IDBREAK-1 = 'SERVEXT'                                 
015318*            AND WS-IDEXCUST-1 = '2278'                                   
015319*              COMPUTE DAPP-SUVAT-BILLIT-APP(WS-MX) ROUNDED =             
015320*                     (DAPP-SUNTO-APP(WS-MX) *                            
015321*                      VAT-REVAT) /                                       
015322*                      100                                                
015323*              END-COMPUTE                                                
015324*              COMPUTE DAPP-SUBTO-APP(WS-MX)     =                        
015325*                      DAPP-SUNTO-APP(WS-MX) +                            
015326*                      DAPP-SUVAT-BILLIT-APP(WS-MX)                       
015327*              END-COMPUTE                                                
015328*            ELSE                                                         
015329**** HERE WE CHECK IF IT'S A DDGS FROM A EU COUNTRY                       
015330               MOVE WS-IDDC         TO LANDX2-IDLANDX2                    
015331               IF LANDX2-EU-IDLANDX2                                      
015332                 IF FCUS-FLDIRVAT = 'J'                                   
015333                   COMPUTE DAPP-SUVAT-BILLIT-APP(WS-MX) ROUNDED =         
015334                          (DAPP-SUNTO-APP(WS-MX) *                        
015335                           VAT-REVAT) /                                   
015336                           100                                            
015337                   END-COMPUTE                                            
015338                   COMPUTE DAPP-SUBTO-APP(WS-MX)   =                      
015339                           DAPP-SUNTO-APP(WS-MX) +                        
015340                           DAPP-SUVAT-BILLIT-APP(WS-MX)                   
015341                   END-COMPUTE                                            
015342                 ELSE                                                     
015343                   MOVE ZERO TO DAPP-SUVAT-BILLIT-APP(WS-MX)              
015344                                VAT-REVAT                                 
015345                   MOVE DAPP-SUNTO-APP(WS-MX)                             
015346                             TO DAPP-SUBTO-APP(WS-MX)                     
015347                 END-IF                                                   
015348               ELSE                                                       
015349                 MOVE ZERO TO DAPP-SUVAT-BILLIT-APP(WS-MX)                
015350                                VAT-REVAT                                 
015351                 MOVE DAPP-SUNTO-APP(WS-MX)                               
015352                             TO DAPP-SUBTO-APP(WS-MX)                     
015353               END-IF                                                     
015354*            END-IF                                                       
015355           ELSE                                                           
015356             COMPUTE DAPP-SUVAT-BILLIT-APP(WS-MX) ROUNDED =               
015357                    (DAPP-SUNTO-APP(WS-MX) *                              
015358                     VAT-REVAT) /                                         
015359                     100                                                  
015360             END-COMPUTE                                                  
015361             COMPUTE DAPP-SUBTO-APP(WS-MX)         =                      
015362                     DAPP-SUNTO-APP(WS-MX) +                              
015363                     DAPP-SUVAT-BILLIT-APP(WS-MX)                         
015364             END-COMPUTE                                                  
015365           END-IF                                                         
015366         END-IF                                                           
015367       END-IF                                                             
015368     END-IF                                                               
015369     .                                                                    
015370     EJECT                                                                
015371                                                                          
015380* --- DB2 SECTIONS  ---                                                   
015400*                                                                         
017210                                                                          
017220 DB2-OPEN-CRS-MISC1 SECTION.                                              
017240     EXEC SQL                                                             
017250         DECLARE MISC1-CRS CURSOR WITH ROWSET POSITIONING FOR             
017252     SELECT   DISTINCT                                                    
017253              T01DLIN.IDLEGSEL,                                           
017260              T01DLIN.DAEXDAT,                                            
017261              T01DLIN.TIEXTID,                                            
017262              T01DLIN.KDVALISO,                                           
017263              T01DLIN.IDLANDX3_SEND,                                      
017264              T01DLIN.IDLEVNR,                                            
017265              T01DLIN.IDPARTNR,                                           
017266              T01DLIN.KDFINDOC,                                           
017267              T01DLIN.FLSOFT,                                             
017268              T01DLIN.FLFREE,                                             
017269              T01DLIN.FLPRIV,                                             
017270              T01DLIN.IDBREAK_1,                                          
017271              T01DLIN.IDBREAK_2,                                          
017272              T01CUGR.KDAPPEND,                                           
017273              T01DLIN.IDAPPEND                                            
017534                                                                          
017535     FROM     T01PROC,                                                    
017536              T01DLIN,                                                    
017537              T01FCUS,                                                    
017538              T01CUGR                                                     
017539                                                                          
017540     WHERE    T01PROC.IDSYSTEM      = 'WF02'                              
017541       AND    T01DLIN.IDLEGSEL      = T01PROC.IDLEGSEL                    
017542       AND    T01DLIN.DAEXDAT       = T01PROC.DAEXDAT                     
017543       AND    T01DLIN.TIEXTID       = T01PROC.TIEXTID                     
017544       AND    T01FCUS.IDLEGSEL      = T01DLIN.IDLEGSEL                    
017545       AND    T01FCUS.IDPARTNR      = T01DLIN.IDPARTNR                    
017546       AND    T01FCUS.KDSTATUS      = :WS-CURRENT-VERSION                 
017547       AND    T01FCUS.DADELDAT      = :WS-ACTIVE                          
017548       AND    T01CUGR.IDLEGSEL      = T01FCUS.IDLEGSEL                    
017549       AND    T01CUGR.KDPARTTY      = T01FCUS.KDPARTTY                    
017550       AND    T01CUGR.KDPARTGR      = T01FCUS.KDPARTGR                    
017551       AND    T01CUGR.KDSTATUS      = :WS-CURRENT-VERSION                 
017552       AND    T01CUGR.DADELDAT      = :WS-ACTIVE                          
017553       AND    T01CUGR.KDAPPEND      > :WS-SPACE                           
017612     END-EXEC                                                             
017613                                                                          
017614     EXEC SQL OPEN MISC1-CRS                                              
017615     END-EXEC                                                             
017616                                                                          
017617     MOVE 000            TO GOOD-SQLCODES                                 
017618     MOVE SQLCODE        TO SQLCODE-WS                                    
017619     PERFORM DB2-STATUS-CHECK                                             
017620     .                                                                    
017621     EJECT                                                                
017622                                                                          
017705 DB2-FETCH-CRS-MISC1 SECTION.                                             
017706     EXEC SQL                                                             
017707       FETCH NEXT ROWSET FROM MISC1-CRS FOR 100 ROWS                      
017708       INTO :DLIN-IDLEGSEL,                                               
017709            :DLIN-DAEXDAT,                                                
017710            :DLIN-TIEXTID,                                                
017711            :DLIN-KDVALISO,                                               
017712            :DLIN-IDLANDX3-SEND,                                          
017713            :DLIN-IDLEVNR,                                                
017714            :DLIN-IDPARTNR,                                               
017715            :DLIN-KDFINDOC,                                               
017716            :DLIN-FLSOFT,                                                 
017717            :DLIN-FLFREE,                                                 
017718            :DLIN-FLPRIV,                                                 
017719            :DLIN-IDBREAK-1,                                              
017720            :DLIN-IDBREAK-2,                                              
017721            :WS-DLIN-KDAPPEND,                                            
017730            :DLIN-IDAPPEND                                                
017905     END-EXEC                                                             
017910                                                                          
017920     MOVE 000100         TO GOOD-SQLCODES                                 
018000     MOVE SQLCODE        TO SQLCODE-WS                                    
018200     PERFORM DB2-STATUS-CHECK                                             
018300     .                                                                    
019172     EJECT                                                                
019173                                                                          
019174 DB2-CLOSE-CRS-MISC1 SECTION.                                             
019175     EXEC SQL                                                             
019176         CLOSE MISC1-CRS                                                  
019177     END-EXEC                                                             
019178     .                                                                    
019179     EJECT                                                                
019180                                                                          
019190 DB2-OPEN-CRS-MISC2 SECTION.                                              
019200     EXEC SQL                                                             
019201          DECLARE MISC2-CRS CURSOR WITH ROWSET POSITIONING FOR            
019202     SELECT   DISTINCT                                                    
019203              T01DLIN.IDLEGSEL,                                           
019204              T01DLIN.DAEXDAT,                                            
019205              T01DLIN.TIEXTID,                                            
019206              T01DLIN.KDVALISO,                                           
019207              T01DLIN.IDLANDX3_SEND,                                      
019208              T01DLIN.IDLEVNR,                                            
019209              T01DLIN.IDPARTNR,                                           
019210              T01DLIN.KDFINDOC,                                           
019211              T01DLIN.FLSOFT,                                             
019212              T01DLIN.FLFREE,                                             
019213              T01DLIN.FLPRIV,                                             
019214              T01DLIN.IDBREAK_1,                                          
019215              T01DLIN.IDBREAK_2,                                          
019216              T01DLIN.KDVAT,                                              
019217              T01DLIN.IDDC,                                               
019218              T01DLIN.IDEXCUST_1                                          
019219                                                                          
019220     FROM     T01PROC,                                                    
019221              T01DLIN                                                     
019222                                                                          
019223     WHERE    T01PROC.IDSYSTEM      = 'WF02'                              
019224       AND    T01DLIN.IDLEGSEL      = T01PROC.IDLEGSEL                    
019225       AND    T01DLIN.DAEXDAT       = T01PROC.DAEXDAT                     
019226       AND    T01DLIN.TIEXTID       = T01PROC.TIEXTID                     
019235     END-EXEC                                                             
019236                                                                          
019237     EXEC SQL OPEN MISC2-CRS                                              
019238     END-EXEC                                                             
019239                                                                          
019240     MOVE 000            TO GOOD-SQLCODES                                 
019241     MOVE SQLCODE        TO SQLCODE-WS                                    
019242     PERFORM DB2-STATUS-CHECK                                             
019243     .                                                                    
019244     EJECT                                                                
019245                                                                          
019246 DB2-FETCH-CRS-MISC2 SECTION.                                             
019247     EXEC SQL                                                             
019248      FETCH NEXT ROWSET FROM MISC2-CRS FOR 100 ROWS                       
019249      INTO  :DLIN-IDLEGSEL,                                               
019250            :DLIN-DAEXDAT,                                                
019251            :DLIN-TIEXTID,                                                
019252            :DLIN-KDVALISO,                                               
019253            :DLIN-IDLANDX3-SEND,                                          
019254            :DLIN-IDLEVNR,                                                
019255            :DLIN-IDPARTNR,                                               
019256            :DLIN-KDFINDOC,                                               
019257            :DLIN-FLSOFT,                                                 
019258            :DLIN-FLFREE,                                                 
019259            :DLIN-FLPRIV,                                                 
019260            :DLIN-IDBREAK-1,                                              
019261            :DLIN-IDBREAK-2,                                              
019262            :DLIN-KDVAT,                                                  
019263            :DLIN-IDDC,                                                   
019264            :DLIN-IDEXCUST-1                                              
019265     END-EXEC                                                             
019266                                                                          
019267     MOVE 000100         TO GOOD-SQLCODES                                 
019268     MOVE SQLCODE        TO SQLCODE-WS                                    
019269     PERFORM DB2-STATUS-CHECK                                             
019270     .                                                                    
019271     EJECT                                                                
019272                                                                          
019273 DB2-CLOSE-CRS-MISC2 SECTION.                                             
019274     EXEC SQL                                                             
019275         CLOSE MISC2-CRS                                                  
019276     END-EXEC                                                             
019277     .                                                                    
019278     EJECT                                                                
019279                                                                          
019280 DB2-SELECT-DLIN1 SECTION.                                                
019281     EXEC SQL                                                             
019282     SELECT   SUM(T01DLIN.SUNTO),                                         
019283              SUM(T01DLIN.SUVAT_BILLIT),                                  
019284              SUM(T01DLIN.SUBTO)                                          
019285                                                                          
019286     INTO     :WS-SUNTO-APP,                                              
019287              :WS-SUVAT-BILLIT-APP,                                       
019288              :WS-SUBTO-APP                                               
019289                                                                          
019290     FROM     T01DLIN                                                     
019291                                                                          
019292     WHERE    T01DLIN.IDLEGSEL      = :WS-IDLEGSEL                        
019293       AND    T01DLIN.DAEXDAT       = :WS-DAEXDAT                         
019294       AND    T01DLIN.TIEXTID       = :WS-TIEXTID                         
019295       AND    T01DLIN.KDVALISO      = :WS-KDVALISO                        
019296       AND    T01DLIN.IDLANDX3_SEND = :WS-IDLANDX3-SEND                   
019297       AND    T01DLIN.IDLEVNR       = :WS-IDLEVNR                         
019298       AND    T01DLIN.IDPARTNR      = :WS-IDPARTNR                        
019299       AND    T01DLIN.KDFINDOC      = :WS-KDFINDOC                        
019300       AND    T01DLIN.FLSOFT        = :WS-FLSOFT                          
019301       AND    T01DLIN.FLFREE        = :WS-FLFREE                          
019302       AND    T01DLIN.FLPRIV        = :WS-FLPRIV                          
019303       AND    T01DLIN.IDBREAK_1     = :WS-IDBREAK-1                       
019304       AND    T01DLIN.IDBREAK_2     = :WS-IDBREAK-2                       
019305       AND    T01DLIN.IDAPPEND      = :WS-IDAPPEND                        
019306     GROUP BY                                                             
019307              T01DLIN.IDLEGSEL                                            
019308             ,T01DLIN.DAEXDAT                                             
019309             ,T01DLIN.TIEXTID                                             
019310             ,T01DLIN.KDVALISO                                            
019311             ,T01DLIN.IDLANDX3_SEND                                       
019312             ,T01DLIN.IDLEVNR                                             
019313             ,T01DLIN.IDPARTNR                                            
019314             ,T01DLIN.KDFINDOC                                            
019315             ,T01DLIN.FLSOFT                                              
019316             ,T01DLIN.FLFREE                                              
019317             ,T01DLIN.FLPRIV                                              
019318             ,T01DLIN.IDBREAK_1                                           
019319             ,T01DLIN.IDBREAK_2                                           
019320             ,T01DLIN.IDAPPEND                                            
019326     END-EXEC                                                             
019327                                                                          
019328     MOVE 000            TO GOOD-SQLCODES                                 
019329     MOVE SQLCODE        TO SQLCODE-WS                                    
019330     PERFORM DB2-STATUS-CHECK                                             
019331     .                                                                    
019332     EJECT                                                                
019333                                                                          
019334 DB2-SELECT-DLIN2 SECTION.                                                
019335     EXEC SQL                                                             
019336     SELECT   SUM(T01DLIN.SUNTO),                                         
019337              0,                                                          
019338              0                                                           
019339                                                                          
019340     INTO     :WS-SUNTO-APP,                                              
019341              :WS-SUVAT-BILLIT-APP,                                       
019342              :WS-SUBTO-APP                                               
019343                                                                          
019344     FROM     T01DLIN                                                     
019345                                                                          
019346     WHERE    T01DLIN.IDLEGSEL      = :WS-IDLEGSEL                        
019347       AND    T01DLIN.DAEXDAT       = :WS-DAEXDAT                         
019348       AND    T01DLIN.TIEXTID       = :WS-TIEXTID                         
019349       AND    T01DLIN.KDVALISO      = :WS-KDVALISO                        
019350       AND    T01DLIN.IDLANDX3_SEND = :WS-IDLANDX3-SEND                   
019351       AND    T01DLIN.IDLEVNR       = :WS-IDLEVNR                         
019352       AND    T01DLIN.IDPARTNR      = :WS-IDPARTNR                        
019353       AND    T01DLIN.KDFINDOC      = :WS-KDFINDOC                        
019354       AND    T01DLIN.FLSOFT        = :WS-FLSOFT                          
019355       AND    T01DLIN.FLFREE        = :WS-FLFREE                          
019356       AND    T01DLIN.FLPRIV        = :WS-FLPRIV                          
019357       AND    T01DLIN.IDBREAK_1     = :WS-IDBREAK-1                       
019358       AND    T01DLIN.IDBREAK_2     = :WS-IDBREAK-2                       
019359       AND    T01DLIN.KDVAT         = :WS-KDVAT                           
019360     GROUP BY                                                             
019361              T01DLIN.IDLEGSEL                                            
019362             ,T01DLIN.DAEXDAT                                             
019363             ,T01DLIN.TIEXTID                                             
019364             ,T01DLIN.KDVALISO                                            
019365             ,T01DLIN.IDLANDX3_SEND                                       
019366             ,T01DLIN.IDLEVNR                                             
019367             ,T01DLIN.IDPARTNR                                            
019368             ,T01DLIN.KDFINDOC                                            
019369             ,T01DLIN.FLSOFT                                              
019370             ,T01DLIN.FLFREE                                              
019371             ,T01DLIN.FLPRIV                                              
019372             ,T01DLIN.IDBREAK_1                                           
019373             ,T01DLIN.IDBREAK_2                                           
019374             ,T01DLIN.KDVAT                                               
019375     END-EXEC                                                             
019376                                                                          
019377     MOVE 000            TO GOOD-SQLCODES                                 
019378     MOVE SQLCODE        TO SQLCODE-WS                                    
019379     PERFORM DB2-STATUS-CHECK                                             
019380     .                                                                    
019381     EJECT                                                                
019382                                                                          
019433 DB2-SELECT-BUSINESS SECTION.                                             
019434     EXEC SQL                                                             
019435     SELECT   T01BURE.FLVATCHK                                            
019436             ,T01FCUS.FLDIRVAT                                            
019437                                                                          
019438     INTO     :BURE-FLVATCHK                                              
019439             ,:FCUS-FLDIRVAT                                              
019440                                                                          
019441     FROM     T01BURE,                                                    
019442              T01FCUS                                                     
019443                                                                          
019444     WHERE    T01BURE.IDLEGSEL      = :WS-IDLEGSEL                        
019445       AND    T01BURE.KDFINDOC      = :WS-KDFINDOC                        
019446       AND    T01BURE.KDSTATUS      = 1                                   
019447       AND    T01BURE.DADELDAT      = '00000000'                          
019448       AND    T01FCUS.IDLEGSEL      = :WS-IDLEGSEL                        
019449       AND    T01FCUS.IDPARTNR      = :WS-IDPARTNR                        
019450       AND    T01FCUS.KDSTATUS      = 1                                   
019451       AND    T01FCUS.DADELDAT      = '00000000'                          
019452       AND    T01BURE.IDLEGSEL      = T01FCUS.IDLEGSEL                    
019453       AND    T01BURE.KDPARTTY      = T01FCUS.KDPARTTY                    
019454       AND    T01BURE.KDPARTGR      = T01FCUS.KDPARTGR                    
019455     END-EXEC                                                             
019456                                                                          
019457     MOVE 000            TO GOOD-SQLCODES                                 
019458     MOVE SQLCODE        TO SQLCODE-WS                                    
019459     PERFORM DB2-STATUS-CHECK                                             
019460     .                                                                    
019461     EJECT                                                                
019462                                                                          
019463 DB2-SELECT-VAT-VERS1 SECTION.                                            
019464     EXEC SQL                                                             
019465     SELECT   T01VAT.REVAT                                                
019466                                                                          
019467     INTO     :VAT-REVAT                                                  
019468                                                                          
019469     FROM     T01VAT                                                      
019470                                                                          
019471     WHERE    T01VAT.IDLEGSEL      = :WS-IDLEGSEL                         
019472       AND    T01VAT.IDLANDX2      = :WS-IDLANDX3-SEND                    
019473       AND    T01VAT.KDVAT         = :WS-KDVAT                            
019474       AND    T01VAT.DADELDAT      = :WS-ACTIVE                           
019475     END-EXEC                                                             
019476                                                                          
019477     MOVE 000100         TO GOOD-SQLCODES                                 
019478     MOVE SQLCODE        TO SQLCODE-WS                                    
019479     PERFORM DB2-STATUS-CHECK                                             
019480     .                                                                    
019481     EJECT                                                                
019482                                                                          
019483 DB2-SELECT-VAT-VERS2 SECTION.                                            
019484     EXEC SQL                                                             
019485     SELECT   DISTINCT                                                    
019486              T01VAT.REVAT                                                
019487                                                                          
019488     INTO     :VAT-REVAT                                                  
019489                                                                          
019490     FROM     T01VAT                                                      
019491                                                                          
019492     WHERE    T01VAT.IDLEGSEL      = :WS-IDLEGSEL                         
019493       AND    T01VAT.KDVAT         = :WS-KDVAT                            
019494       AND    T01VAT.DADELDAT      = :WS-ACTIVE                           
019495     END-EXEC                                                             
019496                                                                          
019497     MOVE 000            TO GOOD-SQLCODES                                 
019498     MOVE SQLCODE        TO SQLCODE-WS                                    
019499     PERFORM DB2-STATUS-CHECK                                             
019500     .                                                                    
019501     EJECT                                                                
019502                                                                          
019503 DB2-SELECT-CUGR-INRE SECTION.                                            
019504     EXEC SQL                                                             
019505     SELECT   T01CUGR.FLVAT,                                              
019506              T01INRE.FLVAT,                                              
019507              T01INRE.FLVAT_PRIV                                          
019508                                                                          
019509     INTO     :CUGR-FLVAT,                                                
019510              :INRE-FLVAT,                                                
019511              :INRE-FLVAT-PRIV                                            
019512                                                                          
019513     FROM     T01FCUS,                                                    
019514              T01CUGR,                                                    
019515              T01INRE                                                     
019516                                                                          
019517     WHERE    T01FCUS.IDLEGSEL      = :WS-IDLEGSEL                        
019518       AND    T01FCUS.IDPARTNR      = :WS-IDPARTNR                        
019519       AND    T01FCUS.KDSTATUS      = 1                                   
019520       AND    T01FCUS.DADELDAT      = '00000000'                          
019521       AND    T01INRE.IDLEGSEL      = :WS-IDLEGSEL                        
019522       AND    T01INRE.IDLANDX3_SEND = :WS-IDLANDX3-SEND                   
019523       AND    T01INRE.IDLANDX3_REC  = T01FCUS.IDLANDX3                    
019524       AND    T01INRE.KDSTATUS      = 1                                   
019525       AND    T01INRE.DADELDAT      = '00000000'                          
019526       AND    T01CUGR.IDLEGSEL      = T01FCUS.IDLEGSEL                    
019527       AND    T01CUGR.KDPARTTY      = T01FCUS.KDPARTTY                    
019528       AND    T01CUGR.KDPARTGR      = T01FCUS.KDPARTGR                    
019529       AND    T01CUGR.KDSTATUS      = 1                                   
019530       AND    T01CUGR.DADELDAT      = '00000000'                          
019531     END-EXEC                                                             
019532                                                                          
019533     MOVE 000            TO GOOD-SQLCODES                                 
019534     MOVE SQLCODE        TO SQLCODE-WS                                    
019535     PERFORM DB2-STATUS-CHECK                                             
019536     .                                                                    
019537     EJECT                                                                
019538                                                                          
019539 DB2-INSERT-DAPP1 SECTION.                                                
019540     EXEC SQL INSERT INTO T01DAPP                                         
019541        (                                                                 
019542         IDLEGSEL,                                                        
019543         DAEXDAT,                                                         
019544         TIEXTID,                                                         
019545         KDVALISO,                                                        
019546         IDLANDX3_SEND,                                                   
019547         IDLEVNR,                                                         
019548         IDPARTNR,                                                        
019549         KDFINDOC,                                                        
019550         FLSOFT,                                                          
019551         FLFREE,                                                          
019552         FLPRIV,                                                          
019553         IDBREAK_1,                                                       
019554         IDBREAK_2,                                                       
019555         KDAPPEND,                                                        
019556         IDAPPEND,                                                        
019557         SUNTO_APP,                                                       
019558         SUVAT_BILLIT_APP,                                                
019559         SUBTO_APP                                                        
019560        )                                                                 
019561       VALUES                                                             
019562        (                                                                 
019563         :DLIN-IDLEGSEL,                                                  
019564         :DLIN-DAEXDAT,                                                   
019565         :DLIN-TIEXTID,                                                   
019566         :DLIN-KDVALISO,                                                  
019567         :DLIN-IDLANDX3-SEND,                                             
019568         :DLIN-IDLEVNR,                                                   
019569         :DLIN-IDPARTNR,                                                  
019570         :DLIN-KDFINDOC,                                                  
019571         :DLIN-FLSOFT,                                                    
019572         :DLIN-FLFREE,                                                    
019573         :DLIN-FLPRIV,                                                    
019574         :DLIN-IDBREAK-1,                                                 
019575         :DLIN-IDBREAK-2,                                                 
019576         :WS-DLIN-KDAPPEND,                                               
019577         :DLIN-IDAPPEND,                                                  
019578         :DAPP-SUNTO-APP,                                                 
019579         :DAPP-SUVAT-BILLIT-APP,                                          
019580         :DAPP-SUBTO-APP                                                  
019581        ) FOR :WS-MULTIFETCH ROWS ATOMIC                                  
019582     END-EXEC                                                             
019583                                                                          
019584     MOVE 000            TO GOOD-SQLCODES                                 
019585     MOVE SQLCODE        TO SQLCODE-WS                                    
019586     PERFORM DB2-STATUS-CHECK                                             
019587     .                                                                    
019588     EJECT                                                                
019589                                                                          
019590 DB2-INSERT-DAPP2 SECTION.                                                
019591     EXEC SQL INSERT INTO T01DAPP                                         
019592        (                                                                 
019593         IDLEGSEL,                                                        
019594         DAEXDAT,                                                         
019595         TIEXTID,                                                         
019596         KDVALISO,                                                        
019597         IDLANDX3_SEND,                                                   
019598         IDLEVNR,                                                         
019599         IDPARTNR,                                                        
019600         KDFINDOC,                                                        
019601         FLSOFT,                                                          
019602         FLFREE,                                                          
019603         FLPRIV,                                                          
019604         IDBREAK_1,                                                       
019605         IDBREAK_2,                                                       
019606         KDAPPEND,                                                        
019607         IDAPPEND,                                                        
019608         SUNTO_APP,                                                       
019609         SUVAT_BILLIT_APP,                                                
019610         SUBTO_APP                                                        
019611        )                                                                 
019612       VALUES                                                             
019613        (                                                                 
019614         :DLIN-IDLEGSEL,                                                  
019615         :DLIN-DAEXDAT,                                                   
019616         :DLIN-TIEXTID,                                                   
019617         :DLIN-KDVALISO,                                                  
019618         :DLIN-IDLANDX3-SEND,                                             
019619         :DLIN-IDLEVNR,                                                   
019620         :DLIN-IDPARTNR,                                                  
019621         :DLIN-KDFINDOC,                                                  
019622         :DLIN-FLSOFT,                                                    
019623         :DLIN-FLFREE,                                                    
019624         :DLIN-FLPRIV,                                                    
019625         :DLIN-IDBREAK-1,                                                 
019626         :DLIN-IDBREAK-2,                                                 
019627         :WS-DLIN-KDAPPEND,                                               
019628         :WS-DLIN-IDAPPEND,                                               
019629         :DAPP-SUNTO-APP,                                                 
019630         :DAPP-SUVAT-BILLIT-APP,                                          
019631         :DAPP-SUBTO-APP                                                  
019632        ) FOR :WS-MULTIFETCH ROWS ATOMIC                                  
019633     END-EXEC                                                             
019634                                                                          
019635     MOVE 000            TO GOOD-SQLCODES                                 
019636     MOVE SQLCODE        TO SQLCODE-WS                                    
019637     PERFORM DB2-STATUS-CHECK                                             
019638     .                                                                    
019639     EJECT                                                                
019640                                                                          
019641 DB2-STATUS-CHECK SECTION.                                                
019642     SET SQLCODE-IX         TO 1                                          
019643     SEARCH GOOD-SQLCODE AT END                                           
019644           CALL ABEND USING RKOD-ABEND-DB2                                
019645        WHEN GOOD-SQLCODE (SQLCODE-IX) = SQLCODE-WS                       
019646           CONTINUE                                                       
019650     END-SEARCH                                                           
019700     .                                                                    
