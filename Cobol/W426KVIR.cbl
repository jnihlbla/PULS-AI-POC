000100*COMPOPT STDSUB=YES                                                       
000200 ID DIVISION.                                                             
000300     SKIP2                                                                
000400 PROGRAM-ID.     W426KVIR.                                                
000500*AUTHOR.         KENT JEBSEN                                              
000600*DATE-WRITTEN.   JANUARI -98.                                             
000700                                                                          
000800*                                                                         
000900*    REMARKS.                                                             
001000*                                                                         
001100*    FUNKTION:                                                            
001200*        SUBPROGRAM SOM REDIGERAR TRANSAR TILL VIR                        
001300*        OCH ANROPAR W006PRC1 SOM SKICKAR TRANSARNA MED VCOM              
001400*                                                                         
001500*        PROGRAMMET LÄSER      W6KVAE (W6H7)                              
001600*        PROGRAMMET LÄSER      W6LEVA (W6F1)                              
001700*        PROGRAMMET LÄSER      WLBENA (WDD3)                              
001800*        PROGRAMMET LÄSER      WDP3                                       
001900*        PROGRAMMET LÄSER      WLLEVA (WDF1)                              
002000*                                                                         
002100*                                                                         
002200                                                                          
002300 ENVIRONMENT DIVISION.                                                    
002400     EJECT                                                                
002500 DATA DIVISION.                                                           
002600 WORKING-STORAGE SECTION.                                                 
002700                                                                          
002800* CHECKED BY WY2000                                                       
002900     SKIP3                                                                
003000 77  FELTEXT                     PIC X(999) VALUE SPACE.                  
003100 77  IDPGM                       PIC X(08)  VALUE 'W426KVIR'.             
003200 77  JA                          PIC X      VALUE 'J'.                    
003300 77  NEJ                         PIC X      VALUE 'N'.                    
003400 77  IX                          PIC 9(2)   VALUE ZERO.                   
003500 77  WS-IDMEMO                   PIC X(16)  VALUE SPACE.                  
003600 77  WS-IDMAIL                   PIC X(64)  VALUE SPACE.                  
003700 77  WS-POS                      PIC 9      VALUE ZERO.                   
003800 77  WS-ANSKAFFARE               PIC X(30)  VALUE SPACE.                  
003900 77  WS-INKOPARE                 PIC X(30)  VALUE SPACE.                  
004000 77  WS-BEART                    PIC X(25)  VALUE SPACE.                  
004100 77  WS-BELEV                    PIC X(30)  VALUE SPACE.                  
004200 77  WS-IDPRTLST                 PIC X(8)   VALUE SPACE.                  
004300 77  WS-IDAVINR-9                PIC 9(9)   VALUE ZERO.                   
004400 77  WS-IDARTNR-NUM              PIC 9(9)   VALUE ZERO.                   
004500 77  WS-KVAVIS-NUM               PIC 9(6)   VALUE ZERO.                   
004600 77  WS-KVANTMOT-NUM             PIC 9(6)   VALUE ZERO.                   
004700 77  WS-KVANTAL                  PIC 9(6)   VALUE ZERO.                   
004800 77  COUNTER                     PIC 99     VALUE ZERO.                   
004900 77  COUNTER2                    PIC 99     VALUE ZERO.                   
005000                                                                          
005100 01 TABELL-FELTEXT.                                                       
005200     03  FEL-TABELL OCCURS 20.                                            
005300       05  TAB-TEKRFEL          PIC X(66)   VALUE SPACE.                  
005400                                                                          
005500 01 NYCKLAR.                                                              
005600     03 NKL-PLANT               PIC X.                                    
005700     03 NKL-REPTYPE             PIC X.                                    
005800     03 NKL-REPSERNO            PIC 9(5).                                 
005900                                                                          
006000*    -COPY PI30001   -PRE VIR1-                                           
006100*    -COPY PI30002   -PRE VIR2-                                           
006200*    -COPY PI30003   -PRE VIR3-                                           
006300*    -COPY W006PRVC                                                       
006400*                                                                         
006500*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
006600 01  GENERELLA-SUBPROGRAM.                                                
006700     03  CBLTDLI                 PIC X(8)   VALUE 'CBLTDLI '.             
006800     03  FELLOG                  PIC X(8)   VALUE 'FELLOG  '.             
006900     03  W006PRC1                PIC X(8)   VALUE 'W006PRC1'.             
007000     EJECT                                                                
007100*    --- W006PRAR, AREA FÖR W006PRC1                                      
007200 01  FILLER                    PIC X(16) VALUE 'W006PRAR********'.        
007300                                                                          
007400*01  -COPY W006PRAR                                                       
007500     EJECT                                                                
007600*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
007700*                                                                         
007800 01  FILLER                      PIC X(16)  VALUE 'IMS-WS'.               
007900                                                                          
008000 01  NYCKLAR-TILL-DLI.                                                    
008100*----> DIREKTNYCKEL TILL W6H701                                           
008200                                                                          
008300     03  W-IDKR-X.                                                        
008400         05  W-IDKR              PIC 9(05)  VALUE ZERO.                   
008500                                                                          
008600*----> DIREKTNYCKEL TILL WDD301                                           
008700                                                                          
008800     03  W-IDARTNR-X.                                                     
008900         05  W-IDARTNR           PIC S9(9)  VALUE ZERO COMP-3.            
009000                                                                          
009100*----> DIREKTNYCKEL TILL WDD311                                           
009200                                                                          
009300     03  W-IDSKYLT-X.                                                     
009400         05  W-IDSKYLT           PIC  X(3)  VALUE 'GB '.                  
009500                                                                          
009600*----> LEVREGISTER W6F1                                                   
009700                                                                          
009800     03  W-IDLEVNR-X.                                                     
009900         05  W-IDLEVNR           PIC X(5)   VALUE SPACE.                  
010000                                                                          
010100     03  W-IDLEVG-X.                                                      
010200         05  W-IDLEVG            PIC S9(5)  VALUE ZERO COMP-3.            
010300                                                                          
010400*----> PERSONREGISTER WDP3                                                
010500                                                                          
010600     03  W-KDARBTYP-X.                                                    
010700         05  W-KDARBTYP          PIC X(8)    VALUE SPACE.                 
010800     03  W-IDPERSON-X.                                                    
010900         05  W-IDPERSON          PIC S9(3)   VALUE +0 COMP-3.             
011000*    --- STATUS-KOD FRÅN IMS                                              
011100                                                                          
011200 01  STATUS-WS                   PIC  X(02).                              
011300     88  SEGMENT-FINNS                       VALUE '  '.                  
011400     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
011500     88  END-OF-DATA                         VALUE 'GB'.                  
011600     SKIP2                                                                
011700 01  GODK-STATUSKODER.                                                    
011800     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
011900                                                                          
012000 01  SSA1                        PIC X(64).                               
012100 01  SSA2                        PIC X(64).                               
012200     EJECT                                                                
012300                                                                          
012400 01  PA-TEXT               PIC X(66)                                      
012500           VALUE '       ***  WE EXPECT AN INVOICE FROM YOU ***'.         
012600 01  PA-TEXT2              PIC X(66)                                      
012700           VALUE 'THIS DOES NOT APPLY TO SELF BILLING SUPPLIERS'.         
012800*                                                                         
012900*                                                                         
013000*    --- IMS FUNKTIONSKODER                                               
013100*01  -COPY W0003                                                          
013200     EJECT                                                                
013300*    ---  DLI INPUT-OUTPUT AREA                                           
013400 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA1'.        
013500                                                                          
013600 01  DLI-IO-AREA1.                                                        
013700                                                                          
013800     03  IO-AREA1                PIC X(382)  VALUE SPACE.                 
013900                                                                          
014000     03  W6KVAE01 REDEFINES IO-AREA1.                                     
014100*        05 -COPY W6H701                                                  
014200 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA2'.        
014300                                                                          
014400 01  DLI-IO-AREA2.                                                        
014500                                                                          
014600     03  IO-AREA2                PIC X(991)  VALUE SPACE.                 
014700     03  W6KVAE14 REDEFINES IO-AREA2.                                     
014800*        05 -COPY W6H714                                                  
014900     EJECT                                                                
015000 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA3'.        
015100                                                                          
015200 01  DLI-IO-AREA3.                                                        
015300                                                                          
015400     03  IO-AREA3                PIC X(128)  VALUE SPACE.                 
015500                                                                          
015600     03  WLBENA01 REDEFINES IO-AREA3.                                     
015700*        05 -COPY WDD301                                                  
015800     03  WLBENA11 REDEFINES IO-AREA3.                                     
015900*        05 -COPY WDD311                                                  
016000     EJECT                                                                
016100 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA4'.        
016200                                                                          
016300 01  DLI-IO-AREA4.                                                        
016400     03  IO-AREA4                PIC X(275)  VALUE SPACE.                 
016500     03  W6LEVA11 REDEFINES IO-AREA4.                                     
016600*        05 -COPY W6F111                                                  
016700     EJECT                                                                
016800 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-P311'.         
016900                                                                          
017000 01  DLI-IO-P311.                                                         
017100*    03  -COPY WDP311                                                     
017200     EJECT                                                                
017300 01  DLI-IO-AREA6.                                                        
017400     03  IO-AREA6                PIC X(300)  VALUE SPACE.                 
017500     03  WLLEVA01 REDEFINES IO-AREA6.                                     
017600*        05  -COPY WDF101                                                 
017700     SKIP3                                                                
017800     03  WLLEVA14 REDEFINES IO-AREA6.                                     
017900*        05  -COPY WDF106                                                 
018000     EJECT                                                                
018100 LINKAGE SECTION.                                                         
018200                                                                          
018300*                                                                         
018400*    -COPY W426KVIR                                                       
018500*                                                                         
018600     EJECT                                                                
018700*01  -COPY W0009      -PRE ALT2-                                          
018800     EJECT                                                                
018900*01  -COPY W0008      -PRE KVAE-                                          
019000     05  FILLER                  PIC X.                                   
019100     EJECT                                                                
019200*01  -COPY W0008      -PRE BENA-                                          
019300     05  FILLER                  PIC X.                                   
019400     EJECT                                                                
019500*01  -COPY W0008      -PRE W6F1-                                          
019600     05  FILLER                  PIC X.                                   
019700     EJECT                                                                
019800*01  -COPY W0008      -PRE WDP3-                                          
019900     05  FILLER                  PIC X.                                   
020000     EJECT                                                                
020100*01  -COPY W0008      -PRE LEVA-                                          
020200     05  FILLER                  PIC X.                                   
020300     EJECT                                                                
020400 PROCEDURE DIVISION  USING KVIR-W426KVIR                                  
020500                           ALT2-PCB                                       
020600                           KVAE-PCB                                       
020700                           BENA-PCB                                       
020800                           W6F1-PCB                                       
020900                           WDP3-PCB                                       
021000                           LEVA-PCB.                                      
021100                                                                          
021200     MOVE KVIR-IDKR TO  W-IDKR                                            
021300     PERFORM IMS-GU-KVAE01                                                
021400                                                                          
021500     IF SEGMENT-FINNS                                                     
021600       PERFORM B-HAMTA-FIXA-DATA                                          
021700       PERFORM C-SKAPA-UTTRANSAR                                          
021800     END-IF                                                               
021900                                                                          
022000     MOVE ZERO TO RETURN-CODE                                             
022100     GOBACK                                                               
022200     .                                                                    
022300     EJECT                                                                
022400 B-HAMTA-FIXA-DATA SECTION.                                               
022500                                                                          
022600     MOVE ZERO  TO COUNTER                                                
022700                   COUNTER2                                               
022800     MOVE SPACE TO WS-IDMAIL                                              
022900                   WS-IDMEMO                                              
023000     INSPECT KVIR-IDMAIL-ANSVARIG                                         
023100             TALLYING COUNTER FOR CHARACTERS BEFORE '@'                   
023200     INSPECT KVIR-IDMAIL-ANSVARIG                                         
023300             TALLYING COUNTER2 FOR CHARACTERS AFTER '@'                   
023400     IF COUNTER2 > 0                                                      
023500       MOVE KVIR-IDMAIL-ANSVARIG(1:COUNTER)                               
023600                                      TO WS-IDMAIL(1:COUNTER)             
023700       MOVE '(A)'                     TO WS-IDMAIL(COUNTER + 1:3)         
023800       MOVE KVIR-IDMAIL-ANSVARIG(COUNTER + 2:COUNTER2)                    
023900                                TO WS-IDMAIL(COUNTER + 4:COUNTER2)        
024000     ELSE                                                                 
024100       MOVE KVIR-IDMAIL-ANSVARIG(1:16) TO WS-IDMEMO                       
024200     END-IF                                                               
024300     PERFORM BA-HAEMTA-BENAEMNING                                         
024400     IF KVIR-IDANSK > 0                                                   
024500       PERFORM BB-HAEMTA-ANSK                                             
024600     END-IF                                                               
024700     PERFORM BC-FIXA-FELTEXT                                              
024800     IF KVIR-IDINK > SPACE                                                
024900       PERFORM BD-HAEMTA-INKOPARE                                         
025000     END-IF                                                               
025100     PERFORM BE-HAEMTA-LEVNAMN                                            
025200     .                                                                    
025300     EJECT                                                                
025400 BA-HAEMTA-BENAEMNING SECTION.                                            
025500** --- ALLTID ENGELSK BENÄMNING                                           
025600     MOVE KR-IDARTNR   TO  W-IDARTNR                                      
025700     PERFORM IMS-GU-BENA                                                  
025800                                                                          
025900     IF SEGMENT-FINNS                                                     
026000       MOVE TEXT-BEART TO WS-BEART                                        
026100     END-IF                                                               
026200     .                                                                    
026300     EJECT                                                                
026400 BB-HAEMTA-ANSK SECTION.                                                  
026500** -- HÄMTAR ANSKAFFARNAMN FRÅN WDP3                                      
026600       MOVE KVIR-IDANSK         TO W-IDPERSON                             
026700       MOVE 'ANSK    '          TO W-KDARBTYP                             
026800       PERFORM IMS-GU-WDP311                                              
026900       IF SEGMENT-FINNS                                                   
027000         MOVE PERS-IDNAMN(1:30) TO WS-ANSKAFFARE                          
027100       END-IF                                                             
027200     .                                                                    
027300     EJECT                                                                
027400 BC-FIXA-FELTEXT SECTION.                                                 
027500                                                                          
027600     PERFORM IMS-GNP-KVAE14                                               
027700                                                                          
027800     IF SEGMENT-FINNS                                                     
027900       MOVE +1 TO IX                                                      
028000       PERFORM UNTIL IX > 15                                              
028100         MOVE TEXT-TEKRFEL(IX) TO TAB-TEKRFEL(IX)                         
028200         ADD +1 TO IX                                                     
028300       END-PERFORM                                                        
028400     END-IF                                                               
028500                                                                          
028600     IF KR-IDKRFEL = 'PA'                                                 
028700       MOVE PA-TEXT  TO TAB-TEKRFEL(19)                                   
028800       MOVE PA-TEXT2 TO TAB-TEKRFEL(20)                                   
028900     END-IF                                                               
029000                                                                          
029100     IF KR-KVART-RET > 0                                                  
029200        PERFORM BCA-HAMTA-GODSADRESS                                      
029300     END-IF                                                               
029400     .                                                                    
029500     EJECT                                                                
029600 BCA-HAMTA-GODSADRESS SECTION.                                            
029700** --- GODSET SKA SKICKAS I RETUR. HÄMTAR GODSADRESS PÅ W6F111.           
029800** --- ADRESSEN LÄGGS I DE 5 SISTA RADERNA AV FELTEXTEN.                  
029900     MOVE KR-IDLEVNR TO  W-IDLEVNR                                        
030000     MOVE KR-IDLEVG  TO  W-IDLEVG                                         
030100                                                                          
030200     PERFORM IMS-GU-W6LEVA11                                              
030300                                                                          
030400     IF SEGMENT-FINNS                                                     
030500       MOVE 'GODSADRESS;RETURN TO:' TO TAB-TEKRFEL(16)                    
030600       MOVE GADR-BELEV              TO TAB-TEKRFEL(17)(1:35)              
030700       MOVE ';'                     TO TAB-TEKRFEL(17)(36:1)              
030800       MOVE KR-ADATTENT(1:30)       TO TAB-TEKRFEL(17)(37:30)             
030900       MOVE GADR-ADLEV-RAD1         TO TAB-TEKRFEL(18)                    
031000       MOVE GADR-ADLEV-RAD2         TO TAB-TEKRFEL(19)                    
031100       MOVE GADR-ADLEV-ORT          TO TAB-TEKRFEL(20)(1:35)              
031200       MOVE ';'                     TO TAB-TEKRFEL(20)(36:1)              
031300       MOVE GADR-ADLEVLND           TO TAB-TEKRFEL(20)(38:20)             
031400     END-IF                                                               
031500     .                                                                    
031600     EJECT                                                                
031700 BD-HAEMTA-INKOPARE SECTION.                                              
031800** -- HÄMTAR INKÖPARNAMN FRÅN WDP3                                        
031900     IF KVIR-IDINK (1:3) NUMERIC                                          
032000        MOVE KVIR-IDINK (1:3)    TO W-IDPERSON                            
032100     ELSE                                                                 
032200        IF KVIR-IDINK (2:3) NUMERIC                                       
032300           MOVE KVIR-IDINK (2:3) TO W-IDPERSON                            
032400        ELSE                                                              
032500           MOVE ZERO             TO W-IDPERSON                            
032600        END-IF                                                            
032700     END-IF                                                               
032800     MOVE 'INK     '          TO W-KDARBTYP                               
032900     PERFORM IMS-GU-WDP311                                                
033000     IF SEGMENT-FINNS                                                     
033100       MOVE PERS-IDNAMN(1:30) TO WS-INKOPARE                              
033200     END-IF                                                               
033300     .                                                                    
033400     EJECT                                                                
033500 BE-HAEMTA-LEVNAMN SECTION.                                               
033600** -- HÄMTAR LEVERANTÖRSNAMN FRÅN WDF1                                    
033700                                                                          
033800     MOVE KR-IDLEVNR      TO W-IDLEVNR                                    
033900     MOVE KR-IDLEVG       TO W-IDLEVG                                     
034000                                                                          
034100     PERFORM IMS-GU-WLLEVA01                                              
034200     PERFORM IMS-GNP-WLLEVA14                                             
034300     MOVE ADR-BELEV(1:30) TO WS-BELEV                                     
034400     .                                                                    
034500     EJECT                                                                
034600 C-SKAPA-UTTRANSAR   SECTION.                                             
034700     PERFORM CA-SKAPA-NYCKELFAELT                                         
034800     PERFORM S02-PRT-OPEN                                                 
034900     PERFORM CB-SKAPA-RECORD1                                             
035000     PERFORM CC-SKAPA-RECORD2                                             
035100                                                                          
035200     MOVE SPACE TO WS-IDMAIL                                              
035300                   WS-IDMEMO                                              
035400     MOVE ZERO TO COUNTER                                                 
035500                    COUNTER2                                              
035600     INSPECT KVIR-IDMAIL-LEV                                              
035700             TALLYING COUNTER FOR CHARACTERS BEFORE '@'                   
035800     INSPECT KVIR-IDMAIL-LEV                                              
035900             TALLYING COUNTER2 FOR CHARACTERS AFTER '@'                   
036000     IF COUNTER2 > 0                                                      
036100       MOVE KVIR-IDMAIL-LEV(1:COUNTER)  TO WS-IDMAIL(1:COUNTER)           
036200       MOVE '(A)'                   TO WS-IDMAIL(COUNTER + 1:3)           
036300       MOVE KVIR-IDMAIL-LEV(COUNTER + 2:COUNTER2)                         
036400                              TO WS-IDMAIL(COUNTER + 4:COUNTER2)          
036500     END-IF                                                               
036600     MOVE KVIR-IDLEVFAX TO WS-IDMEMO                                      
036700                                                                          
036800     PERFORM CD-SKAPA-RECORD3                                             
036900     PERFORM S04-PRT-CLOSE                                                
037000     .                                                                    
037100     EJECT                                                                
037200 CA-SKAPA-NYCKELFAELT SECTION.                                            
037300** KEYS                                                                   
037400     MOVE 'R'                        TO NKL-PLANT                         
037500     IF KR-IDKRFEL(1:1) = 'P' OR 'K'                                      
037600        MOVE 'A'                     TO NKL-REPTYPE                       
037700     ELSE                                                                 
037800        MOVE 'T'                     TO NKL-REPTYPE                       
037900     END-IF                                                               
038000     MOVE KR-IDKR                    TO NKL-REPSERNO                      
038100     .                                                                    
038200     EJECT                                                                
038300 CB-SKAPA-RECORD1 SECTION.                                                
038400** KEYS                                                                   
038500     MOVE NYCKLAR                    TO VIR1-KEYS                         
038600                                                                          
038700** THE-REST                                                               
038800     MOVE '1'                        TO VIR1-RECTYPE                      
038900     IF KR-FLANNULL = 'J'                                                 
039000       MOVE 'C'                      TO VIR1-STATUS                       
039100     ELSE                                                                 
039200       MOVE 'N'                      TO VIR1-STATUS                       
039300     END-IF                                                               
039400                                                                          
039500     MOVE KR-IDLEVNR                 TO VIR1-SUPPNO                       
039600     MOVE FUNCTION CURRENT-DATE(1:8) TO VIR1-REPDATE                      
039700     MOVE KR-IDARTNR                 TO WS-IDARTNR-NUM                    
039800     MOVE WS-IDARTNR-NUM             TO VIR1-PARTNO                       
039900     MOVE WS-ANSKAFFARE              TO VIR1-MCTRL                        
040000     MOVE KR-BEKRANS                 TO VIR1-ISSNAME                      
040100     MOVE KR-IDKRATLF                TO VIR1-ISSTEL                       
040200     MOVE '+46-31596565    '         TO VIR1-ISSFAX                       
040300     MOVE WS-IDMEMO                  TO VIR1-ISSMEMO                      
040400     MOVE TABELL-FELTEXT             TO VIR1-DESCRIPT                     
040500     MOVE SPACE                      TO VIR1-VANR                         
040600     MOVE KR-KDHANDCO                TO VIR1-HANDLCDE                     
040700     MOVE '0000000'                  TO VIR1-HANDLCST                     
040800     IF KR-KDHANDCO = 2                                                   
040900       MOVE 'SEK'                    TO VIR1-CURRCDE                      
041000     ELSE                                                                 
041100       MOVE SPACE                    TO VIR1-CURRCDE                      
041200     END-IF                                                               
041300     MOVE SPACE                      TO VIR1-HANDLHOUR                    
041400     MOVE KR-KDDISP                  TO VIR1-DISPCDE                      
041500     MOVE SPACE                      TO VIR1-BATCHNO                      
041600     MOVE KR-KVAVIS                  TO WS-KVAVIS-NUM                     
041700     MOVE WS-KVAVIS-NUM              TO VIR1-SHIPQTY                      
041800                                                                          
041900     MOVE KR-IDAVINR                 TO WS-IDAVINR-9                      
042000     MOVE WS-IDAVINR-9               TO VIR1-SHIPID                       
042100                                                                          
042200** --SÄTTER 4-STÄLLIGT ÅRTAL                                              
042300                                                                          
042400     IF KR-DAAVSDAT = ZERO                                                
042500       MOVE SPACE                    TO VIR1-SHIPDATE                     
042600     ELSE                                                                 
042700       MOVE KR-DAAVSDAT              TO VIR1-SHIPDATE                     
042800     END-IF                                                               
042900                                                                          
043000                                                                          
043100     IF NKL-REPTYPE = 'T'                                                 
043200        MOVE KR-IDKRFEL              TO VIR1-DEVCDE                       
043300** TILLÄGG 980507                                                         
043400** JUSTERAD KVANT SKA INGÅ I REJECTED QUANTITY                            
043500        COMPUTE WS-KVANTAL = KR-KVART-RET + KR-KVART-SKROT +              
043600                              KR-KVART-KJUST                              
043700        MOVE WS-KVANTAL              TO VIR1-REJQTY                       
043800        IF KR-FLINKANS = JA                                               
043900           MOVE 'PURCH'              TO VIR1-RESPVCC                      
044000        ELSE                                                              
044100           MOVE 'PLANT'              TO VIR1-RESPVCC                      
044200        END-IF                                                            
044300        MOVE '000000000'             TO VIR1-PARTNOP1                     
044400        MOVE '000000'                TO VIR1-QTYPALL1                     
044500     ELSE                                                                 
044600        MOVE '00'                    TO VIR1-DEVCDE                       
044700        MOVE '000000'                TO VIR1-REJQTY                       
044800        MOVE SPACE                   TO VIR1-RESPVCC                      
044900        MOVE WS-IDARTNR-NUM          TO VIR1-PARTNOP1                     
045000        MOVE KR-KVANTMOT             TO WS-KVANTMOT-NUM                   
045100        MOVE WS-KVANTMOT-NUM         TO VIR1-QTYPALL1                     
045200     END-IF                                                               
045300                                                                          
045400     MOVE '000000000'                TO VIR1-PARTNOP2                     
045500     MOVE '000000'                   TO VIR1-QTYPALL2                     
045600     MOVE '000000000'                TO VIR1-PARTNOP3                     
045700     MOVE '000000'                   TO VIR1-QTYPALL3                     
045800     MOVE SPACE                      TO VIR1-CHASSNO                      
045900                                        VIR1-MILEAGE                      
046000                                        VIR1-PRODDATE                     
046100                                        VIR1-VCCQNO                       
046200     MOVE WS-IDMAIL                  TO VIR1-ISSEMAIL                     
092905     MOVE +2000                      TO PRC1-KVLRECL                      
046500     MOVE VIR1-PI30001               TO PRC1-DATA                         
046600     PERFORM S03-PRT-SEND                                                 
046700     .                                                                    
046800     EJECT                                                                
046900 CC-SKAPA-RECORD2 SECTION.                                                
047000                                                                          
047100     MOVE SPACE                      TO VIR2-PI30002                      
047200                                                                          
047300** KEYS                                                                   
047400     MOVE NYCKLAR                    TO VIR2-KEYS                         
047500                                                                          
047600** THE-REST                                                               
047700     MOVE '2'                        TO VIR2-RECTYPE                      
047800     IF KVIR-IDINK > SPACE                                                
047900       IF KVIR-IDINK (1:1) NUMERIC                                        
048000          MOVE KVIR-IDINK (1:3)     TO VIR2-PURCHNO                       
048100       ELSE                                                               
048200          MOVE KVIR-IDINK (2:3)     TO VIR2-PURCHNO                       
048300       END-IF                                                             
048400       MOVE WS-INKOPARE              TO VIR2-PURCHNAME                    
048500     ELSE                                                                 
048600       MOVE KVIR-IDANSK              TO VIR2-PURCHNO                      
048700       MOVE WS-ANSKAFFARE            TO VIR2-PURCHNAME                    
048800     END-IF                                                               
048900     MOVE WS-BELEV                   TO VIR2-SUPPNAME                     
049000     MOVE SPACE                      TO VIR2-CARFAM                       
049100     MOVE WS-BEART                   TO VIR2-PARTNAME                     
049200                                                                          
049300     IF NKL-REPTYPE = 'A'                                                 
049400       MOVE WS-BEART                 TO VIR2-PARTNMP1                     
049500     ELSE                                                                 
049600       MOVE SPACE                    TO VIR2-PARTNMP1                     
049700     END-IF                                                               
049800                                                                          
049900     MOVE SPACE                      TO VIR2-PARTNMP2                     
050000                                        VIR2-PARTNMP3                     
050100     MOVE +2000                      TO PRC1-KVLRECL                      
050200     MOVE VIR2-PI30002               TO PRC1-DATA                         
050300     PERFORM S03-PRT-SEND                                                 
050400     .                                                                    
050500     EJECT                                                                
050600 CD-SKAPA-RECORD3 SECTION.                                                
050700                                                                          
050800     MOVE SPACE                      TO VIR3-PI30003                      
050900                                                                          
051000** KEYS                                                                   
051100     MOVE NYCKLAR                    TO VIR3-KEYS                         
051200                                                                          
051300** THE-REST                                                               
051400     MOVE '3'                        TO VIR3-RECTYPE                      
051500     MOVE KR-ADATTENT(1:30)          TO VIR3-ATT                          
051600     MOVE WS-IDMEMO                  TO VIR3-ATTFAX                       
051700     MOVE WS-IDMAIL                  TO VIR3-ATTEMAIL                     
051800     MOVE +2000                      TO PRC1-KVLRECL                      
051900     MOVE VIR3-PI30003               TO PRC1-DATA                         
052000     PERFORM S03-PRT-SEND                                                 
052100     .                                                                    
052200     EJECT                                                                
052300 S02-PRT-OPEN  SECTION.                                                   
052400                                                                          
052500     MOVE 'W426Z1SE'       TO    PRT-IDVCOM                               
052600     MOVE 'W426KVIR'       TO    PRT-IDCPYTXT                             
052700     MOVE 'VIR     '       TO    WS-IDPRTLST                              
101852     MOVE +2000            TO    PRC1-KVLRECL                             
052900     MOVE SPACE            TO    PRC1-TEVCOMST                            
053000     MOVE SPACE            TO    PRC1-IDVCINIT                            
053100     MOVE SPACE            TO    PRC1-DATA                                
053200                                                                          
053300     CALL W006PRC1 USING         PRT-VCOM                                 
053400                                 PRT-OPEN                                 
053500                                 WS-IDPRTLST                              
053600                                 ALT2-PCB                                 
053700                                 PRC1-W006PRVC                            
053800     .                                                                    
053900     EJECT                                                                
054000 S03-PRT-SEND  SECTION.                                                   
054100                                                                          
054200     CALL W006PRC1 USING         PRT-VCOM                                 
054300                                 PRT-WRITE                                
054400                                 WS-IDPRTLST                              
054500                                 ALT2-PCB                                 
054600                                 PRC1-W006PRVC                            
054700     .                                                                    
054800     EJECT                                                                
054900 S04-PRT-CLOSE SECTION.                                                   
055000                                                                          
055100     MOVE SPACE            TO    PRC1-DATA                                
055200     CALL W006PRC1 USING         PRT-VCOM                                 
055300                                 PRT-CLOSE                                
055400                                 WS-IDPRTLST                              
055500                                 ALT2-PCB                                 
055600                                 PRC1-W006PRVC                            
055700     .                                                                    
055800     EJECT                                                                
055900 IMS-GU-KVAE01 SECTION.                                                   
056000     STRING 'W6KVAE01(IDKR     =' W-IDKR-X ')'                            
056100          DELIMITED BY SIZE INTO SSA1                                     
056200     MOVE '  GE' TO GODK-STATUSKODER                                      
056300     CALL CBLTDLI USING GU KVAE-PCB DLI-IO-AREA1 SSA1                     
056400     MOVE KVAE-STATUS-CODE TO STATUS-WS                                   
056500     PERFORM IMS-STATUSKONTROLL                                           
056600     .                                                                    
056700     EJECT                                                                
056800 IMS-GNP-KVAE14 SECTION.                                                  
056900     MOVE 'W6KVAE14 ' TO SSA1                                             
057000     MOVE '  GE' TO GODK-STATUSKODER                                      
057100     CALL CBLTDLI USING GNP KVAE-PCB DLI-IO-AREA2 SSA1                    
057200     MOVE KVAE-STATUS-CODE TO STATUS-WS                                   
057300     PERFORM IMS-STATUSKONTROLL                                           
057400     .                                                                    
057500     EJECT                                                                
057600 IMS-GU-BENA SECTION.                                                     
057700     STRING 'WLBENA01(WDD3BSEQ =' W-IDARTNR-X ')'                         
057800          DELIMITED BY SIZE INTO SSA1                                     
057900     STRING 'WLBENA11(IDSKYLT  =' W-IDSKYLT-X ')'                         
058000          DELIMITED BY SIZE INTO SSA2                                     
058100     MOVE '  GE' TO GODK-STATUSKODER                                      
058200     CALL CBLTDLI USING GU BENA-PCB DLI-IO-AREA3 SSA1 SSA2                
058300     MOVE BENA-STATUS-CODE TO STATUS-WS                                   
058400     PERFORM IMS-STATUSKONTROLL                                           
058500     .                                                                    
058600     EJECT                                                                
058700 IMS-GU-W6LEVA11 SECTION.                                                 
058800     STRING 'W6LEVA01(IDLEVNR  =' W-IDLEVNR-X ')'                         
058900          DELIMITED BY SIZE INTO SSA1                                     
059000     STRING 'W6LEVA11(IDLEVG   =' W-IDLEVG-X ')'                          
059100          DELIMITED BY SIZE INTO SSA2                                     
059200     MOVE '  GE' TO GODK-STATUSKODER                                      
059300     CALL CBLTDLI USING GU W6F1-PCB DLI-IO-AREA4 SSA1 SSA2                
059400     MOVE W6F1-STATUS-CODE TO STATUS-WS                                   
059500     PERFORM IMS-STATUSKONTROLL                                           
059600     .                                                                    
059700     EJECT                                                                
059800 IMS-GU-WDP311 SECTION.                                                   
059900     STRING 'WDP301  (KDARBTYP =' W-KDARBTYP-X ')'                        
060000          DELIMITED BY SIZE INTO SSA1                                     
060100     STRING 'WDP311  (IDPERSON =' W-IDPERSON-X ')'                        
060200          DELIMITED BY SIZE INTO SSA2                                     
060300     MOVE '  GE' TO GODK-STATUSKODER                                      
060400     CALL CBLTDLI USING GU WDP3-PCB DLI-IO-P311 SSA1 SSA2                 
060500     MOVE WDP3-STATUS-CODE TO STATUS-WS                                   
060600     PERFORM IMS-STATUSKONTROLL                                           
060700     .                                                                    
060800     EJECT                                                                
060900 IMS-GU-WLLEVA01 SECTION.                                                 
061000     STRING 'WLLEVA01(IDLEVNR  =' W-IDLEVNR-X ')'                         
061100          DELIMITED BY SIZE INTO SSA1                                     
061200     MOVE '  ' TO GODK-STATUSKODER                                        
061300     CALL CBLTDLI USING GU LEVA-PCB DLI-IO-AREA6 SSA1                     
061400     MOVE LEVA-STATUS-CODE TO STATUS-WS                                   
061500     PERFORM IMS-STATUSKONTROLL                                           
061600     .                                                                    
061700     EJECT                                                                
061800 IMS-GNP-WLLEVA14 SECTION.                                                
061900     MOVE 'WLLEVA14 '       TO SSA1                                       
062000     MOVE '  ' TO GODK-STATUSKODER                                        
062100     CALL CBLTDLI USING GNP LEVA-PCB DLI-IO-AREA6 SSA1                    
062200     MOVE LEVA-STATUS-CODE TO STATUS-WS                                   
062300     PERFORM IMS-STATUSKONTROLL                                           
062400     .                                                                    
062500     EJECT                                                                
062600 IMS-STATUSKONTROLL SECTION.                                              
062700                                                                          
062800     SET STATUS-IX TO 1                                                   
062900     SEARCH GODK-STATUS                                                   
063000       AT END CALL FELLOG                                                 
063100       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
063200     END-SEARCH                                                           
063300     .                                                                    
063400     EJECT                                                                
