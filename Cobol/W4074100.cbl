000100*                                                                         
000200******************************************************************        
000300*     THIS PROGRAM ALSO HAS A WEB-LDC VERSION CALLED WL0156      *        
000400******************************************************************        
000500*                                                                         
000600 ID DIVISION.                                                             
000700                                                                          
000800 PROGRAM-ID.     W4074100.                                                
000900 AUTHOR.         LARS THELL.                                              
001000 DATE-WRITTEN.   95/05/22.                                                
001100 DATE-COMPILED.                                                           
001200                                                                          
001300*    FUNKTION:                                                            
001400*        REGISTRERING AV MOTTAGNA RETURER HOS RETURTERMINAL.              
001500*                                                                         
001600*        PROGRAMMET UPPDATERAR WLRETA (WDA3)                              
001700*        PROGRAMMET UPPDATERAR WLUSEA (WDP7)                              
001800*        PROGRAMMET LÄSER      WLRETG (WDA3)                              
001900*        PROGRAMMET LÄSER      WLKREE (WDA2)                              
002000*        PROGRAMMET LÄSER      WL4103 (WDR1)                              
002100*                                                                         
002200* E'TRACKER:    4230251  2007-05 LDC3 WEB                                 
002300*                                                                         
002400*    INDATA.                                                              
002500*        TRANSAKTION: W4T741                                              
002600*        MID:         W4I74101                                            
002700*                                                                         
002800*    UTDATA.                                                              
002900*        MOD:         W4O74101                                            
003000                                                                          
003100     SKIP3                                                                
003200 ENVIRONMENT DIVISION.                                                    
003300     EJECT                                                                
003400 DATA DIVISION.                                                           
003500 WORKING-STORAGE SECTION.                                                 
003600                                                                          
003700*    -- CHECKED BY WY2000                                                 
003800 77  IDPGM                       PIC X(08)   VALUE 'W4074100'.            
003900                                                                          
004000*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
004100 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
004200                                                                          
004300 77  JA                          PIC X       VALUE 'J'.                   
004400 77  NEJ                         PIC X       VALUE 'N'.                   
004500 77  W-REG                       PIC X(1)    VALUE '1'.                   
004600 77  W-PLUS                      PIC X       VALUE '+'.                   
004700*  INNEHÅLLER X'3F'                                                       
004800 77  W-X3F                       PIC X       VALUE ''.                   
004900                                                                          
005000*    --- INDEX FÖR BLÄDDRINGSRADER                                        
005100 77  INDX                        PIC S9(4)  VALUE +0    COMP SYNC.        
005200 77  INDX2                       PIC S9(4)  VALUE +0    COMP SYNC.        
005300 77  MAX-INDX                    PIC S9(4)  VALUE +14   COMP SYNC.        
005400*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
005500                                                                          
005600 77  INDATA-SW                   PIC X       VALUE 'J'.                   
005700     88  INDATA-OK                           VALUE 'J'.                   
005800     88  INDATA-FEL                          VALUE 'N'.                   
005900                                                                          
006000 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
006100     88  NYCKLAR-OK                          VALUE 'J'.                   
006200     88  NYCKLAR-FEL                         VALUE 'N'.                   
006300                                                                          
006400 77  IDDC-RET-SW                 PIC X       VALUE 'N'.                   
006500     88  IDDC-RET-CDC                        VALUE 'J'.                   
006600                                                                          
006700 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
006800     88  EGEN-MID                            VALUE '4741'.                
006900     88  GODK-MID                            VALUE '4741'.                
007000     88  HELP-MID                            VALUE '0551'.                
007001                                                                          
007100*      --- VALID IDDC CODES                                               
007200*                                                                         
007300*01    -COPY WWDCKONS                                                     
007400       EJECT                                                              
007500 77  W-FLFARLIG                  PIC X(1).                                
007510 77  W-FLBUYBAC                  PIC X(1)    VALUE 'N'.                   
007600 77  W-IDPERSON                  PIC S9(3)   COMP-3.                      
007700 77  W-KDARBTYP                  PIC X(8).                                
007901 77  W-KVRADER                   PIC S9(5)   COMP-3 VALUE ZERO.           
007902                                                                          
008001 01  W-KDANMORS                  PIC X(2).                                
008002     88  KDANMORS-BUYBAC-98                  VALUE '98'.                  
008003                                                                          
008101     EJECT                                                                
008201*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
008301 01  GENERELLA-SUBPROGRAM.                                                
008401     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
008501     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
008601     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
008701     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
008801     03  W418ANSV                PIC X(8)    VALUE 'W418ANSV'.            
008901     EJECT                                                                
009001*    --- PARAMETRAR TILL SUBPROGRAM W418ANSV                              
009101*01 -COPY W418ANSV                                                        
009201     EJECT                                                                
009301*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
009401*01 -COPY WMEDAREA                                                        
009501     EJECT                                                                
009601 01  MESSAGE-CODES.                                                       
009701     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
009801     03  ERR-OTILL-UPD           PIC X(3)    VALUE '007'.                 
009901     03  INF-UPDATE-NOT-DONE     PIC X(3)    VALUE '034'.                 
010001     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
010101     03  INF-FIRST-PAGE          PIC X(3)    VALUE '006'.                 
010201     03  INF-MORE-INFO-EXISTS    PIC X(3)    VALUE '105'.                 
010301     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
010401     03  ERR-RAD-FINNS-REDAN     PIC X(3)    VALUE '245'.                 
010501     03  ERR-FINNS-EJ-PA-REG     PIC X(3)    VALUE '010'.                 
010601     03  ERR-FEL-STATUS          PIC X(3)    VALUE '079'.                 
010701     EJECT                                                                
010801*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
010901*                                                                         
011001 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
011101     SKIP3                                                                
011201*01 -COPY WMSGINIT                                                        
011301     SKIP3                                                                
011401*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
011501*                                                                         
011601 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
011701     SKIP3                                                                
011801*01  MID -COPY W4I74101                                                   
011901     EJECT                                                                
012001 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
012101     SKIP3                                                                
012201*01  -COPY WMSGAREA                                                       
012301     EJECT                                                                
012401     03  MOD REDEFINES MSG-AREA.                                          
012501*      05  -COPY W4O74101  -PRE MOD-                                      
012601     EJECT                                                                
012701 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
012801     SKIP3                                                                
012901*01  -COPY WMFSAREA                                                       
013001     EJECT                                                                
013101*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
013201*                                                                         
013301     EJECT                                                                
013401 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
013501     SKIP3                                                                
013601 01  NYCKLAR-TILL-DLI.                                                    
013701     03  W-WDA301KY-X.                                                    
013801         05  W-IDDC              PIC  X(2)    VALUE SPACE.                
013901         05  W-DAREGDAT          PIC  9(8)    VALUE ZERO.                 
014001         05  W-TIKLOCK           PIC S9(9)    VALUE ZERO  COMP-3.         
014101     03  W-WDA3F1KY-MIN-X.                                                
014201         05  W-IDDC-MIN          PIC  X(2)    VALUE SPACE.                
014301         05  W-IDDISTR-MIN       PIC S9(5)    VALUE ZERO  COMP-3.         
014401         05  W-IDKUNDNR-MIN      PIC S9(7)    VALUE ZERO  COMP-3.         
014501         05  W-IDRAPPNR-MIN      PIC  9(7)    VALUE ZERO.                 
014601         05  W-IDRT-MIN          PIC  X(3)    VALUE SPACE.                
014701         05  W-IDRTLOP-MIN       PIC  9(3)    VALUE ZERO.                 
014801         05  W-IDKOLLI-MIN       PIC S9(5)    VALUE ZERO  COMP-3.         
014901         05  W-DAREGDAT-MIN      PIC  9(8)    VALUE ZERO.                 
015001         05  W-TIKLOCK-MIN       PIC S9(9)    VALUE ZERO  COMP-3.         
015101     03  W-WDA3F1KY-MAX-X.                                                
015201         05  W-IDDC-MAX          PIC  X(2)    VALUE SPACE.                
015301         05  W-IDDISTR-MAX       PIC S9(5)    VALUE ZERO  COMP-3.         
015401         05  W-IDKUNDNR-MAX      PIC S9(7)    VALUE ZERO  COMP-3.         
015501         05  W-IDRAPPNR-MAX      PIC  9(7)    VALUE ZERO.                 
015601         05  W-IDRT-MAX          PIC  X(3)    VALUE SPACE.                
015701         05  W-IDRTLOP-MAX       PIC  9(3)    VALUE ZERO.                 
015801         05  W-IDKOLLI-MAX       PIC S9(5)    VALUE ZERO  COMP-3.         
015901         05  W-DAREGDAT-MAX      PIC  9(8)    VALUE ZERO.                 
016001         05  W-TIKLOCK-MAX       PIC S9(9)    VALUE ZERO  COMP-3.         
016101     03  W-IDLEVANM-X.                                                    
016201         05  W-IDDISTR-ANM       PIC S9(5)    VALUE ZERO  COMP-3.         
016301         05  W-IDKUNDNR-ANM      PIC S9(7)    VALUE ZERO  COMP-3.         
016401         05  W-IDRAPPNR-ANM      PIC  9(7)    VALUE ZERO.                 
016501     SKIP2                                                                
016601*    --- STATUS-KOD FRÅN IMS                                              
016701 01  STATUS-WS                   PIC XX.                                  
016801     88  SEGMENT-FINNS                       VALUE '  '.                  
016901     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
017001     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
017101     SKIP2                                                                
017201 01  GODK-STATUSKODER.                                                    
017301     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
017401     SKIP3                                                                
017501 01  SSA1                        PIC X(128).                              
017601 01  SSA2                        PIC X(192).                              
017701     EJECT                                                                
017801*    --- IMS FUNKTIONSKODER                                               
017901*01  -COPY W0003                                                          
018001     EJECT                                                                
018101*    ---  DLI INPUT-OUTPUT AREA                                           
018201 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
018301     SKIP3                                                                
018401 01  DLI-IO-AREA.                                                         
018501     03  IO-AREA                 PIC X(300)  VALUE SPACE.                 
018601     SKIP3                                                                
018701     03  WLRETA01 REDEFINES IO-AREA.                                      
018801*        05  -COPY WDA301                                                 
018901     EJECT                                                                
019001 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA2'.        
019101     SKIP3                                                                
019201 01  DLI-IO-AREA2.                                                        
019301     03  IO-AREA2                PIC X(300)  VALUE SPACE.                 
019401     SKIP3                                                                
019501     03  WLRETG01 REDEFINES IO-AREA2.                                     
019601*        05  -COPY WDA3F1                                                 
019701     EJECT                                                                
019801     03  WLKREE01 REDEFINES IO-AREA2.                                     
019901*        05  -COPY WDA201                                                 
020001     EJECT                                                                
020101     03  WLKREE11 REDEFINES IO-AREA2.                                     
020201*        05  -COPY WDA211                                                 
020301     EJECT                                                                
020401 LINKAGE SECTION.                                                         
020501                                                                          
020601*01  -COPY W0009   -PRE MSG-                                              
020701*01  -COPY W0008   -PRE USEA-                                             
020801     05  FILLER                  PIC X.                                   
020901     EJECT                                                                
021001*01  -COPY W0008  -PRE RETA-                                              
021101     05  FILLER                  PIC X.                                   
021201     EJECT                                                                
021301*01  -COPY W0008  -PRE RETG-                                              
021401     05  FILLER                  PIC X.                                   
021501     EJECT                                                                
021601*01  -COPY W0008  -PRE KREE-                                              
021701     05  FILLER                  PIC X.                                   
021801     EJECT                                                                
021901*01  -COPY W0008  -PRE ANSV-4113-                                         
022001     05  FILLER                  PIC X.                                   
022101     EJECT                                                                
022201*01  -COPY W0008  -PRE ANSV-4115-                                         
022301     05  FILLER                  PIC X.                                   
022401     EJECT                                                                
022501*01  -COPY W0008  -PRE ANSV-4117-                                         
022601     05  FILLER                  PIC X.                                   
022701     EJECT                                                                
022801 PROCEDURE DIVISION  USING MSG-PCB                                        
022901                           USEA-PCB RETA-PCB RETG-PCB                     
023001                           KREE-PCB                                       
023101                           ANSV-4113-PCB                                  
023201                           ANSV-4115-PCB                                  
023301                           ANSV-4117-PCB.                                 
023401                                                                          
023501     ENTRY 'DLITCBL' USING MSG-PCB                                        
023601                           USEA-PCB RETA-PCB RETG-PCB                     
023701                           KREE-PCB                                       
023801                           ANSV-4113-PCB                                  
023901                           ANSV-4115-PCB                                  
024001                           ANSV-4117-PCB.                                 
024101                                                                          
024201     PERFORM IMS-GET-MSG                                                  
024301     IF SEGMENT-FINNS                                                     
024401       PERFORM A-INIT                                                     
024501       PERFORM B-KOLLA-NYCKLAR                                            
024601       IF NYCKLAR-OK                                                      
024701         MOVE MID-MODFAELT-IN          TO MOD-INPUT                       
024801         INSPECT MOD-INPUT REPLACING ALL W-PLUS BY W-X3F                  
024901         IF EGEN-MID OR HELP-MID                                          
025001           PERFORM G-KOLLA-INPUT                                          
025101           IF INDATA-OK AND EGEN-MID                                      
025201             PERFORM H-UPPDATERA                                          
025301           END-IF                                                         
025401         ELSE                                                             
025501           PERFORM MFS-RENSA-FAELT-IN                                     
025601         END-IF                                                           
025701       ELSE                                                               
025801         PERFORM MFS-RENSA-FAELT-IN                                       
025901       END-IF                                                             
026001       COMPUTE MSG-KVLL = LENGTH OF MOD-W4O74101 + 4                      
026101       PERFORM IMS-INSERT-MSG                                             
026201     END-IF                                                               
026301                                                                          
026401     MOVE ZERO TO RETURN-CODE                                             
026501     GOBACK                                                               
026601     .                                                                    
026701     EJECT                                                                
026801 A-INIT SECTION.                                                          
026901                                                                          
027001     IF MSG-DUBBLA-TRANSKODER                                             
027101       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W4I74101                 
027201       MOVE MSG-IDTRANS-2  TO MFS-IDTRANS                                 
027301       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
027401     ELSE                                                                 
027501       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W4I74101                  
027601       MOVE MSG-IDTRANS-1  TO MFS-IDTRANS                                 
027701       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
027801     END-IF                                                               
027901                                                                          
028001     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
028101     MOVE MSG-IDPFK   TO MFS-IDPFK                                        
028201     MOVE MFS-IDTRANS TO W-IDTRANS                                        
028301                                                                          
028401     MOVE LOW-VALUE       TO MSG-AREA                                     
028501     MOVE 'W4O74101'      TO MFS-IDMOD                                    
028601     MOVE '4741'          TO MOD-IDTRANS                                  
028701     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                    
028801                                                                          
028901     MOVE SPACE                       TO MED-IDMFSINF                     
029001     MOVE SPACE                       TO MED-IDMFSFEL                     
029101                                                                          
029201     MOVE JA  TO IDDC-RET-SW                                              
029301                                                                          
029401     MOVE LOW-VALUE     TO W-WDA3F1KY-MIN-X                               
029501     MOVE HIGH-VALUE    TO W-WDA3F1KY-MAX-X                               
029601     .                                                                    
029701     EJECT                                                                
029801 B-KOLLA-NYCKLAR SECTION.                                                 
029901                                                                          
030001     MOVE ALL '+'           TO MSGI-WMSGINIT                              
030101     MOVE '001'             TO MSGI-KDCALL                                
030201     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
030301     MOVE '4741'               TO MSGI-IDTRANS                            
030401     MOVE MSG-LTERM-NAME       TO MSGI-IDLTERM-USER                       
030501     IF EGEN-MID                                                          
030601        MOVE MID-IDDISTR-IN TO MSGI-IDDISTR                               
030701     END-IF                                                               
030801     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
030901                                                                          
031001     MOVE MSGI-IDLAND-SPR   TO MED-IDSKYLT                                
031101     MOVE WC-CDC-SE         TO W-IDDC                                     
031201                               W-IDDC-MIN                                 
031301                               W-IDDC-MAX                                 
031401                                                                          
031501     MOVE JA TO NYCKLAR-SW                                                
031601                                                                          
031701*    -- KONTROLL AV IDDISTR                                               
031801     MOVE MFS-RENSA-FAELT   TO MOD-IDDISTR-IN                             
031901     IF MID-IDDISTR-IN      NOT = ALL '+'                                 
032001        MOVE '7'            TO MFS-IDPFK                                  
032101        MOVE SPACE          TO MFS-KDTRTYP                                
032201     END-IF                                                               
032301                                                                          
032401     IF MSGI-IDDISTR        NUMERIC AND                                   
032501        MSGI-IDDISTR        > ZERO                                        
032601        MOVE MSGI-IDDISTR   TO W-IDDISTR-ANM                              
032701                               W-IDDISTR-MIN                              
032801                               W-IDDISTR-MAX                              
032901                               MOD-IDDISTR-UT                             
033001     ELSE                                                                 
033101        MOVE NEJ            TO NYCKLAR-SW                                 
033201     END-IF                                                               
033301                                                                          
033401*    -- KONTROLL AV IDRT-KEY                                              
033501     IF MSGI-IDRT-KEY = SPACE OR                                          
033601        MSGI-IDRT-KEY = 'CDC' OR                                          
033701        MSGI-IDRT-KEY = 'US1' OR                                          
033901        MSGI-IDRT-KEY = 'US3' OR                                          
033902        MSGI-IDRT-KEY = 'US4' OR                                          
033903        MSGI-IDRT-KEY = 'US5' OR                                          
033904        MSGI-IDRT-KEY = 'US6' OR                                          
033905        MSGI-IDRT-KEY = 'ET2' OR                                          
034001        MSGI-IDRT-KEY = 'CA1'                                             
034101        MOVE NEJ            TO NYCKLAR-SW                                 
034201     END-IF                                                               
034301                                                                          
034401     IF NYCKLAR-FEL                                                       
034501       IF MSGI-IDRT-KEY = SPACE OR                                        
034601          MSGI-IDRT-KEY = 'CDC' OR                                        
034701          MSGI-IDRT-KEY = 'US1' OR                                        
034901          MSGI-IDRT-KEY = 'US3' OR                                        
034902          MSGI-IDRT-KEY = 'US4' OR                                        
034903          MSGI-IDRT-KEY = 'US5' OR                                        
034904          MSGI-IDRT-KEY = 'US6' OR                                        
034905          MSGI-IDRT-KEY = 'ET2' OR                                        
035001          MSGI-IDRT-KEY = 'CA1'                                           
035101         MOVE MFS-RENSA-FAELT TO MOD-IDDISTR-UT                           
035201         MOVE ERR-OTILL-UPD   TO MED-IDMFSFEL                             
035301       ELSE                                                               
035401         MOVE ERR-WRONG-KEY   TO MED-IDMFSFEL                             
035501       END-IF                                                             
035601       CALL WMEDKONV USING MED-WMEDAREA                                   
035701       MOVE MED-MFSFEL      TO MOD-TEMFSFEL                               
035801       PERFORM MFS-RENSA-FAELT-IN                                         
035901     END-IF                                                               
036001     .                                                                    
036101     EJECT                                                                
036201 G-KOLLA-INPUT SECTION.                                                   
036301                                                                          
036401     MOVE JA                     TO INDATA-SW                             
036501     IF MID-INPUT = ALL '+'                                               
036601       MOVE INF-UPDATE-NOT-DONE  TO MED-IDMFSFEL                          
036701       CALL WMEDKONV USING MED-WMEDAREA                                   
036801       MOVE MED-MFSFEL           TO MOD-TEMFSFEL                          
036901       PERFORM MFS-ROER-EJ-FAELT-IN                                       
037001       MOVE NEJ                  TO INDATA-SW                             
037101     ELSE                                                                 
037201                                                                          
037301       PERFORM GA-FORMELL-KONTROLL                                        
037401       IF INDATA-OK                                                       
037501          PERFORM GB-LOGISK-KONTROLL                                      
037601       END-IF                                                             
037701                                                                          
037801       IF INDATA-FEL                                                      
037901         IF MED-IDMFSFEL = SPACE                                          
038001           MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                      
038101         END-IF                                                           
038201         CALL WMEDKONV USING MED-WMEDAREA                                 
038301         MOVE MED-MFSFEL           TO MOD-TEMFSFEL                        
038401         PERFORM MFS-ROER-EJ-FAELT-IN                                     
038501       END-IF                                                             
038601     END-IF                                                               
038701     .                                                                    
038801     EJECT                                                                
038901                                                                          
039001 GA-FORMELL-KONTROLL   SECTION.                                           
039101                                                                          
039201     MOVE +1                        TO INDX                               
039301     PERFORM UNTIL INDX             > MAX-INDX                            
039401                                                                          
039501       IF MID-INPUT-RAD(INDX)       NOT = ALL '+'                         
039601                                                                          
039701          PERFORM GAA-KOLLA-IDKUNDNR                                      
039801          PERFORM GAB-KOLLA-IDRAPPNR                                      
039901          PERFORM GAC-KOLLA-KVKOLLI                                       
040001          PERFORM GAD-KOLLA-RT-PAA-SIDA                                   
040101                                                                          
040201          IF MID-IDFRASED(INDX)       NOT = ALL '+'                       
040301            MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDFRASED-ATTR(INDX)          
040401          END-IF                                                          
040501                                                                          
040601          IF MID-TERETNOT(INDX)       NOT = ALL '+'                       
040701            MOVE MFS-ALFA-FAELT-RAETT TO MOD-TERETNOT-ATTR(INDX)          
040801          END-IF                                                          
040901       ELSE                                                               
041001          MOVE MFS-NUM-FAELT-RAETT   TO MOD-IDKUNDNR-ATTR(INDX)           
041101          MOVE MFS-NUM-FAELT-RAETT   TO MOD-IDRAPPNR-ATTR(INDX)           
041201          MOVE MFS-NUM-FAELT-RAETT   TO MOD-KVKOLLI-AF-ATTR(INDX)         
041301          MOVE MFS-ALFA-FAELT-RAETT  TO MOD-IDFRASED-ATTR(INDX)           
041401          MOVE MFS-ALFA-FAELT-RAETT  TO MOD-TERETNOT-ATTR(INDX)           
041501       END-IF                                                             
041601                                                                          
041701       ADD +1                        TO INDX                              
041801     END-PERFORM                                                          
041901     .                                                                    
042001     EJECT                                                                
042101                                                                          
042201 GAA-KOLLA-IDKUNDNR     SECTION.                                          
042301                                                                          
042401     IF MID-IDKUNDNR(INDX)          NOT = ALL '+'                         
042501        IF MID-IDKUNDNR(INDX)       NOT NUMERIC                           
042601           MOVE MFS-NUM-FAELT-FEL   TO MOD-IDKUNDNR-ATTR(INDX)            
042701           MOVE NEJ                 TO INDATA-SW                          
042801        ELSE                                                              
042901           MOVE MFS-NUM-FAELT-RAETT TO MOD-IDKUNDNR-ATTR(INDX)            
043001        END-IF                                                            
043101     ELSE                                                                 
043201        MOVE MFS-NUM-FAELT-FEL      TO MOD-IDKUNDNR-ATTR(INDX)            
043301        MOVE NEJ                    TO INDATA-SW                          
043401     END-IF                                                               
043501     .                                                                    
043601     EJECT                                                                
043701 GAB-KOLLA-IDRAPPNR     SECTION.                                          
043801                                                                          
043901     IF MID-IDRAPPNR(INDX)          NOT = ALL '+'                         
044001        IF MID-IDRAPPNR(INDX)       NOT NUMERIC                           
044101           MOVE MFS-NUM-FAELT-FEL   TO MOD-IDRAPPNR-ATTR(INDX)            
044201           MOVE NEJ                 TO INDATA-SW                          
044301        ELSE                                                              
044401           MOVE MFS-NUM-FAELT-RAETT TO MOD-IDRAPPNR-ATTR(INDX)            
044501        END-IF                                                            
044601     ELSE                                                                 
044701        MOVE MFS-NUM-FAELT-FEL      TO MOD-IDRAPPNR-ATTR(INDX)            
044801        MOVE NEJ                    TO INDATA-SW                          
044901     END-IF                                                               
045001                                                                          
045101     .                                                                    
045201     EJECT                                                                
045301 GAC-KOLLA-KVKOLLI      SECTION.                                          
045401                                                                          
045501     IF MID-KVKOLLI(INDX)           NOT = ALL '+'                         
045601        IF MID-KVKOLLI(INDX)        NOT NUMERIC                           
045701           MOVE MFS-NUM-FAELT-FEL   TO MOD-KVKOLLI-AF-ATTR(INDX)          
045801           MOVE NEJ                 TO INDATA-SW                          
045901        ELSE                                                              
046001           MOVE MFS-NUM-FAELT-RAETT TO MOD-KVKOLLI-AF-ATTR(INDX)          
046101        END-IF                                                            
046201     ELSE                                                                 
046301        MOVE MFS-NUM-FAELT-FEL      TO MOD-KVKOLLI-AF-ATTR(INDX)          
046401        MOVE NEJ                    TO INDATA-SW                          
046501     END-IF                                                               
046601                                                                          
046701     .                                                                    
046801     EJECT                                                                
046901 GAD-KOLLA-RT-PAA-SIDA    SECTION.                                        
047001                                                                          
047101     COMPUTE INDX2                 =  INDX + 1                            
047201     PERFORM UNTIL INDX2           >  MAX-INDX OR                         
047301        MID-IDRAPPNR(INDX)         =  MID-IDRAPPNR(INDX2)                 
047401         ADD +1                    TO INDX2                               
047501     END-PERFORM                                                          
047601                                                                          
047701     IF INDX2                      >  MAX-INDX                            
047801         CONTINUE                                                         
047901     ELSE                                                                 
048001         MOVE MFS-NUM-FAELT-FEL    TO MOD-IDRAPPNR-ATTR(INDX)             
048101         MOVE NEJ                  TO INDATA-SW                           
048201     END-IF                                                               
048301     .                                                                    
048401     EJECT                                                                
048501 GB-LOGISK-KONTROLL   SECTION.                                            
048601                                                                          
048701     MOVE +1                         TO INDX                              
048801     PERFORM UNTIL INDX              > MAX-INDX                           
048901                                                                          
049001        IF MID-IDKUNDNR(INDX)        NOT = ALL '+' AND                    
049101           MID-IDRAPPNR(INDX)        NOT = ALL '+'                        
049201            MOVE MID-IDKUNDNR(INDX)  TO W-IDKUNDNR-ANM                    
049301                                           W-IDKUNDNR-MIN                 
049401                                           W-IDKUNDNR-MAX                 
049501            MOVE MID-IDRAPPNR(INDX)  TO W-IDRAPPNR-ANM                    
049601                                           W-IDRAPPNR-MIN                 
049701                                           W-IDRAPPNR-MAX                 
049801                                                                          
049901            PERFORM IMS-GU-WLKREE01                                       
050001            IF SEGMENT-SAKNAS OR                                          
050101               (SEGMENT-FINNS AND ANM-KDLEVANM NOT = '4') OR              
050201               (SEGMENT-FINNS AND ANM-IDFTG    NOT = '57')                
050301                                                                          
050401               IF MED-IDMFSFEL = SPACE                                    
050501                 IF SEGMENT-SAKNAS                                        
050601                   MOVE ERR-FINNS-EJ-PA-REG  TO MED-IDMFSFEL              
050701                 ELSE                                                     
050801                   IF SEGMENT-FINNS AND ANM-KDLEVANM NOT = '4'            
050901                     MOVE ERR-FEL-STATUS     TO MED-IDMFSFEL              
051001                   ELSE                                                   
051101                     MOVE ERR-OTILL-UPD      TO MED-IDMFSFEL              
051201                   END-IF                                                 
051301                 END-IF                                                   
051401               END-IF                                                     
051501                                                                          
051601               MOVE MFS-NUM-FAELT-FEL TO MOD-IDKUNDNR-ATTR(INDX)          
051701                                         MOD-IDRAPPNR-ATTR(INDX)          
051801               MOVE NEJ               TO INDATA-SW                        
051901            ELSE                                                          
052001               PERFORM IMS-GNP-WLKREE11                                   
052101               PERFORM UNTIL SEGMENT-SAKNAS OR IDDC-RET-SW = NEJ          
052201                 IF LEV-IDDC-RET NOT = WC-CDC-SE                          
052301                   MOVE NEJ TO IDDC-RET-SW                                
052401                   IF MED-IDMFSFEL = SPACE                                
052501                     MOVE ERR-OTILL-UPD   TO MED-IDMFSFEL                 
052601                   END-IF                                                 
052701                   MOVE MFS-NUM-FAELT-FEL TO                              
052801                                         MOD-IDKUNDNR-ATTR(INDX)          
052901                                         MOD-IDRAPPNR-ATTR(INDX)          
053001                   MOVE NEJ               TO INDATA-SW                    
053101                 ELSE                                                     
053201                   PERFORM IMS-GNP-WLKREE11                               
053301                 END-IF                                                   
053401               END-PERFORM                                                
053501                                                                          
053601               PERFORM IMS-GU-WLRETG01                                    
053701               IF SEGMENT-FINNS                                           
053801                 IF MED-IDMFSFEL = SPACE                                  
053901                   MOVE ERR-RAD-FINNS-REDAN TO MED-IDMFSFEL               
054001                 END-IF                                                   
054101                 MOVE MFS-NUM-FAELT-FEL TO MOD-IDKUNDNR-ATTR(INDX)        
054201                                           MOD-IDRAPPNR-ATTR(INDX)        
054301                 MOVE NEJ               TO INDATA-SW                      
054401               END-IF                                                     
054501            END-IF                                                        
054601        END-IF                                                            
054701        ADD +1             TO INDX                                        
054801     END-PERFORM                                                          
054901     .                                                                    
055001     EJECT                                                                
055101                                                                          
055201 H-UPPDATERA SECTION.                                                     
055301                                                                          
055401     MOVE +1                   TO INDX                                    
055501     PERFORM UNTIL INDX        > MAX-INDX                                 
055601                                                                          
055701       IF MID-IDKUNDNR(INDX)   NOT = ALL '+'                              
055801          PERFORM HA-SKAPA-WLRETA01                                       
055901       END-IF                                                             
056001       ADD +1                  TO INDX                                    
056101                                                                          
056201     END-PERFORM                                                          
056301                                                                          
056401     MOVE INF-UPDATE-DONE      TO MED-IDMFSINF                            
056501     CALL WMEDKONV USING MED-WMEDAREA                                     
056601     MOVE MED-MFSINF           TO MOD-TEMFSINF                            
056701     PERFORM MFS-FORM-ATTR                                                
056801     PERFORM MFS-RENSA-FAELT-IN                                           
056901     .                                                                    
057001     EJECT                                                                
057101                                                                          
057201 HA-SKAPA-WLRETA01 SECTION.                                               
057301                                                                          
057401     PERFORM HAA-HAEMTA-LEV-ANM-UPPG                                      
057501     PERFORM HAB-HAEMTA-ANSVARIG                                          
057601                                                                          
057701     MOVE WC-CDC-SE            TO RET-IDDC                                
057801     MOVE FUNCTION CURRENT-DATE (1:8) TO RET-DAREGDAT                     
057901     ACCEPT RET-TIKLOCK        FROM TIME                                  
058001                                                                          
058101     MOVE MSGI-IDDISTR         TO RET-IDDISTR                             
058201     MOVE MID-IDKUNDNR(INDX)   TO RET-IDKUNDNR                            
058301     MOVE MID-IDRAPPNR(INDX)   TO RET-IDRAPPNR                            
058401     MOVE SPACE                TO RET-ADINLOMR                            
058501                                  RET-ADINLOMR-LOSS                       
058601                                  RET-ADINLOMR-MOT                        
058701     MOVE W-FLFARLIG           TO RET-FLFARLIG                            
058801     MOVE ZERO                 TO RET-IDANSTNR-LOSS                       
058901                                  RET-IDANSTNR-MOT                        
059001     IF MID-IDFRASED(INDX) = ALL '+'                                      
059101       MOVE SPACE                TO RET-IDFRASED-AAF                      
059201     ELSE                                                                 
059301       MOVE MID-IDFRASED(INDX)   TO RET-IDFRASED-AAF                      
059401     END-IF                                                               
059501     MOVE SPACE                TO RET-IDFRASED-CDC                        
059601     MOVE ZERO                 TO RET-IDKOLLI                             
059701     MOVE W-IDPERSON           TO RET-IDPERSON                            
059801     MOVE MSGI-IDRT-KEY        TO RET-IDRT                                
059901     MOVE ZERO                 TO RET-IDRTLOP                             
060001     MOVE W-KDARBTYP           TO RET-KDARBTYP                            
060101     MOVE ZERO                 TO RET-KDKOLSTA                            
060201     MOVE W-REG                TO RET-KDRETSTA                            
060301     MOVE MID-KVKOLLI(INDX)    TO RET-KVKOLLI-AAF                         
060401     MOVE ZERO                 TO RET-KVKOLLI-LOSS                        
060501                                  RET-KVKOLLI-MOT                         
060601     MOVE W-KVRADER            TO RET-KVRADER                             
060701     IF MID-TERETNOT(INDX) = ALL '+'                                      
060801       MOVE SPACE                TO RET-TERETNOT                          
060901     ELSE                                                                 
061001       MOVE MID-TERETNOT(INDX)   TO RET-TERETNOT                          
061101     END-IF                                                               
061201     MOVE ZERO                 TO RET-TIINLMOT                            
061301                                  RET-TIKLAR                              
061401                                  RET-TILOSSN                             
061501                                  RET-DASNDDAT                            
061601                                  RET-DARETANK                            
061701                                  RET-TIREGDAT-TRRT                       
061801                                  RET-TISNDDAT-TRRT                       
061901     MOVE SPACE                TO RET-IDRT-TRANSIT                        
061902     MOVE W-FLBUYBAC           TO RET-FLBUYBAC                            
062001                                                                          
062101     PERFORM IMS-ISRT-WLRETA01                                            
062201     PERFORM UNTIL SEGMENT-FINNS                                          
062301       MOVE FUNCTION CURRENT-DATE (1:8) TO RET-DAREGDAT                   
062401       ACCEPT RET-TIKLOCK        FROM TIME                                
062501                                                                          
062601       PERFORM IMS-ISRT-WLRETA01                                          
062701     END-PERFORM                                                          
062801     .                                                                    
062901     EJECT                                                                
063001                                                                          
063101 HAA-HAEMTA-LEV-ANM-UPPG SECTION.                                         
063201                                                                          
063301     MOVE MID-IDKUNDNR(INDX)   TO W-IDKUNDNR-ANM                          
063401     MOVE MID-IDRAPPNR(INDX)   TO W-IDRAPPNR-ANM                          
063502     MOVE ZERO                 TO W-KDANMORS                              
063503     MOVE NEJ                  TO W-FLBUYBAC                              
063601                                                                          
063701     PERFORM IMS-GU-WLKREE01                                              
063801     IF SEGMENT-FINNS                                                     
063901        MOVE ANM-FLFARLIG      TO W-FLFARLIG                              
064001        MOVE ANM-KVRADER-RT    TO W-KVRADER                               
064102                                                                          
064201        PERFORM IMS-GNP-WLKREE11                                          
064301        IF SEGMENT-FINNS                                                  
064401          MOVE LEV-KDANMORS    TO W-KDANMORS                              
064402          IF KDANMORS-BUYBAC-98                                           
064403            MOVE JA            TO W-FLBUYBAC                              
064404          END-IF                                                          
064701        END-IF                                                            
064801     END-IF                                                               
064901     .                                                                    
065001     EJECT                                                                
065101                                                                          
065201 HAB-HAEMTA-ANSVARIG SECTION.                                             
065301                                                                          
065401     MOVE 3                    TO ANSV-KDCALL                             
065501     MOVE MSGI-IDDISTR         TO ANSV-IDDISTR                            
065601     MOVE MSGI-IDFTG           TO ANSV-IDFTG                              
065701     MOVE MID-IDKUNDNR(INDX)   TO ANSV-IDKUNDNR                           
065801     MOVE W-KDANMORS           TO ANSV-KDANMORS                           
065901     MOVE WC-CDC-SE            TO ANSV-IDDC                               
066001     MOVE ZERO                 TO ANSV-KDORDKL                            
066101                                  ANSV-ADLAGOMR                           
066201                                                                          
066301     CALL W418ANSV USING ANSV-W418ANSV ANSV-4113-PCB                      
066401                                       ANSV-4115-PCB                      
066501                                       ANSV-4117-PCB                      
066601                                                                          
066701     IF ANSV-OK                                                           
066801        MOVE ANSV-KDARBTYP     TO W-KDARBTYP                              
066901        MOVE ANSV-IDPERSON     TO W-IDPERSON                              
067001     ELSE                                                                 
067101        MOVE 'RET'             TO W-KDARBTYP                              
067201        MOVE 99                TO W-IDPERSON                              
067301     END-IF                                                               
067401     .                                                                    
067501     EJECT                                                                
067601                                                                          
067701 MFS-RENSA-FAELT-IN SECTION.                                              
067801                                                                          
067901     MOVE +1              TO INDX                                         
068001     PERFORM UNTIL INDX   > MAX-INDX                                      
068101       PERFORM MFS-RENSA-RAD-FAELT-IN                                     
068201       ADD +1             TO INDX                                         
068301     END-PERFORM                                                          
068401     .                                                                    
068501                                                                          
068601 MFS-RENSA-RAD-FAELT-IN  SECTION.                                         
068701                                                                          
068801     MOVE MFS-RENSA-FAELT TO MOD-IDKUNDNR   (INDX)                        
068901                             MOD-IDRAPPNR   (INDX)                        
069001                             MOD-KVKOLLI-AF (INDX)                        
069101                             MOD-IDFRASED   (INDX)                        
069201                             MOD-TERETNOT   (INDX)                        
069301                                                                          
069401     .                                                                    
069501     EJECT                                                                
069601 MFS-ROER-EJ-FAELT-IN  SECTION.                                           
069701                                                                          
069801     MOVE +1              TO INDX                                         
069901     PERFORM UNTIL INDX   > MAX-INDX                                      
070001       PERFORM MFS-ROER-EJ-RAD-FAELT-IN                                   
070101       ADD +1             TO INDX                                         
070201     END-PERFORM                                                          
070301     .                                                                    
070401                                                                          
070501 MFS-ROER-EJ-RAD-FAELT-IN  SECTION.                                       
070601                                                                          
070701     MOVE MFS-ROER-EJ-FAELT TO MOD-IDKUNDNR   (INDX)                      
070801                               MOD-IDRAPPNR   (INDX)                      
070901                               MOD-KVKOLLI-AF (INDX)                      
071001                               MOD-IDFRASED   (INDX)                      
071101                               MOD-TERETNOT   (INDX)                      
071201     .                                                                    
071301     SKIP3                                                                
071401 MFS-FORM-ATTR SECTION.                                                   
071501                                                                          
071601     MOVE +1              TO INDX                                         
071701     PERFORM UNTIL INDX   > MAX-INDX                                      
071801       PERFORM MFS-FORM-ATTR-RAD-FAELT-IN                                 
071901       ADD +1             TO INDX                                         
072001     END-PERFORM                                                          
072101     .                                                                    
072201                                                                          
072301 MFS-FORM-ATTR-RAD-FAELT-IN  SECTION.                                     
072401                                                                          
072501     MOVE MFS-FORMATETS-ATTR TO MOD-IDKUNDNR-ATTR   (INDX)                
072601                                MOD-IDRAPPNR-ATTR   (INDX)                
072701                                MOD-KVKOLLI-AF-ATTR (INDX)                
072801                                MOD-IDFRASED-ATTR   (INDX)                
072901                                MOD-TERETNOT-ATTR   (INDX)                
073001     .                                                                    
073101     EJECT                                                                
073201* --- IMS SEKTIONER ---                                                   
073301     SKIP3                                                                
073401 IMS-GET-MSG SECTION.                                                     
073501                                                                          
073601     MOVE '  QC' TO GODK-STATUSKODER                                      
073701     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
073801     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
073901     PERFORM IMS-STATUSKONTROLL                                           
074001     .                                                                    
074101     SKIP3                                                                
074201 IMS-INSERT-MSG SECTION.                                                  
074301                                                                          
074401     IF ENGLISH-TEXT                                                      
074501       MOVE 'N' TO MFS-KDHUVOMR                                           
074601     END-IF                                                               
074701     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
074801     MOVE SPACE TO GODK-STATUSKODER                                       
074901     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
075001     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
075101     PERFORM IMS-STATUSKONTROLL                                           
075201     .                                                                    
075301     EJECT                                                                
075401 IMS-GU-WLRETG01 SECTION.                                                 
075501                                                                          
075601     STRING 'WLRETG01(WDA3F1KY>=' W-WDA3F1KY-MIN-X                        
075701                    '&WDA3F1KY<=' W-WDA3F1KY-MAX-X ')'                    
075801          DELIMITED BY SIZE INTO SSA1                                     
075901     MOVE '  GE' TO GODK-STATUSKODER                                      
076001     CALL CBLTDLI USING GU RETG-PCB DLI-IO-AREA2 SSA1                     
076101     MOVE RETG-STATUS-CODE TO STATUS-WS                                   
076201     PERFORM IMS-STATUSKONTROLL                                           
076301     .                                                                    
076401     SKIP3                                                                
076501 IMS-ISRT-WLRETA01    SECTION.                                            
076601                                                                          
076701     MOVE 'WLRETA01 ' TO SSA1                                             
076801     MOVE '  II' TO GODK-STATUSKODER                                      
076901     CALL CBLTDLI USING ISRT RETA-PCB DLI-IO-AREA SSA1                    
077001     MOVE RETA-STATUS-CODE TO STATUS-WS                                   
077101     PERFORM IMS-STATUSKONTROLL                                           
077201     .                                                                    
077301     EJECT                                                                
077401 IMS-GU-WLKREE01    SECTION.                                              
077501                                                                          
077601     STRING 'WLKREE01(IDLEVANM =' W-IDLEVANM-X ')'                        
077701          DELIMITED BY SIZE INTO SSA1                                     
077801     MOVE '  GE' TO GODK-STATUSKODER                                      
077901     CALL CBLTDLI USING GU KREE-PCB DLI-IO-AREA2 SSA1                     
078001     MOVE KREE-STATUS-CODE TO STATUS-WS                                   
078101     PERFORM IMS-STATUSKONTROLL                                           
078201     .                                                                    
078301     EJECT                                                                
078401 IMS-GNP-WLKREE11 SECTION.                                                
078501                                                                          
078601     MOVE 'WLKREE11 ' TO SSA1                                             
078701     MOVE '  GE' TO GODK-STATUSKODER                                      
078801     CALL CBLTDLI USING GNP KREE-PCB DLI-IO-AREA2 SSA1                    
078901     MOVE KREE-STATUS-CODE TO STATUS-WS                                   
079001     PERFORM IMS-STATUSKONTROLL                                           
079101     .                                                                    
079201     EJECT                                                                
079301 IMS-STATUSKONTROLL SECTION.                                              
079401                                                                          
079501     SET STATUS-IX TO 1                                                   
079601     SEARCH GODK-STATUS                                                   
079701       AT END                                                             
079801         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
079901         DELIMITED BY SIZE INTO FELTEXT                                   
080001         CALL FELLOG                                                      
080101       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
080201         CONTINUE                                                         
081001     END-SEARCH                                                           
090001     .                                                                    
