000100 ID DIVISION.                                                             
000201 PROGRAM-ID.                W4010800.                                     
000301 AUTHOR.                    E RINGQVIST.                                  
000401     DATE-WRITTEN.              JULI -84.                                 
000501     REMARKS.                                                             
000601*                                                                         
000701*    FUNKTION.   TP-PROGRAM. FRÅGE-PROGRAM SOM ANGER                      
000801*                'BUFFERT-SALDO-INFO'.                                    
000901*                NYCKELN BESTÄMMER VILKET LAGER SOM GÄLLER.               
001001*                BASER ÄR  (WDD3) - BENÄMNINGSBAS                         
001101*                          (WDK6) - ARTIKELBAS CDC                        
001201*                          (WDK7) - ARTIKELBAS SDC                        
001301*                          (WDD8) - SALDOBAS.                             
001401*                                                                         
001501*        SE W9011500 SOM EXEMPEL PÅ INTERNSORTERING + BLÄDDRING           
001601*                                                                         
001701*                                                                         
001801*    TRANSAKTION:                                                         
001901*                     W4T108                                              
002001*    INDATA.                                                              
002101*        MID:         W4I10801                                            
002201*    UTDATA.                                                              
002301*        MOD:         W4O10801                                            
002401*    SUBPROGRAM.                                                          
002501*        FELLOG                                                           
002601*                                                                         
002701 ENVIRONMENT DIVISION.                                                    
002801                                                                          
002901 DATA DIVISION.                                                           
003001     EJECT                                                                
003101 WORKING-STORAGE SECTION.                                                 
003201*    -- CHECKED BY WY2000                                                 
003301     SKIP3                                                                
003401 77  PROGRAM-NAMN            PIC X(8)    VALUE 'W4010800'.                
003501 77  JA                      PIC X       VALUE 'J'.                       
003601 77  NEJ                     PIC X       VALUE 'N'.                       
003701 77  IX                      PIC S9(3) COMP-3 VALUE 1.                    
003801 77  SPRAK-IX                PIC S9(3)  VALUE +0    COMP SYNC.            
003901 77  IDDC-WS                 PIC X(2).                                    
004001 77  WS-KVROS-SDC            PIC S9(7) COMP-3 VALUE ZERO.                 
004101 77  MAX-MOD-LENGD           PIC S9(4)   VALUE +539  COMP SYNC.           
004102 77  BATTERY-SW              PIC 9(2).                                    
004103     88  BATTERY-LOC                     VALUE 46.                        
004201 77  WS-IDTRANS              PIC X(4).                                    
004301     88  WS-GODKAEND-BILD      VALUE '4101' '4102' '4103' '4104'          
004401                                     '4105' '4106' '4107' '4108'.         
004501     88  EGEN-MID              VALUE '4108'.                              
004601                                                                          
004701 77  NYCKLAR-SW              PIC X       VALUE 'J'.                       
004801     88  NYCKLAR-OK                      VALUE 'J'.                       
004901     88  NYCKLAR-FEL                     VALUE 'N'.                       
005001                                                                          
005101 77  ARTIKEL-SW              PIC X       VALUE 'J'.                       
005201     88  ARTIKEL-OK                      VALUE 'J'.                       
005301     88  ARTIKEL-FEL                     VALUE 'N'.                       
005401                                                                          
005501     EJECT                                                                
005601*    --- PARAMETRAR TILL SUBPROGRAM WINTSOR                               
005701 01  TABENTRY-PARM.                                                       
005801     03  STEGLANGD               PIC S9(9) COMP  VALUE 78.                
005901     03  ANTAL                   PIC S9(9) COMP.                          
006001     03  NYCKELLANGD             PIC S9(9) COMP  VALUE 18.                
006101                                                                          
006201 01  IX-RAD                      PIC S9(3) COMP-3 VALUE 1.                
006301 01  IX-RAD-TAB                  PIC S9(3) COMP-3 VALUE 1.                
006401                                                                          
006501 01  TAB-MAX                     PIC S9(9) COMP VALUE 800.                
006601     EJECT                                                                
006701*    --- TABELL SOM SORTERAS AV WINTSOR                                   
006801 01  TABELL.                                                              
006901     03  TAB-POST  OCCURS 800.                                            
007001       04  TAB-RAD.                                                       
007101         05  TAB-PLATSTYP        PIC X(1).                                
007201         05  TAB-ADBUFFOMR       PIC 9(2).                                
007301         05  TAB-ADBUFFGANG      PIC 9(2).                                
007401         05  TAB-ADBUFFPL        PIC 9(5).                                
007501         05  TAB-KVBUFF-F        PIC S9(8).                               
007601         05  TAB-KVBUFF-OF       PIC 9(8).                                
007701         05  TAB-KVKOLLI-F       PIC 9(4).                                
007801         05  TAB-KVKOLLI-OF      PIC 9(4).                                
007802         05  TAB-DABUFPAF        PIC 9(8).                                
007901       04  TAB-SORT.                                                      
008802         05  TAB-DABUFPAF-SORT   PIC 9(8).                                
008803         05  TAB-PLATSTYP-SORT   PIC X(1).                                
008901         05  TAB-ADBUFFOMR-SORT  PIC 9(2).                                
009001         05  TAB-ADBUFFGANG-SORT PIC 9(2).                                
009101         05  TAB-ADBUFFPL-SORT   PIC 9(5).                                
009102       04  TAB-SORT-NO.                                                   
009103         05  TAB-PLATSTYP-SORT1    PIC X(1).                              
009104         05  TAB-ADBUFFOMR-SORT1   PIC 9(2).                              
009105         05  TAB-ADBUFFGANG-SORT1  PIC 9(2).                              
009106         05  TAB-ADBUFFPL-SORT1    PIC 9(5).                              
009107         05  TAB-DABUFPAF-SORT1    PIC 9(8).                              
009301                                                                          
009401                                                                          
009501     SKIP3                                                                
009601 01    W-BUFF-IX             PIC S9(9)   VALUE ZERO  COMP-3.              
009701 01    WS-SUBUFF-F           PIC S9(9)   VALUE ZERO  COMP-3.              
009801 01    WS-SUBUFF-OF          PIC S9(9)   VALUE ZERO  COMP-3.              
009901 01    WS-SUKOLLI-F          PIC S9(5)   VALUE ZERO  COMP-3.              
010001 01    WS-SUKOLLI-OF         PIC S9(5)   VALUE ZERO  COMP-3.              
010101                                                                          
010201 01  WS-MSGI-SPAR-AREA.                                                   
010301     03  WS-MSGI-PGM             PIC X(6) VALUE SPACE.                    
010401     03  WS-MSGI-RADNR           PIC 9(3) VALUE ZERO.                     
010501     03  FILLER                  PIC X(191) VALUE SPACE.                  
010601                                                                          
010701                                                                          
010801 01    W-MINKEY-WDD8-X.                                                   
010901     03  W-MINKEY-IDTRANS          PIC  X(4)    VALUE '4108'.             
011001     03  W-MINKEY-WDD8-ENTER.                                             
011101       05  W-MINKEY-ADBUFFOMR-ENTER  PIC S9(3) VALUE ZERO  COMP-3.        
011201       05  W-MINKEY-DABUFPAF-ENTER   PIC  9(8) VALUE ZERO.                
011301       05  W-MINKEY-ADBUFFGANG-ENTER PIC S9(3) VALUE ZERO  COMP-3.        
011401       05  W-MINKEY-ADBUFFPL-ENTER   PIC S9(5) VALUE ZERO  COMP-3.        
011501                                                                          
011601     03  W-MINKEY-WDD8-NEXT.                                              
011701       05  W-MINKEY-ADBUFFOMR-NEXT   PIC S9(3) VALUE ZERO  COMP-3.        
011801       05  W-MINKEY-DABUFPAF-NEXT    PIC  9(8) VALUE ZERO.                
011901       05  W-MINKEY-ADBUFFGANG-NEXT  PIC S9(3) VALUE ZERO  COMP-3.        
012001       05  W-MINKEY-ADBUFFPL-NEXT    PIC S9(5) VALUE ZERO  COMP-3.        
012101                                                                          
012201 01    NYCKLAR-TILL-DLI.                                                  
012301   03    W-IDARTNR-X.                                                     
012401     05    W-IDARTNR         PIC S9(9)   VALUE ZERO  COMP-3.              
012501   03    W-IDDC-X.                                                        
012601     05    W-IDDC            PIC X(2)    VALUE SPACE.                     
012701   03    W-IDLAND-X.                                                      
012801     05    W-IDLAND          PIC X(2)    VALUE SPACE.                     
012901   03    W-IDSKYLT-X.                                                     
013001     05    W-IDSKYLT         PIC X(3).                                    
013101   03    W-WDD811KY-MIN-X.                                                
013201     05    W-IDDC-WDD8-MIN   PIC X(2).                                    
013301     05    W-ADBUFFOMR-MIN   PIC S9(3)   VALUE ZERO  COMP-3.              
013401     05    W-DABUFPAF-MIN    PIC  9(8)   VALUE ZERO.                      
013501     05    W-ADBUFFGANG-MIN  PIC S9(3)   VALUE ZERO  COMP-3.              
013601     05    W-ADBUFFPL-MIN    PIC S9(5)   VALUE ZERO  COMP-3.              
013701                                                                          
013801   03    W-WDD811KY-MAX-X.                                                
013901     05    W-IDDC-WDD8-MAX   PIC X(2).                                    
014001     05    W-ADBUFFOMR-MAX   PIC S9(3)   VALUE +99    COMP-3.             
014101     05    W-DABUFPAF-MAX    PIC  9(8)   VALUE  99999999.                 
014201     05    W-ADBUFFGANG-MAX  PIC S9(3)   VALUE +99    COMP-3.             
014301     05    W-ADBUFFPL-MAX    PIC S9(5)   VALUE +99999 COMP-3.             
014401                                                                          
014501   03  W-IDDC-B6-X.                                                       
014601       05 W-IDDC-B6                  PIC X(2).                            
014701                                                                          
014801     EJECT                                                                
014901 01    DYNAMISKA-SUBPROGRAM.                                              
015001   03  CBLTDLI               PIC X(8)         VALUE 'CBLTDLI'.            
015101   03  FELLOG                PIC X(8)         VALUE 'FELLOG'.             
015201   03  W005INIT              PIC X(8)         VALUE 'W005INIT'.           
015301   03  WMEDKONV              PIC X(8)         VALUE 'WMEDKONV'.           
015401   03  WINTSOR               PIC X(8)         VALUE 'WINTSOR'.            
015501     EJECT                                                                
015601*01 -COPY WWDC99                                                          
015701*01 -COPY WWLNDKON                                                        
015801     EJECT                                                                
015901*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
016001*01 -COPY WMSGINIT                                                        
016101     EJECT                                                                
016201*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
016301*01 -COPY WMEDAREA                                                        
016401     EJECT                                                                
016501 01  FELMEDDELANDE.                                                       
016601     03 ERR-MISSING-IN-REG          PIC X(3) VALUE '010'.                 
016701     03 INF-LAST-PAGE               PIC X(3) VALUE '106'.                 
016801     03 INF-MORE-INFO-EXIST         PIC X(3) VALUE '105'.                 
016901     03 INF-PART-REPLACED           PIC X(3) VALUE '220'.                 
017001     03 INF-PART-EXPIRE             PIC X(3) VALUE '258'.                 
017101     03 INF-REPLACING-PART          PIC X(3) VALUE '259'.                 
017201     03 ERR-WRONG-KEY               PIC X(3) VALUE '401'.                 
017301*                                                                         
017401 01  MEDDELANDE.                                                          
017501   03   FEL1.                                                             
017601     05 FILLER                   PIC X(40)                                
017701          VALUE 'ARTIKEL EJ I  LO 42 ELLER LO 43'.                        
017801     05 FILLER                   PIC X(40)                                
017901          VALUE 'PART NOT IN LO 42 OR IN LO 43'.                          
018001   03  FILLER REDEFINES FEL1.                                             
018101     05  FEL-1                   PIC X(40)   OCCURS 2.                    
018201     EJECT                                                                
018301*               AREOR FÖR MFS OCH SKÄRMHANTERING                          
018401 01    FILLER                PIC X(16)   VALUE 'MFS-WS'.                  
018501                                                                          
018601*01    MID -COPY W4I10801                                                 
018701     EJECT                                                                
018801*01    -COPY WMSGAREA                                                     
018901     EJECT                                                                
019001*  03    MOD -COPY W4O10801           -RED MSG-AREA.                      
019101     EJECT                                                                
019201*01    -COPY WMFSAREA.                                                    
019301     EJECT                                                                
019401******************************************************************        
019501*                                                                         
019601*        ARBETS-AREOR TILL IMS-SEKTIONERNA                                
019701*                                                                         
019801 01    IMS-WS.                                                            
019901   03    FILLER              PIC X(16)   VALUE 'IMS-WS'.                  
020001                                                                          
020101*                                STATUS-KOD FRÅN IMS                      
020201   03    STATUS-WS           PIC XX.                                      
020301     88    SEGMENT-FINNS                 VALUE '  '.                      
020401     88    SEGMENT-SAKNAS                VALUE 'GE'.                      
020501                                                                          
020601   03    GODK-STATUSKODER.                                                
020701     05    GODK-STATUS OCCURS 2 INDEXED BY STATUS-IX PIC XX.              
020801                                                                          
020901 01    SSA1                  PIC X(96).                                   
021001 01    SSA2                  PIC X(96).                                   
021101     EJECT                                                                
021201*                                IMS FUNKTIONSKODER                       
021301*01      -COPY W0003                                                      
021401     EJECT                                                                
021501*                                DLI INPUT-OUTPUT AREOR                   
021601 01    FILLER                PIC X(16)   VALUE 'DLI-IO-AREA'.             
021701 01    DLI-IO-AREA-601.                                                   
021801*      03  -COPY WDK601                                                   
021901                                                                          
022001 01    DLI-IO-AREA-611.                                                   
022101*      03  -COPY WDK611                                                   
022201                                                                          
022301 01    DLI-IO-AREA-711.                                                   
022401*      03  -COPY WDK711                                                   
022501                                                                          
022601 01    DLI-IO-AREA-712.                                                   
022701*      03  -COPY WDK712                                                   
022801                                                                          
022901 01    DLI-IO-AREA-301.                                                   
023001*      03  -COPY WDD301                                                   
023101                                                                          
023201 01    DLI-IO-AREA-311.                                                   
023301*      03  -COPY WDD311                                                   
023401                                                                          
023501 01    DLI-IO-AREA-801.                                                   
023601*      03  -COPY WDD801  -PRE BUFF-                                       
023701                                                                          
023801 01    DLI-IO-AREA-811.                                                   
023901*      03  -COPY WDD811  -PRE BUFF-                                       
024001                                                                          
024101 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
024201 01   DLI-IO-AREA-B601.                                                   
024301*     03  -COPY WDB601                                                    
024401     EJECT                                                                
024501 LINKAGE SECTION.                                                         
024601*01    -COPY W0009     -PRE MSG-                                          
024701                                                                          
024801*01    -COPY W0008     -PRE USEA-                                         
024901         05  FILLER           PIC X.                                      
025001*01    -COPY W0008     -PRE WDD3-                                         
025101         05  FILLER           PIC X.                                      
025201*01    -COPY W0008     -PRE WDK6-                                         
025301         05  FILLER            PIC X.                                     
025401*01    -COPY W0008     -PRE WDK7-                                         
025501         05  FILLER            PIC X.                                     
025601*01    -COPY W0008     -PRE WDD8-                                         
025701         05  FILLER           PIC X.                                      
025801*01    -COPY W0008     -PRE WDB6-                                         
025901         05  FILLER           PIC X.                                      
026001     EJECT                                                                
026101 PROCEDURE DIVISION USING MSG-PCB USEA-PCB WDD3-PCB                       
026201                          WDK6-PCB WDK7-PCB WDD8-PCB WDB6-PCB.            
026301     ENTRY 'DLITCBL' USING MSG-PCB USEA-PCB WDD3-PCB                      
026401                           WDK6-PCB WDK7-PCB WDD8-PCB WDB6-PCB.           
026501                                                                          
026601     PERFORM IMS-GET-MSG                                                  
026701     IF  SEGMENT-FINNS                                                    
026801       PERFORM A-INIT                                                     
026901       IF  NYCKLAR-OK                                                     
027001         MOVE MSGI-IDARTNR TO W-IDARTNR                                   
027101         MOVE IDDC-WS      TO W-IDDC                                      
027201         IF DCS-CDC                                                       
027301           PERFORM IMS-GU-WDK601                                          
027401         ELSE                                                             
027501           PERFORM IMS-GU-WDK711                                          
027601         END-IF                                                           
027701                                                                          
027801         IF  SEGMENT-FINNS                                                
027901           PERFORM S01-ARTIKEL-BEHORIG                                    
028001           IF ARTIKEL-OK                                                  
028101             PERFORM B-BEHANDLA-ART                                       
028201             PERFORM C-BEHANDLA-WDD3                                      
028301             PERFORM D-BEHANDLA-WDD8                                      
028302             IF BATTERY-LOC                                               
028401                PERFORM E-SORTERA-PLATSER                                 
028402             ELSE                                                         
028403                PERFORM E11-SORTERA-PLATSER                               
028404             END-IF                                                       
028501             PERFORM F-VISA-PLATSER                                       
028601           END-IF                                                         
028701         ELSE                                                             
028801           MOVE ERR-MISSING-IN-REG TO MED-IDMFSFEL                        
028901           CALL WMEDKONV USING MED-WMEDAREA                               
029001           MOVE MED-MFSFEL     TO MOD-MESSAGE-RAD1                        
029101         END-IF                                                           
029201       ELSE                                                               
029301         MOVE ERR-WRONG-KEY      TO MED-IDMFSFEL                          
029401         CALL WMEDKONV USING MED-WMEDAREA                                 
029501         MOVE MED-MFSFEL     TO MOD-MESSAGE-RAD1                          
029601       END-IF                                                             
029701                                                                          
029801       PERFORM IMS-INSERT-MSG                                             
029901     END-IF                                                               
030001                                                                          
030101     MOVE ZERO TO RETURN-CODE                                             
030201     GOBACK                                                               
030301     .                                                                    
030401     EJECT                                                                
030501 A-INIT SECTION.                                                          
030601                                                                          
030701     IF MSG-DUBBLA-TRANSKODER                                             
030801       MOVE MSG-IDTRANS-2 TO MFS-IDTRANS                                  
030901       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
031001       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W4I10801                 
031101     ELSE                                                                 
031201       MOVE MSG-IDTRANS-1 TO MFS-IDTRANS                                  
031301       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
031401       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W4I10801                  
031501     END-IF                                                               
031601     MOVE MSG-IDPFK          TO MFS-IDPFK                                 
031701     MOVE MFS-IDTRANS        TO WS-IDTRANS                                
031801                                                                          
031901     MOVE ALL '+'           TO MSGI-WMSGINIT                              
032001     MOVE '001'             TO MSGI-KDCALL                                
032101     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
032201     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
032301     MOVE '4108'            TO MSGI-IDTRANS                               
032401                                                                          
032501     IF MFS-IDTRANS = '4108'                                              
032601     OR (MID-IDARTNR-IN NUMERIC                                           
032701     AND MID-IDARTNR-IN > ZERO)                                           
032801         MOVE MID-IDARTNR-IN TO MSGI-IDARTNR                              
032901     END-IF                                                               
033001                                                                          
033101     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
033201     MOVE MSGI-SPAR-AREA     TO WS-MSGI-SPAR-AREA                         
033301     MOVE MSGI-IDLAND-SPR    TO MED-IDSKYLT                               
033401     MOVE MSGI-IDDC          TO IDDC-WS                                   
033501                                                                          
033601     MOVE JA TO NYCKLAR-SW                                                
033701                                                                          
033801     IF MFS-IDTRANS = '4108'                                              
033901       IF MID-IDDC-IN NOT = ALL '+'                                       
034001         MOVE MID-IDDC-IN TO IDDC-WS                                      
034101                             WS-IDDC                                      
034201       ELSE                                                               
034301         MOVE MID-IDDC-UT TO WS-IDDC                                      
034401       END-IF                                                             
034501     END-IF                                                               
034601                                                                          
034701     MOVE LOW-VALUE TO MSG-IO-AREA                                        
034801     MOVE 'W4O108N1' TO MFS-IDMOD                                         
034901                                                                          
035001     MOVE '4108' TO MOD-IDTRANS                                           
035101                                                                          
035201     MOVE MSGI-IDARTNR    TO MOD-IDARTNR-UT                               
035301     INSPECT MOD-IDARTNR-UT REPLACING LEADING ZERO BY SPACE               
035401     MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-IN                               
035501                             MOD-IDDC-IN                                  
035601                             MOD-MESSAGE-RAD1                             
035701                             MOD-MESSAGE-RAD23                            
035801                                                                          
035901     IF MSGI-IDLAND-SPR = 'GB'                                            
036001        MOVE +2 TO  SPRAK-IX                                              
036101     ELSE                                                                 
036201        MOVE +1 TO SPRAK-IX                                               
036301     END-IF                                                               
036401                                                                          
036501     IF MSGI-IDARTNR  NOT NUMERIC                                         
036601       MOVE NEJ TO NYCKLAR-SW                                             
036701     ELSE                                                                 
036801       MOVE IDDC-WS TO W-IDDC-B6                                          
036901       PERFORM IMS-GU-WDB601                                              
037001       IF DCS-KDDC = SPACE OR DCS-CDC-TR OR DCS-DDC                       
037101         MOVE MSGI-IDDC  TO W-IDDC                                        
037201                            IDDC-WS                                       
037301       ELSE                                                               
037401         MOVE MSGI-IDARTNR    TO W-IDARTNR                                
037501         MOVE IDDC-WS         TO W-IDDC                                   
037601       END-IF                                                             
037701     END-IF                                                               
037801     IF MID-IDARTNR-IN = ALL '+' AND MFS-IDTRANS = '4108'                 
037901       CONTINUE                                                           
038001     ELSE                                                                 
038101       MOVE '7'      TO MFS-IDPFK                                         
038201     END-IF                                                               
038301                                                                          
038401     MOVE IDDC-WS        TO MOD-IDDC-UT                                   
038501                                                                          
038601     MOVE MAX-MOD-LENGD TO MSG-KVLL                                       
038701     .                                                                    
038801     EJECT                                                                
038901 B-BEHANDLA-ART SECTION.                                                  
039001                                                                          
039101     IF DCS-CDC                                                           
039201       PERFORM BA-BEHANDLA-CDC                                            
039301     ELSE                                                                 
039401       PERFORM IMS-GU-WDK601                                              
039402       PERFORM IMS-GNP-WDK611                                             
039501       PERFORM BB-BEHANDLA-SDC                                            
039601     END-IF                                                               
039701     .                                                                    
039801     SKIP3                                                                
039901 BA-BEHANDLA-CDC SECTION.                                                 
040001                                                                          
040101     MOVE ART-REKSIFFR     TO MOD-REKSIFFR                                
040201     MOVE '-'              TO MOD-STRECK                                  
040301     IF  ART-KDERS-UTG     > ZERO                                         
040401       IF ART-KDERS-UTG    = +29 OR +52                                   
040501         MOVE INF-PART-EXPIRE TO MED-IDMFSFEL                             
040601         CALL WMEDKONV USING MED-WMEDAREA                                 
040701         MOVE MED-MFSFEL     TO MOD-MESSAGE-RAD1                          
040801       ELSE                                                               
040901         MOVE INF-PART-REPLACED TO MED-IDMFSFEL                           
041001         CALL WMEDKONV USING MED-WMEDAREA                                 
041101         MOVE MED-MFSFEL     TO MOD-MESSAGE-RAD1                          
041201       END-IF                                                             
041301     ELSE                                                                 
041401       IF ART-FLERS     = JA                                              
041501         MOVE INF-REPLACING-PART   TO MED-IDMFSINF                        
041601         CALL WMEDKONV USING MED-WMEDAREA                                 
041701         MOVE MED-MFSINF     TO MOD-MESSAGE-RAD23                         
041801       END-IF                                                             
042001       PERFORM IMS-GNP-WDK611                                             
042101                                                                          
042201       MOVE CLAG-KVPB-SATS  TO MOD-KVPB-SATS                              
042301       COMPUTE MOD-KVPB-TOT = CLAG-KVPB-SATS + CLAG-KVPB-SEP              
042401       MOVE CLAG-BEFT       TO MOD-BEFT                                   
042501       MOVE CLAG-KVQPACK-1  TO MOD-KVQPACK-1                              
042601       MOVE CLAG-KVQPACK-3  TO MOD-KVQPACK-3                              
042701       MOVE CLAG-KVQPACK-4  TO MOD-KVQPACK-4                              
042801       MOVE CLAG-KVAKS-CDC  TO MOD-KVAKS                                  
042901       MOVE CLAG-KVUTRS     TO MOD-KVUTRS                                 
043001       MOVE CLAG-KVEFRS     TO MOD-KVEFRS                                 
043101       MOVE CLAG-KVLS       TO MOD-KVLS                                   
043201       MOVE CLAG-KVROS      TO MOD-KVROS                                  
043202       MOVE CLAG-KDERS      TO MOD-KDERS                                  
043301       MOVE 'CDC'           TO MOD-RUBR1                                  
043401       MOVE CLAG-ADLAGOMR   TO MOD-ADLAGOMR                               
043501       MOVE CLAG-ADGANG     TO MOD-ADGANG                                 
043601       MOVE CLAG-ADPLATS    TO MOD-ADPLATS                                
043701                                                                          
043801       IF CLAG-ADLAGOMR-SVS > ZERO                                        
043901         MOVE 'P'           TO TAB-PLATSTYP (IX-RAD)                      
044001         MOVE '1'           TO TAB-PLATSTYP-SORT (IX-RAD)                 
044002                               TAB-PLATSTYP-SORT1 (IX-RAD)                
044101         MOVE CLAG-ADLAGOMR-SVS                                           
044201                            TO TAB-ADBUFFOMR (IX-RAD)                     
044301                               TAB-ADBUFFOMR-SORT (IX-RAD)                
044302                               TAB-ADBUFFOMR-SORT1 (IX-RAD)               
044401         MOVE CLAG-ADGANG-SVS                                             
044501                            TO TAB-ADBUFFGANG (IX-RAD)                    
044601                               TAB-ADBUFFGANG-SORT (IX-RAD)               
044602                               TAB-ADBUFFGANG-SORT1 (IX-RAD)              
044701         MOVE CLAG-ADPLATS-SVS                                            
044801                            TO TAB-ADBUFFPL (IX-RAD)                      
044901                               TAB-ADBUFFPL-SORT (IX-RAD)                 
044902                               TAB-ADBUFFPL-SORT1 (IX-RAD)                
045001         MOVE CLAG-KVLS-SVS TO TAB-KVBUFF-F (IX-RAD)                      
045101         COMPUTE WS-SUBUFF-F = WS-SUBUFF-F +                              
045201                               CLAG-KVLS-SVS                              
045301         MOVE ZERO          TO TAB-KVBUFF-OF (IX-RAD)                     
045401                               TAB-KVKOLLI-F (IX-RAD)                     
045501                               TAB-KVKOLLI-OF (IX-RAD)                    
045502                               TAB-DABUFPAF (IX-RAD)                      
045503                               TAB-DABUFPAF-SORT (IX-RAD)                 
045504                               TAB-DABUFPAF-SORT1 (IX-RAD)                
045601         ADD 1              TO IX-RAD                                     
045701       END-IF                                                             
045801                                                                          
045901       MOVE 1               TO IX                                         
046001       PERFORM UNTIL IX > 4                                               
046101       OR CLAG-ADLAGOMR-CD (IX) = ZERO                                    
046201         MOVE 'P'           TO TAB-PLATSTYP (IX-RAD)                      
046301         MOVE '1'           TO TAB-PLATSTYP-SORT (IX-RAD)                 
046302                               TAB-PLATSTYP-SORT1 (IX-RAD)                
046401         MOVE CLAG-ADLAGOMR-CD (IX)                                       
046501                            TO TAB-ADBUFFOMR (IX-RAD)                     
046601                               TAB-ADBUFFOMR-SORT (IX-RAD)                
046602                               TAB-ADBUFFOMR-SORT1 (IX-RAD)               
046701         MOVE CLAG-ADGANG-CD (IX)                                         
046801                            TO TAB-ADBUFFGANG (IX-RAD)                    
046901                               TAB-ADBUFFGANG-SORT (IX-RAD)               
046902                               TAB-ADBUFFGANG-SORT1 (IX-RAD)              
047001         MOVE CLAG-ADPLATS-CD (IX)                                        
047101                            TO TAB-ADBUFFPL (IX-RAD)                      
047201                               TAB-ADBUFFPL-SORT (IX-RAD)                 
047202                               TAB-ADBUFFPL-SORT1 (IX-RAD)                
047301         MOVE CLAG-KVLS-CD (IX)                                           
047401                            TO TAB-KVBUFF-F (IX-RAD)                      
047501         COMPUTE WS-SUBUFF-F = WS-SUBUFF-F +                              
047601                               CLAG-KVLS-CD (IX)                          
047701         MOVE ZERO          TO TAB-KVBUFF-OF (IX-RAD)                     
047801                               TAB-KVKOLLI-F (IX-RAD)                     
047901                               TAB-KVKOLLI-OF (IX-RAD)                    
047902                               TAB-DABUFPAF (IX-RAD)                      
047903                               TAB-DABUFPAF-SORT (IX-RAD)                 
047904                               TAB-DABUFPAF-SORT1 (IX-RAD)                
048001         ADD 1              TO IX                                         
048101                               IX-RAD                                     
048201       END-PERFORM                                                        
048301                                                                          
048401       IF CLAG-KDERS > ZERO                                               
048501         IF CLAG-KDERS = +09 OR +19 OR +29 OR +52                         
048601           MOVE INF-PART-EXPIRE      TO MED-IDMFSFEL                      
048701           CALL WMEDKONV USING MED-WMEDAREA                               
048801           MOVE MED-MFSFEL     TO MOD-MESSAGE-RAD1                        
048901         ELSE                                                             
049001           MOVE INF-PART-REPLACED    TO MED-IDMFSFEL                      
049101           CALL WMEDKONV USING MED-WMEDAREA                               
049201           MOVE MED-MFSFEL     TO MOD-MESSAGE-RAD1                        
049301         END-IF                                                           
049401       END-IF                                                             
049501     END-IF                                                               
049601     .                                                                    
049701     EJECT                                                                
049801 BB-BEHANDLA-SDC SECTION.                                                 
049901                                                                          
050001     MOVE ART-REKSIFFR     TO MOD-REKSIFFR                                
050101     MOVE '-'              TO MOD-STRECK                                  
050201     IF  ART-KDERS-UTG     > ZERO                                         
050301       IF ART-KDERS-UTG    = +29 OR +52                                   
050401         MOVE INF-PART-EXPIRE      TO MED-IDMFSFEL                        
050501         CALL WMEDKONV USING MED-WMEDAREA                                 
050601         MOVE MED-MFSFEL     TO MOD-MESSAGE-RAD1                          
050701       ELSE                                                               
050801         MOVE INF-PART-REPLACED    TO MED-IDMFSFEL                        
050901         CALL WMEDKONV USING MED-WMEDAREA                                 
051001         MOVE MED-MFSFEL     TO MOD-MESSAGE-RAD1                          
051101       END-IF                                                             
051201     ELSE                                                                 
051301       IF ART-FLERS     = JA                                              
051401         MOVE INF-REPLACING-PART   TO MED-IDMFSINF                        
051501         CALL WMEDKONV USING MED-WMEDAREA                                 
051601         MOVE MED-MFSINF     TO MOD-MESSAGE-RAD23                         
051701       END-IF                                                             
051802       MOVE CLAG-KDERS      TO MOD-KDERS                                  
051901                                                                          
052001       MOVE ZERO            TO MOD-KVPB-SATS                              
052101                               MOD-KVPB-TOT                               
052201                               MOD-BEFT                                   
052301                               MOD-KVQPACK-1                              
052401                               MOD-KVQPACK-3                              
052501                               MOD-KVQPACK-4                              
052601                                                                          
052701       COMPUTE WS-KVROS-SDC = SLAG-KVROS-DAG + SLAG-KVROS-BULK            
052801       MOVE WS-KVROS-SDC    TO MOD-KVROS                                  
052901       MOVE SLAG-KVAKS-SDC  TO MOD-KVAKS                                  
053001       MOVE SLAG-KVEFRS     TO MOD-KVEFRS                                 
053101       MOVE SLAG-KVLS       TO MOD-KVLS                                   
053201       MOVE SLAG-KVUTRS     TO MOD-KVUTRS                                 
053301       MOVE SLAG-ADLAGOMR   TO MOD-ADLAGOMR                               
053401       MOVE SLAG-ADGANG     TO MOD-ADGANG                                 
053501       MOVE SLAG-ADPLATS    TO MOD-ADPLATS                                
053601       MOVE MFS-RENSA-FAELT TO MOD-RUBR1                                  
053701                                                                          
053801     END-IF                                                               
053901                                                                          
054001     IF NDC-CN                                                            
054201       MOVE CLAG-BEFT         TO MOD-BEFT                                 
054202       MOVE CLAG-KDERS        TO MOD-KDERS                                
054301       MOVE WC-LAND-CN        TO W-IDLAND                                 
054401       PERFORM IMS-GU-WDK712                                              
054501       IF SEGMENT-FINNS                                                   
054601         IF LART-BEFT > 0                                                 
054701           MOVE LART-BEFT     TO MOD-BEFT                                 
054801         END-IF                                                           
054901       END-IF                                                             
055001     END-IF                                                               
055101     .                                                                    
055201     EJECT                                                                
055301 C-BEHANDLA-WDD3      SECTION.                                            
055401                                                                          
055501     PERFORM IMS-GU-WDD301                                                
055601     IF ENGLISH-TEXT                                                      
055701         MOVE 'GB '          TO W-IDSKYLT                                 
055801         PERFORM IMS-GNP-WDD311                                           
055901         IF SEGMENT-FINNS                                                 
056001             MOVE TEXT-BEART TO MOD-BEART                                 
056101         END-IF                                                           
056201     ELSE                                                                 
056301         MOVE 'S  '          TO W-IDSKYLT                                 
056401         PERFORM IMS-GNP-WDD311                                           
056501         IF SEGMENT-FINNS                                                 
056601             MOVE TEXT-BEART TO MOD-BEART                                 
056701         ELSE                                                             
056801             MOVE SPACE      TO MOD-BEART                                 
056901         END-IF                                                           
057001     END-IF                                                               
057101     .                                                                    
057201     EJECT                                                                
057301 D-BEHANDLA-WDD8      SECTION.                                            
057401                                                                          
057501     PERFORM IMS-GU-WDD801                                                
057601     IF SEGMENT-FINNS                                                     
057701        MOVE IDDC-WS         TO W-IDDC-WDD8-MIN                           
057801                                W-IDDC-WDD8-MAX                           
057901        MOVE ZERO            TO W-ADBUFFOMR-MIN                           
058001                                W-ADBUFFGANG-MIN                          
058101                                W-ADBUFFPL-MIN                            
058201                                W-DABUFPAF-MIN                            
058301                                                                          
058401        PERFORM IMS-GNP-WDD811                                            
058501        MOVE MFS-ROER-EJ-FAELT     TO MOD-KDPAF                           
058601        PERFORM UNTIL SEGMENT-SAKNAS OR IX-RAD > TAB-MAX                  
058701         IF SEGMENT-FINNS                                                 
058801           MOVE 'B'          TO TAB-PLATSTYP (IX-RAD)                     
058901           MOVE '2'          TO TAB-PLATSTYP-SORT (IX-RAD)                
058902                                TAB-PLATSTYP-SORT1 (IX-RAD)               
059001           MOVE BUFF-SALDO-ADBUFFOMR                                      
059101                             TO TAB-ADBUFFOMR (IX-RAD)                    
059201                                TAB-ADBUFFOMR-SORT (IX-RAD)               
059202                                TAB-ADBUFFOMR-SORT1 (IX-RAD)              
059203** FLAG TO SET BATTERY - SO SORT BASED ON DATE!! **                       
059204           IF IDDC-WS = '11'                                              
059205              MOVE BUFF-SALDO-ADBUFFOMR                                   
059206                             TO BATTERY-SW                                
059207           END-IF                                                         
059301           MOVE BUFF-SALDO-ADBUFFGANG                                     
059401                             TO TAB-ADBUFFGANG (IX-RAD)                   
059501                                TAB-ADBUFFGANG-SORT (IX-RAD)              
059502                                TAB-ADBUFFGANG-SORT1 (IX-RAD)             
059601           MOVE BUFF-SALDO-ADBUFFPL                                       
059701                             TO TAB-ADBUFFPL (IX-RAD)                     
059801                                TAB-ADBUFFPL-SORT (IX-RAD)                
059802                                TAB-ADBUFFPL-SORT1 (IX-RAD)               
059901           MOVE BUFF-SALDO-DABUFPAF                                       
060002                             TO TAB-DABUFPAF (IX-RAD)                     
060102                                TAB-DABUFPAF-SORT (IX-RAD)                
060103                                TAB-DABUFPAF-SORT1 (IX-RAD)               
060201           IF BUFF-SALDO-ADBUFFOMR = 1                                    
060301             MOVE BUFF-SALDO-KDPAF                                        
060401                             TO MOD-KDPAF                                 
060501           END-IF                                                         
060601           MOVE BUFF-SALDO-KVBUFF-OF                                      
060701                             TO TAB-KVBUFF-OF (IX-RAD)                    
060801           COMPUTE WS-SUBUFF-OF = WS-SUBUFF-OF +                          
060901                                  BUFF-SALDO-KVBUFF-OF                    
061001           MOVE BUFF-SALDO-KVBUFF-F                                       
061101                             TO TAB-KVBUFF-F (IX-RAD)                     
061201           COMPUTE WS-SUBUFF-F = WS-SUBUFF-F +                            
061301                                  BUFF-SALDO-KVBUFF-F                     
061401           MOVE BUFF-SALDO-KVKOLLI-OF                                     
061501                             TO TAB-KVKOLLI-OF (IX-RAD)                   
061601           COMPUTE WS-SUKOLLI-OF = WS-SUKOLLI-OF +                        
061701                                  BUFF-SALDO-KVKOLLI-OF                   
061801           MOVE BUFF-SALDO-KVKOLLI-F                                      
061901                             TO TAB-KVKOLLI-F (IX-RAD)                    
062001           COMPUTE WS-SUKOLLI-F = WS-SUKOLLI-F +                          
062101                                  BUFF-SALDO-KVKOLLI-F                    
062201           ADD 1             TO IX-RAD                                    
062301         END-IF                                                           
062401         PERFORM IMS-GNP-WDD811                                           
062501        END-PERFORM                                                       
062601        MOVE WS-SUBUFF-F               TO MOD-SUBUFF-F                    
062701        MOVE WS-SUBUFF-OF              TO MOD-SUBUFF-OF                   
062801        MOVE WS-SUKOLLI-F              TO MOD-SUKOLLI-F                   
062901        MOVE WS-SUKOLLI-OF             TO MOD-SUKOLLI-OF                  
063001                                                                          
063101     END-IF                                                               
063201     .                                                                    
063301     EJECT                                                                
063401                                                                          
063501 E-SORTERA-PLATSER SECTION.                                               
063601                                                                          
063701     SUBTRACT 1 FROM IX-RAD                                               
063801     MOVE IX-RAD TO ANTAL                                                 
063901                                                                          
064001     CALL WINTSOR USING TABELL STEGLANGD ANTAL                            
064101                  TAB-SORT (1)  NYCKELLANGD                               
064201     .                                                                    
064301     EJECT                                                                
064401                                                                          
064402 E11-SORTERA-PLATSER SECTION.                                             
064403                                                                          
064404     SUBTRACT 1 FROM IX-RAD                                               
064405     MOVE IX-RAD TO ANTAL                                                 
064406                                                                          
064407     CALL WINTSOR USING TABELL STEGLANGD ANTAL                            
064408                  TAB-SORT-NO (1)  NYCKELLANGD                            
064409     .                                                                    
064410     EJECT                                                                
064420                                                                          
064501 F-VISA-PLATSER SECTION.                                                  
064601                                                                          
064701***  HÄR LÄSES ETT STARTVÄRDE FÖR IX-RAD FRÅN USER-BASEN                  
064801***  SÅ ATT VID BLÄDDRING, START SKER MED RÄTT RAD                        
064901     IF MFS-IDPFK = '8' AND WS-MSGI-PGM = 'W40108'                        
065001        MOVE WS-MSGI-RADNR   TO IX-RAD-TAB                                
065101     ELSE                                                                 
065201        MOVE +1              TO IX-RAD-TAB                                
065301     END-IF                                                               
065401                                                                          
065501     MOVE +1                 TO IX-RAD                                    
065601     PERFORM UNTIL IX-RAD > 7                                             
065701     OR IX-RAD-TAB > TAB-MAX                                              
065801     OR TAB-PLATSTYP (IX-RAD-TAB) = SPACE                                 
065901     OR TAB-PLATSTYP (IX-RAD-TAB) = LOW-VALUE                             
066001       MOVE TAB-PLATSTYP (IX-RAD-TAB)                                     
066101                            TO MOD-PLATSTYP (IX-RAD)                      
066201       MOVE TAB-ADBUFFOMR (IX-RAD-TAB)                                    
066301                            TO MOD-ADBUFFOMR (IX-RAD)                     
066302                               BATTERY-SW                                 
066401       MOVE TAB-ADBUFFGANG (IX-RAD-TAB)                                   
066501                            TO MOD-ADBUFFGANG (IX-RAD)                    
066601       MOVE TAB-ADBUFFPL (IX-RAD-TAB)                                     
066701                            TO MOD-ADBUFFPL (IX-RAD)                      
066801       MOVE TAB-KVBUFF-OF (IX-RAD-TAB)                                    
066901                            TO MOD-KVBUFF-OF (IX-RAD)                     
067001       MOVE TAB-KVBUFF-F (IX-RAD-TAB)                                     
067101                            TO MOD-KVBUFF-F (IX-RAD)                      
067201       MOVE TAB-KVKOLLI-OF (IX-RAD-TAB)                                   
067301                            TO MOD-KVKOLLI-OF (IX-RAD)                    
067401       MOVE TAB-KVKOLLI-F (IX-RAD-TAB)                                    
067501                            TO MOD-KVKOLLI-F (IX-RAD)                     
067502       IF BATTERY-LOC                                                     
067602          MOVE TAB-DABUFPAF  (IX-RAD-TAB)                                 
067603                            TO MOD-DABUFPAF (IX-RAD)                      
067604       ELSE                                                               
067605          MOVE ZERO         TO MOD-DABUFPAF (IX-RAD)                      
067607       END-IF                                                             
067608       ADD 1                TO IX-RAD                                     
067701                               IX-RAD-TAB                                 
067801     END-PERFORM                                                          
067901                                                                          
068001***  OM FLER RADER FINNS, SÅ SPARAS I USERBASEN NÄSTA RADNR               
068101***  FRÅN TABELLEN , ANNARS BLANKAS                                       
068201     IF IX-RAD-TAB       <= ANTAL                                         
068301        MOVE IX-RAD-TAB      TO WS-MSGI-RADNR                             
068401        MOVE 'W40108'        TO WS-MSGI-PGM                               
068501        MOVE INF-MORE-INFO-EXIST                                          
068601                             TO MED-IDMFSINF                              
068701        CALL WMEDKONV USING MED-WMEDAREA                                  
068801        MOVE MED-MFSINF      TO MOD-MESSAGE-RAD23                         
068901     ELSE                                                                 
069001        MOVE SPACE           TO WS-MSGI-SPAR-AREA                         
069101     END-IF                                                               
069201                                                                          
069301     MOVE '002'             TO MSGI-KDCALL                                
069401     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
069501     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
069601     MOVE '4108'            TO MSGI-IDTRANS                               
069701     MOVE WS-MSGI-SPAR-AREA TO MSGI-SPAR-AREA                             
069801     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
069901     .                                                                    
070001     EJECT                                                                
070101 S01-ARTIKEL-BEHORIG  SECTION.                                            
070201                                                                          
070301     MOVE JA             TO ARTIKEL-SW                                    
070401     IF MSGI-IDUSER = 'PCSSE01'                                           
070501**ENDAST LO 42  FÖR ID PCSSE01..........                                  
070601**ENDAST LO 43  FÖR ID PCSSE01.....BATTERI KUNGÄLV                        
070701       IF DCS-CDC                                                         
070801         PERFORM IMS-GNP-WDK611                                           
070901         IF CLAG-ADLAGOMR = 42 OR 43                                      
071001           CONTINUE                                                       
071101         ELSE                                                             
071201           MOVE NEJ      TO ARTIKEL-SW                                    
071301           MOVE FEL-1 (SPRAK-IX)   TO MOD-MESSAGE-RAD1                    
071401         END-IF                                                           
071501       ELSE                                                               
071601         IF SLAG-ADLAGOMR = 42 OR 43                                      
071701           CONTINUE                                                       
071801         ELSE                                                             
071901           MOVE NEJ      TO ARTIKEL-SW                                    
072001           MOVE FEL-1 (SPRAK-IX)   TO MOD-MESSAGE-RAD1                    
072101         END-IF                                                           
072201       END-IF                                                             
072301     END-IF                                                               
072401     .                                                                    
072501     EJECT                                                                
072601* IMS SEKTIONER                                                           
072701 IMS-GET-MSG SECTION.                                                     
072801     MOVE '  QC' TO GODK-STATUSKODER                                      
072901     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
073001     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
073101     PERFORM IMS-STATUSKONTROLL                                           
073201     SKIP3                                                                
073301     .                                                                    
073401 IMS-INSERT-MSG SECTION.                                                  
073501     IF MSGI-IDLAND-SPR NOT = 'GB'                                        
073601       MOVE '0' TO MFS-KDHUVOMR                                           
073701     END-IF                                                               
073801     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
073901     MOVE SPACE TO GODK-STATUSKODER                                       
074001     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
074101     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
074201     PERFORM IMS-STATUSKONTROLL                                           
074301     .                                                                    
074401     EJECT                                                                
074501 IMS-GU-WDD301         SECTION.                                           
074601     STRING 'WDD301  (WDD3BSEQ =' W-IDARTNR-X ')'                         
074701             DELIMITED BY SIZE INTO SSA1                                  
074801     MOVE '  ' TO GODK-STATUSKODER                                        
074901     CALL CBLTDLI USING GU WDD3-PCB DLI-IO-AREA-301 SSA1                  
075001     MOVE WDD3-STATUS-CODE TO STATUS-WS                                   
075101     PERFORM IMS-STATUSKONTROLL                                           
075201     SKIP2                                                                
075301     .                                                                    
075401 IMS-GNP-WDD311          SECTION.                                         
075501     STRING 'WDD311  (IDSKYLT  =' W-IDSKYLT-X ')'                         
075601             DELIMITED BY SIZE INTO SSA1                                  
075701     MOVE '  GE' TO GODK-STATUSKODER                                      
075801     CALL CBLTDLI USING GNP WDD3-PCB DLI-IO-AREA-311 SSA1                 
075901     MOVE WDD3-STATUS-CODE TO STATUS-WS                                   
076001     PERFORM IMS-STATUSKONTROLL                                           
076101     .                                                                    
076201     EJECT                                                                
076301 IMS-GU-WDK601    SECTION.                                                
076401     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
076501            DELIMITED BY SIZE INTO SSA1                                   
076601     MOVE '  GE' TO GODK-STATUSKODER                                      
076701     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-AREA-601 SSA1                  
076801     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
076901     PERFORM IMS-STATUSKONTROLL                                           
077001     SKIP3                                                                
077101     .                                                                    
077201 IMS-GNP-WDK611    SECTION.                                               
077301     MOVE 'WDK611  ' TO SSA1                                              
077401     MOVE '  GE' TO GODK-STATUSKODER                                      
077501     CALL CBLTDLI USING GNP WDK6-PCB DLI-IO-AREA-611 SSA1                 
077601     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
077701     PERFORM IMS-STATUSKONTROLL                                           
077801     .                                                                    
077901     EJECT                                                                
078001 IMS-GU-WDK711     SECTION.                                               
078101     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
078201            DELIMITED BY SIZE INTO SSA1                                   
078301     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
078401            DELIMITED BY SIZE INTO SSA2                                   
078501     MOVE '  GE' TO GODK-STATUSKODER                                      
078601     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-AREA-711 SSA1 SSA2             
078701     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
078801     PERFORM IMS-STATUSKONTROLL                                           
078901     .                                                                    
079001     EJECT                                                                
079101 IMS-GU-WDK712 SECTION.                                                   
079201     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
079301          DELIMITED BY SIZE INTO SSA1                                     
079401     STRING 'WDK712  (IDLAND  = ' W-IDLAND-X ')'                          
079501          DELIMITED BY SIZE INTO SSA2                                     
079601     MOVE '  GE' TO GODK-STATUSKODER                                      
079701     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-AREA-712 SSA1 SSA2             
079801     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
079901     PERFORM IMS-STATUSKONTROLL                                           
080001     .                                                                    
080101     SKIP3                                                                
080201 IMS-GU-WDD801    SECTION.                                                
080301     STRING 'WDD801  (IDARTNR  =' W-IDARTNR-X ')'                         
080401             DELIMITED BY SIZE INTO SSA1                                  
080501     MOVE '  GE' TO GODK-STATUSKODER                                      
080601     CALL CBLTDLI USING GU WDD8-PCB DLI-IO-AREA-801 SSA1                  
080701     MOVE WDD8-STATUS-CODE TO STATUS-WS                                   
080801     PERFORM IMS-STATUSKONTROLL                                           
080901     SKIP3                                                                
081001     .                                                                    
081101 IMS-GNP-WDD811          SECTION.                                         
081201     STRING 'WDD811  (WDD811KY>=' W-WDD811KY-MIN-X                        
081301                    '&WDD811KY<=' W-WDD811KY-MAX-X ')'                    
081401             DELIMITED BY SIZE INTO SSA1                                  
081501     MOVE '  GE' TO GODK-STATUSKODER                                      
081601     CALL CBLTDLI USING GNP WDD8-PCB DLI-IO-AREA-811 SSA1                 
081701     MOVE WDD8-STATUS-CODE TO STATUS-WS                                   
081801     PERFORM IMS-STATUSKONTROLL                                           
081901     .                                                                    
082001     SKIP3                                                                
082101 IMS-GU-WDB601    SECTION.                                                
082201     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
082301          DELIMITED BY SIZE INTO SSA1                                     
082401     MOVE '  GE' TO GODK-STATUSKODER                                      
082501     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
082601     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
082701     PERFORM IMS-STATUSKONTROLL                                           
082801     IF SEGMENT-SAKNAS                                                    
082901         MOVE SPACE TO DCS-KDDC                                           
083001     END-IF                                                               
083101     .                                                                    
083201 IMS-STATUSKONTROLL SECTION.                                              
083301     SET STATUS-IX TO 1                                                   
083401     SEARCH GODK-STATUS AT END CALL FELLOG                                
083501       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
083601     END-SEARCH                                                           
084001     .                                                                    
