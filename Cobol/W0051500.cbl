010000 ID DIVISION.                                                             
020000 PROGRAM-ID.     W0051500.                                                
030000 AUTHOR.         SUSANNE OLSSON.                                          
040000 DATE-WRITTEN.   98/03/31.                                                
050000 DATE-COMPILED.                                                           
060000                                                                          
060100*                OBS!! OBS!! OBS!! OBS!!                                  
060200*                                                                         
060300*    DETTA PGM SKA INTE INSTALLERAS TILL PROD.                            
060400*    DET ÄR SKAPAT BARA FÖR TESTMILJÖERNA. /LASSI                         
060410* !! PSB/ACB FINNS EJ I IMG1                                              
060500*                                                                         
070021*    FUNKTION:                                                            
080000*        PROGRAMMET ÄR ETT FRÅGEPROGRAM, SOM GER EXEMPEL PÅ               
090058*        ARTIKLAR SOM FINNS PÅ ANGIVET DC.ANGES INGET DC FÅS DC           
100058*        FR USERBASEN.SÖKNINGEN SKER MED = ELLER > ANGIVET ART.NR.        
110000*                                                                         
120000*        PROGRAMMET LÄSER      WLARTC (WDK6)                              
130000*        PROGRAMMET LÄSER      WLARTS (WDK7)                              
140000*        PROGRAMMET LÄSER      WLBENA (WDD3B)                             
150000*                                                                         
160000*    INDATA.                                                              
170000*        TRANSAKTION: W0T515                                              
180000*        MID:         W0I51501                                            
190000*                                                                         
200000*    UTDATA.                                                              
210000*        MOD:         W0O51501                                            
220000                                                                          
230000     SKIP3                                                                
240000 ENVIRONMENT DIVISION.                                                    
250000 DATA DIVISION.                                                           
260046     EJECT                                                                
270000 WORKING-STORAGE SECTION.                                                 
280001                                                                          
290001*    -- CHECKED BY WY2000                                                 
300000 77  IDPGM                       PIC X(08)   VALUE 'W0051500'.            
310000                                                                          
320000*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
330000 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
340000                                                                          
350000 77  JA                          PIC X       VALUE 'J'.                   
360000 77  NEJ                         PIC X       VALUE 'N'.                   
370000                                                                          
380015 77  WS-IDARTNR-IN               PIC 9(9)  VALUE ZERO.                    
390011                                                                          
400000*    --- INDEX FÖR BLÄDDRINGSRADER                                        
410047 77  INDX                        PIC S9(9)  VALUE +0    COMP SYNC.        
420047 77  MAX-INDX                    PIC S9(9)  VALUE +12   COMP SYNC.        
430000*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
440000                                                                          
450000                                                                          
460000 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
470000     88  NYCKLAR-OK                          VALUE 'J'.                   
480000     88  NYCKLAR-FEL                         VALUE 'N'.                   
490000                                                                          
500000 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
510000     88  EGEN-MID                            VALUE '0515'.                
520000     88  GODK-MID                            VALUE '0511' '0512'          
530000                                                   '0513' '0514'          
540000                                                   '0515' '0516'          
550000                                                   '0517' '0518'          
560000                                                   '0519'.                
570000     88  HELP-MID                            VALUE '0551'.                
580002                                                                          
590002 77  SW-STARTA-ANNAN-BILD        PIC X       VALUE 'N'.                   
600002     88 STARTA-ANNAN-BILD                    VALUE 'J'.                   
610002                                                                          
620000     EJECT                                                                
630002 01  BILD-HOPP-AREOR.                                                     
640002                                                                          
650052   03  FILLER            PIC X(16)   VALUE 'P-TO-P-IO-AREA'.              
660052   03  P-TO-P-IO-AREA.                                                    
670051     05  FILLER                  PIC S9(4)   VALUE +117 COMP SYNC.        
680051     05  FILLER                  PIC X(1)    VALUE LOW-VALUE.             
690051     05  FILLER                  PIC X(1)    VALUE LOW-VALUE.             
700002     05  P-TO-P-KDTRANS          PIC X(8).                                
710051     05  FILLER                  PIC X(4)    VALUE '0515'.                
720051     05  FILLER                  PIC X(1)    VALUE '2'.                   
730051     05  FILLER                  PIC X(100)  VALUE ALL '+'.               
740002                                                                          
750002     EJECT                                                                
752059*      --- VALID IDDC CODES                                               
753059*                                                                         
754059*01    -COPY WWDC99                                                       
754159                                                                          
755059     EJECT                                                                
760000*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
770000 01  GENERELLA-SUBPROGRAM.                                                
780000     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
790000     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
800000     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
810000     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
820000     EJECT                                                                
830000*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
840000*01 -COPY WMEDAREA                                                        
850000     SKIP3                                                                
860000 01  MESSAGE-CODES.                                                       
870000     03  INF-FIRST-PAGE          PIC X(3)    VALUE '006'.                 
880042     03  INF-LAST-PAGE           PIC X(3)    VALUE '106'.                 
890000     03  INF-MORE-INFO-EXISTS    PIC X(3)    VALUE '105'.                 
900000     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
910004     03  ERR-PART-SUPERSEDED     PIC X(3)    VALUE '018'.                 
920005     03  ERR-KEY-MISSING         PIC X(3)    VALUE '005'.                 
930039     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
940000     EJECT                                                                
950000*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
960000*                                                                         
970000 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
980000     SKIP3                                                                
990000*01 -COPY WMSGINIT                                                        
000000     EJECT                                                                
010000*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
020000*                                                                         
030000 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
040000     SKIP3                                                                
050000*01  MID -COPY W0I51501                                                   
060000     EJECT                                                                
070002 01  FILLER                      PIC X(16)   VALUE 'MSG/MOD-AREA'.        
080000     SKIP3                                                                
090000*01  -COPY WMSGAREA                                                       
100000     EJECT                                                                
110000     03  MOD REDEFINES MSG-AREA.                                          
120000*      05  -COPY W0O51501                                                 
130000     EJECT                                                                
140000 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
150000     SKIP3                                                                
160000*01  -COPY WMFSAREA                                                       
170000     EJECT                                                                
180000*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
190000*                                                                         
200000 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
210047                                                                          
220047*    --- AREA MED DATA SOM SKA SPARAS MELLAN DIALOGSTEGEN                 
230047*                                                                         
240047 01  SPAR-AREA.                                                           
250047     03  SPAR-IDTRANS             PIC X(4)   VALUE '0515'.                
260047     03  SPAR-IDARTNR-ENTER       PIC S9(9)  VALUE ZERO COMP-3.           
270047     03  SPAR-IDARTNR-NEXT        PIC S9(9)  VALUE ZERO COMP-3.           
280000     SKIP3                                                                
290000 01  NYCKLAR-TILL-DLI.                                                    
300000*    --- VÄRDE PÅ BLÄDRINGSNYCKEL FÖR FÖRSTA RADEN PÅ SKÄRMEN             
310000                                                                          
320000     03  W-IDARTNR-X.                                                     
330000         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
340002                                                                          
350000     03  W-KDSEGKEY-X.                                                    
360004         05  W-KDSEGKEY          PIC X(1)    VALUE '1'.                   
370002                                                                          
380000     03  W-IDDC-X.                                                        
390000         05  W-IDDC              PIC X(2)    VALUE SPACE.                 
400002                                                                          
410000     03  W-IDSKYLT-X.                                                     
420000         05  W-IDSKYLT           PIC X(3)    VALUE SPACE.                 
430000     SKIP2                                                                
440000*    --- STATUS-KOD FRÅN IMS                                              
450000 01  STATUS-WS                   PIC XX.                                  
460005     88  STATUS-OK                           VALUE '  '.                  
470005     88  SEGMENT-FINNS                       VALUE '  '.                  
480000     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
490002     88  BASEN-SLUT                          VALUE 'GB'.                  
500005     88  TRANSKOD-FEL                        VALUE 'A1'.                  
510005     88  SECURITY-FEL                        VALUE 'A4'.                  
520000     SKIP2                                                                
530000 01  GODK-STATUSKODER.                                                    
540000     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
550000     SKIP3                                                                
560000 01  SSA1                        PIC X(64).                               
570000 01  SSA2                        PIC X(64).                               
580000     EJECT                                                                
590000*    --- IMS FUNKTIONSKODER                                               
600000*01  -COPY W0003                                                          
610000     EJECT                                                                
620000*    ---  DLI INPUT-OUTPUT AREA                                           
630000                                                                          
640000 01  FILLER         PIC X(16) VALUE 'DLI-IO-WLARTC01'.                    
650000 01  DLI-IO-WLARTC01.                                                     
660002*    03  -COPY WDK601                                                     
670000     EJECT                                                                
680000 01  FILLER         PIC X(16) VALUE 'DLI-IO-WLARTC11'.                    
690000 01  DLI-IO-WLARTC11.                                                     
700002*    03  -COPY WDK611                                                     
710000 01  FILLER         PIC X(16) VALUE 'DLI-IO-WLARTS01'.                    
720000 01  DLI-IO-WLARTS01.                                                     
730002*    03  -COPY WDK701                                                     
740000     EJECT                                                                
750000 01  FILLER         PIC X(16) VALUE 'DLI-IO-WLARTS11'.                    
760000 01  DLI-IO-WLARTS11.                                                     
770002*    03  -COPY WDK711                                                     
780000     EJECT                                                                
790000 01  FILLER         PIC X(16) VALUE 'DLI-IO-WLBENA11'.                    
800000 01  DLI-IO-WLBENA11.                                                     
810001*    03  -COPY WDD311                                                     
820000     EJECT                                                                
830000 LINKAGE SECTION.                                                         
840000*01  -COPY W0009   -PRE MSG-                                              
850005*01  -COPY W0009   -PRE ALT-                                              
860047     EJECT                                                                
870000*01  -COPY W0008   -PRE USEA-                                             
880000     05  FILLER                  PIC X.                                   
890000                                                                          
900000*01  -COPY W0008  -PRE ARTC-                                              
910000     05  FILLER                  PIC X.                                   
920000                                                                          
930047     EJECT                                                                
940000*01  -COPY W0008  -PRE ARTS-                                              
950000     05  FILLER                  PIC X.                                   
960000                                                                          
970000*01  -COPY W0008  -PRE BENA-                                              
980000     05  FILLER                  PIC X.                                   
990000     EJECT                                                                
000005 PROCEDURE DIVISION  USING MSG-PCB ALT-PCB USEA-PCB ARTC-PCB              
010045                           ARTS-PCB BENA-PCB.                             
020000 MAIN SECTION.                                                            
030005     ENTRY 'DLITCBL' USING MSG-PCB ALT-PCB USEA-PCB ARTC-PCB              
040045                           ARTS-PCB BENA-PCB.                             
050000                                                                          
060000     PERFORM IMS-GET-MSG                                                  
070000     IF SEGMENT-FINNS                                                     
080000       PERFORM A-INIT                                                     
090000       PERFORM B-KOLLA-NYCKLAR                                            
100000       IF NYCKLAR-OK                                                      
110002         IF MFS-FIRST                                                     
120002           PERFORM C-FOERSTA-SIDA                                         
130002         ELSE                                                             
140002           IF MFS-NEXT                                                    
150002             PERFORM D-NAESTA-SIDA                                        
160002           ELSE                                                           
170002             PERFORM E-SAMMA-SIDA                                         
180002           END-IF                                                         
190002         END-IF                                                           
200002         IF STARTA-ANNAN-BILD                                             
210002           CONTINUE                                                       
220002         ELSE                                                             
230010           PERFORM F-LAES-VISA-INFO                                       
240002         END-IF                                                           
250002       END-IF                                                             
260002       IF STARTA-ANNAN-BILD                                               
270002         CONTINUE                                                         
280002       ELSE                                                               
290002         COMPUTE MSG-KVLL = LENGTH OF MOD-W0O51501 + 4                    
300002         PERFORM IMS-INSERT-MSG                                           
310002       END-IF                                                             
320002     END-IF                                                               
330000                                                                          
340000     MOVE ZERO TO RETURN-CODE                                             
350000     GOBACK                                                               
360000     .                                                                    
370000     EJECT                                                                
380000 A-INIT SECTION.                                                          
390000                                                                          
400000     IF MSG-DUBBLA-TRANSKODER                                             
410000       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W0I51501                 
420000       MOVE MSG-IDTRANS-2  TO MFS-IDTRANS                                 
430000       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
440000     ELSE                                                                 
450000       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W0I51501                  
460000       MOVE MSG-IDTRANS-1  TO MFS-IDTRANS                                 
470000       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
480000     END-IF                                                               
490000                                                                          
500000     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
510000     MOVE MSG-IDPFK   TO MFS-IDPFK                                        
520000     MOVE MFS-IDTRANS TO W-IDTRANS                                        
530000                                                                          
540000     MOVE LOW-VALUE TO MSG-AREA                                           
550001     MOVE 'W0O515N1' TO MFS-IDMOD                                         
560000     MOVE '0515' TO MOD-IDTRANS                                           
570000     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                    
580000                                                                          
590000     IF EGEN-MID OR HELP-MID                                              
600000       CONTINUE                                                           
610000     ELSE                                                                 
620000       MOVE SPACE TO MFS-KDTRTYP                                          
630000       MOVE '7' TO MFS-IDPFK                                              
640000     END-IF                                                               
650000     .                                                                    
660000     EJECT                                                                
670000 B-KOLLA-NYCKLAR SECTION.                                                 
680000                                                                          
690000     MOVE ALL '+'           TO MSGI-WMSGINIT                              
700000     MOVE '001'             TO MSGI-KDCALL                                
710000     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
720000     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
730000     MOVE '0515'            TO MSGI-IDTRANS                               
740040     IF EGEN-MID                                                          
750002       MOVE MID-IDARTNR-IN  TO MSGI-IDARTNR                               
760045       MOVE MID-IDDC-IN   TO MSGI-IDDC-KEY                                
770002     END-IF                                                               
780000     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
790045     MOVE MSGI-SPAR-AREA TO SPAR-AREA                                     
800000                                                                          
810014     MOVE 'GB'              TO MED-IDSKYLT                                
820001     MOVE '2'               TO MFS-KDMFSFOR                               
830001                                                                          
840000     MOVE JA TO NYCKLAR-SW                                                
850000                                                                          
860000                                                                          
870000*    -- KONTROLL AV IDARTNR                                               
880000     MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-IN                               
890000                                                                          
900000     IF MID-IDARTNR-IN NOT = ALL '+'                                      
910000       MOVE '7'         TO MFS-IDPFK                                      
920000       MOVE SPACE       TO MFS-KDTRTYP                                    
930000     END-IF                                                               
940000     INSPECT MSGI-IDARTNR REPLACING LEADING SPACE BY ZERO                 
950000     IF MSGI-IDARTNR NUMERIC                                              
960016       MOVE MSGI-IDARTNR TO WS-IDARTNR-IN                                 
970000     ELSE                                                                 
980000       MOVE NEJ TO NYCKLAR-SW                                             
990000     END-IF                                                               
000000                                                                          
010000*    -- KONTROLL AV IDDC                                                  
020000     MOVE MFS-RENSA-FAELT TO MOD-IDDC-IN                                  
030000                                                                          
040000     IF MID-IDDC-IN NOT = ALL '+'                                         
050000       MOVE '7'         TO MFS-IDPFK                                      
060000       MOVE SPACE       TO MFS-KDTRTYP                                    
070002     END-IF                                                               
080022     INSPECT MSGI-IDDC-KEY REPLACING LEADING SPACE BY ZERO                
100059     MOVE MSGI-IDDC-KEY TO W-IDDC                                         
140000                                                                          
150000     IF GODK-MID OR NYCKLAR-OK                                            
160002       MOVE MSGI-IDARTNR     TO MOD-IDARTNR-UT                            
170002       INSPECT MOD-IDARTNR-UT REPLACING LEADING ZERO BY SPACE             
180022       MOVE MSGI-IDDC-KEY    TO MOD-IDDC-UT                               
190017     ELSE                                                                 
200017       MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-UT                             
210049                               MOD-IDDC-UT                                
220017     END-IF                                                               
230000                                                                          
240000     IF NYCKLAR-FEL                                                       
250000       MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                                 
260000       CALL WMEDKONV USING MED-WMEDAREA                                   
270000       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
280000       PERFORM MFS-RENSA-FAELT-IN                                         
290000       PERFORM MFS-RENSA-FAELT-UT                                         
300000     END-IF                                                               
310000     .                                                                    
320000     EJECT                                                                
330000 C-FOERSTA-SIDA SECTION.                                                  
340000                                                                          
350000     MOVE INF-FIRST-PAGE TO MED-IDMFSINF                                  
360000     CALL WMEDKONV USING MED-WMEDAREA                                     
370000     MOVE MED-MFSINF TO MOD-TEMFSFEL                                      
380032                                                                          
390000     PERFORM MFS-RENSA-FAELT-IN                                           
400000     .                                                                    
410000     EJECT                                                                
420000 D-NAESTA-SIDA SECTION.                                                   
430000                                                                          
440000     IF SPAR-IDTRANS = '0515'                                             
450010       MOVE SPAR-IDARTNR-NEXT TO W-IDARTNR                                
460000     ELSE                                                                 
470000       PERFORM MFS-RENSA-FAELT-IN                                         
480000     END-IF                                                               
490000     .                                                                    
500000     EJECT                                                                
510000 E-SAMMA-SIDA SECTION.                                                    
520000                                                                          
530000     IF SPAR-IDTRANS = '0515' OR '0551'                                   
540010       MOVE SPAR-IDARTNR-ENTER TO W-IDARTNR                               
550000       IF MID-INPUT = ALL '+'                                             
560000         PERFORM MFS-RENSA-FAELT-IN                                       
570015       ELSE                                                               
580002         MOVE +1 TO INDX                                                  
590002         PERFORM UNTIL INDX > MAX-INDX                                    
600002           IF MID-IDTRANS-HOPP (INDX)  =  ALL '+'                         
610017             MOVE MFS-RENSA-FAELT  TO MOD-IDTRANS-HOPP (INDX)             
620002           ELSE                                                           
630002             IF MID-IDTRANS-HOPP (INDX) NUMERIC                           
640002               PERFORM EA-STARTA-ANNAN-BILD                               
650002               MOVE JA                  TO SW-STARTA-ANNAN-BILD           
660002               MOVE MAX-INDX TO INDX                                      
670002             ELSE                                                         
680006               MOVE MFS-ALFA-FAELT-FEL  TO MOD-IDTRANS-ATTR (INDX)        
690005               MOVE MFS-ROER-EJ-FAELT   TO MOD-IDTRANS-HOPP (INDX)        
700002               MOVE NEJ                 TO SW-STARTA-ANNAN-BILD           
710039               MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                  
720039               CALL WMEDKONV USING MED-WMEDAREA                           
730039               MOVE MED-MFSFEL TO MOD-TEMFSFEL                            
740004             END-IF                                                       
750002           END-IF                                                         
760018           ADD +1 TO INDX                                                 
770002         END-PERFORM                                                      
780000       END-IF                                                             
790000     ELSE                                                                 
800000       PERFORM MFS-RENSA-FAELT-IN                                         
810000     END-IF                                                               
820000     .                                                                    
830000     EJECT                                                                
840002 EA-STARTA-ANNAN-BILD SECTION.                                            
850002                                                                          
860002     MOVE MID-IDARTNR (INDX)             TO MSGI-IDARTNR                  
870006     INSPECT MSGI-IDARTNR REPLACING LEADING SPACE BY ZERO                 
880002                                                                          
890002     MOVE '001'                          TO MSGI-KDCALL                   
900002     MOVE MSG-SIGNON-USERID              TO MSGI-IDUSER                   
910002     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
920002                                                                          
930052     STRING 'W' MID-IDTRANS-HOPP(INDX) (1:1)                              
940052            'T' MID-IDTRANS-HOPP(INDX) (2:3) '  '                         
950053                DELIMITED BY SIZE INTO P-TO-P-KDTRANS                     
960002                                                                          
970002     PERFORM S01-INSERT-ALTMSG                                            
980002     .                                                                    
990002     EJECT                                                                
000000 F-LAES-VISA-INFO SECTION.                                                
010000                                                                          
020010     IF MFS-FIRST                                                         
030010       PERFORM IMS-GN-ARTC-ROT                                            
040010     ELSE                                                                 
050010       PERFORM IMS-GU-ARTC-ROT                                            
060010     END-IF                                                               
070010                                                                          
080010     IF SEGMENT-FINNS                                                     
090010       MOVE ART-IDARTNR    TO SPAR-IDARTNR-ENTER                          
100023       PERFORM FA-VISA-RADER                                              
110010     ELSE                                                                 
120010       MOVE ERR-KEY-MISSING TO MED-IDMFSFEL                               
130010       CALL WMEDKONV USING MED-WMEDAREA                                   
140010       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
150010       PERFORM MFS-RENSA-FAELT-UT                                         
160010       PERFORM MFS-RENSA-FAELT-IN                                         
170010       MOVE W-IDARTNR  TO SPAR-IDARTNR-ENTER                              
180004     END-IF                                                               
190000     .                                                                    
200000     EJECT                                                                
210010 FA-VISA-RADER SECTION.                                                   
220004                                                                          
230010     MOVE +1 TO INDX                                                      
240034     PERFORM UNTIL INDX > MAX-INDX                                        
250037       IF SEGMENT-FINNS                                                   
260048         IF (ART-IDARTNR = WS-IDARTNR-IN                                  
270048             OR ART-IDARTNR > WS-IDARTNR-IN)                              
280044             AND ART-KDERS-UTG = 0                                        
290037           MOVE ART-IDARTNR      TO MOD-IDARTNR (INDX)                    
300013                                   W-IDARTNR                              
301059                                                                          
310059           MOVE W-IDDC          TO WS-IDDC                                
310159                                                                          
311059           IF NOT CDC-SE                                                  
320019             PERFORM IMS-GU-ARTS-BARN                                     
330019             IF SEGMENT-FINNS                                             
340019               MOVE SLAG-IDLEVNR TO MOD-IDLEVNR (INDX)                    
350019               PERFORM FB-FLYTTA-DATA-TILL-MOD                            
360019               PERFORM IMS-GN-ARTC-ROT                                    
370040               ADD +1 TO INDX                                             
380019             ELSE                                                         
390019               PERFORM IMS-GN-ARTC-ROT                                    
400019             END-IF                                                       
410019           ELSE                                                           
420019             PERFORM FB-FLYTTA-DATA-TILL-MOD                              
430019             MOVE ART-IDLEVNR  TO MOD-IDLEVNR (INDX)                      
440019             PERFORM IMS-GN-ARTC-ROT                                      
450040             ADD +1 TO INDX                                               
460019           END-IF                                                         
470010         ELSE                                                             
480010           PERFORM IMS-GN-ARTC-ROT                                        
490010         END-IF                                                           
500010       ELSE                                                               
510038         PERFORM MFS-RENSA-RAD-FAELT-UT                                   
520040         ADD +1 TO INDX                                                   
530010       END-IF                                                             
540036     END-PERFORM                                                          
550036                                                                          
560036* OM DET FINNS ETT 13:E SEGMENT.                                          
570036                                                                          
580036     IF SEGMENT-FINNS                                                     
590036       MOVE ART-IDARTNR TO SPAR-IDARTNR-NEXT                              
600032       MOVE INF-MORE-INFO-EXISTS TO MED-IDMFSINF                          
610031     ELSE                                                                 
620039       MOVE ART-IDARTNR TO SPAR-IDARTNR-NEXT                              
630042       MOVE INF-LAST-PAGE  TO MED-IDMFSINF                                
640031     END-IF                                                               
650042     CALL WMEDKONV USING MED-WMEDAREA                                     
660057     MOVE MED-MFSINF TO MOD-TEMFSINF                                      
670031                                                                          
680031     MOVE '002'      TO MSGI-KDCALL                                       
690031     MOVE '0515'     TO SPAR-IDTRANS                                      
700031     MOVE SPAR-AREA  TO MSGI-SPAR-AREA                                    
710031     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
720000     .                                                                    
730000     EJECT                                                                
740047 FB-FLYTTA-DATA-TILL-MOD SECTION.                                         
750019                                                                          
760049     MOVE ART-KDPRODSL    TO MOD-KDPRODSL (INDX)                          
770049     MOVE ART-IDFKNGRP    TO MOD-IDFKNGRP (INDX)                          
780049     MOVE ART-KDSORT      TO MOD-KDSORT (INDX)                            
790049                                                                          
800049     PERFORM IMS-GNP-ARTC-BARN                                            
810049     IF SEGMENT-FINNS                                                     
820049       MOVE CLAG-KDERS   TO MOD-KDERS (INDX)                              
830049     END-IF                                                               
840049                                                                          
850049     MOVE MED-IDSKYLT     TO W-IDSKYLT                                    
860019     PERFORM IMS-GET-BENA11-BSEQ                                          
870019     IF SEGMENT-FINNS                                                     
880049       MOVE TEXT-BEART    TO MOD-BEART (INDX)                             
890019     ELSE                                                                 
900019       MOVE 'DESCRIPTION MISSING' TO MOD-BEART (INDX)                     
910019     END-IF                                                               
920019                                                                          
930019     .                                                                    
940019     EJECT                                                                
950002 S01-INSERT-ALTMSG SECTION.                                               
960002                                                                          
970002     PERFORM IMS-CHANGE-ALTMSG                                            
980002     IF STATUS-OK                                                         
990002       PERFORM IMS-INSERT-ALTMSG                                          
000002     ELSE                                                                 
010002       IF SECURITY-FEL                                                    
020002         STRING 'NOT AUTHORIZED TO USE '                                  
030054                MID-IDTRANS-HOPP(INDX)                                    
040002                DELIMITED BY SIZE INTO MOD-TEMFSINF                       
050002       ELSE                                                               
060002         STRING 'WRONG PICTURE '                                          
070054                MID-IDTRANS-HOPP(INDX)                                    
080002                DELIMITED BY SIZE INTO MOD-TEMFSINF                       
090002       END-IF                                                             
100002       COMPUTE MSG-KVLL = LENGTH OF MOD-W0O51501 + 4                      
110002       PERFORM MFS-ROER-EJ-FAELT-IN                                       
120002       PERFORM MFS-ROER-EJ-FAELT-UT                                       
130002       PERFORM IMS-INSERT-MSG                                             
140002     END-IF                                                               
150002     .                                                                    
160002     EJECT                                                                
170000 MFS-RENSA-FAELT-UT SECTION.                                              
180000                                                                          
190000*    --- ALLA UTDATA-FÄLT                                                 
200000*    --- UTDATA-FÄLT PÅ BLÄDDRINGSRADER                                   
210002     MOVE +1 TO INDX                                                      
220002     PERFORM UNTIL INDX > MAX-INDX                                        
230002       MOVE MFS-RENSA-FAELT TO MOD-IDARTNR (INDX)                         
240002                               MOD-BEART (INDX)                           
250002                               MOD-KDPRODSL (INDX)                        
260002                               MOD-IDFKNGRP (INDX)                        
270002                               MOD-KDERS (INDX)                           
280002                               MOD-KDSORT(INDX)                           
290002                               MOD-IDLEVNR (INDX)                         
300002       ADD +1 TO INDX                                                     
310002     END-PERFORM                                                          
320002     .                                                                    
330000     SKIP3                                                                
340038 MFS-RENSA-RAD-FAELT-UT         SECTION.                                  
350038                                                                          
360038*    --- UTDATA-FÄLT PÅ BLÄDDRINGSRADER                                   
370038     MOVE MFS-RENSA-FAELT       TO MOD-IDARTNR (INDX)                     
380038                                   MOD-BEART (INDX)                       
390038                                   MOD-KDPRODSL (INDX)                    
400038                                   MOD-IDFKNGRP (INDX)                    
410038                                   MOD-KDERS (INDX)                       
420038                                   MOD-KDSORT (INDX)                      
430038                                   MOD-IDLEVNR (INDX)                     
440038     .                                                                    
450038     EJECT                                                                
460000 MFS-RENSA-FAELT-IN SECTION.                                              
470000                                                                          
480000*    --- ALLA INDATA-FÄLT                                                 
490002     MOVE +1 TO INDX                                                      
500002     PERFORM UNTIL INDX > MAX-INDX                                        
510002       MOVE MFS-RENSA-FAELT TO MOD-IDTRANS-HOPP (INDX)                    
520002       ADD +1 TO INDX                                                     
530002     END-PERFORM                                                          
540000     .                                                                    
550000     EJECT                                                                
560000 MFS-ROER-EJ-FAELT-UT  SECTION.                                           
570000                                                                          
580000*    --- ALLA UTDATA-FÄLT                                                 
590000*    --- INKL BLÄDDRINGSNYCKLAR OCH RAD-DATA                              
600002                                                                          
610000     MOVE +1 TO INDX                                                      
620000     PERFORM UNTIL INDX > MAX-INDX                                        
630000       PERFORM MFS-ROER-EJ-RAD-FAELT-UT                                   
640000       ADD +1 TO INDX                                                     
650000     END-PERFORM                                                          
660005     .                                                                    
670005     EJECT                                                                
680000 MFS-ROER-EJ-RAD-FAELT-UT  SECTION.                                       
690000                                                                          
700000*    --- UTDATA-FÄLT PÅ BLÄDDRINGSRADER                                   
710000     MOVE MFS-ROER-EJ-FAELT TO MOD-IDARTNR (INDX)                         
720002                               MOD-BEART (INDX)                           
730002                               MOD-KDPRODSL (INDX)                        
740002                               MOD-IDFKNGRP (INDX)                        
750002                               MOD-KDERS (INDX)                           
760002                               MOD-KDSORT(INDX)                           
770002                               MOD-IDLEVNR (INDX)                         
780000     .                                                                    
790000     SKIP3                                                                
800000 MFS-ROER-EJ-FAELT-IN  SECTION.                                           
810000                                                                          
820000*    --- ALLA INDATA-FÄLT                                                 
830002                                                                          
840002     MOVE +1 TO INDX                                                      
850002     PERFORM UNTIL INDX > MAX-INDX                                        
860002       MOVE MFS-ROER-EJ-FAELT TO MOD-IDTRANS-HOPP (INDX)                  
870002       ADD +1 TO INDX                                                     
880002     END-PERFORM                                                          
890000     .                                                                    
900000     EJECT                                                                
910000 IMS-GET-MSG SECTION.                                                     
920000                                                                          
930000     MOVE '  QC' TO GODK-STATUSKODER                                      
940000     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
950000     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
960000     PERFORM IMS-STATUSKONTROLL                                           
970000     .                                                                    
980000     SKIP3                                                                
990000 IMS-INSERT-MSG SECTION.                                                  
000000                                                                          
001056* OBS ! SKALL BARA GÖRA EN ENGELSK BILD.                                  
010001*    IF MSGI-IDLAND-SPR = 'SE'                                            
020001*      MOVE '0' TO MFS-KDHUVOMR                                           
030001*    END-IF                                                               
040000     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
050000     MOVE SPACE TO GODK-STATUSKODER                                       
060000     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
070000     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
080000     PERFORM IMS-STATUSKONTROLL                                           
090000     .                                                                    
100000     EJECT                                                                
110002 IMS-CHANGE-ALTMSG SECTION.                                               
120002     MOVE '  A1A4' TO GODK-STATUSKODER                                    
130052     CALL CBLTDLI USING CHNG ALT-PCB P-TO-P-KDTRANS                       
140002     MOVE ALT-STATUS-CODE TO STATUS-WS                                    
150002     PERFORM IMS-STATUSKONTROLL                                           
160002     .                                                                    
170002     SKIP3                                                                
180002 IMS-INSERT-ALTMSG SECTION.                                               
190002     MOVE SPACE TO GODK-STATUSKODER                                       
200053     CALL CBLTDLI USING ISRT ALT-PCB P-TO-P-IO-AREA                       
210002     MOVE ALT-STATUS-CODE TO STATUS-WS                                    
220002     PERFORM IMS-STATUSKONTROLL                                           
230002     .                                                                    
240002     EJECT                                                                
250004 IMS-GU-ARTC-ROT SECTION.                                                 
260000                                                                          
270012     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
280004          DELIMITED BY SIZE INTO SSA1                                     
290004     MOVE '  GE' TO GODK-STATUSKODER                                      
300012     CALL CBLTDLI USING GU ARTC-PCB DLI-IO-WLARTC01 SSA1                  
310000     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
320000     PERFORM IMS-STATUSKONTROLL                                           
330000     .                                                                    
340046     SKIP3                                                                
350004 IMS-GN-ARTC-ROT SECTION.                                                 
360004                                                                          
370004     MOVE 'WLARTC01' TO SSA1                                              
380004     MOVE '  GB' TO GODK-STATUSKODER                                      
390004     CALL CBLTDLI USING GN ARTC-PCB DLI-IO-WLARTC01 SSA1                  
400004     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
410004     PERFORM IMS-STATUSKONTROLL                                           
420004     .                                                                    
430046     SKIP3                                                                
440004 IMS-GNP-ARTC-BARN SECTION.                                               
450004                                                                          
460010     MOVE 'WLARTC11' TO SSA1                                              
470010     MOVE '  ' TO GODK-STATUSKODER                                        
480010     CALL CBLTDLI USING GNP ARTC-PCB DLI-IO-WLARTC11 SSA1                 
490010     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
500010     PERFORM IMS-STATUSKONTROLL                                           
510010     .                                                                    
520010     EJECT                                                                
530010 IMS-GU-ARTS-BARN SECTION.                                                
540010                                                                          
550010     STRING 'WLARTS01(IDARTNR  =' W-IDARTNR-X ')'                         
560010          DELIMITED BY SIZE INTO SSA1                                     
570010     STRING 'WLARTS11(IDDC     =' W-IDDC-X ')'                            
580004          DELIMITED BY SIZE INTO SSA2                                     
590004     MOVE '  GE' TO GODK-STATUSKODER                                      
600004     CALL CBLTDLI USING GU  ARTS-PCB DLI-IO-WLARTS11 SSA1 SSA2            
610040     MOVE ARTS-STATUS-CODE TO STATUS-WS                                   
620004     PERFORM IMS-STATUSKONTROLL                                           
630004     .                                                                    
640041     EJECT                                                                
650004 IMS-GET-BENA11-BSEQ SECTION.                                             
660004                                                                          
670004     STRING 'WLBENA01(WDD3BSEQ =' W-IDARTNR-X ')'                         
680004          DELIMITED BY SIZE INTO SSA1                                     
690004     STRING 'WLBENA11(IDSKYLT  =' W-IDSKYLT-X ')'                         
700004          DELIMITED BY SIZE INTO SSA2                                     
710004     MOVE '  GE' TO GODK-STATUSKODER                                      
720004     CALL CBLTDLI USING GU  BENA-PCB DLI-IO-WLBENA11 SSA1 SSA2            
730004     MOVE BENA-STATUS-CODE TO STATUS-WS                                   
740004     PERFORM IMS-STATUSKONTROLL                                           
750004     .                                                                    
760000     EJECT                                                                
770000 IMS-STATUSKONTROLL SECTION.                                              
780000                                                                          
790000     SET STATUS-IX TO 1                                                   
800000     SEARCH GODK-STATUS                                                   
810000       AT END                                                             
820000         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
830000         DELIMITED BY SIZE INTO FELTEXT                                   
840000         CALL FELLOG                                                      
850000       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
860000         CONTINUE                                                         
870000     END-SEARCH                                                           
880000     .                                                                    
