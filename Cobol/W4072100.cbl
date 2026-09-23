001001*                                                                         
002001******************************************************************        
003001*     THIS PROGRAM ALSO HAS A WEB-LDC VERSION CALLED WL0160      *        
004001******************************************************************        
005001*                                                                         
010000 ID DIVISION.                                                             
020000                                                                          
030001 PROGRAM-ID.     W4072100.                                                
040000 AUTHOR.         LARS THELL.                                              
050001 DATE-WRITTEN.   95/07/10.                                                
060000 DATE-COMPILED.                                                           
070000                                                                          
080000*    FUNKTION:                                                            
090001*        VISAR KÖ MED OBEHANDLADE LEVERANSANMÄRKNINGAR                    
100000*                                                                         
110001*        PROGRAMMET LÄSER      WLKREE (WDA2)                              
120000*                                                                         
120100*    E-TRACKER 10133176 DATE 2011-03-17 DISCR/RETURNS HAZ.MAT             
120200*                                                                         
130000*    INDATA.                                                              
140001*        TRANSAKTION: W4T721                                              
150001*        MID:         W4I72101                                            
160000*                                                                         
170000*    UTDATA.                                                              
180001*        MOD:         W4O72101                                            
190000                                                                          
200000     SKIP3                                                                
210000 ENVIRONMENT DIVISION.                                                    
220000     EJECT                                                                
230000 DATA DIVISION.                                                           
240000 WORKING-STORAGE SECTION.                                                 
241001*    -- CHECKED BY WY2000                                                 
250001 77  IDPGM                       PIC X(08)   VALUE 'W4072100'.            
260000                                                                          
270000*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
280000 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
290000                                                                          
300000 77  JA                          PIC X       VALUE 'J'.                   
310001 77  YES                         PIC X       VALUE 'Y'.                   
320000 77  NEJ                         PIC X       VALUE 'N'.                   
330001                                                                          
340000*    --- INDEX FÖR BLÄDDRINGSRADER                                        
350000 77  INDX                        PIC S9(4)  VALUE +0    COMP SYNC.        
360001 77  MAX-INDX                    PIC S9(4)  VALUE +12   COMP SYNC.        
370001                                                                          
380000*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
390001 77  WS-IDANSV                   PIC  X(6)  VALUE SPACE.                  
400001 77  WS-TILEVANM                 PIC  X(6)  VALUE SPACE.                  
410001 77  WS-KDANMORS                 PIC  X(2)  VALUE SPACE.                  
420001 77  WS-KDKREBEH                 PIC  X(3)  VALUE SPACE.                  
430001 77  WS-FLSUM                    PIC  X(1)  VALUE SPACE.                  
450000                                                                          
460001 77  W-KVRADER-RT                PIC S9(5)  VALUE ZERO COMP-3.            
470001 77  WS-IDPERSON                 PIC  9(3)  VALUE ZERO.                   
471001 77  W-IDPERSON                  PIC  9(3)  VALUE ZERO.                   
480001                                                                          
490001 77  INDATA-SW                   PIC X       VALUE 'J'.                   
500001     88  INDATA-OK                           VALUE 'J'.                   
510001     88  INDATA-FEL                          VALUE 'N'.                   
520001                                                                          
530000 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
540000     88  NYCKLAR-OK                          VALUE 'J'.                   
550000     88  NYCKLAR-FEL                         VALUE 'N'.                   
560000                                                                          
570000 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
580001     88  EGEN-MID                            VALUE '4721'.                
590001     88  GODK-MID                            VALUE '4721'.                
600000     88  HELP-MID                            VALUE '0551'.                
610000     EJECT                                                                
620000*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
630000 01  GENERELLA-SUBPROGRAM.                                                
640000     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
650000     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
660000     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
670000     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
680000     EJECT                                                                
690001*01  -COPY W006PRT                                                        
700001     EJECT                                                                
710000*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
720000*01 -COPY WMEDAREA                                                        
730000     SKIP3                                                                
740000 01  MESSAGE-CODES.                                                       
750000     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
760001     03  ERR-INFO-MISSING        PIC X(3)    VALUE '005'.                 
770000     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '011'.                 
780000     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
790000     03  INF-FIRST-PAGE          PIC X(3)    VALUE '006'.                 
800000     03  INF-MORE-INFO-EXISTS    PIC X(3)    VALUE '105'.                 
810000     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
820001     03  ERR-WRONG-PRINTER       PIC X(3)    VALUE '772'.                 
830000     EJECT                                                                
840000*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
850000*                                                                         
860000 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
870000     SKIP3                                                                
880000*01 -COPY WMSGINIT                                                        
890001     EJECT                                                                
900001*    --- FÄLT FÖR HOPP TILL ANDRA BILDER                                  
910001   77  SW-STARTA-ANNAN-BILD        PIC X       VALUE 'N'.                 
920001     88  STARTA-ANNAN-BILD                     VALUE 'J'.                 
930001                                                                          
940001 01  BILD-HOPP-AREOR.                                                     
950001                                                                          
960001   03    W-BILD               PIC X(4)    VALUE SPACE.                    
970001   03    W-HOPP-IDTRANS.                                                  
980001     05  FILLER               PIC X(1)    VALUE 'W'.                      
990001     05  W-HOPP-IDTRANS-2     PIC X(1).                                   
000001     05  FILLER               PIC X(1)    VALUE 'T'.                      
010001     05  W-HOPP-IDTRANS-4-6   PIC X(3).                                   
020001     05  FILLER               PIC X(2)    VALUE SPACE.                    
030001                                                                          
040001                                                                          
050001   03 FILLER             PIC X(16)   VALUE 'P-TO-P-AREA'.                 
060001   03      P-TO-P-SW.                                                     
070001                                                                          
080001     05  P-TO-P-KVLL             PIC S9(4) VALUE +117 COMP SYNC.          
090001     05  P-TO-P-KDZ1             PIC X(1)  VALUE LOW-VALUE.               
100001     05  P-TO-P-KDZ2             PIC X(1)  VALUE LOW-VALUE.               
110001     05  P-TO-P-KDTRANS          PIC X(8).                                
120001     05  P-TO-P-IDTRANS          PIC X(4).                                
130001     05  P-TO-P-KDMFSFOR         PIC X(1).                                
140001     05  P-TO-P-DATA             PIC X(100) VALUE ALL '+'.                
150001                                                                          
160001     EJECT                                                                
170001*                                                                         
180000*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
190000*                                                                         
200000 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
210000     SKIP3                                                                
220001*01  MID -COPY W4I72101                                                   
230000     EJECT                                                                
240000 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
250000     SKIP3                                                                
260000*01  -COPY WMSGAREA                                                       
270000     EJECT                                                                
280000     03  MOD REDEFINES MSG-AREA.                                          
290001*      05  -COPY W4O72101    -PRE MOD-                                    
300000     EJECT                                                                
310000 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
320000     SKIP3                                                                
330000*01  -COPY WMFSAREA                                                       
340000     EJECT                                                                
350000                                                                          
360001     SKIP2                                                                
370000*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
380000*                                                                         
390000     EJECT                                                                
400000 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
410000     SKIP3                                                                
420001 01  NYCKLAR-TILL-BLAEDDRING.                                             
430001     03  W-MINKEY-WDA2B1KY.                                               
440001         05  W-MINKEYB1-IDFTG      PIC  9(2)          VALUE ZERO.         
441001         05  W-MINKEYB1-KDARBTYP   PIC  X(8)          VALUE SPACE.        
450001         05  W-MINKEYB1-IDPERSON   PIC S9(3)   COMP-3 VALUE ZERO.         
460001         05  W-MINKEYB1-DALEVANM   PIC  9(8)          VALUE ZERO.         
470001         05  W-MINKEYB1-IDDISTR    PIC S9(5)   COMP-3 VALUE ZERO.         
480001         05  W-MINKEYB1-IDKUNDNR   PIC S9(7)   COMP-3 VALUE ZERO.         
490001         05  W-MINKEYB1-IDRAPPNR   PIC  X(7)          VALUE ZERO.         
500001         05  W-MINKEYB1-IDARTNR    PIC S9(9)   COMP-3 VALUE ZERO.         
510001         05  W-MINKEYB1-IDRADNR    PIC S9(5)   COMP-3 VALUE ZERO.         
520000     SKIP3                                                                
530001                                                                          
540000 01  NYCKLAR-TILL-DLI.                                                    
550001                                                                          
560001     03  W-WDA2B1KY-MIN-X.                                                
570001         05  W-IDFTG-B1-MIN      PIC  9(2)          VALUE ZERO.           
571001         05  W-KDARBTYP-B1-MIN   PIC  X(8)          VALUE SPACE.          
580001         05  W-IDPERSON-B1-MIN   PIC S9(3)   COMP-3 VALUE ZERO.           
590001         05  W-DALEVANM-B1-MIN   PIC  9(8)          VALUE ZERO.           
600001         05  W-IDDISTR-B1-MIN    PIC S9(5)   COMP-3 VALUE ZERO.           
610001         05  W-IDKUNDNR-B1-MIN   PIC S9(7)   COMP-3 VALUE ZERO.           
620001         05  W-IDRAPPNR-B1-MIN   PIC  X(7)          VALUE ZERO.           
630001         05  W-IDARTNR-B1-MIN    PIC S9(9)   COMP-3 VALUE ZERO.           
640001         05  W-IDRADNR-B1-MIN    PIC S9(5)   COMP-3 VALUE ZERO.           
650001                                                                          
651001     03  W-WDA2B1KY-MAX-X.                                                
652001         05  W-IDFTG-B1-MAX      PIC  9(2)          VALUE ZERO.           
660001         05  W-KDARBTYP-B1-MAX   PIC  X(8)          VALUE SPACE.          
670001         05  W-IDPERSON-B1-MAX   PIC S9(3)   COMP-3 VALUE ZERO.           
680001         05  W-DALEVANM-B1-MAX   PIC  9(8)          VALUE ZERO.           
690001         05  W-IDDISTR-B1-MAX    PIC S9(5)   COMP-3 VALUE ZERO.           
700001         05  W-IDKUNDNR-B1-MAX   PIC S9(7)   COMP-3 VALUE ZERO.           
710001         05  W-IDRAPPNR-B1-MAX   PIC  X(7)          VALUE ZERO.           
720001         05  W-IDARTNR-B1-MAX    PIC S9(9)   COMP-3 VALUE ZERO.           
730001         05  W-IDRADNR-B1-MAX    PIC S9(5)   COMP-3 VALUE ZERO.           
740001                                                                          
750001     03  W-DALEVANM-MIN-X.                                                
760001         05  W-DALEVANM-MIN      PIC  9(8)          VALUE ZERO.           
770001                                                                          
780001     03  W-DALEVANM-MAX-X.                                                
790001         05  W-DALEVANM-MAX      PIC  9(8)          VALUE ZERO.           
800001                                                                          
810001     03  W-KDANMORS-MIN-X.                                                
820001         05  W-KDANMORS-MIN      PIC  X(2)          VALUE SPACE.          
830001                                                                          
840001     03  W-KDANMORS-MAX-X.                                                
850001         05  W-KDANMORS-MAX      PIC  X(2)          VALUE SPACE.          
860001                                                                          
870001     03  W-KDKREBEH-MIN-X.                                                
880001         05  W-KDKREBEH-MIN      PIC  X(3)          VALUE SPACE.          
890001                                                                          
900001     03  W-KDKREBEH-MAX-X.                                                
910001         05  W-KDKREBEH-MAX      PIC  X(3)          VALUE SPACE.          
920001                                                                          
930000     SKIP2                                                                
940000*    --- STATUS-KOD FRÅN IMS                                              
950000 01  STATUS-WS                   PIC XX.                                  
960001     88  STATUS-OK                           VALUE '  '.                  
970001     88  SEGMENT-FINNS                       VALUE '  '.                  
980000     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
990000     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
000001     88  TRANSKOD-FEL                        VALUE 'A1'.                  
010001     88  SECURITY-FEL                        VALUE 'A4'.                  
020000     SKIP2                                                                
030000 01  GODK-STATUSKODER.                                                    
040000     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
050000     SKIP3                                                                
060001 01  SSA1                        PIC X(192).                              
070000     EJECT                                                                
080000*    --- IMS FUNKTIONSKODER                                               
090000*01  -COPY W0003                                                          
100000     EJECT                                                                
110000*    ---  DLI INPUT-OUTPUT AREA                                           
120000 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
130000     SKIP3                                                                
140000 01  DLI-IO-AREA.                                                         
150000     03  IO-AREA                 PIC X(150)  VALUE SPACE.                 
160000     SKIP3                                                                
170001     03  WLKREG01 REDEFINES IO-AREA.                                      
180001*        05  -COPY WDA2B1                                                 
190001     EJECT                                                                
200000 LINKAGE SECTION.                                                         
210000                                                                          
220000*01  -COPY W0009   -PRE MSG-                                              
230001*01  -COPY W0009   -PRE ALT-                                              
240001     EJECT                                                                
250001*01  -COPY W0008   -PRE USEA-                                             
260001     05  FILLER                  PIC X.                                   
270001     EJECT                                                                
280001*01  -COPY W0008   -PRE KREG-                                             
290001     05  FILLER                  PIC X.                                   
300001     EJECT                                                                
310001 PROCEDURE DIVISION  USING MSG-PCB  ALT-PCB  USEA-PCB                     
320001                           KREG-PCB.                                      
330001     ENTRY 'DLITCBL' USING MSG-PCB  ALT-PCB  USEA-PCB                     
340001                           KREG-PCB.                                      
350000                                                                          
360000     PERFORM IMS-GET-MSG                                                  
370000     IF SEGMENT-FINNS                                                     
380000       PERFORM A-INIT                                                     
390000       PERFORM B-KOLLA-NYCKLAR                                            
400000       IF NYCKLAR-OK                                                      
410001         IF MFS-FIRST                                                     
420001            PERFORM C-FOERSTA-SIDA                                        
430001         ELSE                                                             
440001            IF MFS-NEXT                                                   
450000               PERFORM D-NAESTA-SIDA                                      
460001            ELSE                                                          
470000               PERFORM E-SAMMA-SIDA                                       
480001            END-IF                                                        
490001         END-IF                                                           
500001         IF STARTA-ANNAN-BILD                                             
510001            CONTINUE                                                      
520001         ELSE                                                             
530001            PERFORM F-LAES-VISA-INFO                                      
540001         END-IF                                                           
550000       END-IF                                                             
560001       IF STARTA-ANNAN-BILD                                               
570001          CONTINUE                                                        
580001       ELSE                                                               
590001          COMPUTE MSG-KVLL = LENGTH OF MOD-W4O72101 + 4                   
600001          PERFORM IMS-INSERT-MSG                                          
610001       END-IF                                                             
620000     END-IF                                                               
630000                                                                          
640000     MOVE ZERO TO RETURN-CODE                                             
650000     GOBACK                                                               
660000     .                                                                    
670000     EJECT                                                                
680000 A-INIT SECTION.                                                          
690000                                                                          
700000     IF MSG-DUBBLA-TRANSKODER                                             
710001       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W4I72101                 
720000       MOVE MSG-IDTRANS-2                 TO MFS-IDTRANS                  
730000       MOVE MSG-KDMFSFOR-2                TO MFS-KDMFSFOR                 
740000     ELSE                                                                 
750001       MOVE MSG-INDATA-MINUS-1-TRANSKOD   TO MID-W4I72101                 
760000       MOVE MSG-IDTRANS-1                 TO MFS-IDTRANS                  
770000       MOVE MSG-KDMFSFOR-1                TO MFS-KDMFSFOR                 
780000     END-IF                                                               
790000                                                                          
800000     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
810000     MOVE MSG-IDPFK   TO MFS-IDPFK                                        
820000     MOVE MFS-IDTRANS TO W-IDTRANS                                        
830000                                                                          
840000     MOVE LOW-VALUE TO MSG-AREA                                           
850001     MOVE 'W4O72101' TO MFS-IDMOD                                         
860001     MOVE '4721' TO MOD-IDTRANS                                           
870000     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                    
880000                                                                          
890000     IF EGEN-MID OR HELP-MID                                              
900000       CONTINUE                                                           
910000     ELSE                                                                 
920000       MOVE SPACE TO MFS-KDTRTYP                                          
930000       MOVE '7' TO MFS-IDPFK                                              
940000     END-IF                                                               
950001                                                                          
960001     MOVE LOW-VALUE         TO W-WDA2B1KY-MIN-X                           
970001                               W-DALEVANM-MIN-X                           
980001                               W-KDANMORS-MIN-X                           
990001                               W-KDKREBEH-MIN-X                           
000001                                                                          
010001     MOVE HIGH-VALUE        TO W-WDA2B1KY-MAX-X                           
020001                               W-DALEVANM-MAX-X                           
030001                               W-KDANMORS-MAX-X                           
040001                               W-KDKREBEH-MAX-X                           
050001                                                                          
060000     .                                                                    
070000     EJECT                                                                
080000 B-KOLLA-NYCKLAR SECTION.                                                 
090000                                                                          
100000     MOVE ALL '+'              TO MSGI-WMSGINIT                           
110000     MOVE '001'                TO MSGI-KDCALL                             
120000     MOVE MSG-SIGNON-USERID    TO MSGI-IDUSER                             
120101     MOVE '4721'            TO MSGI-IDTRANS                               
120201     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
120301     IF EGEN-MID                                                          
121001        MOVE MID-IDANSV-IN(1:3)    TO MSGI-KDARBTYP                       
122001        MOVE MID-IDANSV-IN(4:3)    TO WS-IDPERSON                         
122101        INSPECT WS-IDPERSON REPLACING ALL SPACE BY ZERO                   
122201        MOVE WS-IDPERSON           TO MSGI-IDPERSON                       
123001        IF MID-IDANSV-IN           = ALL '+'                              
124001           MOVE '++++++++'         TO MSGI-KDARBTYP                       
125001           MOVE '+++'              TO MSGI-IDPERSON                       
126001        END-IF                                                            
127001     END-IF                                                               
130000     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
140000                                                                          
141001     IF MSGI-IDLAND-SPR = 'GB'                                            
142001       MOVE 'GB'                  TO MED-IDSKYLT                          
143001     ELSE                                                                 
144001       MOVE 'S '                  TO MED-IDSKYLT                          
145001     END-IF                                                               
146001                                                                          
150000     MOVE JA TO NYCKLAR-SW                                                
160000                                                                          
170001     PERFORM BA-KOLLA-IDANSV                                              
180001     PERFORM BB-KOLLA-TILEVANM                                            
190001     PERFORM BC-KOLLA-KDANMORS                                            
200001     PERFORM BD-KOLLA-KDKREBEH                                            
210001     PERFORM BE-KOLLA-FLSUM                                               
220000                                                                          
230000     IF GODK-MID OR NYCKLAR-OK                                            
240001        MOVE MSGI-KDARBTYP        TO MOD-IDANSV-UT(1:3)                   
241101        IF MSGI-IDPERSON           = ZERO                                 
241201          MOVE SPACE              TO MOD-IDANSV-UT(4:3)                   
241301        ELSE                                                              
241401          MOVE MSGI-IDPERSON      TO MOD-IDANSV-UT(4:3)                   
241601        END-IF                                                            
250001        MOVE WS-TILEVANM          TO MOD-TILEVANM-UT                      
260001        MOVE WS-KDANMORS          TO MOD-KDANMORS-UT                      
280001        MOVE WS-KDKREBEH          TO MOD-KDKREBEH-UT                      
290001        MOVE ' '                  TO MOD-FLSUM-UT                         
291001        IF MOD-TILEVANM-UT         = '000000'                             
292001          MOVE '      '           TO MOD-TILEVANM-UT                      
293001        END-IF                                                            
293101        IF MOD-KDANMORS-UT         = '0 '                                 
293201          MOVE '  '               TO MOD-KDANMORS-UT                      
293301        END-IF                                                            
300000     ELSE                                                                 
310001        MOVE MFS-RENSA-FAELT      TO MOD-IDANSV-UT                        
320001                                     MOD-TILEVANM-UT                      
330001                                     MOD-KDANMORS-UT                      
340001                                     MOD-KDKREBEH-UT                      
350001                                     MOD-FLSUM-UT                         
360000     END-IF                                                               
370000                                                                          
380000     IF NYCKLAR-FEL                                                       
390000       MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                                 
400000       CALL WMEDKONV USING MED-WMEDAREA                                   
410000       MOVE MED-MFSFEL    TO MOD-TEMFSFEL                                 
420000       PERFORM MFS-RENSA-FAELT-IN                                         
430000       PERFORM MFS-RENSA-FAELT-UT                                         
440000     END-IF                                                               
450000     .                                                                    
460000     EJECT                                                                
470001                                                                          
480001 BA-KOLLA-IDANSV    SECTION.                                              
490001                                                                          
500001*    -- KONTROLL AV IDANSV DVS KDARBTYP OCH IDPERSON                      
510001                                                                          
520001     MOVE MFS-RENSA-FAELT TO MOD-IDANSV-IN                                
530001                                                                          
540001     IF MID-IDANSV-IN     NOT =  ALL '+'                                  
570001       MOVE '7'           TO MFS-IDPFK                                    
580001       MOVE SPACE         TO MFS-KDTRTYP                                  
600001     END-IF                                                               
610001                                                                          
620001     IF MSGI-KDARBTYP           = SPACE                                   
630001        MOVE NEJ                TO NYCKLAR-SW                             
640001     ELSE                                                                 
641001        MOVE MSGI-IDFTG         TO  W-IDFTG-B1-MIN                        
642001                                    W-IDFTG-B1-MAX                        
650001        MOVE MSGI-KDARBTYP      TO  W-KDARBTYP-B1-MIN                     
660001                                    W-KDARBTYP-B1-MAX                     
670001        IF MSGI-IDPERSON        NUMERIC                                   
670101          IF MSGI-IDPERSON       > ZERO                                   
680001            MOVE MSGI-IDPERSON  TO  W-IDPERSON-B1-MIN                     
690001                                    W-IDPERSON-B1-MAX                     
700001          ELSE                                                            
700101            MOVE ZERO           TO  W-IDPERSON-B1-MIN                     
700201            MOVE 999            TO  W-IDPERSON-B1-MAX                     
701001          END-IF                                                          
702001        END-IF                                                            
710001     END-IF                                                               
720001     .                                                                    
730001     EJECT                                                                
740001                                                                          
750001 BB-KOLLA-TILEVANM SECTION.                                               
760001                                                                          
770001     MOVE MFS-RENSA-FAELT       TO MOD-TILEVANM-IN                        
780001                                                                          
781001     IF EGEN-MID                                                          
790001       IF MID-TILEVANM-IN       = ALL '+'                                 
800001         MOVE MID-TILEVANM-UT   TO WS-TILEVANM                            
810001       ELSE                                                               
820001         MOVE MID-TILEVANM-IN   TO WS-TILEVANM                            
830001         INSPECT WS-TILEVANM REPLACING LEADING SPACE BY ZERO              
840001         MOVE '7'               TO MFS-IDPFK                              
850001         MOVE SPACE             TO MFS-KDTRTYP                            
860001       END-IF                                                             
861001     ELSE                                                                 
862001       MOVE ZERO                TO WS-TILEVANM                            
863001     END-IF                                                               
870001                                                                          
880001     IF WS-TILEVANM NUMERIC AND WS-TILEVANM > ZERO                        
880101       MOVE WS-TILEVANM         TO W-DALEVANM-MIN                         
880201                                   W-DALEVANM-B1-MIN                      
880301                                   W-DALEVANM-MAX                         
880401                                   W-DALEVANM-B1-MAX                      
881001       IF WS-TILEVANM NOT = ZERO                                          
881101         IF WS-TILEVANM < 500000                                          
882001           MOVE 20              TO W-DALEVANM-MIN (1:2)                   
883001                                   W-DALEVANM-B1-MIN (1:2)                
884001                                   W-DALEVANM-MAX (1:2)                   
885001                                   W-DALEVANM-B1-MAX (1:2)                
886001         ELSE                                                             
886101           IF WS-TILEVANM < 999999                                        
887001             MOVE 19            TO W-DALEVANM-MIN (1:2)                   
888001                                   W-DALEVANM-B1-MIN (1:2)                
889001                                   W-DALEVANM-MAX (1:2)                   
889101                                   W-DALEVANM-B1-MAX (1:2)                
889201           ELSE                                                           
889301             MOVE 99999999      TO W-DALEVANM-MIN                         
889401                                   W-DALEVANM-B1-MIN                      
889501                                   W-DALEVANM-MAX                         
889601                                   W-DALEVANM-B1-MAX                      
889701           END-IF                                                         
889801         END-IF                                                           
889901       END-IF                                                             
930001     END-IF                                                               
940001                                                                          
950001     .                                                                    
960001     EJECT                                                                
970001                                                                          
980001 BC-KOLLA-KDANMORS   SECTION.                                             
990001                                                                          
000001     MOVE MFS-RENSA-FAELT       TO MOD-KDANMORS-IN                        
010001                                                                          
011001     IF EGEN-MID                                                          
020001       IF MID-KDANMORS-IN       = ALL '+'                                 
030001         MOVE MID-KDANMORS-UT   TO WS-KDANMORS                            
050001       ELSE                                                               
060001         MOVE MID-KDANMORS-IN TO WS-KDANMORS                              
080001         MOVE '7'               TO MFS-IDPFK                              
090001         MOVE SPACE             TO MFS-KDTRTYP                            
100101       END-IF                                                             
101001     ELSE                                                                 
102001       MOVE SPACE               TO WS-KDANMORS                            
103001     END-IF                                                               
110001                                                                          
120001     IF WS-KDANMORS             NUMERIC                                   
130001       MOVE WS-KDANMORS         TO W-KDANMORS-MIN                         
150001                                   W-KDANMORS-MAX                         
170001     END-IF                                                               
180001                                                                          
190001     .                                                                    
200001     EJECT                                                                
210001 BD-KOLLA-KDKREBEH   SECTION.                                             
220001                                                                          
230001     MOVE MFS-RENSA-FAELT       TO MOD-KDKREBEH-IN                        
240001                                                                          
241001     IF EGEN-MID                                                          
250001       IF MID-KDKREBEH-IN       = ALL '+'                                 
260001         MOVE MID-KDKREBEH-UT   TO WS-KDKREBEH                            
270001       ELSE                                                               
280001         MOVE MID-KDKREBEH-IN   TO WS-KDKREBEH                            
290001         MOVE '7'               TO MFS-IDPFK                              
300001         MOVE SPACE             TO MFS-KDTRTYP                            
310001       END-IF                                                             
311001     ELSE                                                                 
312001       MOVE SPACE               TO WS-KDKREBEH                            
313001     END-IF                                                               
320001                                                                          
330001     IF WS-KDKREBEH             NOT = SPACE                               
340001        MOVE WS-KDKREBEH        TO W-KDKREBEH-MIN                         
360001                                   W-KDKREBEH-MAX                         
380001     END-IF                                                               
390001                                                                          
400001     .                                                                    
410001     EJECT                                                                
420001 BE-KOLLA-FLSUM      SECTION.                                             
430001                                                                          
440001     MOVE MFS-RENSA-FAELT       TO MOD-FLSUM-IN                           
450001                                                                          
460001     IF MID-FLSUM-IN            = ALL '+'                                 
470001       MOVE MID-FLSUM-UT        TO WS-FLSUM                               
480001     ELSE                                                                 
490001       MOVE MID-FLSUM-IN        TO WS-FLSUM                               
500001       MOVE '7'                 TO MFS-IDPFK                              
510001       MOVE SPACE               TO MFS-KDTRTYP                            
520001     END-IF                                                               
530001                                                                          
540001     .                                                                    
550001     EJECT                                                                
560000 C-FOERSTA-SIDA SECTION.                                                  
570000                                                                          
580000     MOVE INF-FIRST-PAGE TO MED-IDMFSINF                                  
590000     CALL WMEDKONV USING MED-WMEDAREA                                     
600000     MOVE MED-MFSINF     TO MOD-TEMFSFEL                                  
610000                                                                          
620000*    --- BLANKA/NOLLA UT BLÄDDRINGSNYCKEL                                 
630000     PERFORM MFS-RENSA-FAELT-IN                                           
640000     .                                                                    
650000     EJECT                                                                
660000 D-NAESTA-SIDA SECTION.                                                   
670000                                                                          
680001     IF MSGI-IDTRANS-OLD            =  '4721'                             
690001*       MOVE MSGI-SSA-KEY-NEXT      TO W-MINKEY-WDA2B1KY                  
691001        MOVE MSGI-SPAR-AREA         TO W-MINKEY-WDA2B1KY                  
700001        MOVE W-MINKEYB1-IDFTG       TO W-IDFTG-B1-MIN                     
701001        MOVE W-MINKEYB1-KDARBTYP    TO W-KDARBTYP-B1-MIN                  
710001        MOVE W-MINKEYB1-IDPERSON    TO W-IDPERSON-B1-MIN                  
720001        MOVE W-MINKEYB1-DALEVANM    TO W-DALEVANM-B1-MIN                  
730001        MOVE W-MINKEYB1-IDDISTR     TO W-IDDISTR-B1-MIN                   
740001        MOVE W-MINKEYB1-IDKUNDNR    TO W-IDKUNDNR-B1-MIN                  
750001        MOVE W-MINKEYB1-IDRAPPNR    TO W-IDRAPPNR-B1-MIN                  
760001        MOVE W-MINKEYB1-IDARTNR     TO W-IDARTNR-B1-MIN                   
770001        MOVE W-MINKEYB1-IDRADNR     TO W-IDRADNR-B1-MIN                   
780000     ELSE                                                                 
790001        MOVE LOW-VALUE              TO W-WDA2B1KY-MIN-X                   
800000        PERFORM MFS-RENSA-FAELT-IN                                        
810000     END-IF                                                               
820000     .                                                                    
830000     EJECT                                                                
840000 E-SAMMA-SIDA SECTION.                                                    
850000                                                                          
851001     IF MSGI-IDTRANS-OLD            =  '4721' AND                         
860001       (EGEN-MID OR HELP-MID)                                             
870001*       MOVE MSGI-SSA-KEY-ENTER     TO W-MINKEY-WDA2B1KY                  
871001        MOVE MSGI-SPAR-AREA         TO W-MINKEY-WDA2B1KY                  
880001        MOVE W-MINKEYB1-IDFTG       TO W-IDFTG-B1-MIN                     
881001        MOVE W-MINKEYB1-KDARBTYP    TO W-KDARBTYP-B1-MIN                  
890001        MOVE W-MINKEYB1-IDPERSON    TO W-IDPERSON-B1-MIN                  
900001        MOVE W-MINKEYB1-DALEVANM    TO W-DALEVANM-B1-MIN                  
910001        MOVE W-MINKEYB1-IDDISTR     TO W-IDDISTR-B1-MIN                   
920001        MOVE W-MINKEYB1-IDKUNDNR    TO W-IDKUNDNR-B1-MIN                  
930001        MOVE W-MINKEYB1-IDRAPPNR    TO W-IDRAPPNR-B1-MIN                  
940001        MOVE W-MINKEYB1-IDARTNR     TO W-IDARTNR-B1-MIN                   
950001        MOVE W-MINKEYB1-IDRADNR     TO W-IDRADNR-B1-MIN                   
960001        IF MID-INPUT                =  ALL '+'                            
970000           PERFORM MFS-RENSA-FAELT-IN                                     
980000        ELSE                                                              
990001           MOVE +1                  TO INDX                               
000001           PERFORM UNTIL INDX       >  MAX-INDX                           
010001              IF MID-KDCMD(INDX) NUMERIC                                  
020001                 PERFORM EA-STARTA-ANNAN-BILD                             
030001                 MOVE JA            TO SW-STARTA-ANNAN-BILD               
040001                 MOVE MAX-INDX      TO INDX                               
050001              END-IF                                                      
060001              ADD +1                TO INDX                               
070001           END-PERFORM                                                    
160001        END-IF                                                            
170000     ELSE                                                                 
180001        MOVE LOW-VALUE           TO W-WDA2B1KY-MIN-X                      
190000        PERFORM MFS-RENSA-FAELT-IN                                        
200000     END-IF                                                               
210000     .                                                                    
220000     EJECT                                                                
230001 EA-STARTA-ANNAN-BILD  SECTION.                                           
240001                                                                          
241001     INSPECT MID-IDDISTR(INDX)  REPLACING LEADING SPACE BY ZERO           
242001     INSPECT MID-IDKUNDNR(INDX) REPLACING LEADING SPACE BY ZERO           
242101     INSPECT MID-IDRAPPNR(INDX) REPLACING LEADING SPACE BY ZERO           
243001     INSPECT MID-IDARTNR(INDX)  REPLACING LEADING SPACE BY ZERO           
244001     INSPECT MID-IDRADNR(INDX)  REPLACING LEADING SPACE BY ZERO           
250001     MOVE MID-IDDISTR(INDX)      TO MSGI-IDDISTR                          
260001     MOVE MID-IDKUNDNR(INDX)     TO MSGI-IDKUNDNR                         
270001     MOVE MID-IDRAPPNR(INDX)     TO MSGI-IDRAPPNR                         
271001     MOVE ZERO                   TO MSGI-IDARTNR (1:1)                    
271101     MOVE MID-IDARTNR(INDX)      TO MSGI-IDARTNR (2:8)                    
272001     MOVE MID-IDRADNR(INDX)      TO MSGI-IDRADNR                          
280001     MOVE '001'                  TO MSGI-KDCALL                           
290001     MOVE MSG-SIGNON-USERID      TO MSGI-IDUSER                           
300001     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
310001                                                                          
320001     MOVE LOW-VALUE              TO P-TO-P-KDZ1                           
330001     MOVE LOW-VALUE              TO P-TO-P-KDZ2                           
340001     MOVE MID-KDCMD(INDX) (1:1)  TO W-HOPP-IDTRANS-2                      
350001     MOVE MID-KDCMD(INDX) (2:3)  TO W-HOPP-IDTRANS-4-6                    
360001     MOVE W-HOPP-IDTRANS         TO P-TO-P-KDTRANS                        
370001     MOVE '4721'                 TO P-TO-P-IDTRANS                        
380001     MOVE MFS-KDMFSFOR           TO P-TO-P-KDMFSFOR                       
390001                                                                          
400001     PERFORM S01-INSERT-ALTMSG                                            
410001     .                                                                    
420001     EJECT                                                                
620000 F-LAES-VISA-INFO SECTION.                                                
630000                                                                          
640001                                                                          
650001     MOVE ZERO         TO W-KVRADER-RT                                    
670001                                                                          
680001     PERFORM IMS-GU-WLKREG01                                              
690001     PERFORM FA-FIXA-ENTER-KEY                                            
700000                                                                          
710000     IF SEGMENT-SAKNAS                                                    
711001        PERFORM FD-FIXA-NEXT-KEY                                          
720001        MOVE ERR-INFO-MISSING   TO MED-IDMFSFEL                           
730000        CALL WMEDKONV USING MED-WMEDAREA                                  
740000        MOVE MED-MFSFEL         TO MOD-TEMFSFEL                           
750000        PERFORM MFS-RENSA-FAELT-UT                                        
760000     ELSE                                                                 
770000       MOVE +1                  TO INDX                                   
780000                                                                          
790001       PERFORM UNTIL INDX        > MAX-INDX                               
800000         IF SEGMENT-FINNS                                                 
810001                                                                          
820001            ADD +1               TO W-KVRADER-RT                          
840001                                                                          
850001            PERFORM FB-REDIGERA-MOD                                       
860000                                                                          
870001            PERFORM IMS-GN-WLKREG01                                       
880001            ADD +1               TO INDX                                  
890001         ELSE                                                             
900001            PERFORM FC-RENSA-RAD                                          
910001            ADD +1               TO INDX                                  
920001         END-IF                                                           
930000       END-PERFORM                                                        
940000                                                                          
950001       PERFORM FD-FIXA-NEXT-KEY                                           
960001       IF SEGMENT-FINNS                                                   
961001          IF WS-FLSUM = JA OR YES                                         
970001             PERFORM FE-ADDERA-RT                                         
980001          END-IF                                                          
981001       END-IF                                                             
990001                                                                          
000001       IF WS-FLSUM =  JA OR YES                                           
010001          MOVE W-KVRADER-RT    TO MOD-KVRADER-RT                          
030001       END-IF                                                             
050000     END-IF                                                               
060000     MOVE '002'                TO MSGI-KDCALL                             
070001     MOVE '4721'               TO MSGI-IDTRANS                            
080000     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
090000     .                                                                    
100000     EJECT                                                                
110000                                                                          
120000 FA-FIXA-ENTER-KEY        SECTION.                                        
130000                                                                          
140000     IF SEGMENT-FINNS                                                     
150001        MOVE SEQB-IDFTG             TO W-MINKEYB1-IDFTG                   
151001        MOVE SEQB-KDARBTYP          TO W-MINKEYB1-KDARBTYP                
160001        MOVE SEQB-IDPERSON          TO W-MINKEYB1-IDPERSON                
180001        MOVE SEQB-DALEVANM          TO W-MINKEYB1-DALEVANM                
200001        MOVE SEQB-IDDISTR           TO W-MINKEYB1-IDDISTR                 
210001        MOVE SEQB-IDKUNDNR          TO W-MINKEYB1-IDKUNDNR                
220001        MOVE SEQB-IDRAPPNR          TO W-MINKEYB1-IDRAPPNR                
221001        MOVE SEQB-IDARTNR           TO W-MINKEYB1-IDARTNR                 
222001        MOVE SEQB-IDRADNR           TO W-MINKEYB1-IDRADNR                 
230001     ELSE                                                                 
240001        MOVE MSGI-IDFTG             TO W-MINKEYB1-IDFTG                   
241001        MOVE MSGI-KDARBTYP          TO W-MINKEYB1-KDARBTYP                
250001        MOVE MSGI-IDPERSON          TO W-MINKEYB1-IDPERSON                
260001        MOVE ZERO                   TO W-MINKEYB1-DALEVANM                
270001                                       W-MINKEYB1-IDDISTR                 
280001                                       W-MINKEYB1-IDKUNDNR                
290001                                       W-MINKEYB1-IDRAPPNR                
310001                                       W-MINKEYB1-IDARTNR                 
311001                                       W-MINKEYB1-IDRADNR                 
320001     END-IF                                                               
330001*    MOVE W-MINKEY-WDA2B1KY         TO MSGI-SSA-KEY-ENTER                 
331001     MOVE W-MINKEY-WDA2B1KY         TO MSGI-SPAR-AREA                     
340000                                                                          
350000     .                                                                    
360000     EJECT                                                                
370001                                                                          
380001 FB-REDIGERA-MOD         SECTION.                                         
390001                                                                          
400001     MOVE SEQB-KDARBTYP      TO MOD-IDANSV   (INDX) (1:3)                 
410001     MOVE SEQB-IDPERSON      TO W-IDPERSON                                
420001     MOVE W-IDPERSON         TO MOD-IDANSV   (INDX) (4:3)                 
430001     MOVE SEQB-DALEVANM(3:6) TO MOD-TILEVANM (INDX)                       
440001                                                                          
510001     MOVE SEQB-IDDISTR       TO MOD-IDDISTR  (INDX)                       
520001     MOVE SEQB-IDKUNDNR      TO MOD-IDKUNDNR (INDX)                       
530001     MOVE SEQB-IDRAPPNR      TO MOD-IDRAPPNR (INDX)                       
540101     MOVE SEQB-IDORDNR7      TO MOD-IDORDNR5 (INDX)                       
550001     MOVE SEQB-IDRADNR       TO MOD-IDRADNR  (INDX)                       
560001     MOVE SEQB-IDARTNR       TO MOD-IDARTNR  (INDX)                       
570001     MOVE SEQB-KVLEVANM-BEKR TO MOD-KVLEVANM-BEKR (INDX)                  
580001     MOVE SEQB-KDANMORS      TO MOD-KDANMORS (INDX)                       
590001     MOVE SEQB-FLTEXT        TO MOD-FLTEXT   (INDX)                       
591001     IF SEQB-FLTEXT           = JA                                        
592001       IF MSGI-IDLAND-SPR = 'GB'                                          
593001         MOVE YES               TO MOD-FLTEXT   (INDX)                    
594001       ELSE                                                               
595001         MOVE SEQB-FLTEXT       TO MOD-FLTEXT   (INDX)                    
596001       END-IF                                                             
597001     ELSE                                                                 
598001       MOVE SEQB-FLTEXT         TO MOD-FLTEXT   (INDX)                    
599001     END-IF                                                               
599101                                                                          
600001     MOVE SEQB-KDKREBEH      TO MOD-KDKREBEH (INDX)                       
600101                                                                          
601001     IF SEQB-KDKREBEH = 'P  '                                             
602001       MOVE 'Q  '            TO MOD-KDKREBEH (INDX)                       
603001     END-IF                                                               
610001     .                                                                    
620001     EJECT                                                                
630001                                                                          
830001                                                                          
840001 FC-RENSA-RAD SECTION.                                                    
850001                                                                          
860001     MOVE MFS-STAENG-FAELT TO MOD-KDCMD-ATTR   (INDX)                     
861001     MOVE MFS-RENSA-FAELT  TO MOD-IDANSV   (INDX)                         
870001                              MOD-TILEVANM (INDX)                         
890001                              MOD-IDDISTR  (INDX)                         
900001                              MOD-IDKUNDNR (INDX)                         
910001                              MOD-IDRAPPNR (INDX)                         
920001                              MOD-IDORDNR5 (INDX)                         
930001                              MOD-IDRADNR  (INDX)                         
940001                              MOD-IDARTNR  (INDX)                         
941001                              MOD-IDARTNR  (INDX)                         
942001                              MOD-KDANMORS (INDX)                         
943001                              MOD-FLTEXT   (INDX)                         
944001                              MOD-KDKREBEH (INDX)                         
950001     .                                                                    
960001     EJECT                                                                
970001                                                                          
980001 FD-FIXA-NEXT-KEY        SECTION.                                         
990000                                                                          
000000     IF SEGMENT-FINNS                                                     
010000        MOVE INF-MORE-INFO-EXISTS   TO MED-IDMFSINF                       
020000        CALL WMEDKONV USING MED-WMEDAREA                                  
030000        MOVE MED-TEMFSINF           TO MOD-TEMFSINF                       
040000                                                                          
041001        MOVE SEQB-IDFTG             TO W-MINKEYB1-IDFTG                   
041101        MOVE SEQB-KDARBTYP          TO W-MINKEYB1-KDARBTYP                
042001        MOVE SEQB-IDPERSON          TO W-MINKEYB1-IDPERSON                
043001        MOVE SEQB-DALEVANM          TO W-MINKEYB1-DALEVANM                
044001        MOVE SEQB-IDDISTR           TO W-MINKEYB1-IDDISTR                 
045001        MOVE SEQB-IDKUNDNR          TO W-MINKEYB1-IDKUNDNR                
046001        MOVE SEQB-IDRAPPNR          TO W-MINKEYB1-IDRAPPNR                
047001        MOVE SEQB-IDARTNR           TO W-MINKEYB1-IDARTNR                 
048001        MOVE SEQB-IDRADNR           TO W-MINKEYB1-IDRADNR                 
049001     ELSE                                                                 
049101        MOVE MSGI-IDFTG             TO W-MINKEYB1-IDFTG                   
049201        MOVE MSGI-KDARBTYP          TO W-MINKEYB1-KDARBTYP                
049301        MOVE MSGI-IDPERSON          TO W-MINKEYB1-IDPERSON                
049401        MOVE ZERO                   TO W-MINKEYB1-DALEVANM                
049501                                       W-MINKEYB1-IDDISTR                 
049601                                       W-MINKEYB1-IDKUNDNR                
049701                                       W-MINKEYB1-IDRAPPNR                
049801                                       W-MINKEYB1-IDARTNR                 
049901                                       W-MINKEYB1-IDRADNR                 
220001     END-IF                                                               
230001     MOVE W-MINKEY-WDA2B1KY         TO MSGI-SPAR-AREA                     
240000                                                                          
250000     .                                                                    
260000     EJECT                                                                
270001 FE-ADDERA-RT  SECTION.                                                   
280001                                                                          
290001     PERFORM UNTIL SEGMENT-SAKNAS                                         
300001        ADD +1               TO W-KVRADER-RT                              
320001                                                                          
330001        PERFORM IMS-GN-WLKREG01                                           
340001     END-PERFORM                                                          
350001                                                                          
360001     .                                                                    
370001     EJECT                                                                
960001 S01-INSERT-ALTMSG SECTION.                                               
970001                                                                          
980001     MOVE P-TO-P-SW            TO MSG-IO-AREA                             
990001     PERFORM IMS-CHANGE-ALTMSG                                            
000001     IF STATUS-OK                                                         
010001       PERFORM IMS-INSERT-ALTMSG                                          
020001     ELSE                                                                 
030001       MOVE LOW-VALUE          TO MSG-AREA                                
040001       MOVE 'W4O72101'         TO MFS-IDMOD                               
050001       MOVE '4721'             TO MOD-IDTRANS                             
060001       MOVE P-TO-P-KDTRANS (2:1) TO W-BILD (1:1)                          
070001       MOVE P-TO-P-KDTRANS (4:3) TO W-BILD (2:3)                          
080001       IF SECURITY-FEL                                                    
090001         STRING 'NOT AUTHORIZED TO USE '                                  
100001                W-BILD                                                    
110001                DELIMITED BY SIZE INTO MOD-TEMFSINF                       
120001       ELSE                                                               
130001         STRING 'WRONG PICTURE '                                          
140001                 W-BILD                                                   
150001                DELIMITED BY SIZE INTO MOD-TEMFSINF                       
160001       END-IF                                                             
170001       COMPUTE MSG-KVLL = LENGTH OF MOD-W4O72101 + 4                      
180001       PERFORM MFS-ROER-EJ-FAELT-IN                                       
190001       PERFORM MFS-ROER-EJ-FAELT-UT                                       
200001       PERFORM IMS-INSERT-MSG                                             
210001     END-IF                                                               
220001     .                                                                    
230001     EJECT                                                                
240000 MFS-RENSA-FAELT-UT SECTION.                                              
250000                                                                          
260001     MOVE MFS-RENSA-FAELT       TO MOD-KVRADER-RT                         
280001                                                                          
290000     MOVE +1                    TO INDX                                   
300000     PERFORM UNTIL INDX         >  MAX-INDX                               
310000        PERFORM MFS-RENSA-RAD-FAELT-UT                                    
320000        ADD +1                  TO INDX                                   
330000     END-PERFORM                                                          
340000     .                                                                    
350000     SKIP3                                                                
360000 MFS-RENSA-RAD-FAELT-UT SECTION.                                          
370000                                                                          
371001     MOVE MFS-RENSA-FAELT TO MOD-IDANSV   (INDX)                          
372001                             MOD-TILEVANM (INDX)                          
373001                             MOD-IDDISTR  (INDX)                          
374001                             MOD-IDKUNDNR (INDX)                          
375001                             MOD-IDRAPPNR (INDX)                          
376001                             MOD-IDORDNR5 (INDX)                          
377001                             MOD-IDRADNR  (INDX)                          
378001                             MOD-IDARTNR  (INDX)                          
379001                             MOD-IDARTNR  (INDX)                          
379101                             MOD-KDANMORS (INDX)                          
379201                             MOD-FLTEXT   (INDX)                          
379301                             MOD-KDKREBEH (INDX)                          
470000     .                                                                    
480000     SKIP3                                                                
490000 MFS-RENSA-FAELT-IN SECTION.                                              
500000                                                                          
510000*    --- ALLA INDATA-FÄLT                                                 
530001                                                                          
540000     MOVE +1 TO INDX                                                      
550000     PERFORM UNTIL INDX         >  MAX-INDX                               
560000       MOVE MFS-RENSA-FAELT     TO MOD-KDCMD(INDX)                        
570000       ADD +1                   TO INDX                                   
580000     END-PERFORM                                                          
590000     .                                                                    
600000     EJECT                                                                
610000 MFS-ROER-EJ-FAELT-UT  SECTION.                                           
620000                                                                          
630001     MOVE MFS-ROER-EJ-FAELT     TO MOD-KVRADER-RT                         
650001                                                                          
660000     MOVE +1                  TO INDX                                     
670000     PERFORM UNTIL INDX       >  MAX-INDX                                 
680000       PERFORM MFS-ROER-EJ-RAD-FAELT-UT                                   
690000       ADD +1                 TO INDX                                     
700000     END-PERFORM                                                          
710000     .                                                                    
720000                                                                          
730000                                                                          
740000 MFS-ROER-EJ-RAD-FAELT-UT  SECTION.                                       
750000                                                                          
760001     MOVE MFS-ROER-EJ-FAELT  TO MOD-IDANSV   (INDX)                       
761001                                MOD-TILEVANM (INDX)                       
762001                                MOD-IDDISTR (INDX)                        
763001                                MOD-IDKUNDNR (INDX)                       
764001                                MOD-IDRAPPNR (INDX)                       
765001                                MOD-IDORDNR5 (INDX)                       
766001                                MOD-IDRADNR (INDX)                        
767001                                MOD-IDARTNR (INDX)                        
768001                                MOD-IDARTNR (INDX)                        
769001                                MOD-KDANMORS (INDX)                       
769101                                MOD-FLTEXT (INDX)                         
769201                                MOD-KDKREBEH (INDX)                       
850000     .                                                                    
860000                                                                          
870000 MFS-ROER-EJ-FAELT-IN  SECTION.                                           
880000                                                                          
890000*    --- ALLA INDATA-FÄLT                                                 
910001                                                                          
920000     MOVE +1 TO INDX                                                      
930000     PERFORM UNTIL INDX         >  MAX-INDX                               
940000       MOVE MFS-ROER-EJ-FAELT   TO MOD-KDCMD(INDX)                        
950000       ADD +1                   TO INDX                                   
960000     END-PERFORM                                                          
970000     .                                                                    
980000     EJECT                                                                
110000                                                                          
120000* --- IMS SEKTIONER ---                                                   
130000     SKIP3                                                                
140000 IMS-GET-MSG SECTION.                                                     
150000                                                                          
160000     MOVE '  QC' TO GODK-STATUSKODER                                      
170000     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
180000     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
190000     PERFORM IMS-STATUSKONTROLL                                           
200000     .                                                                    
210000     SKIP3                                                                
220000 IMS-INSERT-MSG SECTION.                                                  
230000                                                                          
241001     IF MSGI-IDLAND-SPR = 'GB'                                            
250000       MOVE 'N' TO MFS-KDHUVOMR                                           
260000     END-IF                                                               
270000     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
280000     MOVE SPACE TO GODK-STATUSKODER                                       
290000     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
300000     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
310000     PERFORM IMS-STATUSKONTROLL                                           
320000     .                                                                    
330000     EJECT                                                                
340001                                                                          
440001 IMS-CHANGE-ALTMSG SECTION.                                               
450001     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
460001     MOVE '  A1A4' TO GODK-STATUSKODER                                    
470001     CALL CBLTDLI USING CHNG ALT-PCB MSG-KDTRANS-1                        
480001     MOVE ALT-STATUS-CODE TO STATUS-WS                                    
490001     PERFORM IMS-STATUSKONTROLL                                           
500001     .                                                                    
510001     SKIP3                                                                
520001 IMS-INSERT-ALTMSG SECTION.                                               
530001     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
540001     MOVE SPACE TO GODK-STATUSKODER                                       
550001     CALL CBLTDLI USING ISRT ALT-PCB MSG-IO-AREA                          
560001     MOVE ALT-STATUS-CODE TO STATUS-WS                                    
570001     PERFORM IMS-STATUSKONTROLL                                           
580001     .                                                                    
590001     EJECT                                                                
780001                                                                          
790001 IMS-GU-WLKREG01       SECTION.                                           
800001                                                                          
810001     STRING 'WLKREG01(WDA2B1KY>=' W-WDA2B1KY-MIN-X                        
820001                    '&WDA2B1KY<=' W-WDA2B1KY-MAX-X                        
830001                    '&DALEVANM>=' W-DALEVANM-MIN-X                        
840001                    '&DALEVANM<=' W-DALEVANM-MAX-X                        
850001                    '&KDANMORS>=' W-KDANMORS-MIN-X                        
860001                    '&KDANMORS<=' W-KDANMORS-MAX-X ')'                    
861001                    '&KDKREBEH>=' W-KDKREBEH-MIN-X                        
862001                    '&KDKREBEH<=' W-KDKREBEH-MAX-X ')'                    
870001          DELIMITED BY SIZE INTO SSA1                                     
880001     MOVE '  GE'           TO GODK-STATUSKODER                            
890001     CALL CBLTDLI USING GU KREG-PCB DLI-IO-AREA SSA1                      
900001     MOVE KREG-STATUS-CODE TO STATUS-WS                                   
910001     PERFORM IMS-STATUSKONTROLL                                           
920001     .                                                                    
930001                                                                          
940001 IMS-GN-WLKREG01       SECTION.                                           
950001                                                                          
960001     STRING 'WLKREG01(WDA2B1KY>=' W-WDA2B1KY-MIN-X                        
970001                    '&WDA2B1KY<=' W-WDA2B1KY-MAX-X                        
971001                    '&DALEVANM>=' W-DALEVANM-MIN-X                        
972001                    '&DALEVANM<=' W-DALEVANM-MAX-X                        
973001                    '&KDANMORS>=' W-KDANMORS-MIN-X                        
974001                    '&KDANMORS<=' W-KDANMORS-MAX-X ')'                    
975001                    '&KDKREBEH>=' W-KDKREBEH-MIN-X                        
976001                    '&KDKREBEH<=' W-KDKREBEH-MAX-X ')'                    
020001          DELIMITED BY SIZE INTO SSA1                                     
030001     MOVE '  GEGB'           TO GODK-STATUSKODER                          
040001     CALL CBLTDLI USING GN KREG-PCB DLI-IO-AREA SSA1                      
050001     MOVE KREG-STATUS-CODE TO STATUS-WS                                   
060001     PERFORM IMS-STATUSKONTROLL                                           
070001     .                                                                    
080001     EJECT                                                                
090001                                                                          
210001                                                                          
220000 IMS-STATUSKONTROLL SECTION.                                              
230000                                                                          
240000     SET STATUS-IX TO 1                                                   
250000     SEARCH GODK-STATUS                                                   
260000       AT END                                                             
270000         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
280000         DELIMITED BY SIZE INTO FELTEXT                                   
290000         CALL FELLOG                                                      
300000       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
310000         CONTINUE                                                         
320000     END-SEARCH                                                           
330000     .                                                                    
