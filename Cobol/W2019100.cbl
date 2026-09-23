010000 ID DIVISION.                                                             
020000     SKIP2                                                                
030000 PROGRAM-ID.     W2019100.                                                
040000*AUTHOR.         STEFAN KIHLBERG.                                         
050000*DATE-WRITTEN.   92/10/13.                                                
060000                                                                          
070000*    REMARKS.                                                             
080000*                                                                         
090000*    FUNKTION:                                                            
100000*        BAKGRUNDS-MPP                                                    
110000*        TAR IMOT MID FRÅN 6203                                           
120000*                 MID FRÅN W21412 VIA DISPATCHER W006KOM                  
130000*                 MID FRÅN W22114 VIA DISPATCHER W006KOM                  
140000*                                                                         
150000*        UPPDATERAR LARM PÅ WDR5                                          
160000*        PROGRAMMET UPPDATERAR WLXXBU (WDR5)                              
170000*                   LÄSER      WLXXBX (WDR2)                              
180000*                                                                         
190000*    INDATA.                                                              
200000*        TRANSAKTION: W2T191                                              
210000*        MID:         W2I19101                                            
220000*        MOD:         WMSGKOM   (DISPATCHERN)                             
230000*                                                                         
240000*    ÄNDRINGAR:                                                           
250000*        ETRACKER SCR 695521.                                             
260000*        LARM 222 BORTTAGES NÄR LARM 210 SKAPAS. //050127 L.A.            
270000*                                                                         
270400*        20160629                                                         
270500*        ETRACKER: 10273782 BACKORDER ALARM TAKES AWAY 223-ALARM          
270600*        INGER STENING                                                    
270700*                                                                         
280000                                                                          
290000     SKIP3                                                                
300000 ENVIRONMENT DIVISION.                                                    
310000     EJECT                                                                
320000 DATA DIVISION.                                                           
330000 WORKING-STORAGE SECTION.                                                 
340000                                                                          
350000*    -- CHECKED BY WY2000                                                 
360000*                                                                         
370000 77  IDPGM                       PIC X(08)   VALUE 'W2019100'.            
380000                                                                          
390000*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
400000 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
410000                                                                          
420000 77  JA                          PIC X       VALUE 'J'.                   
430000 77  NEJ                         PIC X       VALUE 'N'.                   
440000                                                                          
450000 77  SPRAK-IX                    PIC S9(9)  VALUE +0    COMP SYNC.        
460000                                                                          
470000 77  WS-IDDISTR                  PIC 9(4)   VALUE ZERO.                   
480000 77  WS-IDKUNDNR                 PIC 9(6)   VALUE ZERO.                   
490000                                                                          
500000*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
510000                                                                          
520000 77  INDATA-SW                   PIC X       VALUE 'J'.                   
530000     88  INDATA-OK                           VALUE 'J'.                   
540000     88  INDATA-FEL                          VALUE 'N'.                   
550000                                                                          
560000 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
570000     88  NYCKLAR-OK                          VALUE 'J'.                   
580000     88  NYCKLAR-FEL                         VALUE 'N'.                   
590000                                                                          
600000 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
610000     88  EGEN-MID                            VALUE '2191'.                
620000     88  GODK-MID               VALUE '4160', '4293',                     
630000                                      '4304', '4354',                     
640000                      '4375', '4397', '4398', '5108', '6203'.             
650000     88  HELP-MID                            VALUE '0551'.                
660000     88  DISPATCH-TRANS                      VALUE '2191'.                
670000                                                                          
680000 01  WS-TISENBEK-KL-TEST.                                                 
690000     03  WS-TISENBEK-KL-HH      PIC 9(2)     VALUE ZERO.                  
700000     03  WS-TISENBEK-KL-MM      PIC 9(2)     VALUE ZERO.                  
710000     03  WS-TISENBEK-KL-SS      PIC 9(2)     VALUE ZERO.                  
720000     EJECT                                                                
730000                                                                          
740000 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
750000 01  FILLER REDEFINES DAGENS-DATUM.                                       
760000     03  DAGENS-DATUM-AAR        PIC 9(2).                                
770000     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
780000     03  DAGENS-DATUM-DAG        PIC 9(2).                                
790000                                                                          
800000 01  ARBETSAREOR.                                                         
810000     03  WS-KDLARM               PIC 9(3).                                
820000     03  WS-IDARTNR              PIC 9(9).                                
830000     SKIP3                                                                
840000*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
850000 01  GENERELLA-SUBPROGRAM.                                                
860000     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
870000     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
880000     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
890000     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
900000     03  W009CIA                 PIC X(8)    VALUE 'W009CIA '.            
910000     EJECT                                                                
920000*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
930000*01 -COPY WMEDAREA                                                        
940000     SKIP3                                                                
950000 01  MESSAGE-CODES.                                                       
960000     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
970000     03  INF-PRESS-PF11          PIC X(3)    VALUE '003'.                 
980000     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '011'.                 
990000     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
000000     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
010000     EJECT                                                                
020000*    --- PARAMETRAR TILL SUBPROGRAM W009CIA                               
030000*01 -COPY W009CIA                                                         
040000     EJECT                                                                
050000 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
060000     SKIP3                                                                
070000*01  MID -COPY W2I19101                                                   
080000     EJECT                                                                
090000 01  FILLER                      PIC X(16)  VALUE 'MSG-KOM-AREA'.         
100000     SKIP3                                                                
110000*01  -COPY WMSGKOM                                                        
120000     EJECT                                                                
130000*01  -COPY WMSGAREA.                                                      
140000     EJECT                                                                
150000*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
160000*                                                                         
170000     EJECT                                                                
180000 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
190000     SKIP2                                                                
200000 01  NYCKLAR-TILL-DLI.                                                    
210000     03  W-IDDC-X.                                                        
220000         05  W-IDDC              PIC X(2)    VALUE SPACE.                 
230000                                                                          
240000     03  W-WDGX2223-X.                                                    
250000         05  W-IDHTYP            PIC X(4)    VALUE '2223'.                
260000         05  W-IDANSK            PIC S9(3)   VALUE ZERO COMP-3.           
270000         05  W-VALFRI            PIC X(24)   VALUE LOW-VALUE.             
280000                                                                          
290000     03  W-WDGX2224-X.                                                    
300000         05  W-TISENBEK-DAG      PIC S9(7)   VALUE ZERO COMP-3.           
310000         05  W-TISENBEK-KL       PIC S9(7)   VALUE ZERO COMP-3.           
320000         05  W-KDLARM            PIC S9(3)   VALUE ZERO COMP-3.           
330000     03  W-KDLARM-X.                                                      
340000         05  W-KDLARM-S          PIC S9(3)   VALUE ZERO COMP-3.           
350000     03  W-IDARTNR-X.                                                     
360000         05  W-IDARTNR-S         PIC S9(9)   VALUE ZERO COMP-3.           
370000                                                                          
380000     03  W-WDGXKEY-2231-X.                                                
390000         05  FILLER              PIC X(4)    VALUE '2231'.                
400000         05  FILLER              PIC X(26)   VALUE LOW-VALUE.             
410000     03  W-WDGXKEY-2232-X.                                                
420000         05  W-IDANSK-L          PIC S9(3)   VALUE ZERO COMP-3.           
430000         05  FILLER              PIC X(3)    VALUE LOW-VALUE.             
440000                                                                          
450000     SKIP2                                                                
460000*    --- STATUS-KOD FRÅN IMS                                              
470000 01  STATUS-WS                   PIC XX.                                  
480000     88  SEGMENT-FINNS                       VALUE '  '.                  
490000     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
500000     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
510000     SKIP2                                                                
520000 01  GODK-STATUSKODER.                                                    
530000     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
540000     SKIP3                                                                
550000 01  SSA1                        PIC X(96).                               
560000 01  SSA2                        PIC X(96).                               
570000     EJECT                                                                
580000*    --- IMS FUNKTIONSKODER                                               
590000*01  -COPY W0003                                                          
600000     EJECT                                                                
610000*    ---  DLI INPUT-OUTPUT AREA                                           
620000 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
630000     SKIP3                                                                
640000 01  DLI-IO-AREA.                                                         
650000     03  IO-AREA                 PIC X(150)  VALUE SPACE.                 
660000     SKIP3                                                                
670000     03  WLXXBU01 REDEFINES IO-AREA.                                      
680000*        05  -COPY WDGX2223                                               
690000     SKIP3                                                                
700000     03  WLXXBU11 REDEFINES IO-AREA.                                      
710000*        05  -COPY WDGX2224                                               
720000                                                                          
730000 01  FILLER                      PIC X(16)  VALUE 'DLI-IO-AREA2'.         
740000     SKIP3                                                                
750000 01  DLI-IO-AREA2.                                                        
760000     03  IO-AREA2                PIC X(150)  VALUE SPACE.                 
770000                                                                          
780000     03  WLXXBX11 REDEFINES IO-AREA2.                                     
790000*        05  -COPY WDGX2232   -PRE XXBX-                                  
800000     EJECT                                                                
810000                                                                          
820000 LINKAGE SECTION.                                                         
830000                                                                          
840000*01  -COPY W0009   -PRE MSG-                                              
850000     EJECT                                                                
860000*01  -COPY W0009 -PRE MSGKOM-                                             
870000     EJECT                                                                
880000*01  -COPY W0008  -PRE XXBU-                                              
890000     05  FILLER                  PIC X.                                   
900000     EJECT                                                                
910000*01  -COPY W0008  -PRE XXBX-                                              
920000     05  FILLER                  PIC X.                                   
930000     EJECT                                                                
940000                                                                          
950000                                                                          
960000 PROCEDURE DIVISION  USING MSG-PCB MSGKOM-PCB XXBU-PCB XXBX-PCB.          
970000     ENTRY 'DLITCBL' USING MSG-PCB MSGKOM-PCB XXBU-PCB XXBX-PCB.          
980000                                                                          
990000     PERFORM IMS-GET-MSG                                                  
000000     IF SEGMENT-FINNS                                                     
010000       PERFORM IMS-GET-WMSGKOM                                            
020000       PERFORM A-INIT                                                     
030000       IF MSG-KDTRTYP = 'X'                                               
040000         PERFORM G-HAMTA-LARMANSKAFFARE                                   
050000         PERFORM H-UPPDATERA-WDR5                                         
060000         MOVE '101'          TO MSG-KOM-IDMFSMED                          
070000       ELSE                                                               
080000          CONTINUE                                                        
090000       END-IF                                                             
100000       IF DISPATCH-TRANS                                                  
110000          PERFORM IMS-INSERT-WMSGKOM                                      
120000       END-IF                                                             
130000     END-IF                                                               
140000     MOVE ZERO TO RETURN-CODE                                             
150000     GOBACK                                                               
160000     .                                                                    
170000     EJECT                                                                
180000                                                                          
190000                                                                          
200000 A-INIT SECTION.                                                          
210000                                                                          
220000     MOVE MSG-INDATA-MINUS-1-TRANSKOD                                     
230000                             TO MID-W2I19101                              
240000     MOVE MSG-IDTRANS-1      TO W-IDTRANS                                 
250000     ACCEPT DAGENS-DATUM FROM DATE                                        
260000     .                                                                    
270000     EJECT                                                                
280000                                                                          
290000                                                                          
300000 G-HAMTA-LARMANSKAFFARE SECTION.                                          
310000                                                                          
320000     MOVE MID-IDANSK    TO W-IDANSK-L                                     
330000     PERFORM IMS-GU-XXBX-2231                                             
340000     PERFORM IMS-GNP-XXBX-2232                                            
350000     IF SEGMENT-FINNS                                                     
360000       MOVE XXBX-2232-IDANSK-LARM TO W-IDANSK                             
370000     ELSE                                                                 
380000       MOVE ZERO TO W-IDANSK                                              
390000     END-IF                                                               
400000     .                                                                    
410000     EJECT                                                                
420000                                                                          
430000                                                                          
440000 H-UPPDATERA-WDR5 SECTION.                                                
450000                                                                          
460000     IF (MID-KDLARM = '200' OR MID-KDLARM = '210' OR                      
470000         MID-KDLARM = '500' OR                                            
480000         MID-KDLARM = '221' OR MID-KDLARM = '222' ) AND                   
490000        (MID-IDANSK < '910' OR MID-IDANSK > '914')                        
500000        MOVE MID-KDLARM           TO WS-KDLARM                            
510000        MOVE WS-KDLARM            TO W-KDLARM-S                           
520000        MOVE MID-IDARTNR          TO WS-IDARTNR                           
530000        MOVE WS-IDARTNR           TO W-IDARTNR-S                          
540000        MOVE MID-IDDC             TO W-IDDC                               
550000        PERFORM IMS-GHU-XXBU-WDR550                                       
560000        IF SEGMENT-FINNS                                                  
570000           PERFORM IMS-DLET-XXBU-WDR550                                   
580000        END-IF                                                            
590000*** LARM 222 OCH 223 BORTTAGES NÄR LARM 210 SKAPAS FÖR SAMMA ARTNR        
600000        IF MID-KDLARM = '210'                                             
610000          MOVE '222'              TO WS-KDLARM                            
620000          MOVE WS-KDLARM          TO W-KDLARM-S                           
630000          MOVE MID-IDDC           TO W-IDDC                               
640000          PERFORM IMS-GHU-XXBU-WDR550                                     
650000          IF SEGMENT-FINNS                                                
660000             PERFORM IMS-DLET-XXBU-WDR550                                 
670000          END-IF                                                          
671000                                                                          
680000          MOVE '223'                TO WS-KDLARM                          
690000          MOVE WS-KDLARM            TO W-KDLARM-S                         
700000          MOVE MID-IDDC             TO W-IDDC                             
710000          PERFORM IMS-GHU-XXBU-WDR550                                     
720000          IF SEGMENT-FINNS                                                
730000             PERFORM IMS-DLET-XXBU-WDR550                                 
740000          END-IF                                                          
760000        END-IF                                                            
770000     END-IF                                                               
780000                                                                          
792300     IF MID-KDLARM = '222'                                                
792400        MOVE '223'                TO WS-KDLARM                            
792500        MOVE WS-KDLARM            TO W-KDLARM-S                           
792600        MOVE MID-IDDC             TO W-IDDC                               
792700        PERFORM IMS-GHU-XXBU-WDR550                                       
792800        IF SEGMENT-FINNS                                                  
792900           CONTINUE                                                       
793000        ELSE                                                              
793100           PERFORM HA-CREATE-ALARM                                        
793200        END-IF                                                            
793300     ELSE                                                                 
793400        PERFORM HA-CREATE-ALARM                                           
793500     END-IF                                                               
793700     .                                                                    
793800                                                                          
794000 HA-CREATE-ALARM SECTION.                                                 
795000                                                                          
800000     MOVE '2223'                  TO 2223-IDHTYP                          
810000     MOVE W-IDANSK                TO 2223-IDANSK                          
820000     MOVE LOW-VALUE               TO 2223-LOW-VALUE                       
830000     PERFORM IMS-ISRT-XXBU-WDR501                                         
840000                                                                          
850000     MOVE MID-TISENBEK-DAG        TO W-TISENBEK-DAG                       
860000     MOVE MID-TISENBEK-KL         TO W-TISENBEK-KL                        
870000     MOVE MID-KDLARM              TO W-KDLARM                             
880000                                                                          
890000     MOVE MID-IDDC                TO W-IDDC                               
900000     PERFORM IMS-GU-XXBU-WDR550-IDDC                                      
910000     IF SEGMENT-SAKNAS                                                    
920000**** LARM KAN SAKNAS FÖR MID-IDDC MEN FINNAS FÖR CDC,SAMMA NYCKEL.        
930000        PERFORM HA-FLYTTA-DATA                                            
940000        PERFORM IMS-ISRT-XXBU-WDR550                                      
950000        IF SEGMENT-FINNS-REDAN                                            
960000          PERFORM UNTIL SEGMENT-FINNS                                     
970000            ADD 1                 TO W-TISENBEK-KL                        
980000            PERFORM HB-KOLLA-TIDEN                                        
990000            MOVE W-TISENBEK-KL    TO 2224-TISENBEK-KL                     
000000            PERFORM IMS-ISRT-XXBU-WDR550                                  
010000          END-PERFORM                                                     
020000        END-IF                                                            
030000     ELSE                                                                 
040000        PERFORM UNTIL SEGMENT-SAKNAS                                      
050000           ADD 1                  TO W-TISENBEK-KL                        
060000           PERFORM HB-KOLLA-TIDEN                                         
070000           MOVE MID-IDDC          TO W-IDDC                               
080000           PERFORM IMS-GU-XXBU-WDR550-IDDC                                
090000        END-PERFORM                                                       
100000        PERFORM HA-FLYTTA-DATA                                            
110000        PERFORM IMS-ISRT-XXBU-WDR550                                      
120000        IF SEGMENT-FINNS-REDAN                                            
130000**** LARM KAN SAKNAS FÖR MID-IDDC MEN FINNAS FÖR CDC,SAMMA NYCKEL.        
140000          PERFORM UNTIL SEGMENT-FINNS                                     
150000            ADD 1                 TO W-TISENBEK-KL                        
160000            PERFORM HB-KOLLA-TIDEN                                        
170000            MOVE W-TISENBEK-KL    TO 2224-TISENBEK-KL                     
180000            PERFORM IMS-ISRT-XXBU-WDR550                                  
190000          END-PERFORM                                                     
200000        END-IF                                                            
210000     END-IF                                                               
230000     .                                                                    
240000     EJECT                                                                
250000                                                                          
260000                                                                          
270000 HA-FLYTTA-DATA SECTION.                                                  
280000                                                                          
290000     MOVE W-TISENBEK-DAG      TO 2224-TISENBEK-DAG                        
300000     MOVE W-TISENBEK-KL       TO 2224-TISENBEK-KL                         
310000     MOVE MID-KDLARM          TO 2224-KDLARM                              
320000     MOVE W-IDTRANS           TO 2224-IDTRANS                             
330000     MOVE MSG-KDMFSFOR-1      TO 2224-KDMFSFOR                            
340000     MOVE MID-IDARTNR         TO 2224-IDARTNR                             
350000     MOVE MID-IDDC            TO 2224-IDDC                                
360000     MOVE MID-FLNYLARM        TO 2224-FLNYLARM                            
370000     MOVE MID-IDDISTR         TO WS-IDDISTR                               
380000     MOVE WS-IDDISTR          TO 2224-IDDISTR                             
390000     MOVE MID-IDKUNDNR        TO WS-IDKUNDNR                              
400000     MOVE WS-IDKUNDNR         TO 2224-IDKUNDNR                            
410000     MOVE MID-IDKUNDRF        TO 2224-IDKUNDRF                            
420000     IF MID-IDKR = SPACE OR MID-IDKR NOT NUMERIC                          
430000        MOVE ZERO             TO 2224-IDKR                                
440000     ELSE                                                                 
450000        MOVE MID-IDKR         TO 2224-IDKR                                
460000     END-IF                                                               
470000     MOVE DAGENS-DATUM        TO 2224-TIREGDAT                            
480000     MOVE ZERO                TO 2224-IDLOPNR                             
490000     MOVE MID-IDLEVNR         TO 2224-IDLEVNR                             
500000     .                                                                    
510000     EJECT                                                                
520000                                                                          
530000                                                                          
540000 HB-KOLLA-TIDEN SECTION.                                                  
550000                                                                          
560000     MOVE W-TISENBEK-KL      TO WS-TISENBEK-KL-TEST                       
570000     IF WS-TISENBEK-KL-SS > 59                                            
580000        ADD 1 TO WS-TISENBEK-KL-MM                                        
590000        MOVE ZERO TO WS-TISENBEK-KL-SS                                    
600000        MOVE WS-TISENBEK-KL-TEST TO W-TISENBEK-KL                         
610000        IF WS-TISENBEK-KL-MM > 59                                         
620000           ADD 1 TO WS-TISENBEK-KL-HH                                     
630000           MOVE ZERO TO WS-TISENBEK-KL-MM                                 
640000           MOVE WS-TISENBEK-KL-TEST TO W-TISENBEK-KL                      
650000        END-IF                                                            
660000     END-IF                                                               
670000                                                                          
680000     .                                                                    
690000     EJECT                                                                
700000                                                                          
710000                                                                          
720000                                                                          
730000* --- IMS SEKTIONER ---                                                   
740000     SKIP3                                                                
750000 IMS-GET-MSG SECTION.                                                     
760000                                                                          
770000     MOVE '  QC' TO GODK-STATUSKODER                                      
780000     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
790000     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
800000     PERFORM IMS-STATUSKONTROLL                                           
810000     .                                                                    
820000     EJECT                                                                
830000 IMS-GET-WMSGKOM SECTION.                                                 
840000     MOVE '  QD' TO GODK-STATUSKODER                                      
850000     CALL CBLTDLI USING GN MSG-PCB MSG-KOM-WMSGKOM                        
860000     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
870000     PERFORM IMS-STATUSKONTROLL                                           
880000     .                                                                    
890000     SKIP2                                                                
900000 IMS-INSERT-WMSGKOM SECTION.                                              
910000     MOVE '  ' TO GODK-STATUSKODER                                        
920000     CALL CBLTDLI USING ISRT MSGKOM-PCB MSG-KOM-WMSGKOM                   
930000     MOVE MSGKOM-STATUS-CODE TO STATUS-WS                                 
940000     PERFORM IMS-STATUSKONTROLL                                           
950000     .                                                                    
960000     EJECT                                                                
970000                                                                          
980000                                                                          
990000 IMS-ISRT-XXBU-WDR501 SECTION.                                            
000000                                                                          
010000     MOVE 'WLXXBU01 ' TO SSA1                                             
020000     MOVE '  II' TO GODK-STATUSKODER                                      
030000     CALL CBLTDLI USING ISRT XXBU-PCB DLI-IO-AREA SSA1                    
040000     MOVE XXBU-STATUS-CODE TO STATUS-WS                                   
050000     PERFORM IMS-STATUSKONTROLL                                           
060000     .                                                                    
070000     EJECT                                                                
080000                                                                          
090000                                                                          
100000 IMS-GU-XXBU-WDR550-IDDC SECTION.                                         
110000     STRING 'WLXXBU01(WDGXKEY  =' W-WDGX2223-X ')'                        
120000          DELIMITED BY SIZE INTO SSA1                                     
130000     STRING 'WLXXBU11(WDGXKEY  =' W-WDGX2224-X                            
140000                    '&IDDC     =' W-IDDC-X ')'                            
150000          DELIMITED BY SIZE INTO SSA2                                     
160000     MOVE '  GE' TO GODK-STATUSKODER                                      
170000     CALL CBLTDLI USING GU XXBU-PCB DLI-IO-AREA SSA1 SSA2                 
180000     MOVE XXBU-STATUS-CODE TO STATUS-WS                                   
190000     PERFORM IMS-STATUSKONTROLL                                           
200000     .                                                                    
210000     SKIP3                                                                
331000 IMS-GHU-XXBU-WDR550 SECTION.                                             
332000     STRING 'WLXXBU01(WDGXKEY  =' W-WDGX2223-X ')'                        
333000          DELIMITED BY SIZE INTO SSA1                                     
334000     STRING 'WLXXBU11(KDLARM   =' W-KDLARM-X                              
335000                    '&IDARTNR  =' W-IDARTNR-X                             
336000                    '&IDDC     =' W-IDDC-X ')'                            
337000          DELIMITED BY SIZE INTO SSA2                                     
338000     MOVE '  GE' TO GODK-STATUSKODER                                      
339000     CALL CBLTDLI USING GHU XXBU-PCB DLI-IO-AREA SSA1 SSA2                
339100     MOVE XXBU-STATUS-CODE TO STATUS-WS                                   
339200     PERFORM IMS-STATUSKONTROLL                                           
339300     .                                                                    
340000     EJECT                                                                
350000                                                                          
360000                                                                          
370000 IMS-ISRT-XXBU-WDR550 SECTION.                                            
380000                                                                          
390000     STRING 'WLXXBU01(WDGXKEY  =' W-WDGX2223-X ')'                        
400000          DELIMITED BY SIZE INTO SSA1                                     
410000     STRING 'WLXXBU11   '                                                 
420000          DELIMITED BY SIZE INTO SSA2                                     
430000     MOVE '  II' TO GODK-STATUSKODER                                      
440000     CALL CBLTDLI USING ISRT XXBU-PCB DLI-IO-AREA SSA1 SSA2               
450000     MOVE XXBU-STATUS-CODE TO STATUS-WS                                   
460000     PERFORM IMS-STATUSKONTROLL                                           
470000     .                                                                    
480000     SKIP3                                                                
490000 IMS-DLET-XXBU-WDR550 SECTION.                                            
500000                                                                          
510000     MOVE '  ' TO GODK-STATUSKODER                                        
520000     CALL CBLTDLI USING DLET XXBU-PCB DLI-IO-AREA                         
530000     MOVE XXBU-STATUS-CODE TO STATUS-WS                                   
540000     PERFORM IMS-STATUSKONTROLL                                           
550000     .                                                                    
560000     EJECT                                                                
570000                                                                          
580000                                                                          
590000                                                                          
600000 IMS-GU-XXBX-2231 SECTION.                                                
610000                                                                          
620000     STRING 'WLXXBX01(WDGXKEY  =' W-WDGXKEY-2231-X ')'                    
630000          DELIMITED BY SIZE INTO SSA1                                     
640000     MOVE '  ' TO GODK-STATUSKODER                                        
650000     CALL CBLTDLI USING GU XXBX-PCB DLI-IO-AREA2 SSA1                     
660000     MOVE XXBX-STATUS-CODE TO STATUS-WS                                   
670000     PERFORM IMS-STATUSKONTROLL                                           
680000     .                                                                    
690000                                                                          
700000                                                                          
710000 IMS-GNP-XXBX-2232 SECTION.                                               
720000                                                                          
730000     STRING 'WLXXBX11(WDGXKEY  =' W-WDGXKEY-2232-X ')'                    
740000          DELIMITED BY SIZE INTO SSA1                                     
750000     MOVE '  GE' TO GODK-STATUSKODER                                      
760000     CALL CBLTDLI USING GNP XXBX-PCB DLI-IO-AREA2 SSA1                    
770000     MOVE XXBX-STATUS-CODE TO STATUS-WS                                   
780000     PERFORM IMS-STATUSKONTROLL                                           
790000     .                                                                    
800000                                                                          
810000 IMS-STATUSKONTROLL SECTION.                                              
820000                                                                          
830000     SET STATUS-IX TO 1                                                   
840000     SEARCH GODK-STATUS                                                   
850000       AT END                                                             
860000         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
870000         DELIMITED BY SIZE INTO FELTEXT                                   
880000         CALL FELLOG                                                      
890000       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
900000         CONTINUE                                                         
910000     END-SEARCH                                                           
920000     .                                                                    
930000                                                                          
940000                                                                          
