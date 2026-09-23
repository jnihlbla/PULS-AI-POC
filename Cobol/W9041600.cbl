010000 ID DIVISION.                                                             
020000     SKIP2                                                                
030071 PROGRAM-ID.     W9041600.                                                
040000 AUTHOR.         HENRIK ARONSSON.                                         
050000 DATE-WRITTEN.   MAJ 1990.                                                
060000                                                                          
070000     REMARKS.                                                             
080000*                                                                         
090000*    FUNKTION.                                                            
100000*        - VISAR INFORMATION OM ARTIKEL SAMT INFORMATION OM               
110000*          DE STRUKTURER SÖKT ARTIKEL INGÅR I.                            
120000*                                                                         
130071***      - MED DETTA PROGRAM VÄLJER MAN UT I VILKA STRUKTURER             
140071***        SÖKT ARTIKEL SKALL TAS BORT/BYTAS UT. SJÄLVA BYTET/            
150071***        BORTTAGNINGEN AV ARTIKEL SKER FRÅN BILD 1222.                  
160000*                                                                         
170071***      - PRINTNING AV LISTA SKER MED HJÄLP AV BAKGRUNDS-                
180071***        PROGRAMMET W10291.                                             
190000*                                                                         
200000*        PROGRAMMET ÄR EN UPPDATERINGS-MPP                                
210072***      PROGRAMMET UPPDATERAR WLSATB (WDJ1)                              
220072***      PROGRAMMET UPPDATERAR WLXXAZ (WDR5)                              
230000*        PROGRAMMET LÄSER      WLBENA (WDD3)                              
240040*        PROGRAMMET LÄSER      WLARTC (WDK6)                              
250000*                                                                         
260013*        ÄT SPLIT 930404 BL                                               
270013*           - ÄNDRING GODKÄNDA KDPRODSL                                   
280013*                                                                         
290000*    INDATA.                                                              
300071*        TRANSAKTION: W9T416                                              
310071*        MID:         W90416I1                                            
320000*                                                                         
330000*    UTDATA.                                                              
340071*        MOD:         W90416O1                                            
350000                                                                          
360000     SKIP3                                                                
370000 ENVIRONMENT DIVISION.                                                    
380000     EJECT                                                                
390000 DATA DIVISION.                                                           
400000 WORKING-STORAGE SECTION.                                                 
410051                                                                          
420051*    -COPY WY2000W1                                                       
430051     SKIP3                                                                
440071 77  IDPGM                       PIC X(08)   VALUE 'W9041600'.            
450000                                                                          
460000 77  JA                          PIC X       VALUE 'J'.                   
470000 77  NEJ                         PIC X       VALUE 'N'.                   
480000                                                                          
490000*    --- INDEX FÖR BLÄDDRINGSRADER                                        
500000 77  INDX                        PIC S9(4)  VALUE +0    COMP SYNC.        
510054 77  MAX-INDX                    PIC S9(4)  VALUE +10   COMP SYNC.        
520000                                                                          
530000 77  SPRAK-IX                    PIC S9(9)  VALUE +0    COMP SYNC.        
540000                                                                          
550000*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
560000 77  WS-IDLEVNR                  PIC X(5)    VALUE SPACE.                 
570065 77  WS-IDLEVNR-TIDIGARE-STRUKT  PIC X(5)    VALUE SPACE.                 
580065 77  WS-IDLEVNR-TIDIGARE-RADVAL  PIC X(5)    VALUE SPACE.                 
590000 77  WS-IDARTNR                  PIC X(9)    VALUE SPACE.                 
600000 77  WS-IDARTNR-2                PIC X(9)    VALUE SPACE.                 
610000 77  WS-IDARTNR-NUM              PIC 9(9)    VALUE ZERO.                  
620000 77  WS-IDARTNR-KONVERTERAT      PIC 9(9)    VALUE ZERO.                  
630000 77  WS-BELEVART                 PIC X(30)   VALUE SPACE.                 
640000 77  WS-IDSKYLT                  PIC X(3)    VALUE SPACE.                 
650000 77  WS-1002-SATS                PIC X(1)    VALUE SPACE.                 
660000 77  WS-KDPRODSL                 PIC X(2)    VALUE SPACE.                 
670000 77  WS-KDPRODSL-NUM             PIC 9(2)    VALUE ZERO.                  
680000 77  WS-IDRADNR                  PIC X(4)    VALUE SPACE.                 
690000                                                                          
700000 77  WS-KDPRODSL-GODK            PIC 9(2).                                
710062     88   KDPRODSL-GODK                      VALUE 11 13 14 15 16         
720062                                                   17 18 19               
730062                                                   21 23 24 25 26         
740062                                                   27 28 29               
770065                                                   71 72 73 74.           
780000                                                                          
790000 77  INDATA-SW                   PIC X       VALUE 'J'.                   
800000     88  INDATA-OK                           VALUE 'J'.                   
810000     88  INDATA-FEL                          VALUE 'N'.                   
820000                                                                          
830000 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
840000     88  NYCKLAR-OK                          VALUE 'J'.                   
850000     88  NYCKLAR-FEL                         VALUE 'N'.                   
860000                                                                          
870000 77  ALLT-SW                     PIC X       VALUE 'J'.                   
880000     88  ALLT-OK                             VALUE 'J'.                   
890000                                                                          
900000 77  MID-INPUT-SW                PIC X       VALUE 'J'.                   
910000     88  MID-INPUT-IFYLLD                    VALUE 'J'.                   
920000     88  MID-INPUT-EJ-IFYLLD                 VALUE 'N'.                   
930000                                                                          
940000 77  RAD-SW                      PIC X       VALUE 'J'.                   
950000     88  RAD-OK                              VALUE 'J'.                   
960000                                                                          
970000 77  RADVAL-SW                   PIC X       VALUE 'J'.                   
980000     88  RADVAL-OK                           VALUE 'J'.                   
990000     88  RADVAL-FEL                          VALUE 'N'.                   
000000                                                                          
010000 77  SOEKNYCKELTYP-SW            PIC X       VALUE SPACE.                 
020000     88  SOEKNYCKEL-IDARTNR                  VALUE 'A'.                   
030000     88  SOEKNYCKEL-IDLEVNR-O-BELEVART       VALUE 'I'.                   
040000                                                                          
050000 77  SOEKNYCKEL-LAAST-AV-ANNAN-SW PIC X      VALUE 'N'.                   
060000     88  SOEKNYCKEL-LAAST-AV-ANNAN-USER      VALUE 'J'.                   
070000                                                                          
080000 77  SOEKNYCKEL-LAAST-AV-EGET-ID-SW PIC X    VALUE 'N'.                   
090000     88  SOEKNYCKEL-LAAST-AV-EGET-ID         VALUE 'J'.                   
100000                                                                          
110000 77  IDARTNR-ALT-ERS-PAA-ARTREG-SW  PIC X    VALUE 'N'.                   
120000     88 IDARTNR-ALT-ERS-PAA-ARTREG           VALUE 'J'.                   
130000                                                                          
140000 77  TILLKOMMANDE-ARTIKLAR-FINNS-SW PIC X    VALUE 'N'.                   
150000     88 TILLKOMMANDE-ARTIKLAR-FINNS          VALUE 'J'.                   
160000                                                                          
170000 77  TIDIGARE-VALDA-STRUKT-FINNS-SW PIC X    VALUE 'N'.                   
180000     88 TIDIGARE-VALDA-STRUKT-FINNS          VALUE 'J'.                   
190000                                                                          
200000 77  TIDIGARE-VALD-RAD-FINNS-SW     PIC X    VALUE 'N'.                   
210000     88 TIDIGARE-VALD-RAD-FINNS              VALUE 'J'.                   
220000                                                                          
230000 77  SIGNON-USER-HAR-KONV-STRUKT-SW PIC X    VALUE 'N'.                   
240000     88 SIGNON-USERID-HAR-KONV-STRUKT        VALUE 'J'.                   
250000                                                                          
260041 77  STRUKTURNR-FINNS-PAA-WDK6-SW PIC X.                                  
270041     88 STRUKTURNR-FINNS-PAA-WDK6            VALUE 'J'.                   
280000                                                                          
290009 77  SATS-I-STR-RA           PIC X(3)    VALUE '242'.                     
300009 77  SATS-I-STR-RB           PIC X(3)    VALUE '243'.                     
310009 77  SATS-I-STR-BERPV        PIC X(3)    VALUE '246'.                     
320037 77  SATS-I-STR-CARP         PIC X(3)    VALUE '241'.                     
330009                                                                          
340000 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
350072     88  EGEN-MID                            VALUE '9416'.                
360072     88  GODK-MID                            VALUE '9416'.                
370000     EJECT                                                                
380060*      --- VALID IDDC CODES                                               
390060*                                                                         
400060*01    -COPY WWDC99                                                       
410060       EJECT                                                              
420000*    --- DIVERSE VARIABLER                                                
430000                                                                          
440000 01  WS-MAX-IDARTNR              PIC S9(9)   VALUE +100000000.            
450000 01  WS-MIN-IDARTNR-KONV         PIC S9(9)   VALUE +100000000.            
460000 01  WS-MAX-IDARTNR-KONV         PIC S9(9)   VALUE +999999999.            
470000 01  WS-BEART                    PIC X(25)   VALUE SPACE.                 
480000 01  WS-KDBENHOM                 PIC S9(1)   VALUE ZERO.                  
490000                                                                          
500000 01  IDARTNR                     PIC X       VALUE 'A'.                   
510000 01  IDLEVNR-O-BELEVART          PIC X       VALUE 'I'.                   
520000                                                                          
530000 01  DAGENS-DATUM                PIC 9(6).                                
540000                                                                          
550000 01  WS-SPAR-SPAERWDATUM         PIC 9(6).                                
560000                                                                          
570000 01  TRANS-TILL-1291             PIC X       VALUE SPACE.                 
580000                                                                          
590000 01  MEDDELANDEN.                                                         
600005     03  FEL-1.                                                           
610005         05 FILLER               PIC X(40)   VALUE                        
620000             'UPPDATERING EJ TILLÅTEN                 '.                  
630005         05 FILLER               PIC X(40)   VALUE                        
640005             'UPDATE NOT ALLOWED                      '.                  
650005     03  FILLER REDEFINES FEL-1.                                          
660005         05 FEL1 OCCURS 2        PIC X(40).                               
670005                                                                          
680005     03  FEL-2.                                                           
690005         05 FILLER               PIC X(40)   VALUE                        
700005             'STRUKTURER SAKNAS                       '.                  
710005         05 FILLER               PIC X(40)   VALUE                        
720005             'STRUCTURES ARE MISSING                  '.                  
730005     03  FILLER REDEFINES FEL-2.                                          
740005         05 FEL2 OCCURS 2        PIC X(40).                               
750005                                                                          
760005     03  FEL-3.                                                           
770005         05 FILLER               PIC X(40)   VALUE                        
780005             'ARTIKEL ALTERNATIVT ERSATT              '.                  
790005         05 FILLER               PIC X(40)   VALUE                        
800005             'PART NOT UNAMBIGUOUS SUPERSEDED         '.                  
810005     03  FILLER REDEFINES FEL-3.                                          
820005         05 FEL3 OCCURS 2        PIC X(40).                               
830005                                                                          
840005     03  MED-1.                                                           
850005         05 FILLER               PIC X(61)   VALUE                        
860005             'NYCKEL SPÄRRAD AV ANNAN ANVÄNDARE       '.                  
870005         05 FILLER               PIC X(61)   VALUE                        
880005             'KEY LOCKED BY ANOTHER USER              '.                  
890005     03  FILLER REDEFINES MED-1.                                          
900005         05 MED1 OCCURS 2        PIC X(61).                               
910005                                                                          
920005     03  MED-2.                                                           
930005         05 FILLER               PIC X(61)   VALUE                        
940005             'DU HAR STRUKTUR UNDER BEARBETNING    '.                     
950005         05 FILLER               PIC X(61)   VALUE                        
960005             'YOU HAVE STRUCTURE IN USE               '.                  
970005     03  FILLER REDEFINES MED-2.                                          
980005         05 MED2 OCCURS 2        PIC X(61).                               
990005                                                                          
000005     03  MED-3.                                                           
010005         05 FILLER               PIC X(61)   VALUE                        
020005             'STRUKTUR ÄR UNDER BEARBETNING           '.                  
030005         05 FILLER               PIC X(61)   VALUE                        
040005             'STRUCTURE IS IN USE                     '.                  
050005     03  FILLER REDEFINES MED-3.                                          
060005         05 MED3 OCCURS 2        PIC X(61).                               
070005                                                                          
080005     03  MED-4.                                                           
090005         05 FILLER               PIC X(61)   VALUE                        
100005             'TILLKOMMANDE ARTIKLAR FINNS REGISTRERADE'.                  
110005         05 FILLER               PIC X(61)   VALUE                        
120005             'NEW PART NO. ARE REGISTRED (ON 1222)    '.                  
130005     03  FILLER REDEFINES MED-4.                                          
140005         05 MED4 OCCURS 2        PIC X(61).                               
150005                                                                          
160005     03  MED-5.                                                           
170005         05 FILLER               PIC X(61)   VALUE                        
180005             'BORTTAG EJ MÖJLIG                       '.                  
190005         05 FILLER               PIC X(61)   VALUE                        
200005             'DELETE NOT POSSIBLE                     '.                  
210005     03  FILLER REDEFINES MED-5.                                          
220005         05 MED5 OCCURS 2        PIC X(61).                               
230005                                                                          
240005     03  MED-6.                                                           
250005         05 FILLER               PIC X(27)   VALUE                        
260005             'LISTA KÖAD FÖR UTSKRIFT PÅ '.                               
270005         05 FILLER               PIC X(34).                               
280005         05 FILLER               PIC X(27)   VALUE                        
290005             'LIST IS QUEUED TO PRINTER  '.                               
300005         05 FILLER               PIC X(34).                               
310005                                                                          
320006     03  FILLER REDEFINES MED-6.                                          
330006         05 MED6 OCCURS 2.                                                
340006            07 FILLER            PIC X(27).                               
350006            07 MED6-IDLTERM      PIC X(8).                                
360006            07 MED6-KOMMATECKEN  PIC X.                                   
370006            07 MED6-BEPRT        PIC X(25).                               
380005                                                                          
390005     03  MED-7.                                                           
400005         05 FILLER               PIC X(61)   VALUE                        
410005             'RADEN U-MÄRKT                           '.                  
420005         05 FILLER               PIC X(61)   VALUE                        
430005             'LINE IS MARKED TO BE DELETED            '.                  
440005     03  FILLER REDEFINES MED-7.                                          
450005         05 MED7 OCCURS 2        PIC X(61).                               
460005                                                                          
470005     03  MED-8.                                                           
480005         05 FILLER               PIC X(61)   VALUE                        
490005            'VAL AV BÅDE 1002-SATS OCH EJ-1002-SATS EJ TILLÅTEN'.         
500005         05 FILLER               PIC X(61)   VALUE                        
510005        'SELECTION OF BOTH 1002-KIT AND NON-1002-KIT NOT ALLOWED'.        
520005     03  FILLER REDEFINES MED-8.                                          
530005         05 MED8 OCCURS 2        PIC X(61).                               
540005                                                                          
550006     03  MED-9.                                                           
560006         05 FILLER           PIC X(61)   VALUE                            
570009         'FEL I W006PRT DEFINITION, KONTAKTA SYSTEMAVD'.                  
580006         05 FILLER           PIC X(61)   VALUE                            
590009         'MAJOR ERROR IN W006PRT, CONTACT YOUR SYSTEM SUPPORT'.           
600006     03  FILLER REDEFINES MED-9.                                          
610007         05 MED9 OCCURS 2 PIC X(61).                                      
620006                                                                          
630000     EJECT                                                                
640000*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
650000 01  GENERELLA-SUBPROGRAM.                                                
660009     03  W006PRT                 PIC X(8)    VALUE 'W006PRT '.            
670002     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
680057     03  WORKDAY                 PIC X(8)    VALUE 'WORKDAY '.            
690000     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
700000     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
710045     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
720000     SKIP3                                                                
730001*   -COPY WWLAND03                                                        
740000     EJECT                                                                
750057*   -COPY WORKAREA                                                        
760000     EJECT                                                                
770009*    --- PARAMETRAR TILL SUBPROGRAM W006PRT                               
780009*   -COPY W006PRT                                                         
790002     EJECT                                                                
800000*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
810001*   -COPY WMEDAREA                                                        
820045     EJECT                                                                
830045*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
840045*   -COPY WMSGINIT                                                        
850000     SKIP3                                                                
860000 01  MESSAGE-CODES.                                                       
870000     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
880000     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '011'.                 
890000     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
900000     03  INF-PRESS-PF11          PIC X(3)    VALUE '003'.                 
910000     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
920000     03  INF-MORE-INFO-EXISTS    PIC X(3)    VALUE '105'.                 
930000     03  INF-LAST-PAGE           PIC X(3)    VALUE '106'.                 
940000     EJECT                                                                
950000*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
960000*                                                                         
970000 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
980000     SKIP3                                                                
990071*01  MID -COPY W90416I1                                                   
000000     EJECT                                                                
010000 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
020000     SKIP3                                                                
030001*01  -COPY WMSGAREA                                                       
040000     EJECT                                                                
050000     03  MOD REDEFINES MSG-AREA.                                          
060071*      05  -COPY W90416O1                                                 
070000     EJECT                                                                
080000 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
090000     SKIP3                                                                
100001*01  -COPY WMFSAREA                                                       
110000     EJECT                                                                
120000*    ---------------------ALT-AREA                                        
130000 01  FILLER                      PIC X(16)   VALUE 'ALT-AREA'.            
140000 01  W-PROG-TO-PROG-SW.                                                   
150030     03  M-SW-LL                 PIC S9(4)   VALUE +300                   
160000                                             COMP SYNC.                   
170000     03  M-SW-Z1-Z2              PIC X(2)    VALUE LOW-VALUE.             
180010     03  M-SW-KDTRANS            PIC X(8)    VALUE 'W1T291X'.             
190000     03  M-SW-IDTRANS            PIC X(4)    VALUE '1291'.                
200000     03  M-SW-KDMFSTYP           PIC X(1)    VALUE '1'.                   
210000     03  MID-W1I29101.                                                    
220001*        05  MID -COPY W1I22101   -PRE R1291-                             
230000     EJECT                                                                
240000*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
250000*                                                                         
260000 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
270000     SKIP3                                                                
280000 01  NYCKLAR-TILL-DLI.                                                    
290000     03  W-WDJ1CSEQ-X.                                                    
300000         05  W-IDLEVNR-X.                                                 
310065             07   W-IDLEVNR      PIC X(5)   VALUE SPACE.                  
320000         05  W-BELEVART-X.                                                
330000             07   W-BELEVART     PIC X(30)  VALUE SPACE.                  
340000         05  W-IDARTNR-X.                                                 
350000             07   W-IDARTNR      PIC S9(9)  VALUE ZERO COMP-3.            
360000                                                                          
370000     03  W-WDJ111KY-X.                                                    
380000         05  W-KDSTRRAD          PIC X      VALUE SPACE.                  
390000         05  W-IDRADNR           PIC S9(5)  VALUE ZERO COMP-3.            
400000                                                                          
410000     03  W-WDGXKEY-X.                                                     
420000         05  W-IDHTYP            PIC X(4)   VALUE SPACE.                  
430000         05  W-LOW-VALUE-2       PIC X(26)  VALUE SPACE.                  
440000                                                                          
450000     03  W-IDARTNR-2-X.                                                   
460000         05  W-IDARTNR-2         PIC S9(9)   VALUE ZERO COMP-3.           
470000                                                                          
480000     03  W-BEART-X.                                                       
490000         05  W-BEART             PIC X(25)  VALUE SPACE.                  
500000                                                                          
510000     03  W-IDUSER-X.                                                      
520000         05  W-IDUSER            PIC X(8)   VALUE SPACE.                  
530000                                                                          
540000     03  W-LOW-VALUE-X.                                                   
550000         05  W-LOW-VALUE         PIC X(4)   VALUE SPACE.                  
560000                                                                          
570000     03  W-IDSKYLT-X.                                                     
580000         05  W-IDSKYLT           PIC X(3)   VALUE SPACE.                  
590000                                                                          
600000     03  W-KDNOTTYP-X.                                                    
610000         05  W-KDNOTTYP          PIC S9(1)  VALUE ZERO COMP-3.            
620000                                                                          
630000     03  W-KDCLAGER-X.                                                    
640000         05  W-KDCLAGER          PIC S9(1)  VALUE ZERO COMP-3.            
650000                                                                          
660000     03  W-MAX-IDARTNR-KONV-X.                                            
670000         05  W-MAX-IDARTNR-KONV  PIC S9(9)  VALUE ZERO COMP-3.            
680000                                                                          
690000     03  W-MIN-IDARTNR-KONV-X.                                            
700000         05  W-MIN-IDARTNR-KONV  PIC S9(9)  VALUE ZERO COMP-3.            
710000                                                                          
720000*    --- STATUS-KOD FRÅN IMS                                              
730000 01  STATUS-WS                   PIC XX.                                  
740000     88  SEGMENT-FINNS                       VALUE '  '.                  
750000     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
760000     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
770000     88  SEGMENT-HOEGRE                      VALUE 'GB'.                  
780000     SKIP2                                                                
790000 01  GODK-STATUSKODER.                                                    
800000     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
810000     SKIP3                                                                
820000 01  SSA1                        PIC X(96).                               
830000 01  SSA2                        PIC X(96).                               
840000 01  SSA3                        PIC X(64).                               
850000     EJECT                                                                
860000*    --- IMS FUNKTIONSKODER                                               
870001*01  -COPY W0003                                                          
880000     EJECT                                                                
890000*    ---  DLI INPUT-OUTPUT AREA                                           
900000 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
910000     SKIP3                                                                
920000 01  DLI-IO-AREA.                                                         
930067     03  IO-AREA                 PIC X(900)  VALUE SPACE.                 
940000     SKIP3                                                                
950000     03  WLARTC01 REDEFINES IO-AREA.                                      
960040*        05  -COPY WDK601                                                 
970000     SKIP3                                                                
980000     03  WLARTC12 REDEFINES IO-AREA.                                      
990040*        05  -COPY WDK611                                                 
000000     SKIP3                                                                
010044     03  WLARTC25 REDEFINES IO-AREA.                                      
020040*        05  -COPY WDK625                                                 
030000     SKIP3                                                                
040000     03  WLSATB01 REDEFINES IO-AREA.                                      
050001*        05  -COPY WDJ101     -PRE SATB01-                                
060000     SKIP3                                                                
070000     03  WLSATB11 REDEFINES IO-AREA.                                      
080001*        05  -COPY WDJ111     -PRE SATB11-                                
090000     SKIP3                                                                
100000     03  WLBENA01 REDEFINES IO-AREA.                                      
110001*        05  -COPY WDD301     -PRE BENA01-                                
120000     SKIP3                                                                
130000     03  WLBENA11 REDEFINES IO-AREA.                                      
140001*        05  -COPY WDD311     -PRE BENA11-                                
150000     SKIP3                                                                
160000     03  WLXXAZ11 REDEFINES IO-AREA.                                      
170001*        05  -COPY WDGX1152   -PRE XXAZ11-                                
180000                                                                          
190000     EJECT                                                                
200000*    ---  DLI INPUT-OUTPUT AREA-2                                         
210000 01  FILLER                      PIC X(16) VALUE 'DLI-IO-AREA-2'.         
220000     SKIP3                                                                
230000 01  DLI-IO-AREA-2.                                                       
240000     03  IO-AREA-2               PIC X(350)  VALUE SPACE.                 
250000     SKIP3                                                                
260000     03  WLSATB-CSEQ REDEFINES IO-AREA-2.                                 
270000         05 WLSATB11.                                                     
280001*            07 -COPY WDJ111     -PRE SATB11C-                            
290000         05 WLSATB01.                                                     
300001*            07 -COPY WDJ101     -PRE SATB01C-                            
310000     EJECT                                                                
320000*    ---  DLI INPUT-OUTPUT AREA-3                                         
330000 01  FILLER                      PIC X(16) VALUE 'DLI-IO-AREA-3'.         
340000     SKIP3                                                                
350000 01  DLI-IO-AREA-3.                                                       
360039     03  IO-AREA-3               PIC X(110) VALUE SPACE.                  
370000     SKIP3                                                                
380038     03  WLARTC01 REDEFINES IO-AREA-3.                                    
390040*        05  -COPY WDK601     -PRE ARTC012-                               
400000     SKIP3                                                                
410000     EJECT                                                                
420000 LINKAGE SECTION.                                                         
430000                                                                          
440001*01  -COPY W0009      -PRE MSG-                                           
450000     EJECT                                                                
460001*01  -COPY W0009      -PRE ALT-                                           
470000     EJECT                                                                
480045*01  -COPY W0008      -PRE USEA-                                          
490000     05  FILLER                  PIC X.                                   
500045     EJECT                                                                
510045*01  -COPY W0008      -PRE ARTC2-                                         
520045     05  FILLER                  PIC X.                                   
530000     EJECT                                                                
540001*01  -COPY W0008      -PRE ARTC-                                          
550000     05  FILLER                  PIC X.                                   
560000     EJECT                                                                
570001*01  -COPY W0008      -PRE SATB-                                          
580000     05  FILLER                  PIC X.                                   
590000     EJECT                                                                
600001*01  -COPY W0008      -PRE SATB-C-                                        
610000     05  FILLER                  PIC X.                                   
620000     EJECT                                                                
630001*01  -COPY W0008      -PRE SATB-D-                                        
640000     05  FILLER                  PIC X.                                   
650000     EJECT                                                                
660001*01  -COPY W0008      -PRE BENA-A-                                        
670000     05  FILLER                  PIC X.                                   
680000     EJECT                                                                
690001*01  -COPY W0008      -PRE BENA-B-                                        
700000     05  FILLER                  PIC X.                                   
710000     EJECT                                                                
720001*01  -COPY W0008      -PRE XXAZ-                                          
730000     05  FILLER                  PIC X.                                   
740000     EJECT                                                                
750045 PROCEDURE DIVISION  USING MSG-PCB ALT-PCB USEA-PCB                       
760045                                           ARTC2-PCB ARTC-PCB             
770000                                   SATB-PCB SATB-C-PCB SATB-D-PCB         
780000                                   BENA-A-PCB BENA-B-PCB XXAZ-PCB.        
790045     ENTRY 'DLITCBL' USING MSG-PCB ALT-PCB USEA-PCB                       
800045                                           ARTC2-PCB ARTC-PCB             
810000                                   SATB-PCB SATB-C-PCB SATB-D-PCB         
820000                                   BENA-A-PCB BENA-B-PCB XXAZ-PCB.        
830000                                                                          
840000     PERFORM IMS-GET-MSG                                                  
850000     IF SEGMENT-FINNS                                                     
860000       PERFORM A-INIT                                                     
870000       PERFORM B-KOLLA-NYCKLAR                                            
880000       IF NYCKLAR-OK                                                      
890071***      IF MFS-UPDATE                                                    
900071***        PERFORM G-KOLLA-INPUT                                          
910071***        IF INDATA-OK                                                   
920071***          PERFORM H-UPPDATERA                                          
930071***        END-IF                                                         
940071***      ELSE                                                             
950000           IF MFS-FIRST                                                   
960000             PERFORM C-FOERSTA-SIDA                                       
970000           ELSE                                                           
980000             IF MFS-NEXT                                                  
990000               PERFORM D-NAESTA-SIDA                                      
000000             ELSE                                                         
010071***            IF MFS-PRINT                                               
020071***              PERFORM I-KOLLA-INPUT-PRINTER                            
030071***              IF INDATA-OK                                             
040071***                PERFORM J-SKICKA-TRANS-TILL-1291                       
050071***              END-IF                                                   
060071***            ELSE                                                       
070000                 PERFORM E-SAMMA-SIDA                                     
080071***            END-IF                                                     
090000             END-IF                                                       
100000           END-IF                                                         
110000           IF ALLT-OK                                                     
120000             PERFORM F-LAES-VISA-INFO                                     
130071***        END-IF                                                         
140071         END-IF                                                           
150000       END-IF                                                             
160071       MOVE LENGTH OF MOD-W90416O1 TO MSG-KVLL                            
170065       ADD            +4           TO MSG-KVLL                            
180000       PERFORM IMS-INSERT-MSG                                             
190000     END-IF                                                               
200000                                                                          
210000     MOVE ZERO TO RETURN-CODE                                             
220000     GOBACK                                                               
230000     .                                                                    
240000     EJECT                                                                
250000 A-INIT SECTION.                                                          
260000                                                                          
270000     IF MSG-DUBBLA-TRANSKODER                                             
280071       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W90416I1                 
290000       MOVE MSG-IDTRANS-2  TO MFS-IDTRANS                                 
300000       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
310000     ELSE                                                                 
320071       MOVE MSG-INDATA-MINUS-1-TRANSKOD TO MID-W90416I1                   
330000       MOVE MSG-IDTRANS-1  TO MFS-IDTRANS                                 
340000       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
350000     END-IF                                                               
360000                                                                          
370073     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
380000     MOVE MSG-IDPFK   TO MFS-IDPFK                                        
390000     MOVE MFS-IDTRANS TO W-IDTRANS                                        
400000                                                                          
410000     MOVE LOW-VALUE  TO MSG-AREA                                          
420071     MOVE 'W90416O1' TO MFS-IDMOD                                         
430071     MOVE '9416'     TO MOD-IDTRANS                                       
440000     MOVE SPACE      TO MOD-TEMFSFEL MOD-TEMFSINF                         
450000                                                                          
460000     IF NOT EGEN-MID                                                      
470000       MOVE SPACE TO MFS-KDTRTYP                                          
480000       MOVE '7'   TO MFS-IDPFK                                            
490000     END-IF                                                               
500000                                                                          
510000     ACCEPT DAGENS-DATUM FROM DATE                                        
520000     MOVE NEJ TO TRANS-TILL-1291                                          
530000     .                                                                    
540000     EJECT                                                                
550000 B-KOLLA-NYCKLAR SECTION.                                                 
560000                                                                          
570000     MOVE JA TO NYCKLAR-SW                                                
580000                                                                          
590000     MOVE MFS-RENSA-FAELT TO MOD-IDLEVNR-IN                               
600000                             MOD-BELEVART-IN                              
610072***                          MOD-IDSKYLT-IN                               
620072***                          MOD-1002-SATS-IN                             
630072***                          MOD-KDPRODSL-IN                              
640000                                                                          
650022     IF GODK-MID                                                          
660072       MOVE ALL '+'           TO MSGI-WMSGINIT                            
670048       MOVE '001'             TO MSGI-KDCALL                              
680048       MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                              
690080                                 MSGI-IDLTERM-USER                        
700072       MOVE '9416'            TO MSGI-IDTRANS                             
710022       IF MID-IDLEVNR-IN = ALL '+'                                        
720022         MOVE MID-IDLEVNR-UT TO WS-IDLEVNR                                
730022       ELSE                                                               
740022         MOVE MID-IDLEVNR-IN TO WS-IDLEVNR                                
750048                                MSGI-IDLEVNR                              
760022         MOVE '7'            TO MFS-IDPFK                                 
770022         MOVE SPACE          TO MFS-KDTRTYP                               
780022       END-IF                                                             
790022                                                                          
800022       IF MID-BELEVART-IN = ALL '+'                                       
810023******** KAN ANTINGEN VARA EN ARTIKELBENÄMNING                            
820023******** ELLER ETT ARTIKELNUMMER                                          
830022         MOVE MID-BELEVART-UT TO WS-BELEVART                              
840029         MOVE MID-IDARTNR-UT  TO WS-IDARTNR                               
850022       ELSE                                                               
860022         MOVE MID-BELEVART-IN TO WS-BELEVART                              
870022         MOVE MID-IDARTNR-IN  TO WS-IDARTNR                               
880022         MOVE '7'             TO MFS-IDPFK                                
890022         MOVE SPACE           TO MFS-KDTRTYP                              
900022       END-IF                                                             
910022       INSPECT WS-IDARTNR REPLACING LEADING SPACE BY ZERO                 
920047                                                                          
930047       IF WS-IDARTNR NUMERIC AND WS-IDARTNR > ZERO                        
940073         MOVE WS-IDARTNR      TO MSGI-IDARTNR                             
950047       END-IF                                                             
960048                                                                          
970048       CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                         
980045                                                                          
990022       IF MID-IDSKYLT-IN = ALL '+'                                        
000022         MOVE MID-IDSKYLT-UT TO WS-IDSKYLT                                
010022       ELSE                                                               
020022         MOVE MID-IDSKYLT-IN TO WS-IDSKYLT                                
030022         MOVE '7'            TO MFS-IDPFK                                 
040022         MOVE SPACE          TO MFS-KDTRTYP                               
050022       END-IF                                                             
060022                                                                          
070073***    IF MID-1002-SATS-IN = ALL '+'                                      
080073***      MOVE MID-1002-SATS-UT TO WS-1002-SATS                            
090073***    ELSE                                                               
100073***      MOVE MID-1002-SATS-IN TO WS-1002-SATS                            
110073***      MOVE '7'              TO MFS-IDPFK                               
120073***      MOVE SPACE            TO MFS-KDTRTYP                             
130073***    END-IF                                                             
140022                                                                          
150073***    IF MID-KDPRODSL-IN = ALL '+'                                       
160073***      MOVE MID-KDPRODSL-UT TO WS-KDPRODSL                              
170073***      INSPECT WS-KDPRODSL REPLACING LEADING SPACE BY ZERO              
180073***    ELSE                                                               
190073***      MOVE MID-KDPRODSL-IN TO WS-KDPRODSL                              
200073***      MOVE '7'             TO MFS-IDPFK                                
210073***      MOVE SPACE           TO MFS-KDTRTYP                              
220073***    END-IF                                                             
230024                                                                          
240022     ELSE                                                                 
250073***     MOVE ZERO TO   WS-KDPRODSL                                        
260018        MOVE SPACE TO  WS-BELEVART                                        
270065                       WS-IDLEVNR                                         
280073***                    WS-1002-SATS                                       
290047        MOVE ALL '+' TO MSGI-WMSGINIT                                     
300047        MOVE '001'             TO MSGI-KDCALL                             
310047        MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                             
320082                                  MSGI-IDLTERM-USER                       
330073        MOVE '9416'            TO MSGI-IDTRANS                            
340047        IF (MID-IDARTNR-IN NUMERIC                                        
350047        AND MID-IDARTNR-IN > ZERO)                                        
360047            MOVE MID-IDARTNR-IN TO MSGI-IDARTNR                           
370047        END-IF                                                            
380047        CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                        
390047        MOVE MSGI-IDARTNR TO WS-IDARTNR                                   
400047        INSPECT WS-IDARTNR REPLACING ALL SPACE BY ZERO                    
410018     END-IF                                                               
420053                                                                          
430053     IF MSGI-IDLAND-SPR = 'GB'                                            
440053       MOVE +2    TO SPRAK-IX                                             
450053       MOVE 'GB ' TO MED-IDSKYLT                                          
460053     ELSE                                                                 
470053       MOVE +1    TO SPRAK-IX                                             
480053       MOVE 'S  ' TO MED-IDSKYLT                                          
490053     END-IF                                                               
500046                                                                          
510073***  IF WS-1002-SATS = 'Y' OR 'J' OR 'N' OR ' '                           
520073***    CONTINUE                                                           
530073***  ELSE                                                                 
540073***    MOVE NEJ TO NYCKLAR-SW                                             
550073***  END-IF                                                               
560000                                                                          
570073***  IF WS-KDPRODSL NUMERIC                                               
580073***    IF WS-KDPRODSL = ZERO                                              
590073***      CONTINUE                                                         
600073***    ELSE                                                               
610073***      MOVE WS-KDPRODSL TO WS-KDPRODSL-GODK                             
620073***      IF KDPRODSL-GODK                                                 
630073***        CONTINUE                                                       
640073***      ELSE                                                             
650073***        MOVE NEJ TO NYCKLAR-SW                                         
660073***      END-IF                                                           
670073***    END-IF                                                             
680073***  ELSE                                                                 
690073***    MOVE NEJ TO NYCKLAR-SW                                             
700073***  END-IF                                                               
710000                                                                          
720000     IF WS-IDSKYLT = SPACE                                                
730005       IF SPRAK-IX = 1                                                    
740005         MOVE 'S' TO WS-IDSKYLT                                           
750005       ELSE                                                               
760005         MOVE 'GB' TO WS-IDSKYLT                                          
770005       END-IF                                                             
780005     END-IF                                                               
790000                                                                          
800000     SET WWLAND03-IX TO +1                                                
810000     SEARCH WWLAND03-IDSKYLT-RAD                                          
820000       AT END                                                             
830000         MOVE NEJ TO NYCKLAR-SW                                           
840000       WHEN WWLAND03-IDSKYLT(WWLAND03-IX) = WS-IDSKYLT                    
850000         CONTINUE                                                         
860000     END-SEARCH                                                           
870000                                                                          
880065     IF WS-IDLEVNR(1:1) NOT = '0' AND '+'                                 
890051                                                                          
900065****   OM IDLEVNR > ' ' ÄR SÖKNYCKELN IDLEVNR TILLS. MED BELEVART         
910065       IF WS-IDLEVNR NOT = SPACE                                          
920000         MOVE IDLEVNR-O-BELEVART TO SOEKNYCKELTYP-SW                      
930000         MOVE WS-IDLEVNR         TO W-IDLEVNR                             
940000         MOVE WS-BELEVART        TO W-BELEVART                            
950000         MOVE ZERO               TO W-IDARTNR                             
960000                                                                          
970000         MOVE ZERO               TO WS-IDARTNR                            
980000       ELSE                                                               
990065         IF WS-IDLEVNR = SPACE                                            
000065******   OM IDLEVNR = TOMT, ÄR SÖKNYCKELN IDARTNR                         
010000           MOVE IDARTNR TO SOEKNYCKELTYP-SW                               
020000           IF (WS-IDARTNR NUMERIC) AND                                    
030000               (WS-IDARTNR > ZERO) AND                                    
040000               (WS-IDARTNR < WS-MIN-IDARTNR-KONV)                         
050000             MOVE WS-IDARTNR TO W-IDARTNR                                 
060065             MOVE SPACE      TO W-IDLEVNR                                 
070065                                WS-IDLEVNR                                
080065                                W-BELEVART                                
090065                                WS-BELEVART                               
100000           ELSE                                                           
110000             MOVE NEJ TO NYCKLAR-SW                                       
120000           END-IF                                                         
130000         ELSE                                                             
140000           MOVE NEJ TO NYCKLAR-SW                                         
150000         END-IF                                                           
160000       END-IF                                                             
170000     ELSE                                                                 
180000       MOVE NEJ                TO NYCKLAR-SW                              
190000       MOVE IDLEVNR-O-BELEVART TO SOEKNYCKELTYP-SW                        
200000     END-IF                                                               
210000                                                                          
220000     IF GODK-MID OR NYCKLAR-OK                                            
230000       MOVE WS-IDLEVNR TO MOD-IDLEVNR-UT                                  
240000       IF SOEKNYCKEL-IDARTNR                                              
250017         MOVE WS-IDARTNR TO MOD-IDARTNR-UT                                
260028                            MOD-IDARTNR-SPAR                              
270000         INSPECT MOD-IDARTNR-UT REPLACING LEADING ZERO BY SPACE           
280028       INSPECT MOD-IDARTNR-SPAR REPLACING LEADING ZERO BY SPACE           
290000       ELSE                                                               
300000         MOVE WS-BELEVART TO MOD-BELEVART-UT                              
310000       END-IF                                                             
320000       MOVE WS-IDSKYLT   TO MOD-IDSKYLT-UT                                
330073***    MOVE WS-1002-SATS TO MOD-1002-SATS-UT                              
340073***    MOVE WS-KDPRODSL  TO MOD-KDPRODSL-UT                               
350073***    INSPECT MOD-KDPRODSL-UT REPLACING LEADING ZERO BY SPACE            
360000                                                                          
370073***    PERFORM BA-KOLLA-OM-SOEKNYCKEL-LAAST                               
380073***    IF SOEKNYCKEL-LAAST-AV-ANNAN-USER                                  
390073***      CONTINUE                                                         
400073***    ELSE                                                               
410073***      IF SOEKNYCKEL-IDARTNR                                            
420073***        PERFORM BB-KOLLA-OM-IDARTNR-ALT-ERSATT                         
430073***      END-IF                                                           
440073***    END-IF                                                             
450000     ELSE                                                                 
460019       IF NOT GODK-MID                                                    
470018          MOVE MFS-RENSA-FAELT TO MOD-IDLEVNR-UT                          
480018                                  MOD-BELEVART-UT                         
490028                                  MOD-IDARTNR-SPAR                        
500018                                  MOD-IDSKYLT-UT                          
510074***                               MOD-1002-SATS-UT                        
520074***                               MOD-KDPRODSL-UT                         
530018       END-IF                                                             
540000     END-IF                                                               
550000                                                                          
560000     IF NYCKLAR-FEL                                                       
570000       MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                                 
580000       CALL WMEDKONV USING MED-WMEDAREA                                   
590000       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
600000       PERFORM MFS-RENSA-FAELT-IN                                         
610000       PERFORM MFS-RENSA-FAELT-UT                                         
620000     END-IF                                                               
630000     .                                                                    
640000     SKIP3                                                                
650000 BA-KOLLA-OM-SOEKNYCKEL-LAAST SECTION.                                    
660000**************************************************                        
670000* OM LÅSNING AV SÖKNYCKEL FINNS OCH DEN ÄR ÄLDRE *                        
680000* ÄN 2 DAGAR TAS LÅSNINGEN BORT. ÄR SÖKNYCKELN   *                        
690000* LÅSTA AV ANNAN ANVÄNDARE SÄTTS EN FLAGGA FÖR   *                        
700000* ATT HINDRA UPPDATERING. OM LÅSNING FINNS UNDER *                        
710000* EGET USERID SPARAS SPÄRWDATUM FÖR ATT ANVÄNDAS *                        
720000* SOM TIREGDAT VID KONV. AV STRUKTUR.  OM TILL-  *                        
730000* KOMMANDE ARTIKLAR FINNS REGISTRERADE (FRÅN     *                        
740000* BILD 1222) SÄTTS EN FLAGGA FÖR ATT HINDRA UPPD.*                        
750000**************************************************                        
760000                                                                          
770000                                                                          
780073***  MOVE NEJ       TO SOEKNYCKEL-LAAST-AV-ANNAN-SW                       
790073***                    SOEKNYCKEL-LAAST-AV-EGET-ID-SW                     
800073***                    TILLKOMMANDE-ARTIKLAR-FINNS-SW                     
810073***                                                                       
820073***  MOVE '1151'    TO W-IDHTYP                                           
830073***  MOVE LOW-VALUE TO W-LOW-VALUE-2                                      
840073***  PERFORM IMS-GET-XXAZ-XXAZ01                                          
850000                                                                          
860073***  MOVE LOW-VALUE         TO W-LOW-VALUE                                
870073***  IF SOEKNYCKEL-IDARTNR                                                
880073***    PERFORM IMS-GET-XXAZ-XXAZ11-IDARTNR                                
890073***  ELSE                                                                 
900073***    PERFORM IMS-GET-XXAZ-XXAZ11-IDLEV-BELE                             
910073***  END-IF                                                               
920000                                                                          
930073***  IF SEGMENT-FINNS                                                     
940073***    MOVE 001                  TO WORK-KDCALL                           
950073***    MOVE WC-CDC-SE            TO WORK-IDDC                             
960073***    MOVE XXAZ11-1152-TIREGDAT TO WORK-TIAAMMDD-FOM                     
970073***    MOVE DAGENS-DATUM         TO WORK-TIAAMMDD-TOM                     
980073***    CALL WORKDAY USING WORK-KDCALL                                     
990073***                       WORK-DATE-AREA                                  
000073***                       WORK-KDSVAR                                     
010073***    IF (WORK-KVWORKD > 2) OR (WORK-KDSVAR-FEL)                         
020000*******  ARB-KDSVAR-FEL INNEBÄR ATT ANTALET DAGAR > 999.                  
030073***      PERFORM IMS-DLET-XXAZ-XXAZ11                                     
040073***    ELSE                                                               
050073***      IF XXAZ11-1152-IDUSER = MSG-SIGNON-USERID                        
060073***        MOVE JA TO SOEKNYCKEL-LAAST-AV-EGET-ID-SW                      
070073***        MOVE XXAZ11-1152-TIREGDAT TO WS-SPAR-SPAERWDATUM               
080073***        MOVE WS-IDARTNR           TO W-IDARTNR                         
090073***        MOVE WS-IDLEVNR           TO W-IDLEVNR                         
100073***        MOVE WS-BELEVART          TO W-BELEVART                        
110073***        MOVE MSG-SIGNON-USERID    TO W-IDUSER                          
120073***        PERFORM IMS-GET-XXAZ-XXAZ21                                    
130073***        IF SEGMENT-FINNS                                               
140073***          MOVE JA TO TILLKOMMANDE-ARTIKLAR-FINNS-SW                    
150073***        END-IF                                                         
160073***      ELSE                                                             
170073***        MOVE JA TO SOEKNYCKEL-LAAST-AV-ANNAN-SW                        
180073***      END-IF                                                           
190073***    END-IF                                                             
200073***  END-IF                                                               
210000     .                                                                    
220000     SKIP3                                                                
230000 BB-KOLLA-OM-IDARTNR-ALT-ERSATT SECTION.                                  
240000                                                                          
250073***  MOVE NEJ TO IDARTNR-ALT-ERS-PAA-ARTREG-SW                            
260000                                                                          
270073***  MOVE WS-IDARTNR TO W-IDARTNR-2                                       
280073***  PERFORM IMS-GET-ARTC-ARTC01                                          
290073***  IF SEGMENT-FINNS                                                     
300073***    PERFORM IMS-GET-ARTC-ARTC11                                        
310073***    IF SEGMENT-FINNS                                                   
320073***      IF CLAG-KDERS = 04 OR 05 OR 06                                   
330073***        MOVE JA TO IDARTNR-ALT-ERS-PAA-ARTREG-SW                       
340073***      END-IF                                                           
350073***    END-IF                                                             
360073***  END-IF                                                               
370000                                                                          
380000     .                                                                    
390000     EJECT                                                                
400000 C-FOERSTA-SIDA SECTION.                                                  
410000                                                                          
420000*    --- BLANKA/NOLLA UT BLÄDDRINGSNYCKEL                                 
430000     MOVE JA   TO ALLT-SW                                                 
440000     .                                                                    
450000     EJECT                                                                
460000 D-NAESTA-SIDA SECTION.                                                   
470000                                                                          
480000     MOVE JA TO ALLT-SW                                                   
490000     .                                                                    
500000     EJECT                                                                
510000 E-SAMMA-SIDA SECTION.                                                    
520000                                                                          
530073***  PERFORM S05-KOLLA-OM-MID-INPUT-IFYLLD                                
540073***  IF MID-INPUT-EJ-IFYLLD                                               
550000       MOVE JA TO ALLT-SW                                                 
560073***  ELSE                                                                 
570073***    MOVE NEJ TO ALLT-SW                                                
580073***    MOVE INF-PRESS-PF11 TO MED-IDMFSINF                                
590073***    CALL WMEDKONV USING MED-WMEDAREA                                   
600073***    MOVE MED-MFSINF TO MOD-TEMFSINF                                    
610073***    PERFORM MFS-ROR-EJ-FAELT-IN                                        
620073***    PERFORM MFS-ROR-EJ-FAELT-UT                                        
630073***    PERFORM MFS-LAS-IN-IGEN                                            
640073***  END-IF                                                               
650000     .                                                                    
660000     EJECT                                                                
670000 F-LAES-VISA-INFO SECTION.                                                
680000                                                                          
690000     MOVE +1  TO INDX                                                     
700000     PERFORM S06-LAES-RADDATA-FOERSTA                                     
710000     IF RAD-OK                                                            
720077***    PERFORM FA-LAEGG-UT-ARTIKEL-INFO                                   
730000       MOVE SATB01C-STR-IDARTNR  TO MOD-IDARTNR-ENTER                     
740000       MOVE SATB11C-RAD-KDSTRRAD TO MOD-KDSTRRAD-ENTER                    
750000       MOVE SATB11C-RAD-IDRADNR  TO MOD-IDRADNR-ENTER                     
760000     ELSE                                                                 
770000       MOVE ZERO  TO MOD-IDARTNR-ENTER                                    
780000                     MOD-IDRADNR-ENTER                                    
790000       MOVE SPACE TO MOD-KDSTRRAD-ENTER                                   
800005       MOVE FEL2 (SPRAK-IX) TO MOD-TEMFSFEL                               
810000     END-IF                                                               
820000                                                                          
830000     PERFORM UNTIL INDX > MAX-INDX                                        
840000       IF RAD-OK                                                          
850000         PERFORM FB-REDIGERA-UTRAD                                        
860000         PERFORM S07-LAES-RADDATA-NAESTA                                  
870000       ELSE                                                               
880075***      MOVE MFS-RENSA-FAELT    TO MOD-SELECT(INDX)                      
890075***      MOVE MFS-FORMATETS-ATTR TO MOD-SELECT-ATTR(INDX)                 
900000         PERFORM MFS-RENSA-RAD-FAELT-UT                                   
910000       END-IF                                                             
920000       ADD 1 TO INDX                                                      
930000     END-PERFORM                                                          
940000                                                                          
950000     IF RAD-OK                                                            
960000       MOVE SATB01C-STR-IDARTNR  TO MOD-IDARTNR-NEXT                      
970000       MOVE SATB11C-RAD-KDSTRRAD TO MOD-KDSTRRAD-NEXT                     
980000       MOVE SATB11C-RAD-IDRADNR  TO MOD-IDRADNR-NEXT                      
990000       MOVE INF-MORE-INFO-EXISTS TO MED-IDMFSINF                          
000000       CALL WMEDKONV USING MED-WMEDAREA                                   
010000       MOVE MED-TEMFSINF TO MOD-TEMFSINF                                  
020000     ELSE                                                                 
030000       MOVE ZERO  TO MOD-IDARTNR-NEXT                                     
040000                     MOD-IDRADNR-NEXT                                     
050000       MOVE SPACE TO MOD-KDSTRRAD-NEXT                                    
060000       IF MFS-IDPFK NOT = '7'                                             
070000         MOVE INF-LAST-PAGE  TO MED-IDMFSINF                              
080000         CALL WMEDKONV USING MED-WMEDAREA                                 
090000         MOVE MED-TEMFSINF TO MOD-TEMFSINF                                
100000       END-IF                                                             
110000     END-IF                                                               
120000                                                                          
130077***  MOVE MFS-ROER-EJ-FAELT TO MOD-KDPRTVAL                               
140000     IF MOD-TEMFSFEL = SPACE                                              
150000       IF IDARTNR-ALT-ERS-PAA-ARTREG                                      
160000********* OM FELTEXT EJ IFYLLD VISA ATT                                   
170000********* ARTIKEL ÄR ALTERNATIVT ERSATT                                   
180005         MOVE FEL3 (SPRAK-IX) TO MOD-TEMFSFEL                             
190000       END-IF                                                             
200000     END-IF                                                               
210000     .                                                                    
220000     SKIP3                                                                
230000 FA-LAEGG-UT-ARTIKEL-INFO SECTION.                                        
240000**************************************************                        
250000* HÄR LÄGGS INFORMATION OM DEN SÖKTA ARTIKELN UT *                        
260000**************************************************                        
270000                                                                          
280077***  MOVE WS-IDARTNR TO W-IDARTNR-2                                       
290077***  PERFORM IMS-GET-ARTC-ARTC01                                          
300077***  IF SEGMENT-FINNS                                                     
310077***    MOVE 3 TO W-KDNOTTYP                                               
320077***    PERFORM IMS-GET-ARTC-ARTC25                                        
330077***    IF SEGMENT-FINNS                                                   
340077***      MOVE NOT-TEARTNOT TO MOD-TEARTNOT                                
350077***    ELSE                                                               
360077***      MOVE MFS-RENSA-FAELT TO MOD-TEARTNOT                             
370077***    END-IF                                                             
380054                                                                          
390077***    MOVE 7 TO W-KDNOTTYP                                               
400077***    PERFORM IMS-GET-ARTC-ARTC25                                        
410077***    IF SEGMENT-FINNS                                                   
420077***      MOVE NOT-TEARTNOT TO MOD-TEARTNOT-7                              
430077***    ELSE                                                               
440077***      MOVE MFS-RENSA-FAELT TO MOD-TEARTNOT-7                           
450077***    END-IF                                                             
460054                                                                          
470077***    PERFORM S01-HAEMTA-BEART-BSEQ                                      
480077***    MOVE WS-BEART TO MOD-BEART-UT                                      
490077***  ELSE                                                                 
500077***    IF WS-IDSKYLT = 'S  '                                              
510077***      MOVE SATB11C-RAD-BEART-SVE TO MOD-BEART-UT                       
520077***    ELSE                                                               
530077***      MOVE SATB11C-RAD-BEART-SVE TO W-BEART                            
540077***      MOVE SATB11C-RAD-KDBENHOM  TO WS-KDBENHOM                        
550077***      PERFORM S02-HAEMTA-BEART-ASEQ                                    
560077***      MOVE WS-BEART TO MOD-BEART-UT                                    
570077***    END-IF                                                             
580000                                                                          
590077***    MOVE MFS-RENSA-FAELT TO MOD-TEARTNOT                               
600077***                            MOD-TEARTNOT-7                             
610000                                                                          
620077***  END-IF                                                               
630000     .                                                                    
640000     SKIP3                                                                
650000 FB-REDIGERA-UTRAD SECTION.                                               
660000                                                                          
670000     MOVE SATB01C-STR-IDARTNR  TO MOD-IDARTNR (INDX)                      
680000     MOVE SATB01C-STR-IDSTRTYP TO MOD-IDSTRTYP(INDX)                      
690000     MOVE SATB11C-RAD-IDRADNR  TO MOD-IDRADNR (INDX)                      
700000     MOVE SATB11C-RAD-REANTPSA TO MOD-REANTPSA(INDX)                      
710000                                                                          
720000     MOVE SATB01C-STR-IDARTNR TO W-IDARTNR-2                              
730000     PERFORM IMS-GET-ARTC-ARTC01                                          
740000     IF SEGMENT-FINNS                                                     
750000****** OM DEN HITTADE STRUKTUREN FINNS PÅ ARTREG                          
760041       MOVE ART-IDLEVNR   TO MOD-IDLEVNR(INDX)                            
770041       MOVE ART-KDERS-UTG TO MOD-KDERS  (INDX)                            
780041       PERFORM IMS-GET-ARTC-ARTC11                                        
790014       IF SEGMENT-FINNS                                                   
800050         MOVE CLAG-KDERS    TO MOD-KDERS  (INDX)                          
810050         MOVE CLAG-KDPSLLOC TO MOD-KDPSLLOC  (INDX)                       
820014       END-IF                                                             
830041       IF STRUKTURNR-FINNS-PAA-WDK6                                       
840041******   ART-KDPRODSL LIGGER I IO-AREA-3 EFTER LÄSNING I S04              
850041         MOVE ARTC012-ART-KDPRODSL TO MOD-KDPRODSL(INDX)                  
860000       ELSE                                                               
870000         MOVE MFS-RENSA-FAELT TO MOD-KDPRODSL(INDX)                       
880000       END-IF                                                             
890000       PERFORM S01-HAEMTA-BEART-BSEQ                                      
900000       MOVE WS-BEART TO MOD-BEART(INDX)                                   
910000     ELSE                                                                 
920000       MOVE SATB01C-STR-KDPRODSL TO MOD-KDPRODSL(INDX)                    
930000       MOVE MFS-RENSA-FAELT      TO MOD-KDERS(INDX)                       
940000                                    MOD-IDLEVNR(INDX)                     
950000       IF WS-IDSKYLT = 'S  '                                              
960000         MOVE SATB01C-STR-BEART-SVE TO MOD-BEART(INDX)                    
970000       ELSE                                                               
980000         MOVE SATB01C-STR-BEART-SVE TO W-BEART                            
990000         MOVE SATB01C-STR-KDBENHOM  TO WS-KDBENHOM                        
000000         PERFORM S02-HAEMTA-BEART-ASEQ                                    
010000         MOVE WS-BEART TO MOD-BEART(INDX)                                 
020000       END-IF                                                             
030000     END-IF                                                               
040000                                                                          
050075***  COMPUTE WS-IDARTNR-KONVERTERAT = 999999999 -                         
060075***                                   SATB01C-STR-IDARTNR                 
070075***  MOVE WS-IDARTNR-KONVERTERAT TO W-IDARTNR-2                           
080075***  PERFORM IMS-GET-SATB-SATB01                                          
090075***  IF SEGMENT-FINNS                                                     
100075***    MOVE 001                 TO WORK-KDCALL                            
110075***    MOVE WC-CDC-SE           TO WORK-IDDC                              
120075***    MOVE SATB01-STR-TIREGDAT TO WORK-TIAAMMDD-FOM                      
130075***    MOVE DAGENS-DATUM        TO WORK-TIAAMMDD-TOM                      
140075***    CALL WORKDAY USING WORK-KDCALL                                     
150075***                       WORK-DATE-AREA                                  
160075***                       WORK-KDSVAR                                     
170075***    IF (WORK-KVWORKD > 2) OR (WORK-KDSVAR-FEL)                         
180000*******  OM MAN TRÄFFAR PÅ EN KONVERTERAD STRUKTUR SOM                    
190000*******  ÄR ÄLDRE ÄN 2 DAGAR TAS DEN BORT.                                
200000*******  ARB-KDSVAR-FEL INNEBÄR ATT ANTALET DAGAR > 999.                  
210075***      PERFORM IMS-DLET-SATB                                            
220075***    ELSE                                                               
230075***      IF (SATB01-STR-IDUSER = MSG-SIGNON-USERID) AND                   
240075***          (SOEKNYCKEL-LAAST-AV-EGET-ID)                                
250000*********  OM DEN KONVERTERADE STRUKTUREN KONVERTERATS                    
260000*********  AV SIGNON-USERID OCH 'UNDER' DENNA SÖKNYCKEL                   
270075***                                                                       
280075***        MOVE SPACE               TO W-KDSTRRAD                         
290000*********  RADERNA LIGGER MED KDSTRRAD = ' ' I VALDA STRUKTUR             
300075***        MOVE SATB11C-RAD-IDRADNR TO W-IDRADNR                          
310075***        PERFORM IMS-GET-SATB-SATB11                                    
320075***        IF SEGMENT-FINNS                                               
330000***********  RADEN UTVALD                                                 
340075***          MOVE MFS-FORMATETS-ATTR TO MOD-SELECT-ATTR(INDX)             
350075***          MOVE '*'                TO MOD-SELECT(INDX)                  
360075***        ELSE                                                           
370075***          MOVE MFS-FORMATETS-ATTR TO MOD-SELECT-ATTR(INDX)             
380075***          MOVE MFS-RENSA-FAELT    TO MOD-SELECT(INDX)                  
390075***        END-IF                                                         
400000                                                                          
410075***      ELSE                                                             
420075***        MOVE MFS-FORMATETS-ATTR TO MOD-SELECT-ATTR(INDX)               
430075***        MOVE MFS-RENSA-FAELT    TO MOD-SELECT(INDX)                    
440075***      END-IF                                                           
450075***    END-IF                                                             
460075***  ELSE                                                                 
470075***    MOVE MFS-FORMATETS-ATTR TO MOD-SELECT-ATTR(INDX)                   
480075***    MOVE MFS-RENSA-FAELT    TO MOD-SELECT(INDX)                        
490075***  END-IF                                                               
500000     .                                                                    
510000     EJECT                                                                
520000                                                                          
530000 G-KOLLA-INPUT SECTION.                                                   
540000                                                                          
550074***  MOVE JA TO INDATA-SW                                                 
560074***  IF (SOEKNYCKEL-LAAST-AV-ANNAN-USER) OR                               
570074***      (TILLKOMMANDE-ARTIKLAR-FINNS)                                    
580074***    MOVE NEJ TO INDATA-SW                                              
590074***    PERFORM MFS-ROR-EJ-FAELT-IN                                        
600074***    PERFORM MFS-ROR-EJ-FAELT-UT                                        
610074***    MOVE FEL1 (SPRAK-IX) TO MOD-TEMFSFEL                               
620074***    IF SOEKNYCKEL-LAAST-AV-ANNAN-USER                                  
630074***      MOVE MED1 (SPRAK-IX) TO MOD-TEMFSINF                             
640074***    ELSE                                                               
650074***      MOVE MED4 (SPRAK-IX) TO MOD-TEMFSINF                             
660074***    END-IF                                                             
670074***  ELSE                                                                 
680074***    PERFORM S05-KOLLA-OM-MID-INPUT-IFYLLD                              
690074***    IF MID-INPUT-EJ-IFYLLD                                             
700074***      MOVE ERR-PF11-AND-NO-DATA TO MED-IDMFSFEL                        
710074***      CALL WMEDKONV USING MED-WMEDAREA                                 
720074***      MOVE MED-MFSFEL TO MOD-TEMFSFEL                                  
730074***      PERFORM MFS-ROR-EJ-FAELT-IN                                      
740074***      PERFORM MFS-ROR-EJ-FAELT-UT                                      
750074***      MOVE NEJ TO INDATA-SW                                            
760074***    ELSE                                                               
770074***      PERFORM GA-KOLLA-OM-UPPDAT-TILLATEN                              
780074***      IF INDATA-OK                                                     
790074***        PERFORM GB-FORMELL-INDATAKONTROLL                              
800074***        IF INDATA-OK                                                   
810074***          MOVE +1  TO INDX                                             
820074***          MOVE NEJ TO TIDIGARE-VALD-RAD-FINNS-SW                       
830074***          PERFORM UNTIL INDX > MAX-INDX                                
840074***            IF MID-SELECT(INDX) NOT = ALL '+'                          
850074***              IF MID-SELECT(INDX) = 'S'                                
860074***                PERFORM GC-KOLLA-UPPL-KONV                             
870074***              ELSE                                                     
880074***                IF MID-SELECT(INDX) = 'B' OR 'D'                       
890074***                  PERFORM GD-KOLLA-BORTTAG                             
900074***                ELSE                                                   
910074***                  IF MID-SELECT(INDX) = '*'                            
920074***                    MOVE MFS-ALFA-FAELT-RAETT TO                       
930074***                           MOD-SELECT-ATTR(INDX)                       
940074***                  ELSE                                                 
950074***                    IF MID-SELECT(INDX) = ' '                          
960074***                      MOVE MFS-RENSA-FAELT TO MOD-SELECT(INDX)         
970074***                    END-IF                                             
980074***                  END-IF                                               
990074***                END-IF                                                 
000074***              END-IF                                                   
010074***            END-IF                                                     
020074***            ADD 1 TO INDX                                              
030074***          END-PERFORM                                                  
040074***                                                                       
050074***          IF INDATA-FEL                                                
060074***            MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                  
070074***            CALL WMEDKONV USING MED-WMEDAREA                           
080074***            MOVE MED-MFSFEL TO MOD-TEMFSFEL                            
090074***            PERFORM MFS-ROR-EJ-FAELT-UT                                
100074***            PERFORM MFS-ROR-EJ-FAELT-IN                                
110074***          END-IF                                                       
120074***        ELSE                                                           
130074***          MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                    
140074***          CALL WMEDKONV USING MED-WMEDAREA                             
150074***          MOVE MED-MFSFEL TO MOD-TEMFSFEL                              
160074***          PERFORM MFS-ROR-EJ-FAELT-UT                                  
170074***          PERFORM MFS-ROR-EJ-FAELT-IN                                  
180074***        END-IF                                                         
190074***      ELSE                                                             
200074***        MOVE FEL1 (SPRAK-IX) TO MOD-TEMFSFEL                           
210074***        PERFORM MFS-LAS-IN-IGEN                                        
220074***        PERFORM MFS-ROR-EJ-FAELT-UT                                    
230074***        PERFORM MFS-ROR-EJ-FAELT-IN                                    
240074***      END-IF                                                           
250074***    END-IF                                                             
260000                                                                          
270074***  END-IF                                                               
280074***  MOVE MFS-ROER-EJ-FAELT TO MOD-KDPRTVAL                               
290000     .                                                                    
300000     SKIP3                                                                
310000 GA-KOLLA-OM-UPPDAT-TILLATEN SECTION.                                     
320000****************************************************************          
330000* UPPDATERING ENDAST TILLÅTEN OM 'SIGNON-USERID' :             *          
340000* ¤ INTE HAR NÅGON GÄLLANDE KONVERTERAD STRUKTUR ALLS    *                
350000* ¤ HAR KONVERTERAD STRUKTUR/ER OCH SÖKT ARTIKEL (NYCKEL)      *          
360000*   FINNS REGISTRERAD PÅ WLXXAZ(WDR5,LÅSNING AV ART KOMB USER) *          
370000*   I 'GILTIG FORM' (EJ ÄLDRE ÄN 2 DAGAR).                     *          
380000*   DETTA INNEBÄR ATT DE KONVERTERADE STRUKTURERNA HAR KONVERT-*          
390000*   ERATS UNDER AKTUELL SÖKNYCKEL OCH USER-ID.                 *          
400000****************************************************************          
410000                                                                          
420074***  MOVE MSG-SIGNON-USERID   TO W-IDUSER                                 
430074***  MOVE WS-MIN-IDARTNR-KONV TO W-MIN-IDARTNR-KONV                       
440074***  MOVE WS-MAX-IDARTNR-KONV TO W-MAX-IDARTNR-KONV                       
450000                                                                          
460074***  PERFORM IMS-GET-SATB-DSEQ-UNIK                                       
470074***  PERFORM UNTIL SEGMENT-SAKNAS                                         
480074***    IF SEGMENT-FINNS                                                   
490074***      MOVE 001                 TO WORK-KDCALL                          
500074***      MOVE WC-CDC-SE           TO WORK-IDDC                            
510074***      MOVE SATB01-STR-TIREGDAT TO WORK-TIAAMMDD-FOM                    
520074***      MOVE DAGENS-DATUM        TO WORK-TIAAMMDD-TOM                    
530074***      CALL WORKDAY USING WORK-KDCALL                                   
540074***                         WORK-DATE-AREA                                
550074***                         WORK-KDSVAR                                   
560000                                                                          
570074***      IF (WORK-KVWORKD > 2) OR (WORK-KDSVAR-FEL)                       
580000*******  ARB-KDSVAR-FEL INNEBÄR ATT ANTALET DAGAR > 999.                  
590074***        MOVE SATB01-STR-IDARTNR TO W-IDARTNR-2                         
600074***        PERFORM IMS-GET-SATB-SATB01                                    
610074***        PERFORM IMS-DLET-SATB                                          
620000                                                                          
630074***        PERFORM IMS-GET-SATB-DSEQ-NEXT                                 
640074***      ELSE                                                             
650000********** 'GÄLLANDE' KONVERTERAD STRUKTUR. MAN SÄTTER                    
660000********** STATUS TILL SEGMENT-SAKNAS FÖR ATT BRYTA                       
670074***        MOVE 'GE' TO STATUS-WS                                         
680074***        IF SOEKNYCKEL-LAAST-AV-EGET-ID                                 
690000********** STRUKTUREN KONV. 'UNDER' DENNA NYCKEL & USERID                 
700074***          MOVE JA TO TIDIGARE-VALDA-STRUKT-FINNS-SW                    
710074***          MOVE SATB01-STR-IDLEVNR TO WS-IDLEVNR-TIDIGARE-STRUKT        
720074***          CONTINUE                                                     
730074***        ELSE                                                           
740074***          MOVE NEJ             TO INDATA-SW                            
750074***          MOVE MED2 (SPRAK-IX) TO MOD-TEMFSINF                         
760074***        END-IF                                                         
770074***      END-IF                                                           
780074***    END-IF                                                             
790074***  END-PERFORM                                                          
800000     .                                                                    
810000     SKIP3                                                                
820000 GB-FORMELL-INDATAKONTROLL SECTION.                                       
830000******************************************                                
840000* KONTROLL ATT DEN VALDA RADEN ÄR IFYLLD *                                
850000* OCH ATT INPUT I SÅ FALL ÄR GODKÄND     *                                
860000******************************************                                
870000                                                                          
880074***  MOVE +1 TO INDX                                                      
890074***  PERFORM UNTIL INDX > MAX-INDX                                        
900074***    IF (MID-SELECT(INDX) = ALL '+') OR                                 
910074***       (MID-SELECT(INDX) = SPACE)                                      
920074***      CONTINUE                                                         
930074***    ELSE                                                               
940074***      INSPECT MID-IDRADNR(INDX) REPLACING LEADING SPACE BY ZERO        
950074***      INSPECT MID-IDARTNR(INDX) REPLACING LEADING SPACE BY ZERO        
960000                                                                          
970074***      IF MID-IDRADNR(INDX) = ZERO AND MID-IDARTNR(INDX) = ZERO         
980074***        MOVE NEJ                TO INDATA-SW                           
990074***        MOVE MFS-ALFA-FAELT-FEL TO MOD-SELECT-ATTR(INDX)               
000074***      ELSE                                                             
010074***        IF (MID-SELECT(INDX) = 'S') OR                                 
020074***            (MID-SELECT(INDX) = 'B' OR 'D') OR                         
030074***             (MID-SELECT(INDX) = '*')                                  
040074***          MOVE MFS-ALFA-FAELT-RAETT TO MOD-SELECT-ATTR(INDX)           
050074***        ELSE                                                           
060074***          MOVE NEJ                TO INDATA-SW                         
070074***          MOVE MFS-ALFA-FAELT-FEL TO MOD-SELECT-ATTR(INDX)             
080074***        END-IF                                                         
090074***      END-IF                                                           
100074***    END-IF                                                             
110074***    ADD 1 TO INDX                                                      
120074***  END-PERFORM                                                          
130000     .                                                                    
140000     SKIP3                                                                
150000 GC-KOLLA-UPPL-KONV SECTION.                                              
160000************************************************************              
170000* KONTROLL ATT DEN VALDA RADEN EJ ÄR U-MÄRKT, ATT DEN      *              
180000* VALDA RADEN (STRUKTUREN) EJ FINNS UNDER BEARBETNING AV   *              
190000* ANNAN ANVÄNDARE SAMT ATT OM ARTIKEL ALTERNATIVT ERSATT   *              
200000* SÅ FÅR MAN VÄLJA ANTINGEN ENBART 1002-SATSER ELLER ENBART*              
210000* ICKE-1002-SATSER . EVENTUELL UTGÅNGEN KONVERTERAD        *              
220000* STRUKTUR TOGS BORT I FC-RED ..  .                        *              
230000************************************************************              
240000                                                                          
250074***  MOVE JA TO RADVAL-SW                                                 
260000                                                                          
270074***  INSPECT MID-IDARTNR(INDX) REPLACING LEADING SPACE BY ZERO            
280074***  MOVE MID-IDARTNR(INDX) TO WS-IDARTNR-NUM                             
290000                                                                          
300074***  MOVE MID-IDRADNR(INDX) TO WS-IDRADNR                                 
310074***  INSPECT WS-IDRADNR REPLACING LEADING SPACE BY ZERO                   
320000                                                                          
330074***  MOVE WS-IDARTNR-NUM TO W-IDARTNR-2                                   
340074***  PERFORM IMS-GET-SATB-SATB01                                          
350074***  IF SEGMENT-FINNS                                                     
360074***    MOVE '0'        TO W-KDSTRRAD                                      
370074***    MOVE WS-IDRADNR TO W-IDRADNR                                       
380074***    PERFORM IMS-GET-SATB-SATB11                                        
390074***    IF SEGMENT-FINNS                                                   
400074***      IF SATB11-RAD-KDISATS = 'U'                                      
410074***        MOVE NEJ TO INDATA-SW                                          
420074***                    RADVAL-SW                                          
430074***      END-IF                                                           
440074***    ELSE                                                               
450074***      MOVE '9'        TO W-KDSTRRAD                                    
460074***      MOVE WS-IDRADNR TO W-IDRADNR                                     
470074***      PERFORM IMS-GET-SATB-SATB11                                      
480074***      IF SATB11-RAD-KDISATS = 'U'                                      
490074***        MOVE NEJ TO INDATA-SW                                          
500074***                    RADVAL-SW                                          
510074***      END-IF                                                           
520074***    END-IF                                                             
530000                                                                          
540074***    IF RADVAL-FEL                                                      
550074***      MOVE MFS-ALFA-FAELT-FEL TO MOD-SELECT-ATTR(INDX)                 
560074***      IF MOD-TEMFSINF = SPACE                                          
570074***        MOVE MED7 (SPRAK-IX) TO MOD-TEMFSINF                           
580074***      END-IF                                                           
590074***    END-IF                                                             
600074***  END-IF                                                               
610000                                                                          
620074***  IF RADVAL-OK                                                         
630074***    COMPUTE WS-IDARTNR-KONVERTERAT = 999999999 -                       
640074***                                     WS-IDARTNR-NUM                    
650074***    MOVE WS-IDARTNR-KONVERTERAT TO W-IDARTNR-2                         
660074***    PERFORM IMS-GET-SATB-SATB01                                        
670074***    IF SEGMENT-FINNS                                                   
680074***      IF SATB01-STR-IDUSER = MSG-SIGNON-USERID                         
690000********   STRUKTUREN ÄR REDAN KONVERTERAD AV ANVÄNDAREN.                 
700000********   KOLLA OM RAD REDAN VALD                                        
710074***                                                                       
720074***        MOVE SPACE             TO W-KDSTRRAD                           
730000*********  RADERNA LIGGER MED KDSTRRAD = ' ' I VALDA STRUKTUR             
740074***        MOVE MID-IDRADNR(INDX) TO WS-IDRADNR                           
750074***        INSPECT WS-IDRADNR REPLACING LEADING SPACE BY ZERO             
760074***        MOVE WS-IDRADNR TO W-IDRADNR                                   
770074***        PERFORM IMS-GET-SATB-SATB11                                    
780074***        IF SEGMENT-FINNS                                               
790000***********  RADEN REDAN UTVALD                                           
800000***********  SÄTTER MID-SELECT(INDX) TILL '*' (BEHANDLAS EJ).             
810074***          MOVE '*' TO MID-SELECT(INDX)                                 
820074***        END-IF                                                         
830074***      ELSE                                                             
840074***        MOVE NEJ                TO INDATA-SW                           
850074***        MOVE MFS-ALFA-FAELT-FEL TO MOD-SELECT-ATTR(INDX)               
860074***        IF MOD-TEMFSINF = SPACE                                        
870074***          MOVE MED3 (SPRAK-IX) TO MOD-TEMFSINF                         
880074***        END-IF                                                         
890074***      END-IF                                                           
900074***    ELSE                                                               
910074***      IF IDARTNR-ALT-ERS-PAA-ARTREG                                    
920074***        PERFORM GCA-KOLLA-ATT-IDLEVNR-RAETT                            
930074***        IF INDATA-FEL                                                  
940074***          IF MOD-TEMFSINF = SPACE                                      
950074***            MOVE MED8 (SPRAK-IX) TO MOD-TEMFSINF                       
960074***          END-IF                                                       
970074***        END-IF                                                         
980074***      END-IF                                                           
990074***    END-IF                                                             
000074***  END-IF                                                               
010000     .                                                                    
020000     SKIP3                                                                
030000 GCA-KOLLA-ATT-IDLEVNR-RAETT SECTION.                                     
040000************************************************************              
050000* NÄR ARTIKEL ALTERNATIVT ERSATT FÅR MAN VÄLJA ANTINGEN    *              
060000* BARA 1002-SATSER ELLER BARA ICKE-1002-SATSER.            *              
070000************************************************************              
080000                                                                          
090074***  MOVE WS-IDARTNR-NUM TO W-IDARTNR-2                                   
100074***  PERFORM IMS-GET-SATB-SATB01                                          
110074***  IF SEGMENT-FINNS                                                     
120074***    IF TIDIGARE-VALDA-STRUKT-FINNS                                     
130000********* JÄMFÖR MED TIDIGARE VALDA STRUKTURER                            
140074***      IF WS-IDLEVNR-TIDIGARE-STRUKT = '1002 '                          
150074***        IF SATB01-STR-IDLEVNR = '1002 '                                
160074***          MOVE MFS-ALFA-FAELT-RAETT TO MOD-SELECT-ATTR(INDX)           
170074***        ELSE                                                           
180074***          MOVE NEJ                TO INDATA-SW                         
190074***          MOVE MFS-ALFA-FAELT-FEL TO MOD-SELECT-ATTR(INDX)             
200074***        END-IF                                                         
210074***      ELSE                                                             
220074***        IF SATB01-STR-IDLEVNR NOT = '1002 '                            
230074***          MOVE MFS-ALFA-FAELT-RAETT TO MOD-SELECT-ATTR(INDX)           
240074***        ELSE                                                           
250074***          MOVE NEJ                TO INDATA-SW                         
260074***          MOVE MFS-ALFA-FAELT-FEL TO MOD-SELECT-ATTR(INDX)             
270074***        END-IF                                                         
280074***      END-IF                                                           
290074***    ELSE                                                               
300000********* JÄMFÖR MED TIDIGARE VALD RAD                                    
310074***      IF TIDIGARE-VALD-RAD-FINNS                                       
320074***        IF WS-IDLEVNR-TIDIGARE-RADVAL = '1002 '                        
330074***          IF SATB01-STR-IDLEVNR = '1002 '                              
340074***            MOVE MFS-ALFA-FAELT-RAETT TO MOD-SELECT-ATTR(INDX)         
350074***          ELSE                                                         
360074***            MOVE NEJ                TO INDATA-SW                       
370074***            MOVE MFS-ALFA-FAELT-FEL TO MOD-SELECT-ATTR(INDX)           
380074***          END-IF                                                       
390074***        ELSE                                                           
400074***          IF SATB01-STR-IDLEVNR NOT = '1002 '                          
410074***            MOVE MFS-ALFA-FAELT-RAETT TO MOD-SELECT-ATTR(INDX)         
420074***          ELSE                                                         
430074***            MOVE NEJ                TO INDATA-SW                       
440074***            MOVE MFS-ALFA-FAELT-FEL TO MOD-SELECT-ATTR(INDX)           
450074***          END-IF                                                       
460074***        END-IF                                                         
470074***      ELSE                                                             
480074***        MOVE JA                 TO TIDIGARE-VALD-RAD-FINNS-SW          
490074***        MOVE SATB01-STR-IDLEVNR TO WS-IDLEVNR-TIDIGARE-RADVAL          
500074***      END-IF                                                           
510074***    END-IF                                                             
520074***  END-IF                                                               
530000     .                                                                    
540000     SKIP3                                                                
550000 GD-KOLLA-BORTTAG SECTION.                                                
560000************************************************************              
570000* BORTTTAG FÅR ENDAST SKE AV VALD STRUKTURRAD DÄR KONVERT- *              
580000* AD STRUKTUR FINNS UNDER EGET USER-ID OCH STRUKTURRADEN   *              
590000* FINNS UPPLAGD (VALD TIDIGARE)                            *              
600000************************************************************              
610000                                                                          
620074***  MOVE JA TO RADVAL-SW                                                 
630000                                                                          
640074***  MOVE MID-IDARTNR(INDX) TO WS-IDARTNR-2                               
650074***  INSPECT WS-IDARTNR-2 REPLACING LEADING SPACE BY ZERO                 
660074***  MOVE WS-IDARTNR-2 TO WS-IDARTNR-NUM                                  
670074***  COMPUTE WS-IDARTNR-KONVERTERAT = 999999999 -                         
680078***                                   WS-IDARTNR-NUM                      
690074***  MOVE WS-IDARTNR-KONVERTERAT TO W-IDARTNR-2                           
700074***  PERFORM IMS-GET-SATB-SATB01                                          
710074***  IF SEGMENT-FINNS                                                     
720074***    IF MSG-SIGNON-USERID = SATB01-STR-IDUSER                           
730074***      MOVE SPACE           TO W-KDSTRRAD                               
740000******   RADERNA LIGGER MED KDSTRRAD = ' ' I VALDA STRUKTUR               
750074***      MOVE MID-IDRADNR(INDX) TO WS-IDRADNR                             
760074***      INSPECT WS-IDRADNR REPLACING LEADING SPACE BY ZERO               
770074***      MOVE WS-IDRADNR TO W-IDRADNR                                     
780074***      PERFORM IMS-GET-SATB-SATB11                                      
790074***      IF SEGMENT-FINNS                                                 
800074***        CONTINUE                                                       
810074***      ELSE                                                             
820074***        MOVE NEJ TO INDATA-SW                                          
830074***                    RADVAL-SW                                          
840074***      END-IF                                                           
850074***    ELSE                                                               
860074***      MOVE NEJ TO INDATA-SW                                            
870074***                  RADVAL-SW                                            
880074***    END-IF                                                             
890074***  ELSE                                                                 
900074***    MOVE NEJ TO INDATA-SW                                              
910074***                RADVAL-SW                                              
920074***  END-IF                                                               
930000                                                                          
940074***  IF RADVAL-FEL                                                        
950074***    MOVE MFS-ALFA-FAELT-FEL TO MOD-SELECT-ATTR(INDX)                   
960074***    IF MOD-TEMFSINF = SPACE                                            
970074***      MOVE MED5 (SPRAK-IX) TO MOD-TEMFSINF                             
980074***    END-IF                                                             
990074***  END-IF                                                               
000000     .                                                                    
010000     EJECT                                                                
020000 H-UPPDATERA SECTION.                                                     
030000                                                                          
040074***  MOVE +1 TO INDX                                                      
050074***  PERFORM UNTIL INDX > MAX-INDX                                        
060074***    IF MID-SELECT(INDX) = 'S'                                          
070074***      PERFORM HA-LAEGG-UPP-STRUKTUR-RAD-KONV                           
080074***      MOVE MFS-FORMATETS-ATTR TO MOD-SELECT-ATTR(INDX)                 
090074***      MOVE '*'                TO MOD-SELECT(INDX)                      
100074***    ELSE                                                               
110074***      IF MID-SELECT(INDX) = 'B' OR 'D'                                 
120074***        PERFORM HB-TABORT-STRUKTUR-RAD                                 
130074***        MOVE MFS-FORMATETS-ATTR TO MOD-SELECT-ATTR(INDX)               
140074***        MOVE MFS-RENSA-FAELT    TO MOD-SELECT(INDX)                    
150074***      ELSE                                                             
160074***        IF MID-SELECT(INDX) = '*'                                      
170074***          MOVE MFS-FORMATETS-ATTR TO MOD-SELECT-ATTR(INDX)             
180074***          MOVE '*'                TO MOD-SELECT(INDX)                  
190074***        ELSE                                                           
200074***          MOVE MFS-ROER-EJ-FAELT TO MOD-SELECT(INDX)                   
210074***        END-IF                                                         
220074***      END-IF                                                           
230074***    END-IF                                                             
240074***    ADD 1 TO INDX                                                      
250074***  END-PERFORM                                                          
260000                                                                          
270074***  PERFORM MFS-ROR-EJ-FAELT-UT                                          
280000                                                                          
290074***  MOVE INF-UPDATE-DONE TO MED-IDMFSINF                                 
300074***  CALL WMEDKONV USING MED-WMEDAREA                                     
310074***  MOVE MED-MFSINF TO MOD-TEMFSINF                                      
320000* * * MFS-ROR-EJ-FAELT TILL FASTA VÄRDEN                                  
330000     .                                                                    
340000     EJECT                                                                
350000 HA-LAEGG-UPP-STRUKTUR-RAD-KONV SECTION.                                  
360000                                                                          
370074***  IF SOEKNYCKEL-LAAST-AV-EGET-ID                                       
380074***    CONTINUE                                                           
390074***  ELSE                                                                 
400074***    PERFORM HAA-LAGG-UPP-ARTIKEL-PA-WLXXAZ                             
410074***  END-IF                                                               
420000                                                                          
430074***  MOVE MID-IDARTNR(INDX) TO WS-IDARTNR-2                               
440074***  INSPECT WS-IDARTNR-2 REPLACING LEADING SPACE BY ZERO                 
450074***  MOVE WS-IDARTNR-2 TO WS-IDARTNR-NUM                                  
460074***  COMPUTE WS-IDARTNR-KONVERTERAT = 999999999 -                         
470078***                                   WS-IDARTNR-NUM                      
480000                                                                          
490000*********** LÄGG UPP STRUKTUR I KONVERTERAD FORM                          
500000                                                                          
510074***  MOVE WS-IDARTNR-KONVERTERAT TO W-IDARTNR-2                           
520074***  PERFORM IMS-GET-SATB-SATB01                                          
530074***  IF SEGMENT-FINNS                                                     
540074***    CONTINUE                                                           
550074***  ELSE                                                                 
560074***    MOVE WS-IDARTNR-2 TO W-IDARTNR-2                                   
570074***    PERFORM IMS-GET-SATB-SATB01                                        
580000                                                                          
590074***    MOVE WS-IDARTNR-KONVERTERAT TO SATB01-STR-IDARTNR                  
600074***    MOVE MSG-SIGNON-USERID      TO SATB01-STR-IDUSER                   
610074***    MOVE WS-SPAR-SPAERWDATUM    TO SATB01-STR-TIREGDAT                 
620074***    PERFORM IMS-ISRT-SATB-SATB01                                       
630074***  END-IF                                                               
640000                                                                          
650000*********** KOPIERA BERÖRD STRUKTURRAD                                    
660000                                                                          
670074***  MOVE WS-IDARTNR-2 TO W-IDARTNR-2                                     
680074***  PERFORM IMS-GET-SATB-SATB01                                          
690000                                                                          
700074***  MOVE MID-IDRADNR(INDX) TO WS-IDRADNR                                 
710074***  INSPECT WS-IDRADNR REPLACING LEADING SPACE BY ZERO                   
720074***  MOVE WS-IDRADNR TO W-IDRADNR                                         
730074***  MOVE '0'        TO W-KDSTRRAD                                        
740074***  PERFORM IMS-GET-SATB-SATB11                                          
750074***  IF SEGMENT-FINNS                                                     
760074***    CONTINUE                                                           
770074***  ELSE                                                                 
780074***    MOVE '9'      TO W-KDSTRRAD                                        
790074***    PERFORM IMS-GET-SATB-SATB11                                        
800074***  END-IF                                                               
810000                                                                          
820074***  IF SEGMENT-FINNS                                                     
830074***    MOVE WS-IDARTNR-KONVERTERAT TO W-IDARTNR-2                         
840074***    MOVE SPACE                  TO SATB11-RAD-KDSTRRAD                 
850074***    PERFORM IMS-ISRT-SATB-SATB11                                       
860074***  END-IF                                                               
870000     .                                                                    
880000     SKIP3                                                                
890000 HAA-LAGG-UPP-ARTIKEL-PA-WLXXAZ SECTION.                                  
900000***********************************************************               
910000* SÖKT ARTIKEL (IDARTNR ELLER IDLEVNR+BELEVART) LÄGGS UPP *               
920000* OCH ÄR DÄRMED 'LÅST' FÖR ANDRA USERID                   *               
930000***********************************************************               
940000                                                                          
950074***  MOVE '1151'    TO W-IDHTYP                                           
960074***  MOVE LOW-VALUE TO W-LOW-VALUE-2                                      
970074***  IF SOEKNYCKEL-IDARTNR                                                
980074***    MOVE WS-IDARTNR      TO XXAZ11-1152-IDARTNR                        
990074***    MOVE SPACE           TO XXAZ11-1152-IDLEVNR                        
000074***                            XXAZ11-1152-BELEVART                       
010074***  ELSE                                                                 
020074***    MOVE WS-IDLEVNR      TO XXAZ11-1152-IDLEVNR                        
030074***    MOVE WS-BELEVART     TO XXAZ11-1152-BELEVART                       
040074***    MOVE ZERO            TO XXAZ11-1152-IDARTNR                        
050074***  END-IF                                                               
060074***  MOVE MSG-SIGNON-USERID TO XXAZ11-1152-IDUSER                         
070074***  MOVE LOW-VALUE         TO XXAZ11-1152-LOW-VALUE                      
080074***  MOVE SPACE             TO XXAZ11-1152-FLAGGA                         
090079***                            XXAZ11-1152-IDAO                           
100074***  MOVE DAGENS-DATUM      TO XXAZ11-1152-TIREGDAT                       
110074***  SPÄRWDATUM SPARAS UNDAN                                              
120074***                            WS-SPAR-SPAERWDATUM                        
130074***  MOVE ZERO              TO XXAZ11-1152-TISTODAT                       
140074***  PERFORM IMS-ISRT-XXAZ-XXAZ11                                         
150074***  MOVE JA TO SOEKNYCKEL-LAAST-AV-EGET-ID-SW                            
160000     .                                                                    
170000     SKIP3                                                                
180000 HB-TABORT-STRUKTUR-RAD SECTION.                                          
190000***********************************************************               
200000* BORTTAG (ÅNGRA) VALD STRUKTURRAD. OM INGA FLER RADER    *               
210000* FINNS EFTER BORTTAG TAS OCKSÅ DEN KONV. STRUKTUREN BORT *               
220000***********************************************************               
230000                                                                          
240074***  MOVE MID-IDARTNR(INDX) TO WS-IDARTNR-2                               
250074***  INSPECT WS-IDARTNR-2 REPLACING LEADING SPACE BY ZERO                 
260074***  MOVE WS-IDARTNR-2 TO WS-IDARTNR-NUM                                  
270074***  COMPUTE WS-IDARTNR-KONVERTERAT = 999999999 -                         
280078***                                   WS-IDARTNR-NUM                      
290074***  MOVE WS-IDARTNR-KONVERTERAT TO W-IDARTNR-2                           
300074***  PERFORM IMS-GET-SATB-SATB01                                          
310074***  IF SEGMENT-FINNS                                                     
320074***    MOVE SPACE             TO W-KDSTRRAD                               
330000****** RADERNA LIGGER MED KDSTRRAD = ' ' I VALDA STRUKTUR                 
340074***    MOVE MID-IDRADNR(INDX) TO WS-IDRADNR                               
350074***    INSPECT WS-IDRADNR REPLACING LEADING SPACE BY ZERO                 
360074***    MOVE WS-IDRADNR TO W-IDRADNR                                       
370074***    PERFORM IMS-GET-SATB-SATB11                                        
380074***    IF SEGMENT-FINNS                                                   
390074***      PERFORM IMS-DLET-SATB                                            
400000                                                                          
410074***      PERFORM IMS-GET-SATB-SATB01                                      
420074***      PERFORM IMS-GET-SATB-SATB11-OKVAL                                
430074***      IF SEGMENT-FINNS                                                 
440000*********  FLER RADER FINNS VALDA FÖR AKTUELL STRUKTUR                    
450074***        CONTINUE                                                       
460074***      ELSE                                                             
470000*********  INGA RADER FINNS. TA BORT KONVERTERAD STRUKTUR                 
480074***        PERFORM IMS-GET-SATB-SATB01                                    
490074***        PERFORM IMS-DLET-SATB                                          
500074***      END-IF                                                           
510074***    END-IF                                                             
520074***  END-IF                                                               
530000                                                                          
540074***  PERFORM HBA-KOLLA-OM-SPAERR-SKALL-BORT                               
550000     .                                                                    
560000     SKIP3                                                                
570000 HBA-KOLLA-OM-SPAERR-SKALL-BORT SECTION.                                  
580000***********************************************                           
590000* OM INGA GÄLLANDE KONVERTERADE STRUKTURER    *                           
600000* FINNS KVAR TAS 'SPÄRREN' BORT PÅ SÖKNYCKELN *                           
610000***********************************************                           
620000                                                                          
630074***  MOVE NEJ TO SIGNON-USER-HAR-KONV-STRUKT-SW                           
640000                                                                          
650074***  MOVE MSG-SIGNON-USERID   TO W-IDUSER                                 
660074***  MOVE WS-MIN-IDARTNR-KONV TO W-MIN-IDARTNR-KONV                       
670074***  MOVE WS-MAX-IDARTNR-KONV TO W-MAX-IDARTNR-KONV                       
680000                                                                          
690074***  PERFORM IMS-GET-SATB-DSEQ-UNIK                                       
700074***  PERFORM UNTIL SEGMENT-SAKNAS                                         
710074***    IF SEGMENT-FINNS                                                   
720074***      MOVE 001                 TO WORK-KDCALL                          
730074***      MOVE WC-CDC-SE           TO WORK-IDDC                            
740074***      MOVE SATB01-STR-TIREGDAT TO WORK-TIAAMMDD-FOM                    
750074***      MOVE DAGENS-DATUM        TO WORK-TIAAMMDD-TOM                    
760074***      CALL WORKDAY USING WORK-KDCALL                                   
770074***                         WORK-DATE-AREA                                
780074***                         WORK-KDSVAR                                   
790074***      IF (WORK-KVWORKD > 2) OR (WORK-KDSVAR-FEL)                       
800000*******  ARB-KDSVAR-FEL INNEBÄR ATT ANTALET DAGAR > 999.                  
810074***        MOVE SATB01-STR-IDARTNR TO W-IDARTNR-2                         
820074***        PERFORM IMS-GET-SATB-SATB01                                    
830074***        PERFORM IMS-DLET-SATB                                          
840000                                                                          
850074***        PERFORM IMS-GET-SATB-DSEQ-NEXT                                 
860074***      ELSE                                                             
870000********** 'GÄLLANDE' KONVERTERAD STRUKTUR. MAN SÄTTER                    
880000********** STATUS TILL SEGMENT-SAKNAS FÖR ATT BRYTA                       
890074***        MOVE 'GE' TO STATUS-WS                                         
900074***        MOVE JA   TO SIGNON-USER-HAR-KONV-STRUKT-SW                    
910074***      END-IF                                                           
920074***    END-IF                                                             
930074***  END-PERFORM                                                          
940000                                                                          
950000                                                                          
960074***  IF SIGNON-USERID-HAR-KONV-STRUKT                                     
970074***    CONTINUE                                                           
980074***  ELSE                                                                 
990000*****  'SIGNON-USERID' SAKNAR GÄLLANDE KONVERTERADE STRUKTURER            
000074***    PERFORM IMS-GET-XXAZ-XXAZ01                                        
010074***    MOVE WS-IDARTNR        TO W-IDARTNR                                
020074***    MOVE WS-IDLEVNR        TO W-IDLEVNR                                
030074***    MOVE WS-BELEVART       TO W-BELEVART                               
040074***    MOVE MSG-SIGNON-USERID TO W-IDUSER                                 
050074***    PERFORM IMS-GET-XXAZ-XXAZ11                                        
060074***    IF SEGMENT-FINNS                                                   
070074***      PERFORM IMS-DLET-XXAZ-XXAZ11                                     
080074***      MOVE NEJ TO SOEKNYCKEL-LAAST-AV-EGET-ID-SW                       
090074***    END-IF                                                             
100074***  END-IF                                                               
110000     .                                                                    
120000     EJECT                                                                
130000 I-KOLLA-INPUT-PRINTER SECTION.                                           
140000                                                                          
150074***  MOVE NEJ TO ALLT-SW                                                  
160000                                                                          
170074***  IF MID-KDPRTVAL = ALL '+'                                            
180074***    MOVE NEJ TO INDATA-SW                                              
190074***  ELSE                                                                 
200074***    IF MID-KDPRTVAL = 'A' OR 'B' OR 'C' OR 'D'                         
210074***      MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDPRTVAL-ATTR                   
220074***      EVALUATE MID-KDPRTVAL                                            
230074***        WHEN 'A' MOVE SATS-I-STR-RA     TO PRT-IDPRTLST                
240074***        WHEN 'B' MOVE SATS-I-STR-RB     TO PRT-IDPRTLST                
250074***        WHEN 'C' MOVE SATS-I-STR-BERPV  TO PRT-IDPRTLST                
260074**         WHEN 'D' MOVE SATS-I-STR-CARP   TO PRT-IDPRTLST                
270074***      END-EVALUATE                                                     
280002*        **************************************************               
290002*        *  HÄMTAR PRINTERNS LOGISKA NAMN TILL TEMFSINF   *               
300002*        **************************************************               
310003                                                                          
320074***      MOVE 1                TO PRT-KDCALL                              
330074***      CALL W006PRT USING PRT-W006PRT                                   
340074***      IF PRT-IDLTERM = 'SAKNAS  '                                      
350074***        MOVE NEJ TO INDATA-SW                                          
360074***        MOVE MED9 (SPRAK-IX) TO MOD-TEMFSINF                           
370074***      ELSE                                                             
380074***        MOVE PRT-IDLTERM  TO MED6-IDLTERM     (SPRAK-IX)               
390074***        MOVE PRT-BEPRTLST TO MED6-BEPRT       (SPRAK-IX)               
400074***        MOVE ','          TO MED6-KOMMATECKEN (SPRAK-IX)               
410074***      END-IF                                                           
420074***    ELSE                                                               
430074***      MOVE NEJ TO INDATA-SW                                            
440074***    END-IF                                                             
450074***  END-IF                                                               
460000                                                                          
470074***  PERFORM MFS-ROR-EJ-FAELT-IN                                          
480074***  PERFORM MFS-ROR-EJ-FAELT-UT                                          
490074***  PERFORM MFS-LAS-IN-IGEN                                              
500000                                                                          
510074***  IF INDATA-FEL                                                        
520074***    MOVE MFS-ALFA-FAELT-FEL   TO MOD-KDPRTVAL-ATTR                     
530074***    MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                          
540074***    CALL WMEDKONV USING MED-WMEDAREA                                   
550074***    MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
560074***  END-IF                                                               
570000     .                                                                    
580000     EJECT                                                                
590000 J-SKICKA-TRANS-TILL-1291 SECTION.                                        
600000*****************************************                                 
610000* STARTA BAKGRUNDSPGM SOM PRINTAR LISTA *                                 
620000*****************************************                                 
630000                                                                          
640074***  MOVE MID-W1I22101 TO MID-W1I29101                                    
650074***  PERFORM IMS-INSERT-ALT-MSG                                           
660074***  MOVE MED6 (SPRAK-IX) TO MOD-TEMFSINF                                 
670000     .                                                                    
680000     EJECT                                                                
690000 S01-HAEMTA-BEART-BSEQ SECTION.                                           
700000                                                                          
710000     PERFORM IMS-GET-BENA-BENA01-BSEQ                                     
720000     IF SEGMENT-FINNS                                                     
730000       MOVE WS-IDSKYLT TO W-IDSKYLT                                       
740000       PERFORM IMS-GET-BENA-BENA11-BSEQ                                   
750000       IF SEGMENT-FINNS                                                   
760000         MOVE BENA11-TEXT-BEART TO WS-BEART                               
770000       ELSE                                                               
780000         MOVE SPACE             TO WS-BEART                               
790000       END-IF                                                             
800000     ELSE                                                                 
810000       MOVE SPACE TO WS-BEART                                             
820000     END-IF                                                               
830000     .                                                                    
840000     EJECT                                                                
850000 S02-HAEMTA-BEART-ASEQ SECTION.                                           
860000                                                                          
870000     MOVE 'S' TO W-IDSKYLT                                                
880000     PERFORM IMS-GET-BENA-BENA01-ASEQ                                     
890000     PERFORM UNTIL SEGMENT-SAKNAS OR                                      
900000                    WS-KDBENHOM = BENA01-BEN-KDHOMONYM                    
910000       IF SEGMENT-FINNS                                                   
920000         IF WS-KDBENHOM = BENA01-BEN-KDHOMONYM                            
930000           CONTINUE                                                       
940000         ELSE                                                             
950000           PERFORM IMS-GET-BENA-BENA01-ASEQ-NEXT                          
960000         END-IF                                                           
970000       ELSE                                                               
980000         MOVE SPACE TO WS-BEART                                           
990000       END-IF                                                             
000000     END-PERFORM                                                          
010000                                                                          
020000     IF SEGMENT-FINNS                                                     
030000       MOVE WS-IDSKYLT TO W-IDSKYLT                                       
040000       PERFORM IMS-GET-BENA-BENA11-ASEQ                                   
050000       IF SEGMENT-FINNS                                                   
060000         MOVE BENA11-TEXT-BEART TO WS-BEART                               
070000       ELSE                                                               
080000         MOVE SPACE             TO WS-BEART                               
090000       END-IF                                                             
100000     END-IF                                                               
110000     .                                                                    
120000     EJECT                                                                
130000 S04-KOLLA-OM-RAD-SKA-MED SECTION.                                        
140000                                                                          
150000     MOVE NEJ         TO RAD-SW                                           
160041                         STRUKTURNR-FINNS-PAA-WDK6-SW                     
170076***  MOVE WS-KDPRODSL TO WS-KDPRODSL-NUM                                  
180000                                                                          
190000     MOVE SATB01C-STR-IDARTNR TO W-IDARTNR-2                              
200038     PERFORM IMS-GET-ARTC01-PCB2                                          
210000     IF SEGMENT-FINNS                                                     
220041       MOVE JA TO STRUKTURNR-FINNS-PAA-WDK6-SW                            
230000     END-IF                                                               
230176                                                                          
230276     MOVE SPACE TO WS-1002-SATS                                           
232076     MOVE ZERO TO WS-KDPRODSL-NUM                                         
233076     MOVE JA TO RAD-SW                                                    
240000                                                                          
250076***  IF WS-1002-SATS = 'Y' OR 'J'                                         
260076***    IF SATB01C-STR-IDLEVNR = '1002 '                                   
270076***      IF WS-KDPRODSL-NUM = 0                                           
280000           MOVE JA TO RAD-SW                                              
290076***      ELSE                                                             
300076***        IF STRUKTURNR-FINNS-PAA-WDK6                                   
310076***          IF ARTC012-ART-KDPRODSL = WS-KDPRODSL-NUM                    
320076***            MOVE JA TO RAD-SW                                          
330076***          END-IF                                                       
340076***        ELSE                                                           
350076***          IF SATB01C-STR-KDPRODSL = WS-KDPRODSL-NUM                    
360076***            MOVE JA TO RAD-SW                                          
370076***          END-IF                                                       
380076***        END-IF                                                         
390076***      END-IF                                                           
400076***    END-IF                                                             
410076***  ELSE                                                                 
420076***    IF WS-1002-SATS = 'N'                                              
430076***      IF SATB01C-STR-IDLEVNR NOT = '1002 '                             
440076***        IF WS-KDPRODSL-NUM = 0                                         
450076***          MOVE JA TO RAD-SW                                            
460076***        ELSE                                                           
470076***          IF STRUKTURNR-FINNS-PAA-WDK6                                 
480076***            IF ARTC012-ART-KDPRODSL = WS-KDPRODSL-NUM                  
490076***              MOVE JA TO RAD-SW                                        
500076***            END-IF                                                     
510076***          ELSE                                                         
520076***            IF SATB01C-STR-KDPRODSL = WS-KDPRODSL-NUM                  
530076***              MOVE JA TO RAD-SW                                        
540076***            END-IF                                                     
550076***          END-IF                                                       
560076***        END-IF                                                         
570076***      END-IF                                                           
580076***    ELSE                                                               
590000******** WS-1002-SATS = SPACE                                             
600076***      IF WS-KDPRODSL-NUM = 0                                           
610076***        MOVE JA TO RAD-SW                                              
620076***      ELSE                                                             
630076***        IF STRUKTURNR-FINNS-PAA-WDK6                                   
640076***          IF ARTC012-ART-KDPRODSL = WS-KDPRODSL-NUM                    
650076***            MOVE JA TO RAD-SW                                          
660076***          END-IF                                                       
670076***        ELSE                                                           
680076***          IF SATB01C-STR-KDPRODSL = WS-KDPRODSL-NUM                    
690076***            MOVE JA TO RAD-SW                                          
700076***          END-IF                                                       
710076***        END-IF                                                         
720076***      END-IF                                                           
730000                                                                          
740076***    END-IF                                                             
750000                                                                          
760076**   END-IF                                                               
770000     .                                                                    
780000     EJECT                                                                
790000 S05-KOLLA-OM-MID-INPUT-IFYLLD SECTION.                                   
800000                                                                          
810075***  MOVE NEJ TO MID-INPUT-SW                                             
820000                                                                          
830075***  MOVE +1 TO INDX                                                      
840075***  PERFORM UNTIL (INDX > MAX-INDX) OR (MID-INPUT-IFYLLD)                
850075***    IF MID-SELECT(INDX) NOT = ALL '+'                                  
860075***      IF MID-SELECT(INDX) = '*' OR ' '                                 
870075***        CONTINUE                                                       
880075***      ELSE                                                             
890075***        MOVE JA TO MID-INPUT-SW                                        
900075***      END-IF                                                           
910075***    END-IF                                                             
920075***    ADD +1 TO INDX                                                     
930075***  END-PERFORM                                                          
940000     .                                                                    
950000     EJECT                                                                
960000 S06-LAES-RADDATA-FOERSTA SECTION.                                        
970000                                                                          
980000     MOVE NEJ TO RAD-SW                                                   
990000                                                                          
000000                                                                          
010000     IF (MFS-ENTER) AND (MID-IDARTNR-ENTER > 0)                           
020000       MOVE MID-IDARTNR-ENTER  TO W-IDARTNR-2                             
030000       MOVE MID-KDSTRRAD-ENTER TO W-KDSTRRAD                              
040000       MOVE MID-IDRADNR-ENTER  TO W-IDRADNR                               
050000       PERFORM IMS-GET-SATB-CSEQ-UNIK                                     
060000       IF SEGMENT-FINNS                                                   
070000         CONTINUE                                                         
080000       ELSE                                                               
090000         PERFORM IMS-GET-SATB-CSEQ-NEXT                                   
100000       END-IF                                                             
110000     ELSE                                                                 
120000       IF (MFS-NEXT) AND (MID-IDARTNR-NEXT > 0)                           
130000         MOVE MID-IDARTNR-NEXT  TO W-IDARTNR-2                            
140000         MOVE MID-KDSTRRAD-NEXT TO W-KDSTRRAD                             
150000         MOVE MID-IDRADNR-NEXT  TO W-IDRADNR                              
160000         PERFORM IMS-GET-SATB-CSEQ-UNIK                                   
170000         IF SEGMENT-FINNS                                                 
180000           CONTINUE                                                       
190000         ELSE                                                             
200000           PERFORM IMS-GET-SATB-CSEQ-NEXT                                 
210000         END-IF                                                           
220000       ELSE                                                               
230000         PERFORM IMS-GET-SATB-CSEQ-NEXT                                   
240000       END-IF                                                             
250000     END-IF                                                               
260000                                                                          
270000     PERFORM UNTIL SEGMENT-SAKNAS OR RAD-OK                               
280000                                                                          
290000       IF SEGMENT-FINNS                                                   
300000         IF SATB01C-STR-IDARTNR NOT < WS-MAX-IDARTNR                      
310000********** NÄR MAN 'FÅR IN' ETT KONVERTERAT STRUKTURNR                    
320000********** FINNS DET BARA KONV STRUKTURER KVAR OCH                        
330000********** MAN SÄTTER STATUS TILL SEGMENT-SAKNAS                          
340000           MOVE 'GE' TO STATUS-WS                                         
350000         ELSE                                                             
360000           IF SATB01C-STR-TIBORT > 0                                      
370000             PERFORM IMS-GET-SATB-CSEQ-NEXT                               
380000           ELSE                                                           
390051             MOVE SATB11C-RAD-TISTODAT   TO TMP1-YYMMDD                   
400051             MOVE DAGENS-DATUM           TO TMP2-YYMMDD                   
410051             PERFORM WY2000P1                                             
420051             IF TMP1-YYMMDD > TMP2-YYMMDD                                 
430000               PERFORM S04-KOLLA-OM-RAD-SKA-MED                           
440000               IF RAD-OK                                                  
450000                 CONTINUE                                                 
460000               ELSE                                                       
470000                 PERFORM IMS-GET-SATB-CSEQ-NEXT                           
480000               END-IF                                                     
490000             ELSE                                                         
500000               PERFORM IMS-GET-SATB-CSEQ-NEXT                             
510000             END-IF                                                       
520000           END-IF                                                         
530000         END-IF                                                           
540000       END-IF                                                             
550000                                                                          
560000     END-PERFORM                                                          
570000     .                                                                    
580000     EJECT                                                                
590000 S07-LAES-RADDATA-NAESTA SECTION.                                         
600000                                                                          
610000     MOVE NEJ TO RAD-SW                                                   
620000                                                                          
630000     PERFORM IMS-GET-SATB-CSEQ-NEXT                                       
640000                                                                          
650000     PERFORM UNTIL (SEGMENT-SAKNAS OR RAD-OK)                             
660000                                                                          
670000       IF SEGMENT-FINNS                                                   
680000         IF SATB01C-STR-IDARTNR NOT < WS-MAX-IDARTNR                      
690000********** NÄR MAN 'FÅR IN' ETT KONVERTERAT STRUKTURNR                    
700000********** FINNS DET BARA KONV STRUKTURER KVAR OCH                        
710000********** MAN SÄTTER STATUS TILL SEGMENT-SAKNAS                          
720000           MOVE 'GE' TO STATUS-WS                                         
730000         ELSE                                                             
740000           IF SATB01C-STR-TIBORT > 0                                      
750000             PERFORM IMS-GET-SATB-CSEQ-NEXT                               
760000           ELSE                                                           
770051             MOVE SATB11C-RAD-TISTODAT   TO TMP1-YYMMDD                   
780051             MOVE DAGENS-DATUM           TO TMP2-YYMMDD                   
790051             PERFORM WY2000P1                                             
800051             IF TMP1-YYMMDD > TMP2-YYMMDD                                 
810000               PERFORM S04-KOLLA-OM-RAD-SKA-MED                           
820000               IF RAD-OK                                                  
830000                 CONTINUE                                                 
840000               ELSE                                                       
850000                 PERFORM IMS-GET-SATB-CSEQ-NEXT                           
860000               END-IF                                                     
870000             ELSE                                                         
880000               PERFORM IMS-GET-SATB-CSEQ-NEXT                             
890000             END-IF                                                       
900000           END-IF                                                         
910000         END-IF                                                           
920000       END-IF                                                             
930000                                                                          
940000     END-PERFORM                                                          
950000     .                                                                    
960000     EJECT                                                                
970000 MFS-RENSA-FAELT-UT SECTION.                                              
980000                                                                          
990000*    --- ALLA UTDATA-FÄLT                                                 
000000*    --- INKL. BLÄDDRINGSNYCKLAR                                          
010079***  MOVE MFS-RENSA-FAELT TO MOD-BEART-UT                                 
020073***                          MOD-TEARTNOT                                 
030073***                          MOD-TEARTNOT-7                               
040079     MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-ENTER                            
050000                             MOD-KDSTRRAD-ENTER                           
060000                             MOD-IDRADNR-ENTER                            
070000                             MOD-IDARTNR-NEXT                             
080000                             MOD-KDSTRRAD-NEXT                            
090000                             MOD-IDRADNR-NEXT                             
100000     MOVE +1 TO INDX                                                      
110000     PERFORM UNTIL INDX > MAX-INDX                                        
120000       PERFORM MFS-RENSA-RAD-FAELT-UT                                     
130000       ADD +1 TO INDX                                                     
140000     END-PERFORM                                                          
150000     .                                                                    
160000     SKIP2                                                                
170000 MFS-RENSA-RAD-FAELT-UT SECTION.                                          
180000                                                                          
190000*    --- UTDATA-FÄLT PÅ BLÄDDRINGSRADER                                   
200000     MOVE MFS-RENSA-FAELT TO MOD-IDRADNR (INDX)                           
210000                             MOD-IDARTNR (INDX)                           
220000                             MOD-BEART   (INDX)                           
230000                             MOD-REANTPSA(INDX)                           
240000                             MOD-IDSTRTYP(INDX)                           
250000                             MOD-KDERS   (INDX)                           
260000                             MOD-IDLEVNR (INDX)                           
270000                             MOD-KDPRODSL(INDX)                           
280050                             MOD-KDPSLLOC(INDX)                           
290000     .                                                                    
300000     SKIP2                                                                
310000 MFS-RENSA-FAELT-IN SECTION.                                              
320000                                                                          
330000*    --- ALLA INDATA-FÄLT                                                 
340073***  MOVE +1 TO INDX                                                      
350073***  PERFORM UNTIL INDX > MAX-INDX                                        
360073***    MOVE MFS-RENSA-FAELT TO MOD-SELECT(INDX)                           
370073***    ADD 1 TO INDX                                                      
380073***  END-PERFORM                                                          
390073***  MOVE MFS-RENSA-FAELT TO MOD-KDPRTVAL                                 
400000     .                                                                    
410000     EJECT                                                                
420000 MFS-ROR-EJ-FAELT-UT  SECTION.                                            
430000                                                                          
440000*    --- ALLA UTDATA-FÄLT                                                 
450000*    --- INKL BLÄDDRINGSNYCKLAR OCH RAD-DATA                              
460000     MOVE MFS-ROER-EJ-FAELT TO MOD-IDLEVNR-UT                             
470000                               MOD-BELEVART-UT                            
480028                               MOD-IDARTNR-SPAR                           
490000                               MOD-IDSKYLT-UT                             
500073***                            MOD-1002-SATS-UT                           
510073***                            MOD-KDPRODSL-UT                            
520073***                            MOD-BEART-UT                               
530073***                            MOD-TEARTNOT                               
540073***                            MOD-TEARTNOT-7                             
550000                               MOD-IDARTNR-ENTER                          
560000                               MOD-KDSTRRAD-ENTER                         
570000                               MOD-IDRADNR-ENTER                          
580000                               MOD-IDARTNR-NEXT                           
590000                               MOD-KDSTRRAD-NEXT                          
600000                               MOD-IDRADNR-NEXT                           
610000                                                                          
620000     MOVE +1 TO INDX                                                      
630000     PERFORM UNTIL INDX > MAX-INDX                                        
640000       PERFORM MFS-ROR-EJ-RAD-FAELT-UT                                    
650000       ADD +1 TO INDX                                                     
660000     END-PERFORM                                                          
670000     .                                                                    
680000     SKIP2                                                                
690000 MFS-ROR-EJ-RAD-FAELT-UT  SECTION.                                        
700000                                                                          
710000*    --- UTDATA-FÄLT PÅ BLÄDDRINGSRADER                                   
720000     MOVE MFS-ROER-EJ-FAELT TO MOD-IDRADNR (INDX)                         
730000                               MOD-IDARTNR (INDX)                         
740000                               MOD-BEART   (INDX)                         
750000                               MOD-REANTPSA(INDX)                         
760000                               MOD-IDSTRTYP(INDX)                         
770000                               MOD-KDERS   (INDX)                         
780000                               MOD-IDLEVNR (INDX)                         
790000                               MOD-KDPRODSL(INDX)                         
800050                               MOD-KDPSLLOC(INDX)                         
810000     .                                                                    
820000     SKIP2                                                                
830000 MFS-ROR-EJ-FAELT-IN  SECTION.                                            
840000                                                                          
850000*    --- ALLA INDATA-FÄLT                                                 
860073***  MOVE +1 TO INDX                                                      
870073***  PERFORM UNTIL INDX > MAX-INDX                                        
880073***    MOVE MFS-ROER-EJ-FAELT TO MOD-SELECT(INDX)                         
890073***    ADD 1 TO INDX                                                      
900073***  END-PERFORM                                                          
910073***  MOVE MFS-ROER-EJ-FAELT TO MOD-KDPRTVAL                               
920000     .                                                                    
930000     EJECT                                                                
940000 MFS-LAS-IN-IGEN SECTION.                                                 
950000                                                                          
960000*    --- ALLA INDATA-FÄLT                                                 
970073***  MOVE +1 TO INDX                                                      
980073***  PERFORM UNTIL INDX > MAX-INDX                                        
990073***    MOVE MFS-ADD-LAES-IN-FAELT TO MOD-SELECT-ATTR(INDX)                
000073***    ADD 1 TO INDX                                                      
010073***  END-PERFORM                                                          
020000     .                                                                    
030000     EJECT                                                                
040000* --- IMS SEKTIONER ---                                                   
050000     SKIP3                                                                
060000 IMS-GET-MSG SECTION.                                                     
070000                                                                          
080000     MOVE '  QC' TO GODK-STATUSKODER                                      
090000     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
100000     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
110000     PERFORM IMS-STATUSKONTROLL                                           
120000     .                                                                    
130000     SKIP3                                                                
140000 IMS-INSERT-MSG SECTION.                                                  
150000                                                                          
160071***  IF MSGI-IDLAND-SPR NOT = 'GB'                                        
170071***    MOVE '0' TO MFS-KDHUVOMR                                           
180071***  END-IF                                                               
190000     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
200000     MOVE SPACE TO GODK-STATUSKODER                                       
210000     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
220000     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
230000     PERFORM IMS-STATUSKONTROLL                                           
240000     .                                                                    
250000     EJECT                                                                
260000 IMS-INSERT-ALT-MSG SECTION.                                              
270000                                                                          
280000     MOVE SPACE TO GODK-STATUSKODER                                       
290000     CALL CBLTDLI USING ISRT ALT-PCB W-PROG-TO-PROG-SW                    
300000     MOVE ALT-STATUS-CODE TO STATUS-WS                                    
310000     PERFORM IMS-STATUSKONTROLL                                           
320000     .                                                                    
330000     EJECT                                                                
340000 IMS-DLET-SATB SECTION.                                                   
350000                                                                          
360000     MOVE '  ' TO GODK-STATUSKODER                                        
370000     CALL CBLTDLI USING DLET SATB-PCB DLI-IO-AREA                         
380000     MOVE SATB-STATUS-CODE TO STATUS-WS                                   
390000     PERFORM IMS-STATUSKONTROLL                                           
400000     .                                                                    
410000     SKIP3                                                                
420000 IMS-GET-SATB-SATB01 SECTION.                                             
430000                                                                          
440000     STRING 'WLSATB01(IDARTNR  =' W-IDARTNR-2-X ')'                       
450000          DELIMITED BY SIZE INTO SSA1                                     
460000     MOVE '  GE' TO GODK-STATUSKODER                                      
470000     CALL CBLTDLI USING GHU SATB-PCB DLI-IO-AREA SSA1                     
480000     MOVE SATB-STATUS-CODE TO STATUS-WS                                   
490000     PERFORM IMS-STATUSKONTROLL                                           
500000     .                                                                    
510000     SKIP3                                                                
520000 IMS-GET-SATB-SATB11 SECTION.                                             
530000                                                                          
540000     STRING 'WLSATB11*F(WDJ111KY =' W-WDJ111KY-X ')'                      
550000          DELIMITED BY SIZE INTO SSA1                                     
560000     MOVE '  GE' TO GODK-STATUSKODER                                      
570000     CALL CBLTDLI USING GHNP SATB-PCB DLI-IO-AREA SSA1                    
580000     MOVE SATB-STATUS-CODE TO STATUS-WS                                   
590000     PERFORM IMS-STATUSKONTROLL                                           
600000     .                                                                    
610000     SKIP3                                                                
620000 IMS-GET-SATB-SATB11-OKVAL SECTION.                                       
630000                                                                          
640000     MOVE 'WLSATB11 ' TO SSA1                                             
650000     MOVE '  GE' TO GODK-STATUSKODER                                      
660000     CALL CBLTDLI USING GHNP SATB-PCB DLI-IO-AREA SSA1                    
670000     MOVE SATB-STATUS-CODE TO STATUS-WS                                   
680000     PERFORM IMS-STATUSKONTROLL                                           
690000     .                                                                    
700000     SKIP3                                                                
710000 IMS-ISRT-SATB-SATB01 SECTION.                                            
720000                                                                          
730000     MOVE 'WLSATB01 ' TO SSA1                                             
740000     MOVE '  ' TO GODK-STATUSKODER                                        
750000     CALL CBLTDLI USING ISRT SATB-PCB DLI-IO-AREA SSA1                    
760000     MOVE SATB-STATUS-CODE TO STATUS-WS                                   
770000     PERFORM IMS-STATUSKONTROLL                                           
780000     .                                                                    
790000     SKIP3                                                                
800000 IMS-ISRT-SATB-SATB11 SECTION.                                            
810000                                                                          
820000     STRING 'WLSATB01(IDARTNR  =' W-IDARTNR-2-X ')'                       
830000          DELIMITED BY SIZE INTO SSA1                                     
840000     MOVE 'WLSATB11 ' TO SSA2                                             
850000     MOVE '  ' TO GODK-STATUSKODER                                        
860000     CALL CBLTDLI USING ISRT SATB-PCB DLI-IO-AREA SSA1 SSA2               
870000     MOVE SATB-STATUS-CODE TO STATUS-WS                                   
880000     PERFORM IMS-STATUSKONTROLL                                           
890000     .                                                                    
900000     EJECT                                                                
910000 IMS-GET-SATB-CSEQ-NEXT SECTION.                                          
920000                                                                          
930000     STRING 'WLSATB11*D(WDJ1CSEQ =' W-WDJ1CSEQ-X ')'                      
940000          DELIMITED BY SIZE INTO SSA1                                     
950000     MOVE 'WLSATB01 ' TO SSA2                                             
960000     MOVE '  GE' TO GODK-STATUSKODER                                      
970000     CALL CBLTDLI USING GN SATB-C-PCB DLI-IO-AREA-2 SSA1 SSA2             
980000     MOVE SATB-C-STATUS-CODE TO STATUS-WS                                 
990000     PERFORM IMS-STATUSKONTROLL                                           
000000     .                                                                    
010000     SKIP3                                                                
020000 IMS-GET-SATB-CSEQ-UNIK SECTION.                                          
030000                                                                          
040000     STRING 'WLSATB11*D(WDJ1CSEQ =' W-WDJ1CSEQ-X                          
050000                      '&WDJ111KY =' W-WDJ111KY-X ')'                      
060000          DELIMITED BY SIZE INTO SSA1                                     
070000     STRING 'WLSATB01(IDARTNR  =' W-IDARTNR-2-X ')'                       
080000          DELIMITED BY SIZE INTO SSA2                                     
090000     MOVE '  GE' TO GODK-STATUSKODER                                      
100000     CALL CBLTDLI USING GU SATB-C-PCB DLI-IO-AREA-2 SSA1 SSA2             
110000     MOVE SATB-C-STATUS-CODE TO STATUS-WS                                 
120000     PERFORM IMS-STATUSKONTROLL                                           
130000     .                                                                    
140000     EJECT                                                                
150000 IMS-GET-SATB-DSEQ-NEXT SECTION.                                          
160000**** OBS! ENDAST KONVERTERADE STRUKTURER 'TAS IN'                         
170000                                                                          
180000     STRING 'WLSATB01(WDJ1DSEQ>=' W-IDUSER-X                              
190000                                  W-MIN-IDARTNR-KONV-X                    
200000                    '&WDJ1DSEQ<=' W-IDUSER-X                              
210000                                  W-MAX-IDARTNR-KONV-X ')'                
220000          DELIMITED BY SIZE INTO SSA1                                     
230000     MOVE '  GE' TO GODK-STATUSKODER                                      
240000     CALL CBLTDLI USING GHN SATB-D-PCB DLI-IO-AREA SSA1                   
250000     MOVE SATB-D-STATUS-CODE TO STATUS-WS                                 
260000     PERFORM IMS-STATUSKONTROLL                                           
270000     .                                                                    
280000     SKIP3                                                                
290000 IMS-GET-SATB-DSEQ-UNIK SECTION.                                          
300000**** OBS! ENDAST KONVERTERADE STRUKTURER 'TAS IN'                         
310000                                                                          
320000     STRING 'WLSATB01(WDJ1DSEQ>=' W-IDUSER-X                              
330000                                  W-MIN-IDARTNR-KONV-X                    
340000                    '&WDJ1DSEQ<=' W-IDUSER-X                              
350000                                  W-MAX-IDARTNR-KONV-X ')'                
360000          DELIMITED BY SIZE INTO SSA1                                     
370000     MOVE '  GE' TO GODK-STATUSKODER                                      
380000     CALL CBLTDLI USING GHU SATB-D-PCB DLI-IO-AREA SSA1                   
390000     MOVE SATB-D-STATUS-CODE TO STATUS-WS                                 
400000     PERFORM IMS-STATUSKONTROLL                                           
410000     .                                                                    
420000     EJECT                                                                
430000 IMS-GET-XXAZ-XXAZ01 SECTION.                                             
440000                                                                          
450000     STRING 'WLXXAZ01(WDGXKEY  =' W-WDGXKEY-X ')'                         
460000          DELIMITED BY SIZE INTO SSA1                                     
470000     MOVE '    ' TO GODK-STATUSKODER                                      
480000     CALL CBLTDLI USING GU XXAZ-PCB DLI-IO-AREA SSA1                      
490000     MOVE XXAZ-STATUS-CODE TO STATUS-WS                                   
500000     PERFORM IMS-STATUSKONTROLL                                           
510000     .                                                                    
520000     SKIP3                                                                
530000 IMS-GET-XXAZ-XXAZ11 SECTION.                                             
540000                                                                          
550000     STRING 'WLXXAZ11*F(WDGXKEY  =' W-IDLEVNR-X                           
560000                                    W-BELEVART-X                          
570000                                    W-IDARTNR-X                           
580000                                    W-IDUSER-X                            
590000                                    W-LOW-VALUE-X ')'                     
600000          DELIMITED BY SIZE INTO SSA1                                     
610000     MOVE '  GE' TO GODK-STATUSKODER                                      
620000     CALL CBLTDLI USING GHNP XXAZ-PCB DLI-IO-AREA SSA1                    
630000     MOVE XXAZ-STATUS-CODE TO STATUS-WS                                   
640000     PERFORM IMS-STATUSKONTROLL                                           
650000     .                                                                    
660000     SKIP3                                                                
670000 IMS-GET-XXAZ-XXAZ11-IDARTNR SECTION.                                     
680000                                                                          
690000     STRING 'WLXXAZ11*F(IDARTNR  =' W-IDARTNR-X ')'                       
700000          DELIMITED BY SIZE INTO SSA1                                     
710000     MOVE '  GE' TO GODK-STATUSKODER                                      
720000     CALL CBLTDLI USING GHNP XXAZ-PCB DLI-IO-AREA SSA1                    
730000     MOVE XXAZ-STATUS-CODE TO STATUS-WS                                   
740000     PERFORM IMS-STATUSKONTROLL                                           
750000     .                                                                    
760000     SKIP3                                                                
770000 IMS-GET-XXAZ-XXAZ11-IDLEV-BELE SECTION.                                  
780000                                                                          
790000     STRING 'WLXXAZ11*F(IDLEVNR  =' W-IDLEVNR-X                           
800000                    '&BELEVART =' W-BELEVART-X ')'                        
810000          DELIMITED BY SIZE INTO SSA1                                     
820000     MOVE '  GE' TO GODK-STATUSKODER                                      
830000     CALL CBLTDLI USING GHNP XXAZ-PCB DLI-IO-AREA SSA1                    
840000     MOVE XXAZ-STATUS-CODE TO STATUS-WS                                   
850000     PERFORM IMS-STATUSKONTROLL                                           
860000     .                                                                    
870000     SKIP3                                                                
880000 IMS-ISRT-XXAZ-XXAZ11 SECTION.                                            
890000                                                                          
900000     STRING 'WLXXAZ01(WDGXKEY  =' W-WDGXKEY-X ')'                         
910000          DELIMITED BY SIZE INTO SSA1                                     
920000     MOVE 'WLXXAZ11 ' TO SSA2                                             
930000     MOVE '  ' TO GODK-STATUSKODER                                        
940000     CALL CBLTDLI USING ISRT XXAZ-PCB DLI-IO-AREA SSA1 SSA2               
950000     MOVE XXAZ-STATUS-CODE TO STATUS-WS                                   
960000     PERFORM IMS-STATUSKONTROLL                                           
970000     .                                                                    
980000     SKIP3                                                                
990000 IMS-DLET-XXAZ-XXAZ11 SECTION.                                            
000000                                                                          
010000     MOVE '  ' TO GODK-STATUSKODER                                        
020000     CALL CBLTDLI USING DLET XXAZ-PCB DLI-IO-AREA                         
030000     MOVE XXAZ-STATUS-CODE TO STATUS-WS                                   
040000     PERFORM IMS-STATUSKONTROLL                                           
050000     .                                                                    
060000     SKIP2                                                                
070000 IMS-GET-XXAZ-XXAZ21 SECTION.                                             
080000                                                                          
090000     STRING 'WLXXAZ11*F(WDGXKEY  =' W-IDLEVNR-X                           
100000                                    W-BELEVART-X                          
110000                                    W-IDARTNR-X                           
120000                                    W-IDUSER-X                            
130000                                    W-LOW-VALUE-X ')'                     
140000          DELIMITED BY SIZE INTO SSA1                                     
150000     MOVE 'WLXXAZ21 ' TO SSA2                                             
160000     MOVE '  GE'      TO GODK-STATUSKODER                                 
170000     CALL CBLTDLI USING GHNP XXAZ-PCB DLI-IO-AREA SSA1 SSA2               
180000     MOVE XXAZ-STATUS-CODE TO STATUS-WS                                   
190000     PERFORM IMS-STATUSKONTROLL                                           
200000     .                                                                    
210000     EJECT                                                                
220038 IMS-GET-ARTC01-PCB2 SECTION.                                             
230000                                                                          
240038     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-2-X ')'                       
250000          DELIMITED BY SIZE INTO SSA1                                     
260000     MOVE '  GE' TO GODK-STATUSKODER                                      
270038     CALL CBLTDLI USING GU ARTC2-PCB DLI-IO-AREA-3 SSA1                   
280038     MOVE ARTC2-STATUS-CODE TO STATUS-WS                                  
290000     PERFORM IMS-STATUSKONTROLL                                           
300000     .                                                                    
310000     EJECT                                                                
320000 IMS-GET-ARTC-ARTC01 SECTION.                                             
330000                                                                          
340000     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-2-X ')'                       
350000          DELIMITED BY SIZE INTO SSA1                                     
360000     MOVE '  GE' TO GODK-STATUSKODER                                      
370000     CALL CBLTDLI USING GU ARTC-PCB DLI-IO-AREA SSA1                      
380000     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
390000     PERFORM IMS-STATUSKONTROLL                                           
400000     .                                                                    
410000     SKIP3                                                                
420041 IMS-GET-ARTC-ARTC25 SECTION.                                             
430000                                                                          
440041     MOVE 'WLARTC11 ' TO SSA1                                             
450041     STRING 'WLARTC25(KDNOTTYP =' W-KDNOTTYP-X ')'                        
460000          DELIMITED BY SIZE INTO SSA2                                     
470000     MOVE '  GE' TO GODK-STATUSKODER                                      
480000     CALL CBLTDLI USING GNP ARTC-PCB DLI-IO-AREA SSA1 SSA2                
490000     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
500000     PERFORM IMS-STATUSKONTROLL                                           
510000     .                                                                    
520000     SKIP3                                                                
530041 IMS-GET-ARTC-ARTC11 SECTION.                                             
540000                                                                          
550041     MOVE 'WLARTC11 ' TO SSA1                                             
560000     MOVE '  GE' TO GODK-STATUSKODER                                      
570041     CALL CBLTDLI USING GNP ARTC-PCB DLI-IO-AREA SSA1                     
580000     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
590000     PERFORM IMS-STATUSKONTROLL                                           
600000     .                                                                    
610000     EJECT                                                                
620000 IMS-GET-BENA-BENA01-ASEQ SECTION.                                        
630000                                                                          
640000     STRING 'WLBENA01(WDD3ASEQ =' W-IDSKYLT-X                             
650000                                  W-BEART-X ')'                           
660000          DELIMITED BY SIZE INTO SSA1                                     
670000     MOVE '  GE' TO GODK-STATUSKODER                                      
680000     CALL CBLTDLI USING GU BENA-A-PCB DLI-IO-AREA SSA1                    
690000     MOVE BENA-A-STATUS-CODE TO STATUS-WS                                 
700000     PERFORM IMS-STATUSKONTROLL                                           
710000     .                                                                    
720000     SKIP3                                                                
730000 IMS-GET-BENA-BENA01-ASEQ-NEXT SECTION.                                   
740000                                                                          
750000     STRING 'WLBENA01(WDD3ASEQ =' W-IDSKYLT-X                             
760000                                  W-BEART-X ')'                           
770000          DELIMITED BY SIZE INTO SSA1                                     
780000     MOVE '  GE' TO GODK-STATUSKODER                                      
790000     CALL CBLTDLI USING GN BENA-A-PCB DLI-IO-AREA SSA1                    
800000     MOVE BENA-A-STATUS-CODE TO STATUS-WS                                 
810000     PERFORM IMS-STATUSKONTROLL                                           
820000     .                                                                    
830000     SKIP3                                                                
840000 IMS-GET-BENA-BENA11-ASEQ SECTION.                                        
850000                                                                          
860000     STRING 'WLBENA11(IDSKYLT  =' W-IDSKYLT-X ')'                         
870000          DELIMITED BY SIZE INTO SSA1                                     
880000     MOVE '  GE' TO GODK-STATUSKODER                                      
890000     CALL CBLTDLI USING GNP BENA-A-PCB DLI-IO-AREA SSA1                   
900000     MOVE BENA-A-STATUS-CODE TO STATUS-WS                                 
910000     PERFORM IMS-STATUSKONTROLL                                           
920000     .                                                                    
930000     EJECT                                                                
940000 IMS-GET-BENA-BENA01-BSEQ SECTION.                                        
950000                                                                          
960000     STRING 'WLBENA01(WDD3BSEQ =' W-IDARTNR-2-X ')'                       
970000          DELIMITED BY SIZE INTO SSA1                                     
980000     MOVE '  GE' TO GODK-STATUSKODER                                      
990000     CALL CBLTDLI USING GU BENA-B-PCB DLI-IO-AREA SSA1                    
000000     MOVE BENA-B-STATUS-CODE TO STATUS-WS                                 
010000     PERFORM IMS-STATUSKONTROLL                                           
020000     .                                                                    
030000     SKIP3                                                                
040000 IMS-GET-BENA-BENA11-BSEQ SECTION.                                        
050000                                                                          
060000     STRING 'WLBENA11(IDSKYLT  =' W-IDSKYLT-X ')'                         
070000          DELIMITED BY SIZE INTO SSA1                                     
080000     MOVE '  GE' TO GODK-STATUSKODER                                      
090000     CALL CBLTDLI USING GNP BENA-B-PCB DLI-IO-AREA SSA1                   
100000     MOVE BENA-B-STATUS-CODE TO STATUS-WS                                 
110000     PERFORM IMS-STATUSKONTROLL                                           
120000     .                                                                    
130000     EJECT                                                                
140000 IMS-STATUSKONTROLL SECTION.                                              
150000                                                                          
160000     SET STATUS-IX TO 1                                                   
170000     SEARCH GODK-STATUS                                                   
180000       AT END CALL FELLOG                                                 
190000       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
200000     END-SEARCH                                                           
210000     .                                                                    
220051     EJECT                                                                
230051*    -COPY WY2000P1                                                       
