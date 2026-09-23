000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     WXTR9000.                                                
000400*AUTHOR.         STEFAN KIHLBERG/PER FREDRIKSSON                          
000500*DATE-WRITTEN.   95/03/09.                                                
000600                                                                          
000700*                                                                         
000800*    FUNKTION:                                                            
000900*        PROGRAMMET LÄSER FLERA GRUNDFILER MED UPPGIFTER FRÅN             
001000*        OLIKA DATABASER.                                                 
001100*        SKAPAR FILEN WXTR90 MED ARTIKELUPPGIFTER.                        
001200*        SKRIVER ENDAST ARTIKLAR MED KDERS-UTG = 0                        
001300*        KVBR-TOT TILLAGT 950405 PF                                       
001400*        KDPSLLOC TILLAGT 970405 SG                                       
001500*                                                                         
001600*    ABENDKODER:                                                          
001700*        U0016 -  . . . .                                                 
001800*        U1000 -  . . . .                                                 
001900*                                                                         
002000*    E-TRACKER: 7450319  2008-HÖST  VOHF                                  
002100*    E-TRACKER:10254592  2015       DECOMISSION VOHF                      
002200     SKIP3                                                                
002300 ENVIRONMENT DIVISION.                                                    
002400     SKIP2                                                                
002500 INPUT-OUTPUT SECTION.                                                    
002600                                                                          
002700 FILE-CONTROL.                                                            
002800     SKIP2                                                                
002900*          --- TYPE OF EXECUTION - DAILY OR ORDERED                       
003000     SELECT PARMFIL                    ASSIGN TO WXTR90DA.                
003100     SKIP2                                                                
003200*          --- SAMTLIGA DATAELEMENT FRÅN WDK601 OCH WDK611                
003300     SELECT W01160                     ASSIGN TO WXTR90D1.                
003400     SKIP2                                                                
003500*                                                                         
003600     SELECT W01168                     ASSIGN TO WXTR90D2.                
003700     SKIP2                                                                
003800*          --- BESTÄLLNINGSREST FRÅN WDD9                                 
003900     SELECT W01173                     ASSIGN TO WXTR90D4.                
004000     SKIP2                                                                
004100*          --- SAMTLIGA BENÄMNINGAR PER ARTIKEL FRÅN WDD3                 
004200     SELECT W01174                     ASSIGN TO WXTR90D5.                
004300     SKIP2                                                                
004400*          --- FIL MED UPPGIFTER FRÅN CDC OCH SUMMERADE SDC               
004500     SELECT WXTR90                     ASSIGN TO WXTR90D6.                
004600     SKIP2                                                                
004700*          --- FIL MED UPPGIFTER FRÅN SDC/NDC                             
004800     SELECT W01184                     ASSIGN TO WXTR90D7.                
004900     SKIP2                                                                
005000*          --- ERSÄTTNINGAR                                               
005100     SELECT W01103                     ASSIGN TO WXTR90D8.                
005200     SKIP2                                                                
005300     SELECT WXTR9A                     ASSIGN TO WXTR90D9.                
005400     EJECT                                                                
005500 DATA DIVISION.                                                           
005600     SKIP3                                                                
005700 FILE SECTION.                                                            
005800     SKIP3                                                                
005900 FD  PARMFIL                                                              
006000     RECORDING       F                                                    
006100     BLOCK CONTAINS  0.                                                   
006200                                                                          
006300 01  FILLER       PIC X(80).                                              
006400                                                                          
006500                                                                          
006600                                                                          
006700 FD  W01160                                                               
006800     RECORDING       F                                                    
006900     BLOCK CONTAINS  0.                                                   
007000                                                                          
007100*01  -COPY W01160      -L.                                                
007200                                                                          
007300                                                                          
007400                                                                          
007500 FD  W01168                                                               
007600     RECORDING       F                                                    
007700     BLOCK CONTAINS  0.                                                   
007800                                                                          
007900*01  -COPY W01168      -L.                                                
008000                                                                          
008100                                                                          
008200                                                                          
008300 FD  W01173                                                               
008400     RECORDING       F                                                    
008500     BLOCK CONTAINS  0.                                                   
008600                                                                          
008700*01  -COPY W01173      -L.                                                
008800                                                                          
008900                                                                          
009000                                                                          
009100 FD  W01174                                                               
009200     RECORDING       F                                                    
009300     BLOCK CONTAINS  0.                                                   
009400                                                                          
009500*01  -COPY W01174      -L.                                                
009600                                                                          
009700                                                                          
009800                                                                          
009900 FD  WXTR90                                                               
010000     RECORDING       F                                                    
010100     BLOCK CONTAINS  0.                                                   
010200                                                                          
010300*01  POST -COPY WXTR90   -PRE WXTR90-    -L.                              
010400                                                                          
010500 FD  WXTR9A                                                               
010600     RECORDING       F                                                    
010700     BLOCK CONTAINS  0.                                                   
010800                                                                          
010900*01  POST -COPY WXTR9B  -PRE WXTR9A-    -L.                               
011000                                                                          
011100                                                                          
011200 FD  W01184                                                               
011300     RECORDING       F                                                    
011400     BLOCK CONTAINS  0.                                                   
011500                                                                          
011600*01  -COPY W01184      -L.                                                
011700     EJECT                                                                
011800                                                                          
011900 FD  W01103                                                               
012000     RECORDING       F                                                    
012100     BLOCK CONTAINS  0.                                                   
012200                                                                          
012300*01  -COPY WSUPERS      -L.                                               
012400     SKIP3                                                                
012500 WORKING-STORAGE SECTION.                                                 
012600                                                                          
012700                                                                          
012800*    -- CHECKED BY WY2000                                                 
012900 77  IDPGM                       PIC X(8)    VALUE 'WXTR9000'.            
013000 77  JA                          PIC X       VALUE 'J'.                   
013100 77  NEJ                         PIC X       VALUE 'N'.                   
013200 77  WS-CDC-11                   PIC X(2)    VALUE '11'.                  
013300                                                                          
013400 01  SWITCHAR.                                                            
013500     03  SW-IDAO-FLYTTAD         PIC X        VALUE 'N'.                  
013600       88  IDAO-FLYTTAD                       VALUE 'J'.                  
013700                                                                          
013800 77  PARMFIL-EOF-SW              PIC X       VALUE 'N'.                   
013900     88  END-OF-PARMFIL                      VALUE 'J'.                   
014000                                                                          
014100 77  W01160-EOF-SW               PIC X       VALUE 'N'.                   
014200     88  END-OF-W01160                       VALUE 'J'.                   
014300                                                                          
014400 77  W01168-EOF-SW               PIC X       VALUE 'N'.                   
014500     88  END-OF-W01168                       VALUE 'J'.                   
014600                                                                          
014700 77  W01173-EOF-SW               PIC X       VALUE 'N'.                   
014800     88  END-OF-W01173                       VALUE 'J'.                   
014900                                                                          
015000 77  W01174-EOF-SW               PIC X       VALUE 'N'.                   
015100     88  END-OF-W01174                       VALUE 'J'.                   
015200                                                                          
015300 77  W01184-EOF-SW               PIC X       VALUE 'N'.                   
015400     88  END-OF-W01184                       VALUE 'J'.                   
015500                                                                          
015600 77  W01103-EOF-SW               PIC X       VALUE 'N'.                   
015700     88  END-OF-W01103                       VALUE 'J'.                   
015800                                                                          
015900     EJECT                                                                
016000 01  ARBETSAREOR.                                                         
016100     03  WS-SPAR-IDARTNR      PIC S9(9)      VALUE ZERO COMP-3.           
016200 01  WS-KVPB-REF-US           PIC S9(6)V9(1) VALUE ZERO COMP-3.           
016300 01  WS-KVPB-REF-NDC-TOT      PIC S9(6)V9(3) VALUE ZERO COMP-3.           
016400 01  WS-KVPB-REF-SDC-TOT      PIC S9(6)V9(3) VALUE ZERO COMP-3.           
016500 01  WS-STOCKOH               PIC S9(7)      VALUE ZERO.                  
016600 01  WS-BEART-ENG             PIC X(25)      VALUE SPACE.                 
016700 01  DAGENS-DATUM             PIC 9(6)       VALUE ZERO.                  
016800 01  FILLER REDEFINES DAGENS-DATUM.                                       
016900     03  DAGENS-DATUM-AAR        PIC 9(2).                                
017000     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
017100     03  DAGENS-DATUM-DAG        PIC 9(2).                                
017200     SKIP3                                                                
017300     EJECT                                                                
017400*      --- VALID IDDC                                                     
017500*01  -COPY WWDC99    -PRE VAL-                                            
017600     EJECT                                                                
017700*                                                                         
017800 01    WS-IDDC-TABELL.                                                    
017900    03 WS-VALID-IDDC  OCCURS 100 INDEXED BY WS-IDDC-IX.                   
018000       05 WS-IDDC             PIC X(2).                                   
018100       05 WS-KDDC             PIC X(2).                                   
018200          88 WS-CDC           VALUE 'C '.                                 
018300          88 WS-CDC-TR        VALUE 'TR'.                                 
018400          88 WS-SDC           VALUE 'S '.                                 
018500          88 WS-NDC-NA        VALUE 'NA'.                                 
018600          88 WS-NDC-PF        VALUE 'NP'.                                 
018700          88 WS-NDC-CN        VALUE 'NC'.                                 
018710          88 WS-NDC-SA        VALUE 'NS'.                                 
018720          88 WS-NDC-OTHERS    VALUE 'NX'.                                 
018800          88 WS-DDC           VALUE 'D '.                                 
018900       05 WS-IDLANDX2         PIC X(2).                                   
019000     EJECT                                                                
020000*     -- FÖR REDIGERING AV IDAVINR FRÅN IDFS                              
020100 01  IDFS-TO-IDAVI.                                                       
030000  02     WS-IDAVINR              PIC 9(7)    VALUE ZERO.                  
040000  02     FILLER                  REDEFINES WS-IDAVINR.                    
050000   03    WS-IDAVINR-TKN          OCCURS 7                                 
060000                                 PIC 9(1).                                
070000  02     WS-IDFS                 PIC X(8)    VALUE SPACE.                 
080000  02     FILLER                  REDEFINES WS-IDFS.                       
090000   03    WS-IDFS-TKN             OCCURS 8                                 
100000                                 PIC 9(1).                                
110000*     -- FÄLTLÄNGD IDFS                                                   
120000  02     K-IDFS-LNG              PIC S9(9)   VALUE +8   COMP SYNC.        
130000*     -- FÄLTLÄNGD IDAVINR                                                
140000  02     K-IDAVINR-LNG           PIC S9(9)   VALUE +7   COMP SYNC.        
150000                                                                          
160000 01      IX-INDEXVARIABLER.                                               
170000*     -- TECKEN I WS-IDFS                                                 
180000  02     IX-IDFS                 PIC S9(9)   VALUE ZERO COMP SYNC.        
190000*     -- TECKEN I WS-IDAVINR                                              
200000  02     IX-IDAVINR              PIC S9(9)   VALUE ZERO COMP SYNC.        
200100     EJECT                                                                
200200 01  DYNAMISKA-SUBPROGRAM.                                                
200300*                                                                         
200400     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
200500     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
200600     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
200700     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
200800     SKIP2                                                                
200900*    --- PARAMETRAR TILL ABEND                                            
201000                                                                          
201100 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
201200 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
201300     SKIP2                                                                
201400 01  FELTEXT.                                                             
201500     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
201600     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
201700     EJECT                                                                
201800*    --- PARAMETRAR TILL POSTSUM                                          
201900*                                                                         
202000*01  -COPY W0005   -PRE  POSTSUM-                                         
202100     EJECT                                                                
202200 01  PARMFIL-AREA-START          PIC X(24)   VALUE                        
202300                                 'PARMFIL-AREA-START '.                   
202400     SKIP2                                                                
202500                                                                          
202600 01  PARM-AREA.                                                           
202700     05 PARM-BATCHTYP            PIC X(03)   VALUE SPACES.                
202800        88 DAILYBATCH                        VALUE 'DAY' .                
202900     EJECT                                                                
203000 01  W01160-AREA-START           PIC X(24)   VALUE                        
203100                                 'W01160-AREA-START  '.                   
203200     SKIP2                                                                
203300                                                                          
203400*01  AREA -COPY W01160     -PRE W01160-                                   
203500     EJECT                                                                
203600 01  W01168-AREA-START           PIC X(24)   VALUE                        
203700                                 'W01168-AREA-START  '.                   
203800     SKIP2                                                                
203900                                                                          
204000*01  AREA -COPY W01168     -PRE W01168-                                   
204100     EJECT                                                                
204200 01  W01173-AREA-START           PIC X(24)   VALUE                        
204300                                 'W01173-AREA-START  '.                   
204400     SKIP2                                                                
204500                                                                          
204600*01  AREA -COPY W01173     -PRE W01173-                                   
204700     EJECT                                                                
204800 01  W01174-AREA-START           PIC X(24)   VALUE                        
204900                                 'W01174-AREA-START  '.                   
205000     SKIP2                                                                
205100                                                                          
205200*01  AREA -COPY W01174     -PRE W01174-                                   
205300     EJECT                                                                
205400 01  W01184-AREA-START           PIC X(24)   VALUE                        
205500                                 'W01184-AREA-START  '.                   
205600     SKIP2                                                                
205700                                                                          
205800*01  AREA -COPY W01184     -PRE W01184-                                   
205900     EJECT                                                                
206000                                                                          
206100 01  W01103-AREA-START           PIC X(24)   VALUE                        
206200                                 'W01184-AREA-START  '.                   
206300     SKIP2                                                                
206400                                                                          
206500*01  AREA -COPY WSUPERS    -PRE W01103-                                   
206600     EJECT                                                                
206700                                                                          
206800*01  AREA -COPY WXTR90     -PRE U90-                                      
206900     EJECT                                                                
207000*01  AREA -COPY WXTR9B     -PRE UT2-                                      
207100     EJECT                                                                
207200*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
207300*                                                                         
207400     EJECT                                                                
207500 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
207600     SKIP3                                                                
207700 01  NYCKLAR-TILL-DLI.                                                    
207800     03  W-IDDC-B6-X.                                                     
207900         05  W-IDDC-B6           PIC X(2)   VALUE SPACE.                  
208000                                                                          
208100     03  W-IDDC-B616-X.                                                   
208200         05  W-IDDC-B616         PIC X(2)   VALUE SPACE.                  
208300     SKIP2                                                                
208400*    --- STATUS-KOD FRÅN IMS                                              
208500 01  STATUS-WS                   PIC XX.                                  
208600     88  SEGMENT-FINNS                       VALUE '  '.                  
208700     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
208800     88  BASEN-SLUT                          VALUE 'GB'.                  
208900     SKIP2                                                                
209000 01  GODK-STATUSKODER.                                                    
209100     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
209200     SKIP3                                                                
209300 01  SSA1                        PIC X(64).                               
209400 01  SSA2                        PIC X(64).                               
209500     EJECT                                                                
209600*    --- IMS FUNKTIONSKODER                                               
209700*01  -COPY W0003                                                          
209800     EJECT                                                                
209900*    ---  DLI INPUT-OUTPUT AREA                                           
210000 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
210100 01   DLI-IO-AREA-B601.                                                   
210200*     03  -COPY WDB601                                                    
210300*                                                                         
210400 01  FILLER               PIC X(16)   VALUE 'WDB616 AREA'.                
210500 01  DLI-IO-WDB616.                                                       
210600*    03  -COPY WDB616                                                     
210700     EJECT                                                                
210800 LINKAGE SECTION.                                                         
210900                                                                          
211000*01  -COPY W0008      -PRE WDB6-                                          
211100     05  FILLER                  PIC X.                                   
211200     EJECT                                                                
211300 PROCEDURE DIVISION  USING WDB6-PCB.                                      
211400 MAIN SECTION.                                                            
211500     ENTRY 'DLITCBL' USING WDB6-PCB.                                      
211600                                                                          
211700     PERFORM A-INIT                                                       
211800     PERFORM B-LAES-INFILER                                               
211900     PERFORM UNTIL END-OF-W01160                                          
212000        MOVE W01160-CLAG-IDARTNR TO WS-SPAR-IDARTNR                       
212100        PERFORM C-NOLLSTALL                                               
212200        PERFORM D-BEHANDLA-W01160                                         
212300        PERFORM E-BEHANDLA-W01168                                         
212400        PERFORM G-BEHANDLA-W01173                                         
212500        PERFORM H-BEHANDLA-W01174                                         
212600        PERFORM I-BEHANDLA-W01184                                         
212700        IF U90-KDERS-UTG = 0                                              
212800          PERFORM J-BEHANDLA-KVPB-REF                                     
212900          PERFORM S21-SKRIV-WXTR90                                        
213000        END-IF                                                            
213100        PERFORM S01-LAES-W01160                                           
213200     END-PERFORM                                                          
213300     PERFORM Z-FINIT                                                      
213400                                                                          
213500     MOVE ZERO TO RETURN-CODE                                             
213600     GOBACK                                                               
213700     .                                                                    
213800     EJECT                                                                
213900                                                                          
214000                                                                          
214100 A-INIT SECTION.                                                          
214200                                                                          
214300     OPEN INPUT  PARMFIL                                                  
214400                 W01160                                                   
214500                 W01168                                                   
214600                 W01173                                                   
214700                 W01174                                                   
214800                 W01184                                                   
214900                 W01103                                                   
215000     OPEN OUTPUT WXTR90                                                   
215100                 WXTR9A                                                   
215200     MOVE ZERO TO WS-KVPB-REF-SDC-TOT                                     
215300                  WS-KVPB-REF-NDC-TOT                                     
215400                  WS-KVPB-REF-US                                          
215500                                                                          
215600     ACCEPT DAGENS-DATUM  FROM DATE                                       
215700     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
215800                                                                          
215900     PERFORM AA-LADDA-DC-TABELL                                           
216000     .                                                                    
216100     EJECT                                                                
216200 AA-LADDA-DC-TABELL SECTION.                                              
216300                                                                          
216400     INITIALIZE WS-IDDC-TABELL                                            
216500     SET WS-IDDC-IX TO +1                                                 
216600     PERFORM IMS-GN-WDB601                                                
216700     PERFORM UNTIL BASEN-SLUT                                             
216800        MOVE DCS-IDDC     TO WS-IDDC(WS-IDDC-IX)                          
216900        MOVE DCS-KDDC     TO WS-KDDC(WS-IDDC-IX)                          
217000        MOVE DCS-IDLANDX2 TO WS-IDLANDX2(WS-IDDC-IX)                      
217100        PERFORM IMS-GN-WDB601                                             
217200        SET WS-IDDC-IX UP BY +1                                           
217300        IF WS-IDDC-IX > 100                                               
217400           MOVE 'DC-TABELLEN FULL' TO FELTEXT                             
217500           CALL FELLOG                                                    
217600        END-IF                                                            
217700     END-PERFORM                                                          
217800     .                                                                    
217900     EJECT                                                                
218000 B-LAES-INFILER SECTION.                                                  
218100                                                                          
218200     PERFORM S01-LAES-W01160                                              
218300     PERFORM S02-LAES-W01168                                              
218400     PERFORM S04-LAES-W01173                                              
218500     PERFORM S05-LAES-W01174                                              
218600     PERFORM S07-LAES-W01184                                              
218700     PERFORM S08-LAES-W01103                                              
218800     PERFORM S09-LAES-PARMFIL                                             
218900     .                                                                    
219000     EJECT                                                                
219100                                                                          
219200                                                                          
219300 C-NOLLSTALL SECTION.                                                     
219400                                                                          
219500     INITIALIZE        U90-AREA                                           
219600                       UT2-AREA                                           
219700     .                                                                    
219800     EJECT                                                                
219900                                                                          
220000                                                                          
220100 D-BEHANDLA-W01160 SECTION.                                               
220200                                                                          
220300                                                                          
220400     MOVE W01160-CLAG-IDARTNR      TO U90-IDARTNR                         
220500     MOVE W01160-CLAG-REKSIFFR     TO U90-REKSIFFR                        
220600     MOVE WS-CDC-11                TO U90-IDDC                            
220700                                                                          
220800     PERFORM DA-FLYTTA-ARTIKEL-INFO                                       
220900     PERFORM DB-FLYTTA-CDC-INFO                                           
221000     .                                                                    
221100     EJECT                                                                
221200                                                                          
221300 DA-FLYTTA-ARTIKEL-INFO.                                                  
221400     MOVE W01160-CLAG-BEFT          TO U90-BEFT                           
221500     MOVE W01160-CLAG-FLAVRART      TO U90-FLAVRART                       
221600     MOVE W01160-CLAG-FLERS         TO U90-FLERS                          
221700     MOVE W01160-CLAG-FLGEMART      TO U90-FLGEMART                       
221800     MOVE W01160-CLAG-FLIART        TO U90-FLIART                         
221900     MOVE W01160-CLAG-FLJIT         TO U90-FLJIT                          
222000     MOVE W01160-CLAG-FLLSRDEL      TO U90-FLLSRDEL                       
222100     MOVE W01160-CLAG-FLTPO1        TO U90-FLTPO1                         
222200     MOVE W01160-CLAG-IDANSK        TO U90-IDANSK                         
222300     MOVE W01160-CLAG-IDARTNR-EMBQ0 TO U90-IDARTNR-EMBQ0                  
222400     MOVE W01160-CLAG-IDARTNR-EMBQ1 TO U90-IDARTNR-EMBQ1                  
222500     MOVE W01160-CLAG-IDARTNR-EMBQ2 TO U90-IDARTNR-EMBQ2                  
222600     MOVE W01160-CLAG-IDARTNR-EMBQ3 TO U90-IDARTNR-EMBQ3                  
222700     MOVE W01160-CLAG-IDARTNR-EMBQ4 TO U90-IDARTNR-EMBQ4                  
222800     MOVE W01160-CLAG-IDBERED       TO U90-IDBERED                        
222900     MOVE W01160-CLAG-IDFKNGRP      TO U90-IDFKNGRP                       
223000     MOVE W01160-CLAG-IDINK         TO U90-IDINK                          
223100     MOVE W01160-CLAG-IDLEVNR       TO U90-IDLEVNR                        
223200     MOVE W01160-CLAG-IDLKTO        TO U90-IDLKTO                         
223300     MOVE W01160-CLAG-IDPROJ        TO U90-IDPROJ                         
223400     MOVE W01160-CLAG-IDPSN         TO U90-IDPSN                          
223500     MOVE W01160-CLAG-IDKAT(1)      TO U90-IDKAT-1                        
223600     MOVE W01160-CLAG-IDKAT(2)      TO U90-IDKAT-2                        
223700     MOVE W01160-CLAG-IDKAT(3)      TO U90-IDKAT-3                        
223800     MOVE W01160-CLAG-IDSTATNR(1)   TO U90-IDSTATNR-1                     
223900     MOVE W01160-CLAG-IDSTATNR(2)   TO U90-IDSTATNR-2                     
224000     MOVE W01160-CLAG-IDSTATNR(3)   TO U90-IDSTATNR-3                     
224100     MOVE W01160-CLAG-IDSTATNR(4)   TO U90-IDSTATNR-4                     
224200     MOVE W01160-CLAG-IDSTATNR(5)   TO U90-IDSTATNR-5                     
224300     MOVE W01160-CLAG-IDSTATNR(6)   TO U90-IDSTATNR-6                     
224400     MOVE W01160-CLAG-KDAGE         TO U90-KDAGE                          
224500     MOVE W01160-CLAG-KDARTHNT      TO U90-KDARTHNT                       
224600     MOVE W01160-CLAG-KDARTURS      TO U90-KDARTURS                       
224700     MOVE W01160-CLAG-KDBPSR        TO U90-KDBPSR                         
224800     MOVE W01160-CLAG-KDEMBKOD-0    TO U90-KDEMBKOD-0                     
224900     MOVE W01160-CLAG-KDEMBKOD-1    TO U90-KDEMBKOD-1                     
225000     MOVE W01160-CLAG-KDEMBKOD-2    TO U90-KDEMBKOD-2                     
225100     MOVE W01160-CLAG-KDERS         TO U90-KDERS                          
225200     MOVE W01160-CLAG-KDERS-UTG     TO U90-KDERS-UTG                      
225300     MOVE W01160-CLAG-KDFARLIG      TO U90-KDFARLIG                       
225400     MOVE W01160-CLAG-KDGK          TO U90-KDGK                           
225500     MOVE W01160-CLAG-KDPRODSL      TO U90-KDPRODSL                       
225600     MOVE W01160-CLAG-KDSORT        TO U90-KDSORT                         
225700     MOVE W01160-CLAG-KDSPEEMB      TO U90-KDSPEEMB                       
225800     MOVE W01160-CLAG-KDSRA         TO U90-KDSRA                          
225900     MOVE W01160-CLAG-KDUART        TO U90-KDUART                         
226000     MOVE W01160-CLAG-KDVVKL        TO U90-KDVVKL                         
226100     MOVE W01160-CLAG-KDYTBEH       TO U90-KDYTBEH                        
226200     MOVE W01160-CLAG-KVPALL        TO U90-KVPALL                         
226300     MOVE W01160-CLAG-KVQPACK-0     TO U90-KVQPACK-0                      
226400     MOVE W01160-CLAG-KVQPACK-1     TO U90-KVQPACK-1                      
226500     MOVE W01160-CLAG-KVQPACK-2     TO U90-KVQPACK-2                      
226600     MOVE W01160-CLAG-KVQPACK-3     TO U90-KVQPACK-3                      
226700     MOVE W01160-CLAG-KVQPACK-4     TO U90-KVQPACK-4                      
226800     MOVE W01160-CLAG-PRARTSTD      TO U90-PRARTBES                       
226900     MOVE W01160-CLAG-PRARTSJK      TO U90-PRARTSJK                       
227000     MOVE W01160-CLAG-PRARTSTD      TO U90-PRARTSTD                       
227100     MOVE W01160-CLAG-PRINK         TO U90-PRINK                          
227200     MOVE W01160-CLAG-TIERSDAT      TO U90-TIERSDAT                       
227300     MOVE W01160-CLAG-TIFINLV       TO U90-TIFINLV                        
227400     MOVE W01160-CLAG-VKART         TO U90-VKART                          
227500     MOVE W01160-CLAG-VLARTNTO      TO U90-VLARTNTO                       
227600     MOVE W01160-CLAG-TISKROT        TO U90-TISKROT                       
227700     MOVE W01160-CLAG-FLCDART        TO U90-FLCDART                       
227800     MOVE W01160-CLAG-ADLAGOMR-CD(1) TO U90-ADLAGOMR-CD-1                 
227900     MOVE W01160-CLAG-ADGANG-CD(1)   TO U90-ADGANG-CD-1                   
228000     MOVE W01160-CLAG-ADPLATS-CD(1)  TO U90-ADPLATS-CD-1                  
228100     MOVE W01160-CLAG-ADLAGOMR-CD(2) TO U90-ADLAGOMR-CD-2                 
228200     MOVE W01160-CLAG-ADGANG-CD(2)   TO U90-ADGANG-CD-2                   
228300     MOVE W01160-CLAG-ADPLATS-CD(2)  TO U90-ADPLATS-CD-2                  
228400     MOVE W01160-CLAG-ADLAGOMR-CD(3) TO U90-ADLAGOMR-CD-3                 
228500     MOVE W01160-CLAG-ADGANG-CD(3)   TO U90-ADGANG-CD-3                   
228600     MOVE W01160-CLAG-ADPLATS-CD(3)  TO U90-ADPLATS-CD-3                  
228700     MOVE W01160-CLAG-ADLAGOMR-CD(4) TO U90-ADLAGOMR-CD-4                 
228800     MOVE W01160-CLAG-ADGANG-CD(4)   TO U90-ADGANG-CD-4                   
228900     MOVE W01160-CLAG-ADPLATS-CD(4)  TO U90-ADPLATS-CD-4                  
229000     MOVE W01160-CLAG-KVLS-CD(1)     TO U90-KVLS-CD-1                     
229100     MOVE W01160-CLAG-KVLS-CD(2)     TO U90-KVLS-CD-2                     
229200     MOVE W01160-CLAG-KVLS-CD(3)     TO U90-KVLS-CD-3                     
229300     MOVE W01160-CLAG-KVLS-CD(4)     TO U90-KVLS-CD-4                     
229400     MOVE W01160-CLAG-KVRESS-CD(1)   TO U90-KVRESS-CD-1                   
229500     MOVE W01160-CLAG-KVRESS-CD(2)   TO U90-KVRESS-CD-2                   
229600     MOVE W01160-CLAG-KVRESS-CD(3)   TO U90-KVRESS-CD-3                   
229700     MOVE W01160-CLAG-KVRESS-CD(4)   TO U90-KVRESS-CD-4                   
229800     .                                                                    
229900     EJECT                                                                
230000                                                                          
230100                                                                          
230200 DB-FLYTTA-CDC-INFO.                                                      
230300                                                                          
230400     MOVE W01160-CLAG-ADLAGOMR      TO U90-ADLAGOMR                       
230500     MOVE W01160-CLAG-ADGANG        TO U90-ADGANG                         
230600     MOVE W01160-CLAG-ADPLATS       TO U90-ADPLATS                        
230700     MOVE W01160-CLAG-FLMANAT       TO U90-FLMANAT                        
230800     MOVE W01160-CLAG-FLMANBK       TO U90-FLMANBK                        
230900     MOVE W01160-CLAG-FLMANGK       TO U90-FLMANGK                        
231000     MOVE W01160-CLAG-FLMANKP       TO U90-FLMANKP                        
231100     MOVE W01160-CLAG-FLMANLT       TO U90-FLMANLT                        
231200     MOVE W01160-CLAG-FLMANOSK      TO U90-FLMANOSK                       
231300     MOVE W01160-CLAG-FLMANQ        TO U90-FLMANQ                         
231400     MOVE W01160-CLAG-FLMPB         TO U90-FLMPB                          
231500     MOVE W01160-CLAG-FLREFILL      TO U90-FLREFILL                       
231600     MOVE W01160-CLAG-FLSKROT-BEORD TO U90-FLSKROT-BEORD                  
231700     MOVE W01160-CLAG-FLTOPP        TO U90-FLTOPP                         
231800*                                                                         
231900*    -- REDIGERA IDAVINR FRÅN IDFS                                        
232000     MOVE W01160-CLAG-IDFS-SEN      TO WS-IDFS                            
232100     MOVE ZERO                      TO WS-IDAVINR                         
233000     MOVE K-IDFS-LNG                TO IX-IDFS                            
234000     MOVE K-IDAVINR-LNG             TO IX-IDAVINR                         
235000     PERFORM UNTIL (IX-IDFS      = ZERO                                   
236000                OR  IX-IDAVINR   = ZERO)                                  
237000       IF  WS-IDFS-TKN (IX-IDFS) NUMERIC                                  
238000         MOVE WS-IDFS-TKN (IX-IDFS)                                       
239000                               TO WS-IDAVINR-TKN (IX-IDAVINR)             
240000         SUBTRACT 1            FROM IX-IDAVINR                            
250000       END-IF                                                             
260000       SUBTRACT 1              FROM IX-IDFS                               
270000     END-PERFORM                                                          
280010     MOVE WS-IDAVINR                TO U90-IDAVINR-SEN                    
280100*                                                                         
280300     MOVE W01160-CLAG-IDLEVNR-SEN   TO U90-IDLEVNR-SEN                    
280400     MOVE W01160-CLAG-IDPLANGR-AG   TO U90-IDPLANGR-AG                    
280500     MOVE W01160-CLAG-IDPLANGR-LEV  TO U90-IDPLANGR-LEV                   
280600     MOVE W01160-CLAG-IDPROENH(1)   TO U90-IDPROENH-1                     
280700     MOVE W01160-CLAG-IDPROENH(2)   TO U90-IDPROENH-2                     
280800     MOVE W01160-CLAG-IDPROENH(3)   TO U90-IDPROENH-3                     
280900     MOVE W01160-CLAG-IDPROJUP      TO U90-IDPROJUP                       
281000     MOVE W01160-CLAG-IDRITN        TO U90-IDRITN                         
281100     MOVE W01160-CLAG-KDAVT         TO U90-KDAVT                          
281200     MOVE W01160-CLAG-KDFORPGP      TO U90-KDFORPGP                       
281300     MOVE W01160-CLAG-KDFORPPL      TO U90-KDFORPPL                       
281400     MOVE W01160-CLAG-KDFORPUF      TO U90-KDFORPUF                       
281500     MOVE W01160-CLAG-KDFREKKL      TO U90-KDFREKKL                       
281600     MOVE W01160-CLAG-KDHF          TO U90-KDHF                           
281700     MOVE W01160-CLAG-KDKG          TO U90-KDKG                           
281800     MOVE W01160-CLAG-KDKSP         TO U90-KDKSP                          
281900     MOVE W01160-CLAG-KDLEVSP       TO U90-KDLEVSP                        
282000     MOVE W01160-CLAG-KDLPSP        TO U90-KDLPSP                         
282100     MOVE W01160-CLAG-KDOTFREK      TO U90-KDOTFREK                       
282200     MOVE W01160-CLAG-KDPRISKL      TO U90-KDPRISKL                       
282300     MOVE W01160-CLAG-KDPSLLOC      TO U90-KDPSLLOC                       
282400     MOVE W01160-CLAG-KDTIPPR       TO U90-KDTIPPR                        
282500     MOVE W01160-CLAG-KDVSOP        TO U90-KDVSOP                         
282600     MOVE W01160-CLAG-KDVTH         TO U90-KDVTH                          
282700     MOVE W01160-CLAG-KVAKS-CDC     TO U90-KVAKS-CDC                      
282800     MOVE W01160-CLAG-KVAKS-PAV     TO U90-KVAKS-PAV                      
282900     MOVE W01160-CLAG-KVAKS-T       TO U90-KVAKS-T                        
283000     MOVE W01160-CLAG-KVAP          TO U90-KVAP                           
283100     MOVE W01160-CLAG-KVAVIS-SEN    TO U90-KVAVIS-SEN                     
283200     MOVE W01160-CLAG-KVBK          TO U90-KVBK                           
283300     MOVE W01160-CLAG-KVDAGAR-INLEV TO U90-KVDAGAR-INLEV                  
283400     MOVE W01160-CLAG-KVDAGAR-TT    TO U90-KVDAGAR-TT                     
283500     MOVE W01160-CLAG-KVEFRS        TO U90-KVEFRS                         
283600     MOVE W01160-CLAG-KVFRYSTI      TO U90-KVFRYSTI                       
283700     MOVE W01160-CLAG-KVINVS        TO U90-KVINVS                         
283800     MOVE W01160-CLAG-KVKP          TO U90-KVKP                           
283900     MOVE W01160-CLAG-KVLAAN        TO U90-KVLAAN                         
284000     MOVE W01160-CLAG-KVLS          TO U90-KVLS                           
284100     MOVE W01160-CLAG-KVMAD-SEP     TO U90-KVMAD-SEP                      
284200     MOVE W01160-CLAG-KVMAD-TOT     TO U90-KVMAD-TOT                      
284300     MOVE W01160-CLAG-KVMP          TO U90-KVMP                           
284400     MOVE W01160-CLAG-KVOVERF       TO U90-KVOVERF                        
284500     MOVE W01160-CLAG-KVPB-SATS     TO U90-KVPB-SATS                      
284600     MOVE W01160-CLAG-KVPB-SEP      TO U90-KVPB-SEP                       
284700     MOVE W01160-CLAG-KVPB-TPO      TO U90-KVPB-TPO                       
284800     MOVE W01160-CLAG-KVQ           TO U90-KVQ                            
284900     MOVE W01160-CLAG-KVRESS        TO U90-KVRESS                         
285000     MOVE W01160-CLAG-KVROS         TO U90-KVROS                          
285100     MOVE W01160-CLAG-KVSLAGER      TO U90-KVSLAGER                       
285200     MOVE W01160-CLAG-KVSLUTKP      TO U90-KVSLUTKP                       
285300     MOVE W01160-CLAG-KVSPANT       TO U90-KVSPANT                        
285400     MOVE W01160-CLAG-KVUTRS        TO U90-KVUTRS                         
285500     MOVE W01160-CLAG-KVVECKOR-AT   TO U90-KVVECKOR-AT                    
285600     MOVE W01160-CLAG-KVVECKOR-BT   TO U90-KVVECKOR-BT                    
285700     MOVE W01160-CLAG-KVVECKOR-FT   TO U90-KVVECKOR-FT                    
285800     MOVE W01160-CLAG-KVVECKOR-LT   TO U90-KVVECKOR-LT                    
285900     MOVE W01160-CLAG-PRDIRLON      TO U90-PRDIRLON                       
286000     MOVE W01160-CLAG-PRDMTRL       TO U90-PRDMTRL                        
286100     MOVE W01160-CLAG-PRLFKST       TO U90-PRLFKST                        
286200     MOVE W01160-CLAG-PRORDSK       TO U90-PRORDSK                        
286300     MOVE W01160-CLAG-PROVRPAL      TO U90-PROVRPAL                       
286400     MOVE W01160-CLAG-REDIRLEV      TO U90-REDIRLEV                       
286500     MOVE W01160-CLAG-RESLJUST      TO U90-RESLJUST                       
286600     MOVE W01160-CLAG-TIAVIDAT-SEN  TO U90-TIAVIDAT-SEN                   
286700     MOVE W01160-CLAG-TIINVDAT      TO U90-TIINVDAT                       
286800     MOVE W01160-CLAG-TILPSP        TO U90-TILPSP                         
286900     MOVE W01160-CLAG-TIREGDAT      TO U90-TIREGDAT                       
287000     MOVE W01160-CLAG-TISLJUST      TO U90-TISLJUST                       
287100     MOVE W01160-CLAG-TIURPROD      TO U90-TIURPROD                       
287200     MOVE ZERO                      TO U90-IDPERSON-BUY                   
287300     MOVE W01160-CLAG-PRARTBTO-EXP  TO U90-PRARTBTO-EXP                   
287400     .                                                                    
287500     EJECT                                                                
287600                                                                          
287700 E-BEHANDLA-W01168 SECTION.                                               
287800                                                                          
287900     IF W01168-ART-IDARTNR = WS-SPAR-IDARTNR                              
288000        PERFORM EA-FLYTTA-W01168                                          
288100        PERFORM S02-LAES-W01168                                           
288200     ELSE                                                                 
288300        IF W01168-ART-IDARTNR > WS-SPAR-IDARTNR                           
288400           CONTINUE                                                       
288500        ELSE                                                              
288600           MOVE 'ARTIKEL PÅ FIL W01168 FRÅN WDK9   FINNS EJ               
288700-          'PÅ W01160' TO FELTEXT-STR                                     
288800           DISPLAY W01168-ART-IDARTNR                                     
288900           DISPLAY FELTEXT                                                
289000           PERFORM S99-ABEND                                              
289100        END-IF                                                            
289200     END-IF                                                               
289300     .                                                                    
289400     EJECT                                                                
289500                                                                          
289600                                                                          
289700 EA-FLYTTA-W01168 SECTION.                                                
289800                                                                          
289900* -- *  LAGERINFO                                                         
290000                                                                          
290100     COMPUTE U90-KVOKS       = W01168-ART-KVOKS-BULK +                    
290200                               W01168-ART-KVOKS-DAG +                     
290300                               W01168-ART-KVOKS-VOR                       
290400                                                                          
290500     MOVE W01168-ART-KVPREAVB-BULK TO U90-KVPREAVB-BULK                   
290600     MOVE W01168-ART-KVPREAVB-DAG  TO U90-KVPREAVB-DAG                    
290700     MOVE W01168-ART-KVPREAVB-VOR  TO U90-KVPREAVB-VOR                    
290800     MOVE W01168-ART-KVPRERO-BULK  TO U90-KVPRERO-BULK                    
290900     MOVE W01168-ART-KVPRERO-DAG   TO U90-KVPRERO-DAG                     
291000     MOVE W01168-ART-SUTPO-TOT     TO U90-SUTPO-TOT                       
291100                                                                          
291200     .                                                                    
291300     EJECT                                                                
291400                                                                          
291500 G-BEHANDLA-W01173 SECTION.                                               
291600                                                                          
291700     IF W01173-IDARTNR = WS-SPAR-IDARTNR                                  
291800        PERFORM GA-FLYTTA-W01173                                          
291900        PERFORM S04-LAES-W01173                                           
292000     ELSE                                                                 
292100        IF W01173-IDARTNR > WS-SPAR-IDARTNR                               
292200           CONTINUE                                                       
292300        ELSE                                                              
292400           MOVE 'ARTIKEL PÅ FIL W01173 FRÅN WDD9   FINNS EJ               
292500-          'PÅ W01160' TO FELTEXT-STR                                     
292600           DISPLAY W01173-IDARTNR                                         
292700           DISPLAY FELTEXT                                                
292800           PERFORM S99-ABEND                                              
292900        END-IF                                                            
293000     END-IF                                                               
293100     .                                                                    
293200     EJECT                                                                
293300                                                                          
293400                                                                          
293500 GA-FLYTTA-W01173 SECTION.                                                
293600                                                                          
293700* -- *  LAGERINFO                                                         
293800                                                                          
293900     MOVE W01173-KVBR-TOT      TO U90-KVBR-TOT                            
294000     .                                                                    
294100     EJECT                                                                
294200                                                                          
294300                                                                          
294400 H-BEHANDLA-W01174 SECTION.                                               
294500                                                                          
294600     MOVE  SPACE               TO WS-BEART-ENG                            
294700                                                                          
294800     IF W01174-IDARTNR = WS-SPAR-IDARTNR                                  
294900        PERFORM HA-FLYTTA-W01174                                          
295000        PERFORM S05-LAES-W01174                                           
295100*    ELSE                                                                 
295200*       IF W01174-IDARTNR > WS-SPAR-IDARTNR                               
295300*          CONTINUE                                                       
295400*       ELSE                                                              
295500*          MOVE 'ARTIKEL PÅ FIL W01174 FRÅN WDD3   FINNS EJ               
295600*-         'PÅ W01160' TO FELTEXT-STR                                     
295700*          DISPLAY W01174-IDARTNR                                         
295800*          DISPLAY FELTEXT                                                
295900*          PERFORM S99-ABEND                                              
296000*       END-IF                                                            
296100     END-IF                                                               
296200     .                                                                    
296300     EJECT                                                                
296400                                                                          
296500                                                                          
296600 HA-FLYTTA-W01174 SECTION.                                                
296700                                                                          
296800* -- *  ARTIKELINFO                                                       
296900     MOVE W01174-BEART(8)      TO U90-BEART-SVE                           
297000     MOVE W01174-BEART(4)      TO U90-BEART-ENG                           
297100     MOVE W01174-BEART(4)      TO WS-BEART-ENG                            
297200     .                                                                    
297300     EJECT                                                                
297400 I-BEHANDLA-W01184 SECTION.                                               
297500                                                                          
297600     PERFORM UNTIL W01184-SLAG-IDARTNR NOT < WS-SPAR-IDARTNR              
297700       PERFORM S07-LAES-W01184                                            
297800     END-PERFORM                                                          
297900                                                                          
298000     IF W01184-SLAG-IDARTNR = WS-SPAR-IDARTNR                             
298100       PERFORM UNTIL W01184-SLAG-IDARTNR NOT = WS-SPAR-IDARTNR            
298200          OR END-OF-W01184                                                
298300          PERFORM IA-FLYTTA-W01184                                        
298400          IF DAILYBATCH                                                   
298500             PERFORM IB-WRITE-REPORT-FILE                                 
298600          END-IF                                                          
298700          PERFORM S07-LAES-W01184                                         
298800       END-PERFORM                                                        
298900     END-IF                                                               
299000     .                                                                    
299100     EJECT                                                                
299200 IA-FLYTTA-W01184 SECTION.                                                
299300* -- *  SDCINFO                                                           
299400     IF NOT WS-IDDC (WS-IDDC-IX) = W01184-SLAG-IDDC                       
299500        SET WS-IDDC-IX TO 1                                               
299600        SEARCH WS-VALID-IDDC                                              
299700          AT END                                                          
299800         STRING 'DC SAKNAS PÅ WDB6  ' W01184-SLAG-IDDC                    
299900           DELIMITED BY SIZE INTO FELTEXT                                 
300000         CALL FELLOG                                                      
300100          WHEN WS-IDDC (WS-IDDC-IX) = W01184-SLAG-IDDC CONTINUE           
300200        END-SEARCH                                                        
300300     END-IF                                                               
300400                                                                          
300500     IF WS-SDC (WS-IDDC-IX)                                               
300600        ADD W01184-SLAG-KVPB-REF TO WS-KVPB-REF-SDC-TOT                   
300700     ELSE                                                                 
300800        IF WS-NDC-NA     (WS-IDDC-IX)                                     
300900        OR WS-NDC-PF     (WS-IDDC-IX)                                     
300910        OR WS-NDC-SA     (WS-IDDC-IX)                                     
300920        OR WS-NDC-OTHERS (WS-IDDC-IX)                                     
301000        OR WS-NDC-CN     (WS-IDDC-IX)                                     
301100           ADD W01184-SLAG-KVPB-REF TO WS-KVPB-REF-NDC-TOT                
301200        END-IF                                                            
301300        IF WS-NDC-NA (WS-IDDC-IX)                                         
301400           IF WS-IDLANDX2 (WS-IDDC-IX) = 'US'                             
301500              ADD W01184-SLAG-KVPB-REF TO WS-KVPB-REF-US                  
301600           END-IF                                                         
301700        END-IF                                                            
301800     END-IF                                                               
301900     .                                                                    
302000     EJECT                                                                
302100 IB-WRITE-REPORT-FILE SECTION.                                            
302200                                                                          
302300     MOVE W01184-SLAG-IDDC          TO VAL-WS-IDDC                        
302400     IF VAL-NDC-CN OR VAL-LDC-CN                                          
302500      IF W01160-CLAG-KDSORT NOT = 'SW'                                    
302600        MOVE W01184-SLAG-IDARTNR    TO UT2-IDARTNR                        
302700        MOVE W01160-CLAG-KDERS      TO UT2-KDERS                          
302800        MOVE W01160-CLAG-IDFKNGRP   TO UT2-IDFKNGRP                       
302900        MOVE W01160-CLAG-TIFINLV    TO UT2-TIFINLV                        
303000        MOVE W01184-SLAG-IDDC       TO UT2-IDDC                           
303100        MOVE WS-BEART-ENG           TO UT2-BEART-ENG                      
303200        MOVE W01184-SLAG-KVAKS-SDC  TO UT2-KVAKS-SDC                      
303300        MOVE W01184-SLAG-KVAKS-PAV  TO UT2-KVAKS-PAV                      
303400        MOVE W01184-SLAG-KVROS-BULK TO UT2-KVROS-BULK                     
303500        MOVE W01184-SLAG-KVROS-DAG  TO UT2-KVROS-DAG                      
303600        MOVE W01184-SLAG-PRAVCOST   TO UT2-PRAVCOST                       
303700                                                                          
303800        IF W01184-SLAG-IDLEVNR = '1441'                                   
303900           MOVE W01160-CLAG-IDLEVNR TO UT2-IDLEVNR                        
304000        ELSE                                                              
304100           MOVE W01184-SLAG-IDLEVNR TO UT2-IDLEVNR                        
304200        END-IF                                                            
304300                                                                          
304400        COMPUTE WS-STOCKOH ROUNDED = W01184-SLAG-KVLS -                   
304500             (W01184-SLAG-KVROS-BULK + W01184-SLAG-KVROS-DAG)             
304600        MOVE WS-STOCKOH             TO UT2-STOCKOH                        
304700        MOVE W01184-SLAG-IDDC       TO W-IDDC-B6                          
304800        MOVE W01184-SLAG-IDDC-REF   TO W-IDDC-B616                        
304900        IF W01184-SLAG-IDDC-REF  NOT > SPACE                              
305000           COMPUTE UT2-KVDLTID-TOT  = W01184-SLAG-KVVECKOR-LT * 7         
305100        ELSE                                                              
305200           PERFORM IMS-GU-WDB616                                          
305300           IF SEGMENT-FINNS                                               
305400             MOVE REF-KVDLTID-TOT        TO UT2-KVDLTID-TOT               
305500           END-IF                                                         
305600        END-IF                                                            
305700        PERFORM IBA-FLYTTA-W01103                                         
305800        PERFORM S22-SKRIV-WXTR9A                                          
305900      END-IF                                                              
306000     END-IF                                                               
306100     .                                                                    
306200     EJECT                                                                
306300 IBA-FLYTTA-W01103 SECTION.                                               
306400                                                                          
306500     PERFORM UNTIL W01103-IDARTNR NOT < WS-SPAR-IDARTNR                   
306600          OR END-OF-W01103                                                
306700       PERFORM S08-LAES-W01103                                            
306800     END-PERFORM                                                          
306900                                                                          
307000     IF W01103-IDARTNR = WS-SPAR-IDARTNR                                  
307100        IF W01103-IDARTNR-TILLK IS NUMERIC                                
307200          MOVE W01103-IDARTNR-TILLK  TO UT2-IDARTNR-TILLK                 
307300        ELSE                                                              
307400          MOVE ZERO                  TO UT2-IDARTNR-TILLK                 
307500        END-IF                                                            
307600        PERFORM S08-LAES-W01103                                           
307700     END-IF                                                               
307800     .                                                                    
307900     EJECT                                                                
308000 J-BEHANDLA-KVPB-REF SECTION.                                             
308100                                                                          
308200     MOVE WS-KVPB-REF-US TO U90-KVPB-REF-US                               
308300                                                                          
308400     IF WS-KVPB-REF-NDC-TOT = ZERO                                        
308500        CONTINUE                                                          
308600     ELSE                                                                 
308700       COMPUTE U90-KVPB-NDC-TOT ROUNDED                                   
308800                = WS-KVPB-REF-NDC-TOT                                     
308900     END-IF                                                               
309000     IF WS-KVPB-REF-SDC-TOT = ZERO                                        
309100        CONTINUE                                                          
309200     ELSE                                                                 
309300       COMPUTE U90-KVPB-SDC-TOT ROUNDED                                   
309400                = WS-KVPB-REF-SDC-TOT                                     
309500     END-IF                                                               
309600                                                                          
309700     MOVE ZERO TO WS-KVPB-REF-SDC-TOT                                     
309800                  WS-KVPB-REF-NDC-TOT                                     
309900                  WS-KVPB-REF-US                                          
310000     .                                                                    
310100     EJECT                                                                
310200 Z-FINIT SECTION.                                                         
310300     CLOSE PARMFIL                                                        
310400           W01160                                                         
310500           W01168                                                         
310600           W01173                                                         
310700           W01174                                                         
310800           W01184                                                         
310900           W01103                                                         
311000           WXTR90                                                         
311100           WXTR9A                                                         
311200                                                                          
311300     MOVE 'S' TO POSTSUM-OPKOD                                            
311400     CALL POSTSUM USING POSTSUM-PARM                                      
311500     .                                                                    
311600     EJECT                                                                
311700 S01-LAES-W01160  SECTION.                                                
311800     READ W01160 INTO W01160-AREA                                         
311900     AT END                                                               
312000        SET END-OF-W01160 TO TRUE                                         
312100                                                                          
312200     NOT AT END                                                           
312300        MOVE 'W01160' TO POSTSUM-FDNAMN                                   
312400        MOVE 'WXTR90D1' TO POSTSUM-DDNAMN2                                
312500        CALL POSTSUM USING POSTSUM-PARM                                   
312600     END-READ                                                             
312700     .                                                                    
312800     EJECT                                                                
312900 S02-LAES-W01168  SECTION.                                                
313000     READ W01168 INTO W01168-AREA                                         
313100     AT END                                                               
313200        MOVE +999999999 TO W01168-ART-IDARTNR                             
313300        SET END-OF-W01168 TO TRUE                                         
313400                                                                          
313500     NOT AT END                                                           
313600        MOVE 'W01168' TO POSTSUM-FDNAMN                                   
313700        MOVE 'WXTR90D2' TO POSTSUM-DDNAMN2                                
313800        CALL POSTSUM USING POSTSUM-PARM                                   
313900     END-READ                                                             
314000     .                                                                    
314100     EJECT                                                                
314200 S04-LAES-W01173  SECTION.                                                
314300     READ W01173 INTO W01173-AREA                                         
314400     AT END                                                               
314500        MOVE +999999999 TO W01173-IDARTNR                                 
314600        SET END-OF-W01173 TO TRUE                                         
314700                                                                          
314800     NOT AT END                                                           
314900        MOVE 'W01173' TO POSTSUM-FDNAMN                                   
315000        MOVE 'WXTR90D4' TO POSTSUM-DDNAMN2                                
315100        CALL POSTSUM USING POSTSUM-PARM                                   
315200     END-READ                                                             
315300     .                                                                    
315400     EJECT                                                                
315500 S05-LAES-W01174  SECTION.                                                
315600     READ W01174 INTO W01174-AREA                                         
315700     AT END                                                               
315800        MOVE +999999999 TO W01174-IDARTNR                                 
315900        SET END-OF-W01174 TO TRUE                                         
316000                                                                          
316100     NOT AT END                                                           
316200        MOVE 'W01174' TO POSTSUM-FDNAMN                                   
316300        MOVE 'WXTR90D5' TO POSTSUM-DDNAMN2                                
316400        CALL POSTSUM USING POSTSUM-PARM                                   
316500     END-READ                                                             
316600     .                                                                    
316700     EJECT                                                                
316800 S07-LAES-W01184 SECTION.                                                 
316900     READ W01184 INTO W01184-AREA                                         
317000     AT END                                                               
317100        MOVE +999999999 TO W01184-SLAG-IDARTNR                            
317200        SET END-OF-W01184 TO TRUE                                         
317300                                                                          
317400     NOT AT END                                                           
317500        MOVE 'W01184' TO POSTSUM-FDNAMN                                   
317600        MOVE 'WXTR90D7' TO POSTSUM-DDNAMN2                                
317700        CALL POSTSUM USING POSTSUM-PARM                                   
317800     END-READ                                                             
317900     .                                                                    
318000     EJECT                                                                
318100 S08-LAES-W01103 SECTION.                                                 
318200     READ W01103 INTO W01103-AREA                                         
318300     AT END                                                               
318400        MOVE +999999999 TO W01103-IDARTNR                                 
318500        SET END-OF-W01103 TO TRUE                                         
318600                                                                          
318700     NOT AT END                                                           
318800        MOVE 'W01103' TO POSTSUM-FDNAMN                                   
318900        MOVE 'WXTR90D8' TO POSTSUM-DDNAMN2                                
319000        CALL POSTSUM USING POSTSUM-PARM                                   
319100     END-READ                                                             
319200     .                                                                    
319300     EJECT                                                                
319400 S09-LAES-PARMFIL SECTION.                                                
319500     READ PARMFIL INTO PARM-AREA                                          
319600     AT END                                                               
319700        MOVE SPACES     TO PARM-BATCHTYP                                  
319800        SET END-OF-PARMFIL TO TRUE                                        
319900                                                                          
320000     NOT AT END                                                           
320100        MOVE 'PARMFIL' TO POSTSUM-FDNAMN                                  
320200        MOVE 'WXTR90DA' TO POSTSUM-DDNAMN2                                
320300        CALL POSTSUM USING POSTSUM-PARM                                   
320400     END-READ                                                             
320500     .                                                                    
320600     EJECT                                                                
320700 S21-SKRIV-WXTR90 SECTION.                                                
320800                                                                          
320900     WRITE WXTR90-POST FROM U90-AREA                                      
321000                                                                          
321100     MOVE 'WXTR90' TO POSTSUM-FDNAMN                                      
321200     MOVE 'WXTR90D6' TO POSTSUM-DDNAMN2                                   
321300     CALL POSTSUM USING POSTSUM-PARM                                      
321400     .                                                                    
321500     EJECT                                                                
321600 S22-SKRIV-WXTR9A SECTION.                                                
321700                                                                          
321800     WRITE WXTR9A-POST FROM UT2-AREA                                      
321900                                                                          
322000     MOVE 'WXTR9A' TO POSTSUM-FDNAMN                                      
322100     MOVE 'WXTR90D9' TO POSTSUM-DDNAMN2                                   
322200     CALL POSTSUM USING POSTSUM-PARM                                      
322300     .                                                                    
322400     EJECT                                                                
322500 S99-ABEND SECTION.                                                       
322600                                                                          
322700     SKIP2                                                                
322800     MOVE 'S' TO POSTSUM-OPKOD                                            
322900     CALL POSTSUM USING POSTSUM-PARM                                      
323000     CALL ABEND USING RKOD-ABEND-UTAN-DUMP                                
323100     .                                                                    
323200     EJECT                                                                
323300* --- IMS SEKTIONER ---                                                   
323400     SKIP3                                                                
323500                                                                          
323600 IMS-GU-WDB601    SECTION.                                                
323700     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
323800          DELIMITED BY SIZE INTO SSA1                                     
323900     MOVE '  ' TO GODK-STATUSKODER                                        
324000     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
324100     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
324200     PERFORM IMS-STATUSKONTROLL                                           
324300     .                                                                    
324400     EJECT                                                                
324500 IMS-GN-WDB601    SECTION.                                                
324600     MOVE 'WDB601  ' TO SSA1                                              
324700     MOVE '  GB'     TO GODK-STATUSKODER                                  
324800     CALL CBLTDLI USING GN WDB6-PCB DLI-IO-AREA-B601 SSA1                 
324900     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
325000     PERFORM IMS-STATUSKONTROLL                                           
325100     .                                                                    
325200     EJECT                                                                
325300 IMS-GU-WDB616    SECTION.                                                
325400     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
325500          DELIMITED BY SIZE INTO SSA1                                     
325600     STRING 'WDB616  (IDDCREF  =' W-IDDC-B616-X ')'                       
325700          DELIMITED BY SIZE INTO SSA2                                     
325800     MOVE '  ' TO GODK-STATUSKODER                                        
325900     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-WDB616 SSA1 SSA2               
326000     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
326100     PERFORM IMS-STATUSKONTROLL                                           
326200     .                                                                    
326300     EJECT                                                                
326400 IMS-STATUSKONTROLL SECTION.                                              
326500                                                                          
326600     SET STATUS-IX TO 1                                                   
326700     SEARCH GODK-STATUS                                                   
326800       AT END                                                             
326900         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
327000           DELIMITED BY SIZE INTO FELTEXT                                 
327100         DISPLAY FELTEXT                                                  
327200         CALL FELLOG                                                      
327300       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
327400         CONTINUE                                                         
327500     END-SEARCH                                                           
327600     .                                                                    
