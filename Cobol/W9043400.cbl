010000 ID DIVISION.                                                             
020000     SKIP2                                                                
030001 PROGRAM-ID.     W9043400.                                                
040000 AUTHOR.         HENRIK ARONSSON.                                         
050000 DATE-WRITTEN.   APRIL 1990.                                              
060000                                                                          
070000     REMARKS.                                                             
080000*                                                                         
090000*    FUNKTION.                                                            
100000*        UPPDATERING OCH NYUPPLÄGG AV STRUKTURHUVUD.                      
110000*        BORTTAG AV BEFINTLIG STRUKTUR.                                   
120000*        KOPIERING AV BEFINTLIG STRUKTUR TILL NY STRUKTUR.                
130000*                                                                         
140000*        PROGRAMMET ÄR EN UPPDATERINGS-MPP                                
150000*        PROGRAMMET UPPDATERAR WLSATB (WDJ1)                              
160000*        PROGRAMMET UPPDATERAR WLXXAZ (WDR5)                              
180000*        PROGRAMMET LÄSER      WLBENA (WDD3)                              
190001*        PROGRAMMET LÄSER      WLARTC (WDK6)                              
200000*        PROGRAMMET LÄSER      WDF5                                       
210000*                                                                         
211001*        ÄT SPLIT 930402 BL                                               
212001*           - ÄNDRING GODKÄNDA PRODUKTSLAG                                
213001*                                                                         
214001* KOPIA FRÅN PGM 1021100 FÖR ANVÄNDNING I SPIE2.                          
215001*        PGM-KOD BORTAGET FÖR ATT EJ SKICKA ONÖDIG INFO                   
216001*        TILL SPIE2       JN//040428                                      
217001*                                                                         
218001*                                                                         
220000*    INDATA.                                                              
230001*        TRANSAKTION: W9T434                                              
240001*        MID:         W90434I1                                            
250000*                                                                         
260000*    UTDATA.                                                              
270001*        MOD:         W90434O1                                            
280000*                                                                         
290000*    SUBPGM.                                                              
300000*        FELLOG                                                           
310000*        CBLTDLI                                                          
320001*        WORKDAY                                                          
330000*        W009VADD                                                         
340000                                                                          
350000     SKIP3                                                                
360000 ENVIRONMENT DIVISION.                                                    
370000 DATA DIVISION.                                                           
380000     EJECT                                                                
390000 WORKING-STORAGE SECTION.                                                 
390101*    -COPY WY2000W1                                                       
391001     SKIP3                                                                
400000 77  IDPGM                   PIC X(8)    VALUE 'W1021100'.                
410000 77  JA                      PIC X       VALUE 'J'.                       
420000 77  NEJ                     PIC X       VALUE 'N'.                       
430000 77  SPRAK-IX                PIC S9(9)   VALUE +0   COMP SYNC.            
440000 77  INDX                    PIC S9(9)   VALUE +0   COMP SYNC.            
450000 77  INDX2                   PIC S9(9)   VALUE +0   COMP SYNC.            
460000 77  MAX-IDAO                PIC S9      VALUE +5.                        
470000 77  MAX-TABELL-LAENGD       PIC S9(3)   VALUE +20.                       
480001 77  MAX-MOD-LAENGD          PIC S9(4)   VALUE +557 COMP SYNC.            
500001 77  IDLEVNR-ART-WS          PIC X(5)    VALUE SPACE.                     
510000 77  IDARTNR-WS              PIC X(9)    VALUE SPACE.                     
520000 77  IDARTNR-KONVERTERAT-WS  PIC 9(9).                                    
530000 77  IDARTNR-NYUPPL-WS       PIC 9(9).                                    
540000 77  IDARTNR-NYUPPL-KONVERTERAT-WS  PIC 9(9).                             
550000 77  IDARTNR2-WS             PIC 9(9).                                    
560001 77  IDSKYLT-WS              PIC X(3)   VALUE 'S  '.                      
570000 77  KDPRODSL-WS             PIC 9(3).                                    
580000 77  KDBENHOM-WS             PIC 9(1).                                    
590000 77  KDBENHOM-SPAR           PIC 9(1).                                    
600000 77  KDBENHOM-RASA-SPAR      PIC 9(1).                                    
610000 77  IDFKNGRP-WS             PIC X(4).                                    
620000 77  BEART-SPAR              PIC X(25).                                   
630000 77  BEART-RASA-SPAR         PIC X(25).                                   
640000 77  BEART-SVE-SPAR          PIC X(25).                                   
650000                                                                          
660000 77  INDATA-SW               PIC X       VALUE 'J'.                       
670000   88  INDATA-OK                         VALUE 'J'.                       
680000   88  INDATA-FEL                        VALUE 'N'.                       
690000                                                                          
700000 77  NYCKLAR-SW              PIC X       VALUE 'J'.                       
710000   88  NYCKLAR-OK                        VALUE 'J'.                       
720000   88  NYCKLAR-FEL                       VALUE 'N'.                       
730000                                                                          
740000 77  ALLT-SW                 PIC X       VALUE 'N'.                       
750000   88  ALLT-OK                           VALUE 'J'.                       
760000                                                                          
770000 77  W-IDTRANS               PIC X(4)    VALUE SPACE.                     
780001   88  EGEN-MID                          VALUE '9434'.                    
790001   88  GODK-MID                          VALUE '9434'                     
800000                                               '1212' '1213'              
810000                                               '1214' '1215'.             
820001   88  MID-MED-IDSKYLT                   VALUE '9434' '1212'              
830000                                               '1213' '1214'.             
840000                                                                          
850000 77  INGAAR-I-KOPIERAD-SATS-SW PIC X.                                     
860000   88  INGAAR-I-KOPIERAD-SATS            VALUE 'J'.                       
870000   88  INGAAR-EJ-I-KOPIERAD-SATS         VALUE 'N'.                       
880000                                                                          
890000 77  SATSTABELL-SLUT-SW      PIC X.                                       
900000   88  SATSTABELL-SLUT                   VALUE 'J'.                       
910000                                                                          
920000 77  KDPRODSL-GODK-WS        PIC 9(2).                                    
930001   88  KDPRODSL-GODK                     VALUE 11 13 14 15 16             
940001                                               17 18 19                   
940002                                               21 23 24 25 26             
940003                                               27 28 29                   
990002                                               71 72 73 74.               
000000   EJECT                                                                  
010000*************************************************                         
020000*             UPPDATERINGS-TYPER                *                         
030000*************************************************                         
040000                                                                          
050000 77  UPDATE-TYP              PIC X.                                       
060000   88  UPDATE-AENDRA                     VALUE 'Ä'.                       
070000   88  UPDATE-BORTTAG                    VALUE 'B'.                       
080000   88  UPDATE-NYUPPL                     VALUE 'N'.                       
090000   88  UPDATE-NYUPPL-KOPIERING           VALUE 'K'.                       
100000   EJECT                                                                  
101001*      --- VALID IDDC CODES                                               
102001*                                                                         
103001*01    -COPY WWDCKONS                                                     
104001       EJECT                                                              
110000                                                                          
120000*************************************************                         
130000*         STRUKTURNR-TYP PÅ RASA                *                         
140000*************************************************                         
150000                                                                          
160000********* FÖR IDARTNR-NYCKEL ********************                         
170000                                                                          
180000 77  STRUKTURNR-TYP          PIC X.                                       
190000   88  STRUKTURNR-FINNS                  VALUE 'F'.                       
200000   88  STRUKTURNR-SAKNAS                 VALUE 'S'.                       
210000   88  STRUKTURNR-SPAERRAT               VALUE 'P'.                       
220000   88  STRUKTURNR-BORTTAGET              VALUE 'B'.                       
230000                                                                          
240000 77  FINNS-OKONVERTERAT-SW   PIC X.                                       
250000   88  STRUKTURNR-FINNS-OKONVERTERAT     VALUE 'J'.                       
260000                                                                          
270000 77  FINNS-KONVERTERAT-SW    PIC X.                                       
280000   88  STRUKTURNR-FINNS-KONVERTERAT      VALUE 'J'.                       
290000                                                                          
300000********* FÖR IDARTNR-NYUPPLÄGG *****************                         
310000                                                                          
320000 77  STRUKTURNR-NY-TYP       PIC X.                                       
330000   88  STRUKTURNR-NY-FINNS               VALUE 'F'.                       
340000                                                                          
350000   EJECT                                                                  
360000                                                                          
370000*************************************************                         
380000*         STRUKTURNR-TYP PÅ ARTREG              *                         
390000*************************************************                         
400000                                                                          
410000********* FÖR IDARTNR-NYCKEL ********************                         
420000                                                                          
430000 77  STRUKTURNR-PAA-ARTREG   PIC X.                                       
440000   88  STRUKTURNR-FINNS-PAA-ARTREG       VALUE 'F'.                       
450000                                                                          
460000********* FÖR IDARTNR-NYUPPLÄGG *****************                         
470000                                                                          
480000 77  STRUKTURNR-NY-PAA-ARTREG   PIC X.                                    
490000   88  STRUKTURNR-NY-FINNS-PAA-ARTREG    VALUE 'F'.                       
500000                                                                          
510000   EJECT                                                                  
520000                                                                          
530000 01  AENDRING                PIC X       VALUE 'Ä'.                       
540000 01  BORTTAG                 PIC X       VALUE 'B'.                       
550000 01  NYUPPLAEGG              PIC X       VALUE 'N'.                       
560000 01  NYUPPLAEGG-KOPIERING    PIC X       VALUE 'K'.                       
570000                                                                          
580000 01  FINNS                   PIC X       VALUE 'F'.                       
590000 01  SAKNAS                  PIC X       VALUE 'S'.                       
600000 01  SPAERRAT                PIC X       VALUE 'P'.                       
610000 01  BORTTAGET               PIC X       VALUE 'B'.                       
620000                                                                          
630000 01  DAGENS-DATUM            PIC 9(6).                                    
640000 01  FILLER REDEFINES DAGENS-DATUM.                                       
650000   03  DATUM-AAMM            PIC 9(4).                                    
660000   03  DATUM-DD              PIC 9(2).                                    
670000                                                                          
680000 01  DATUM-AAVV-MELLANLAGR   PIC 9(4).                                    
690000                                                                          
700000 01  W009VADD-AREA.                                                       
710000   03  W009VADD-AAVV         PIC S9(5)   COMP-3.                          
720000   03  ANTAL                 PIC S9(3)   COMP-3.                          
730000                                                                          
740000     EJECT                                                                
750000****************** TABELLER ****************************                  
760000                                                                          
770000 01  SATSTABELL.                                                          
780000   03  SATSNR     OCCURS 20 PIC S9(9)   COMP-3.                           
790000                                                                          
800000     EJECT                                                                
810000 01  MEDDELANDEN.                                                         
811001   03  MED-1.                                                             
812001       05  FILLER            PIC X(40)   VALUE                            
830000           'FYLL I INMATNINGSFÄLT VID UPPDATERING'.                       
831001       05  FILLER            PIC X(40)   VALUE                            
832001           'PF11 AND NO INPUT                    '.                       
833001   03  FILLER REDEFINES MED-1.                                            
834001       05  MED1 OCCURS 2     PIC X(40).                                   
835001                                                                          
836001   03  MED-2.                                                             
837001       05  FILLER            PIC X(40)   VALUE                            
838201           'REGISTRERING UTFÖRD. STRUKTUR EJ KLAR'.                       
839001       05  FILLER            PIC X(40)   VALUE                            
839101           'UPDATED. STRUCTURE NOT COMPLETE      '.                       
839201   03  FILLER REDEFINES MED-2.                                            
839301       05  MED2 OCCURS 2     PIC X(40).                                   
839401                                                                          
839501   03  MED-3.                                                             
839601       05  FILLER            PIC X(60)   VALUE                            
839901           'STRUKTUR KAN EJ TAS BORT. ÄR UNDER BEARBETNING'.              
840001       05  FILLER            PIC X(60)   VALUE                            
840101           'DELETE NOT POSSIBLE. STRUCTURE IN USE'.                       
841001   03  FILLER REDEFINES MED-3.                                            
850001       05  MED3 OCCURS 2     PIC X(60).                                   
851001                                                                          
852001   03  MED-4.                                                             
853001       05  FILLER            PIC X(40)   VALUE                            
853201           'RADER FINNS REGISTRERADE '.                                   
855001       05  FILLER            PIC X(40)   VALUE                            
856001           'STRUCTURE LINES EXIST    '.                                   
857001   03  FILLER REDEFINES MED-4.                                            
858001       05  MED4 OCCURS 2     PIC X(40).                                   
859001                                                                          
860001   03  MED-5.                                                             
870001       05  FILLER            PIC X(40)   VALUE                            
872001           'UPPDATERING UTFÖRD'.                                          
890001       05  FILLER            PIC X(40)   VALUE                            
891001           'UPDATED                  '.                                   
892001   03  FILLER REDEFINES MED-5.                                            
893001       05  MED5 OCCURS 2     PIC X(40).                                   
894001                                                                          
895001   03  MED-6.                                                             
896001       05  FILLER            PIC X(40)   VALUE                            
896201           'TRYCK PF11 FÖR UPPDATERING'.                                  
898001       05  FILLER            PIC X(40)   VALUE                            
899001           'PRESS PF11 TO UPDATE     '.                                   
900001   03  FILLER REDEFINES MED-6.                                            
910001       05  MED6 OCCURS 2     PIC X(40).                                   
911001                                                                          
912001   03  MED-7.                                                             
913001       05  FILLER            PIC X(40)   VALUE                            
913201           'STRUKTUR ÄR UNDER BEARBETNING'.                               
915001       05  FILLER            PIC X(40)   VALUE                            
916001           'STRUCTURE IS IN USE      '.                                   
917001   03  FILLER REDEFINES MED-7.                                            
918001       05  MED7 OCCURS 2     PIC X(40).                                   
919001                                                                          
920001   03  MED-8.                                                             
930001       05  FILLER            PIC X(40)   VALUE                            
932001           'STRUKTUR BORTTAGSMÄRKT'.                                      
950001       05  FILLER            PIC X(40)   VALUE                            
950101           'STRUCTURE IS MARKED TO BE DELETED'.                           
952001   03  FILLER REDEFINES MED-8.                                            
953001       05  MED8 OCCURS 2     PIC X(40).                                   
954001                                                                          
955001   03  MED-9.                                                             
956001       05  FILLER            PIC X(60)   VALUE                            
956201      'STRUKTUR KAN EJ KOPIERAS. INNEHÅLLER INMATAT STRUKTURNR'.          
958001       05  FILLER            PIC X(60)   VALUE                            
959001       'UPDATE NOT POSSIBLE'.                                             
960001   03  FILLER REDEFINES MED-9.                                            
970001       05  MED9 OCCURS 2     PIC X(60).                                   
971001                                                                          
972001   03  MED-10.                                                            
973001       05  FILLER            PIC X(40)   VALUE                            
973201           'STRUKTUR FINNS REDAN PÅ BASEN'.                               
975001       05  FILLER            PIC X(40)   VALUE                            
975101           'STRUCTURE ALREADY REGISTRED'.                                 
977001   03  FILLER REDEFINES MED-10.                                           
978001       05  MED10 OCCURS 2     PIC X(40).                                  
979001                                                                          
980001   03  MED-11.                                                            
990001       05  FILLER            PIC X(60)   VALUE                            
002001           'ARTIKEL ERSÄTTNINGSMÄRKT PÅ ARTIKELREGISTRET'.                
010001       05  FILLER            PIC X(60)   VALUE                            
011001           'THIS PART IS SUPERSEDED'.                                     
012001   03  FILLER REDEFINES MED-11.                                           
013001       05  MED11 OCCURS 2     PIC X(60).                                  
014001                                                                          
015001   03  MED-12.                                                            
016001       05  FILLER            PIC X(60)   VALUE                            
016201           'STRUKTUR KAN EJ KOPIERAS. ÄR UNDER BEARBETNING'.              
018001       05  FILLER            PIC X(60)   VALUE                            
019001           'STRUCTURE IS IN USE'.                                         
020001   03  FILLER REDEFINES MED-12.                                           
030001       05  MED12 OCCURS 2    PIC X(60).                                   
030101                                                                          
030201   03  MED-13.                                                            
030301       05  FILLER            PIC X(60)   VALUE                            
030501           'STRUKTURNR FINNS REGISTRERAD SOM RAD'.                        
030701       05  FILLER            PIC X(60)   VALUE                            
030801           'STRUCTURE EXISTS AS A STRUCTURE LINE'.                        
030901   03  FILLER REDEFINES MED-13.                                           
031001       05  MED13 OCCURS 2     PIC X(60).                                  
032001                                                                          
033001   03  MED-14.                                                            
034001       05  FILLER            PIC X(60)   VALUE                            
034201           'DU HAR STRUKTUR UNDER BEARBETNING'.                           
036001       05  FILLER            PIC X(60)   VALUE                            
037001           'YOU HAVE STRUCTURE IN USE'.                                   
038001   03  FILLER REDEFINES MED-14.                                           
039001       05  MED14 OCCURS 2    PIC X(60).                                   
040001                                                                          
050001   03  MED-15.                                                            
060001       05  FILLER            PIC X(60)   VALUE                            
062001           'ARTIKEL RENSAD PÅ ARTIKELREGISTRET'.                          
080001       05  FILLER            PIC X(60)   VALUE                            
090001           'PART NUMBER DELETED              '.                           
091001   03  FILLER REDEFINES MED-15.                                           
092001       05  MED15 OCCURS 2    PIC X(60).                                   
093001                                                                          
094001   03  MED-16.                                                            
095001       05  FILLER            PIC X(60)   VALUE                            
095201           'GÄLLANDE RADER FINNS'.                                        
097001       05  FILLER            PIC X(60)   VALUE                            
098001           'VALID STRUCTURE LINES EXIST'.                                 
099001   03  FILLER REDEFINES MED-16.                                           
100001       05  MED16 OCCURS 2    PIC X(60).                                   
110001                                                                          
120001   03  MED-17.                                                            
130001       05  FILLER            PIC X(60)   VALUE                            
130201         'STRUKTUR FINNS REGISTRERAD. ENDAST RADER KAN KOPIERAS'.         
132001       05  FILLER            PIC X(60)   VALUE                            
133001          'ONLY STRUCTURE LINES CAN BE COPIED '.                          
134001   03  FILLER REDEFINES MED-17.                                           
135001       05  MED17 OCCURS 2    PIC X(60).                                   
136001                                                                          
137001   03  MED-18.                                                            
138001       05  FILLER            PIC X(60)   VALUE                            
138201           'UPPDATERING UTFÖRD. RADER KOPIERADE'.                         
140001       05  FILLER            PIC X(60)   VALUE                            
150001          'UPDATED. STRUCTURE LINES COPIED'.                              
151001   03  FILLER REDEFINES MED-18.                                           
152001       05  MED18 OCCURS 2    PIC X(60).                                   
153001                                                                          
154001   03  FEL-1.                                                             
155001       05  FILLER            PIC X(40)   VALUE                            
155201           'STRUKTUR SAKNAS'.                                             
157001       05  FILLER            PIC X(40)   VALUE                            
158001           'STRUCTURE IS MISSING'.                                        
159001   03  FILLER REDEFINES FEL-1.                                            
160001       05  FEL1 OCCURS 2     PIC X(40).                                   
170001                                                                          
171001   03  FEL-2.                                                             
172001       05  FILLER            PIC X(40)   VALUE                            
172201           'KORRIGERA UPPLYSTA FÄLT'.                                     
174001       05  FILLER            PIC X(40)   VALUE                            
175001           'CORRECT HIGHLIGHTED FIELDS'.                                  
176001   03  FILLER REDEFINES FEL-2.                                            
177001       05  FEL2 OCCURS 2     PIC X(40).                                   
177101                                                                          
177201   03  FEL-3.                                                             
177301       05  FILLER            PIC X(40)   VALUE                            
177501           'STRUKTURNR EJ NUMERISKT'.                                     
177701       05  FILLER            PIC X(40)   VALUE                            
177801           'STRUCTURE NUMBER NOT NUMERIC'.                                
177901   03  FILLER REDEFINES FEL-3.                                            
178001       05  FEL3 OCCURS 2     PIC X(40).                                   
179001                                                                          
180001   03  FEL-4.                                                             
190001       05  FILLER            PIC X(40)   VALUE                            
192001           'NYCKLAR FEL      '.                                           
210001       05  FILLER            PIC X(40)   VALUE                            
220001           'WRONG KEYS       '.                                           
230001   03  FILLER REDEFINES FEL-4.                                            
240001       05  FEL4 OCCURS 2     PIC X(40).                                   
241001                                                                          
242001   03  FEL-5.                                                             
243001       05  FILLER            PIC X(40)   VALUE                            
243201           'STRUKTURNR FELAKTIGT'.                                        
245001       05  FILLER            PIC X(40)   VALUE                            
246001           'STRUCTURE NUMBER NOT CORRECT'.                                
247001   03  FILLER REDEFINES FEL-5.                                            
248001       05  FEL5 OCCURS 2     PIC X(40).                                   
249001                                                                          
250001   03  FEL-6.                                                             
260001       05  FILLER            PIC X(40)   VALUE                            
262001           'UPPDATERING EJ TILLÅTEN'.                                     
280001       05  FILLER            PIC X(40)   VALUE                            
281001           'UPDATE NOT ALLOWED'.                                          
282001   03  FILLER REDEFINES FEL-6.                                            
283001       05  FEL6 OCCURS 2     PIC X(40).                                   
283101                                                                          
284001   03  FEL-7.                                                             
285001       05  FILLER            PIC X(40)   VALUE                            
285101           'MATA IN NYA NYCKLAR    '.                                     
287001       05  FILLER            PIC X(40)   VALUE                            
288001           'ENTER NEW KEYS    '.                                          
289001   03  FILLER REDEFINES FEL-7.                                            
290001       05  FEL7 OCCURS 2     PIC X(40).                                   
330000                                                                          
340000     EJECT                                                                
350000 01  GENERELLA-SUBPROGRAM.                                                
360000   03  CBLTDLI               PIC X(8)    VALUE 'CBLTDLI '.                
370000   03  FELLOG                PIC X(8)    VALUE 'FELLOG  '.                
380000   03  WMEDKONV              PIC X(8)    VALUE 'WMEDKONV'.                
390001   03  WORKDAY               PIC X(8)    VALUE 'WORKDAY '.                
400000   03  W009VADD              PIC X(8)    VALUE 'W009VADD'.                
401001   03  W005INIT              PIC X(8)    VALUE 'W005INIT'.                
410000     EJECT                                                                
420001*01   -COPY WWLAND03                                                      
440000     EJECT                                                                
450001*01   -COPY WMEDAREA                                                      
470000     EJECT                                                                
480001*01   -COPY WORKAREA                                                      
490001     EJECT                                                                
490101*                     ****   PARAMETRAR TILL W005INIT                     
491001*01   -COPY WMSGINIT                                                      
500000     EJECT                                                                
510000******************************************************************        
520000*                                                                         
530000*                AREOR FÖR MFS OCH SKÄRMHANTERING                         
540000*                                                                         
550000 01  FILLER                  PIC X(16)   VALUE 'MFS-WS'.                  
560000     SKIP3                                                                
570001*01  MID -COPY W90434I1                                                   
590000     EJECT                                                                
600000*01    -COPY WMSGAREA                                                     
620000     EJECT                                                                
630001*  03  MOD -COPY W90434O1  -RED MSG-AREA.                                 
650000     EJECT                                                                
660000*01    -COPY WMFSAREA                                                     
680000     EJECT                                                                
690000******************************************************************        
700000*                                                                         
710000*        ARBETS-AREOR TILL IMS-SEKTIONERNA                                
720000*                                                                         
730000 01  IMS-WS.                                                              
740000   03  FILLER                PIC X(16)   VALUE 'IMS-WS     '.             
750000     SKIP3                                                                
760000*                        **** STATUS-KOD FRÅN IMS                         
770000   03  STATUS-WS             PIC XX.                                      
780000     88  SEGMENT-FINNS                   VALUE '  '.                      
790000     88  SEGMENT-FINNS-REDAN             VALUE 'II'.                      
800000     88  SEGMENT-SAKNAS                  VALUE 'GE'.                      
810000     88  SEGMENT-HOGRE                   VALUE 'GA'.                      
820000                                                                          
830000*         **** STATUS-KODER SOM ANVÄNDS VID KOPIERING                     
840000   03  STATUS-NOT-WS         PIC XX.                                      
850000     88  NOTSEGMENT-SLUT                 VALUE 'GE'.                      
860000                                                                          
870000   03  STATUS-ART-WS         PIC XX.                                      
880000     88  RADSEGMENT-SLUT                 VALUE 'GE'.                      
890000                                                                          
900000   03  GODK-STATUSKODER.                                                  
910000     05  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
920000                                                                          
930000 01  NYCKLAR-TILL-DLI.                                                    
940000   03  W-IDARTNR-X.                                                       
950000     05  W-IDARTNR           PIC S9(9)   VALUE ZERO  COMP-3.              
960000                                                                          
970000   03  W-IDARTNR-I-X.                                                     
980000     05  W-IDARTNR-I         PIC S9(9)   VALUE ZERO  COMP-3.              
990000                                                                          
000000   03  W-IDARTNR-KONV-MIN-X.                                              
010000     05  W-IDARTNR-KONV-MIN  PIC S9(9)   VALUE +100000000 COMP-3.         
020000                                                                          
030000   03  W-IDARTNR-KONV-MAX-X.                                              
040000     05  W-IDARTNR-KONV-MAX  PIC S9(9)   VALUE +999999999 COMP-3.         
050000                                                                          
060000   03  W-IDARTNR-MIN-X.                                                   
070000     05  W-IDARTNR-MIN       PIC S9(9)   VALUE +1 COMP-3.                 
080000                                                                          
090000   03  W-IDARTNR-MAX-X.                                                   
100000     05  W-IDARTNR-MAX       PIC S9(9)   VALUE +99999999  COMP-3.         
110000                                                                          
120000   03  W-IDSKYLT-X.                                                       
130000     05  W-IDSKYLT           PIC  X(3)   VALUE SPACE.                     
140000                                                                          
150000   03  W-KDNOTTYP-X.                                                      
160000     05  W-KDNOTTYP          PIC  S9(1)  VALUE ZERO COMP-3.               
170000                                                                          
210000   03  W-BEART-X.                                                         
220000     05  W-BEART             PIC  X(25)  VALUE SPACE.                     
230000                                                                          
240000   03  W-IDLEVNR-X.                                                       
250001     05  W-IDLEVNR           PIC  X(5)  VALUE SPACE.                      
260000                                                                          
270000   03  W-BELEVART-X.                                                      
280000     05  W-BELEVART          PIC  X(30)  VALUE SPACE.                     
290000                                                                          
300000   03  W-IDBENR-X.                                                        
310000     05  W-IDBENR            PIC  S9(1)  VALUE ZERO  COMP-3.              
320000                                                                          
330000   03  W-KDSTRRAD-X.                                                      
340000     05  W-KDSTRRAD          PIC  X(1)   VALUE SPACE.                     
350000                                                                          
360000   03  W-IDRADNR-X.                                                       
370000     05  W-IDRADNR           PIC  S9(5)  VALUE ZERO  COMP-3.              
380000                                                                          
390000   03  W-IDUSER-X.                                                        
400000     05  W-IDUSER            PIC  X(8)   VALUE SPACE.                     
410000                                                                          
420000   03  W-WDGXKEY-X.                                                       
430000     05  W-IDHTYP            PIC  X(4)   VALUE SPACE.                     
440000     05  W-LOW-VALUE         PIC  X(26)  VALUE SPACE.                     
450000                                                                          
460000 01    SSA1                  PIC X(96).                                   
470000 01    SSA2                  PIC X(96).                                   
480000 01    SSA3                  PIC X(64).                                   
490000     EJECT                                                                
500000*                            IMS FUNKTIONSKODER                           
510000*01    -COPY W0003                                                        
530000     EJECT                                                                
540000*    -------------     DLI INPUT-OUTPUT AREA                              
550000 01  FILLER                  PIC X(16)   VALUE 'DLI-IO-AREA'.             
560000                                                                          
570000 01  DLI-IO-AREA.                                                         
580001   03  IO-AREA               PIC X(900)  VALUE SPACE.                     
590000     SKIP3                                                                
630000*  03  WLBENA01  -COPY WDD301  -PRE BENA01-  -RED IO-AREA.                
650000     EJECT                                                                
660000*  03  WLBENA11  -COPY WDD311  -PRE BENA11-  -RED IO-AREA.                
680000     EJECT                                                                
690000*  03  WDF501    -COPY WDF501                -RED IO-AREA.                
710000     EJECT                                                                
720000*  03  WDF502    -COPY WDF502                -RED IO-AREA.                
740000     EJECT                                                                
750001*  03  WLARTC01  -COPY WDK601                -RED IO-AREA.                
770000     EJECT                                                                
780001*  03  WLARTC11  -COPY WDK611                -RED IO-AREA.                
800000     EJECT                                                                
810001*  03  WLARTC25  -COPY WDK625                -RED IO-AREA.                
830000     EJECT                                                                
900000*  03  WLSATB01  -COPY WDJ101  -PRE SATB01-  -RED IO-AREA.                
920000     EJECT                                                                
930000*  03  WLSATB11  -COPY WDJ111  -PRE SATB11-  -RED IO-AREA.                
950000     EJECT                                                                
960000*  03  WLSATB22  -COPY WDJ122  -PRE SATB22-  -RED IO-AREA.                
980000     EJECT                                                                
990000*  03  WLXXAZ11  -COPY WDGX1152 -PRE XXAZ11-  -RED IO-AREA.               
010000     EJECT                                                                
020000                                                                          
030000   03  WLSATB-CSEQ REDEFINES IO-AREA.                                     
040000*      05 WLSATB11 -COPY WDJ111 -PRE SATB11C-                             
060000         SKIP3                                                            
070000*      05 WLSATB01 -COPY WDJ101 -PRE SATB01C-                             
090000         EJECT                                                            
100000                                                                          
110000*    -------------     DLI INPUT-OUTPUT AREA-2                            
120000 01  FILLER                  PIC X(16)   VALUE 'DLI-IO-AREA-2'.           
130000                                                                          
140000 01  DLI-IO-AREA2.                                                        
150001   03  IO-AREA2             PIC X(240)  VALUE SPACE.                      
160000*  03  WLSATB01  -COPY WDJ101  -PRE SATB01I-  -RED IO-AREA2.              
180000     EJECT                                                                
190000*  03  WLSATB11  -COPY WDJ111  -PRE SATB11I-  -RED IO-AREA2.              
210000     EJECT                                                                
220000*  03  WLSATB22  -COPY WDJ122  -PRE SATB22I-  -RED IO-AREA2.              
240000     EJECT                                                                
250000 LINKAGE SECTION.                                                         
260000*01    -COPY W0009     -PRE MSG-                                          
280000     EJECT                                                                
330001*01    -COPY W0008     -PRE USEA-                                         
350000     05  FILLER              PIC X.                                       
351001     EJECT                                                                
352001*01    -COPY W0008     -PRE WDF5-                                         
353001     05  FILLER              PIC X.                                       
360000     EJECT                                                                
370000*01    -COPY W0008     -PRE ARTC-                                         
390000     05  FILLER              PIC X.                                       
400000     EJECT                                                                
410000*01    -COPY W0008     -PRE SATB-                                         
430000     05  FILLER              PIC X.                                       
440000     EJECT                                                                
450000*01    -COPY W0008     -PRE SATB-I-                                       
470000     05  FILLER              PIC X.                                       
480000     EJECT                                                                
490000*01    -COPY W0008     -PRE SATB-C-                                       
510000     05  FILLER              PIC X.                                       
520000*01    -COPY W0008     -PRE SATB-D-                                       
540000     05  FILLER              PIC X.                                       
550000     EJECT                                                                
560000*01    -COPY W0008     -PRE BENA-A-                                       
580000     05  FILLER              PIC X.                                       
590000     EJECT                                                                
600000*01    -COPY W0008     -PRE BENA-B-                                       
620000     05  FILLER              PIC X.                                       
630000     EJECT                                                                
640000*01    -COPY W0008     -PRE XXAZ-                                         
660000     05  FILLER              PIC X.                                       
670000     EJECT                                                                
680001 PROCEDURE DIVISION USING MSG-PCB USEA-PCB WDF5-PCB                       
690000                           ARTC-PCB SATB-PCB SATB-I-PCB                   
700000                           SATB-C-PCB SATB-D-PCB BENA-A-PCB               
710000                           BENA-B-PCB XXAZ-PCB.                           
720001     ENTRY 'DLITCBL' USING MSG-PCB USEA-PCB WDF5-PCB                      
730000                           ARTC-PCB SATB-PCB SATB-I-PCB                   
740000                           SATB-C-PCB SATB-D-PCB BENA-A-PCB               
750000                           BENA-B-PCB XXAZ-PCB.                           
760000                                                                          
770000     PERFORM IMS-GET-MSG                                                  
780000     IF SEGMENT-FINNS                                                     
790000       PERFORM A-INIT                                                     
800000       PERFORM B-KOLLA-NYCKLAR                                            
810000       IF NYCKLAR-OK                                                      
820000         MOVE NEJ TO ALLT-SW                                              
830000         PERFORM C-KOLLA-STRUKTURNR-TYP                                   
840000         IF (MFS-UPDATE)  AND (NOT STRUKTURNR-SPAERRAT)                   
850000                           AND (NOT STRUKTURNR-BORTTAGET)                 
860000           PERFORM D-KOLLA-UPDATE-TYP                                     
870000           PERFORM E-KOLLA-INPUT                                          
880000           IF INDATA-OK                                                   
890000             PERFORM F-UPPDATERA                                          
900000             MOVE JA TO ALLT-SW                                           
910000           END-IF                                                         
920000         ELSE                                                             
930000           IF MFS-FIRST                                                   
940000             MOVE JA TO ALLT-SW                                           
950000           ELSE                                                           
960000             PERFORM H-SAMMA-SIDA                                         
970000           END-IF                                                         
980000         END-IF                                                           
990000         IF ALLT-OK                                                       
000000           PERFORM G-LAES-VISA-STRUKTUR                                   
010000         END-IF                                                           
020000       END-IF                                                             
030000       MOVE MAX-MOD-LAENGD TO MSG-KVLL                                    
040000       PERFORM IMS-INSERT-MSG                                             
050000     END-IF                                                               
060000                                                                          
070000     MOVE ZERO TO RETURN-CODE                                             
080000     GOBACK                                                               
090000     .                                                                    
100000     EJECT                                                                
110000 A-INIT SECTION.                                                          
120000                                                                          
130000     IF MSG-DUBBLA-TRANSKODER                                             
140001       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W90434I1                 
150000       MOVE MSG-IDTRANS-2  TO MFS-IDTRANS                                 
160000       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
170000     ELSE                                                                 
180001       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W90434I1                  
190000       MOVE MSG-IDTRANS-1  TO MFS-IDTRANS                                 
200000       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
210000     END-IF                                                               
220000                                                                          
230000     MOVE MSG-KDTRTYP          TO MFS-KDTRTYP                             
240000     MOVE MSG-IDPFK   TO MFS-IDPFK                                        
251002     MOVE MFS-IDTRANS TO W-IDTRANS                                        
260000                                                                          
270000     MOVE LOW-VALUE  TO MSG-AREA                                          
280001     MOVE 'W90434O1' TO MFS-IDMOD                                         
290001     MOVE '9434'     TO MOD-IDTRANS                                       
300000     MOVE SPACE      TO MOD-TEMFSFEL MOD-TEMFSINF                         
310000                                                                          
320000     IF NOT EGEN-MID                                                      
330000       MOVE SPACE TO MFS-KDTRTYP                                          
340000       MOVE '7' TO MFS-IDPFK                                              
350000     END-IF                                                               
440000                                                                          
450000     ACCEPT DAGENS-DATUM FROM DATE                                        
460000     MOVE SAKNAS TO STRUKTURNR-TYP                                        
470000     MOVE SPACE  TO STRUKTURNR-PAA-ARTREG                                 
480000                                                                          
490000     MOVE +1 TO INDX                                                      
500000     PERFORM UNTIL INDX > MAX-TABELL-LAENGD                               
510000       MOVE +0 TO SATSNR(INDX)                                            
520000       ADD  +1 TO INDX                                                    
530000     END-PERFORM                                                          
540000                                                                          
550000     .                                                                    
560000     EJECT                                                                
570000 B-KOLLA-NYCKLAR SECTION.                                                 
580000                                                                          
590000     MOVE JA TO NYCKLAR-SW                                                
600001*    MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-IN                               
610001*                            MOD-IDSKYLT-IN                               
620000                                                                          
621001     MOVE ALL '+' TO MSGI-WMSGINIT                                        
622001     MOVE '001'             TO MSGI-KDCALL                                
623001     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
623101                               MSGI-IDLTERM-USER                          
623201     MOVE '9434'            TO MSGI-IDTRANS                               
624001     IF MFS-IDTRANS = '9434'                                              
625001     OR (MID-IDARTNR-IN NUMERIC                                           
625101     AND MID-IDARTNR-IN > ZERO)                                           
626001         MOVE MID-IDARTNR-IN TO MSGI-IDARTNR                              
627001     END-IF                                                               
628001     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
629001     MOVE MSGI-IDARTNR TO IDARTNR-WS                                      
629101     INSPECT IDARTNR-WS REPLACING ALL SPACE BY ZERO                       
629201                                                                          
629301     IF MSGI-IDLAND-SPR = 'GB'                                            
629401       MOVE +2    TO SPRAK-IX                                             
629501       MOVE 'GB ' TO MED-IDSKYLT                                          
629601     ELSE                                                                 
629701       MOVE +1    TO SPRAK-IX                                             
629801       MOVE 'S  ' TO MED-IDSKYLT                                          
629901     END-IF                                                               
630101                                                                          
631000     IF MID-IDARTNR-IN = ALL '+'                                          
640001       CONTINUE                                                           
660000     ELSE                                                                 
680000       MOVE '7'             TO MFS-IDPFK                                  
690000       MOVE SPACE           TO MFS-KDTRTYP                                
700000     END-IF                                                               
710000                                                                          
720000     IF (IDARTNR-WS NUMERIC) AND (IDARTNR-WS > ZERO) AND                  
730000          (IDARTNR-WS < 10000000)                                         
740000       MOVE IDARTNR-WS TO W-IDARTNR                                       
750000     ELSE                                                                 
760001       MOVE NEJ             TO NYCKLAR-SW                                 
770001       MOVE FEL4(SPRAK-IX)  TO MOD-TEMFSFEL                               
780000     END-IF                                                               
790000                                                                          
021001                                                                          
040000     SET WWLAND03-IX TO +1                                                
050000     SEARCH WWLAND03-IDSKYLT-RAD                                          
060000       AT END                                                             
070001         MOVE NEJ               TO NYCKLAR-SW                             
080001         MOVE FEL4(SPRAK-IX)    TO MOD-TEMFSFEL                           
090000       WHEN WWLAND03-IDSKYLT(WWLAND03-IX) = IDSKYLT-WS                    
100000         CONTINUE                                                         
110000     END-SEARCH                                                           
120000                                                                          
130000     IF GODK-MID OR NYCKLAR-OK                                            
161001       CONTINUE                                                           
170000     ELSE                                                                 
200001       MOVE FEL7(SPRAK-IX)  TO MOD-TEMFSFEL                               
210000     END-IF                                                               
220000                                                                          
230000     IF NYCKLAR-FEL                                                       
250000       PERFORM MFS-RENSA-FAELT-UT                                         
270000     END-IF                                                               
280000     .                                                                    
290000     EJECT                                                                
300000 C-KOLLA-STRUKTURNR-TYP SECTION.                                          
310000******************************************                                
320000*  KOLLAR STRUKTURNR-TYP PÅ RASA         *                                
330000******************************************                                
340000                                                                          
350000     MOVE SAKNAS TO STRUKTURNR-TYP                                        
360000     MOVE NEJ      TO FINNS-OKONVERTERAT-SW                               
370000                      FINNS-KONVERTERAT-SW                                
380000                                                                          
390000     MOVE IDARTNR-WS TO W-IDARTNR                                         
400000     PERFORM IMS-GET-SATB01                                               
410000     IF SEGMENT-FINNS                                                     
420000       MOVE FINNS                TO STRUKTURNR-TYP                        
430000       MOVE JA                   TO FINNS-OKONVERTERAT-SW                 
440000       MOVE SATB01-STR-BEART-SVE TO BEART-RASA-SPAR                       
450000       MOVE SATB01-STR-KDBENHOM  TO KDBENHOM-RASA-SPAR                    
460000       IF SATB01-STR-TIBORT > 0                                           
470001         MOVE MED8(SPRAK-IX) TO MOD-TEMFSINF                              
480000         MOVE BORTTAGET TO STRUKTURNR-TYP                                 
490000       END-IF                                                             
500000     END-IF                                                               
510000                                                                          
520000******* KOLLAR OM STRUKTURNUMRET ÄR 'UNDER BEHANDLING'                    
530000                                                                          
540000     MOVE IDARTNR-WS TO IDARTNR2-WS                                       
550000     COMPUTE IDARTNR-KONVERTERAT-WS = 999999999 - IDARTNR2-WS             
560000     MOVE IDARTNR-KONVERTERAT-WS TO W-IDARTNR                             
570000     PERFORM IMS-GET-SATB01                                               
580000     IF SEGMENT-FINNS                                                     
590000                                                                          
600001       MOVE 001                 TO WORK-KDCALL                            
601001       MOVE WC-CDC-SE           TO WORK-IDDC                              
610001       MOVE SATB01-STR-TIREGDAT TO WORK-TIAAMMDD-FOM                      
620001       MOVE DAGENS-DATUM        TO WORK-TIAAMMDD-TOM                      
630001       CALL WORKDAY USING WORK-KDCALL                                     
640001                          WORK-DATE-AREA                                  
650001                          WORK-KDSVAR                                     
660001       IF (WORK-KVWORKD > 2) OR (WORK-KDSVAR-FEL)                         
670000*********** OM MAN TRÄFFAR PÅ EN KONVERTERAD                              
680000*********** STRUKTUR SOM ÄR ÄLDRE ÄN 2 DAGAR TAS DEN BORT.                
690000*********** ARB-KDSVAR-FEL INNEBÄR ATT ANTALET DAGAR > 999.               
700000         PERFORM IMS-DLET-SATB                                            
710000       ELSE                                                               
720000         IF MSG-SIGNON-USERID = SATB01-STR-IDUSER                         
730000           MOVE FINNS                TO STRUKTURNR-TYP                    
740000           MOVE JA                   TO FINNS-KONVERTERAT-SW              
750000           MOVE SATB01-STR-BEART-SVE TO BEART-RASA-SPAR                   
760000           MOVE SATB01-STR-KDBENHOM  TO KDBENHOM-RASA-SPAR                
770000           PERFORM S08-KOLLA-OM-USER-HAR-LAASNING                         
780000           IF INDATA-OK                                                   
790000             CONTINUE                                                     
800000           ELSE                                                           
810001             MOVE MED7(SPRAK-IX)  TO MOD-TEMFSINF                         
820000             MOVE SPAERRAT TO STRUKTURNR-TYP                              
830000           END-IF                                                         
840000         ELSE                                                             
850000*********  STRUKTUREN KONVERTERAD VIA BILD 1221                           
860000*********  UPPDATERING EJ TILLÅTEN                                        
870001           MOVE MED7(SPRAK-IX)    TO MOD-TEMFSINF                         
880001           MOVE SPAERRAT          TO STRUKTURNR-TYP                       
890000         END-IF                                                           
900000       END-IF                                                             
910000      END-IF                                                              
920000                                                                          
930000      IF (NOT MFS-UPDATE) AND (STRUKTURNR-SAKNAS)                         
940001        MOVE FEL1(SPRAK-IX)       TO MOD-TEMFSFEL                         
950000      END-IF                                                              
960000                                                                          
970000      MOVE IDARTNR-WS TO W-IDARTNR                                        
980000      PERFORM IMS-GET-ARTC01                                              
990000      IF SEGMENT-FINNS                                                    
000000        MOVE FINNS TO STRUKTURNR-PAA-ARTREG                               
010000      END-IF                                                              
020000      .                                                                   
030000      EJECT                                                               
040000 D-KOLLA-UPDATE-TYP SECTION.                                              
050000******************************************                                
060000*  KOLLAR TYP AV UPPDATERING VID PF11    *                                
070000******************************************                                
080000                                                                          
130000                                                                          
140000     IF STRUKTURNR-FINNS                                                  
210000           MOVE AENDRING TO UPDATE-TYP                                    
240000     ELSE                                                                 
250000       MOVE NYUPPLAEGG TO UPDATE-TYP                                      
260000     END-IF                                                               
270000     .                                                                    
280000     EJECT                                                                
290000 E-KOLLA-INPUT SECTION.                                                   
300000                                                                          
310000     MOVE JA  TO INDATA-SW                                                
320000                                                                          
330000     IF IDSKYLT-WS = 'S  '                                                
340000       CONTINUE                                                           
350000     ELSE                                                                 
360000       MOVE 'GB' TO IDSKYLT-WS                                            
370000     END-IF                                                               
380000                                                                          
440000     IF MID-IDSTRTYP-IN = SPACE                                           
450000****** BEHANDLAS SOM EJ IFYLLD                                            
460000       MOVE '+' TO MID-IDSTRTYP-IN                                        
470000     END-IF                                                               
480000                                                                          
490000     IF MID-INPUT = ALL '+'                                               
500000       MOVE NEJ TO INDATA-SW                                              
520000       PERFORM MFS-ROER-EJ-FAELT-UT                                       
540000                                                                          
550000       IF (UPDATE-NYUPPL) AND (STRUKTURNR-FINNS-PAA-ARTREG)               
560000*******  VISA ATT STRUKTURTYP MÅSTE FYLLAS I                              
570001         MOVE FEL2(SPRAK-IX)     TO MOD-TEMFSFEL                          
580000         MOVE MFS-ALFA-FAELT-FEL TO MOD-IDSTRTYP-IN-ATTR                  
590000       ELSE                                                               
600001         MOVE MED1(SPRAK-IX) TO MOD-TEMFSINF                              
610000       END-IF                                                             
620000       PERFORM S06-STAENG-EVENTUELLA-FAELT                                
630000     ELSE                                                                 
640000       IF UPDATE-BORTTAG                                                  
650000         PERFORM EA-KOLLA-BORTTAG-INPUT                                   
660000       ELSE                                                               
670000         IF UPDATE-AENDRA                                                 
680000           PERFORM EB-KOLLA-AENDRING-INPUT                                
690000         ELSE                                                             
700000           IF UPDATE-NYUPPL                                               
710000             PERFORM EC-KOLLA-NYUPPLAEGG-INPUT                            
720000           ELSE                                                           
730000             PERFORM ED-KOLLA-KOPIERING-INPUT                             
740000           END-IF                                                         
750000         END-IF                                                           
760000       END-IF                                                             
770000                                                                          
780000       IF INDATA-FEL                                                      
790000         IF MOD-TEMFSFEL = SPACE                                          
800000********** TEMFSFEL FYLLS I OM EJ REDAN IFYLLD                            
810001           MOVE FEL2(SPRAK-IX) TO MOD-TEMFSFEL                            
820000         END-IF                                                           
840000         PERFORM MFS-ROER-EJ-FAELT-UT                                     
860000                                                                          
870000         PERFORM S06-STAENG-EVENTUELLA-FAELT                              
880000                                                                          
890000       END-IF                                                             
900000     END-IF                                                               
910000     .                                                                    
920000     EJECT                                                                
930000 EA-KOLLA-BORTTAG-INPUT SECTION.                                          
940000                                                                          
430000                                                                          
440000       IF (MID-IDSTRTYP-IN = ALL '+')                                     
450000         CONTINUE                                                         
460000       ELSE                                                               
470000         MOVE NEJ                TO INDATA-SW                             
480000         MOVE MFS-ALFA-FAELT-FEL TO MOD-IDSTRTYP-IN-ATTR                  
490000       END-IF                                                             
500000                                                                          
640000                                                                          
650000       IF INDATA-OK                                                       
660000         IF (STRUKTURNR-FINNS-KONVERTERAT) AND                            
670000            (NOT STRUKTURNR-FINNS-OKONVERTERAT)                           
680000********   OM STRUKTUREN BARA FINNS I KONVERTERAD FORM                    
690000           CONTINUE                                                       
700000         ELSE                                                             
710000           IF (STRUKTURNR-FINNS-KONVERTERAT) AND                          
720000              (STRUKTURNR-FINNS-OKONVERTERAT)                             
730000********     OM STRUKTUREN FINNS I BÅDE KONVERTERAD FORM                  
740000********     OCH I 'RIKTIG' FORM                                          
750000             MOVE NEJ                TO INDATA-SW                         
760001             MOVE MED3(SPRAK-IX)     TO MOD-TEMFSINF                      
780000           ELSE                                                           
790000             IF (STRUKTURNR-FINNS-OKONVERTERAT)                           
800000*************  OM STRUKTUREN BARA FINNS I 'RIKTIG' FORM                   
810000                                                                          
820000                                                                          
830000               IF STRUKTURNR-FINNS-PAA-ARTREG                             
840000***************  BORTTAG KAN VARA TILLÅTEN OM STRUKTURTYP = 'S'           
850000***************  OCH SORT PÅ ARTREG EJ ÄR 'SA'                            
860000                                                                          
870000                 MOVE IDARTNR-WS TO W-IDARTNR                             
880000                 PERFORM IMS-GET-SATB01                                   
890000                 IF SATB01-STR-IDSTRTYP = 'S'                             
900001                   PERFORM IMS-GET-ARTC01                                 
910001                   IF ART-KDSORT = 'SA' OR 'TM'                           
920000                     MOVE NEJ                TO INDATA-SW                 
940000                   ELSE                                                   
950000                     PERFORM EAA-KOLLA-ATT-EJ-RADER                       
960000                     IF INDATA-OK                                         
970000                       PERFORM S07-KOLLA-INGAAR-I-ANNAN-SATS              
980000                       IF INDATA-OK                                       
990000                         CONTINUE                                         
000000                       ELSE                                               
010001                         MOVE MED13(SPRAK-IX)    TO MOD-TEMFSINF          
030000                       END-IF                                             
040000                     ELSE                                                 
050001                       MOVE MED16(SPRAK-IX)    TO MOD-TEMFSINF            
070000                     END-IF                                               
080000                   END-IF                                                 
090000                 ELSE                                                     
100000                   MOVE NEJ                TO INDATA-SW                   
120000                 END-IF                                                   
130000               ELSE                                                       
140000                 PERFORM S07-KOLLA-INGAAR-I-ANNAN-SATS                    
150000                 IF INDATA-OK                                             
160000                   CONTINUE                                               
170000                 ELSE                                                     
180001                   MOVE MED13(SPRAK-IX)    TO MOD-TEMFSINF                
200000                 END-IF                                                   
210000               END-IF                                                     
220000             ELSE                                                         
230000*************  OM STRUKTUREN SAKNAS                                       
240000               MOVE NEJ                TO INDATA-SW                       
260000             END-IF                                                       
270000           END-IF                                                         
280000         END-IF                                                           
290000       END-IF                                                             
300000                                                                          
360000     .                                                                    
370000     SKIP3                                                                
380000 EAA-KOLLA-ATT-EJ-RADER SECTION.                                          
390000******************************************                                
400000* KONTROLL ATT INGA GÄLLANDE RADER FINNS *                                
410000******************************************                                
420000                                                                          
430000     PERFORM IMS-GET-SATB11                                               
440000     PERFORM UNTIL SEGMENT-SAKNAS OR INDATA-FEL                           
450000       IF SEGMENT-FINNS                                                   
450101         MOVE SATB11-RAD-TISTODAT   TO TMP1-YYMMDD                        
450201         MOVE DAGENS-DATUM          TO TMP2-YYMMDD                        
451001         PERFORM WY2000P1                                                 
460001         IF TMP1-YYMMDD <= TMP2-YYMMDD                                    
470000           PERFORM IMS-GET-SATB11                                         
480000         ELSE                                                             
490000           MOVE NEJ TO INDATA-SW                                          
500000         END-IF                                                           
510000       END-IF                                                             
520000     END-PERFORM                                                          
530000     .                                                                    
540000     EJECT                                                                
550000 EB-KOLLA-AENDRING-INPUT SECTION.                                         
560000                                                                          
570000     IF STRUKTURNR-FINNS-PAA-ARTREG                                       
580000       PERFORM S01-KOLLA-FINNS-PA-ARTREG-INPU                             
590000     ELSE                                                                 
600000       PERFORM EBA-KOLLA-SAKN-PA-ARTREG-INPU                              
610000     END-IF                                                               
680000                                                                          
800000     .                                                                    
810000     EJECT                                                                
820000 EBA-KOLLA-SAKN-PA-ARTREG-INPU SECTION.                                   
830000*************************************************                         
840000* *KOLL VID ÄNDRING OCH IDARTNR SAKNAS PÅ ARTREG*                         
850000*************************************************                         
860000                                                                          
880000                                                                          
160000                                                                          
170000     IF (MID-IDSTRTYP-IN = ALL '+')                                       
180000       CONTINUE                                                           
190000     ELSE                                                                 
200000       IF (MID-IDSTRTYP-IN = 'S' OR 'R' OR 'K')                           
210000         MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDSTRTYP-IN-ATTR                
220000       ELSE                                                               
230000         MOVE NEJ                TO INDATA-SW                             
240000         MOVE MFS-ALFA-FAELT-FEL TO MOD-IDSTRTYP-IN-ATTR                  
250000       END-IF                                                             
260000     END-IF                                                               
270000     .                                                                    
280000     EJECT                                                                
290000 EC-KOLLA-NYUPPLAEGG-INPUT SECTION.                                       
300000                                                                          
310000     MOVE IDARTNR-WS TO IDARTNR-NYUPPL-WS                                 
320000                                                                          
390000                                                                          
400000     IF INDATA-OK                                                         
410000       MOVE IDARTNR-NYUPPL-WS TO W-IDARTNR                                
420000       PERFORM S07-KOLLA-INGAAR-I-ANNAN-SATS                              
430000       IF INDATA-OK                                                       
440000                                                                          
450000         PERFORM IMS-GET-ARTC01                                           
460000         IF SEGMENT-FINNS                                                 
470001           IF ART-KDERS-UTG > 0                                           
480001             MOVE NEJ               TO INDATA-SW                          
490001             MOVE FEL5(SPRAK-IX)    TO MOD-TEMFSFEL                       
500001             MOVE MED15(SPRAK-IX)   TO MOD-TEMFSINF                       
510000           ELSE                                                           
530001             PERFORM IMS-GET-ARTC11                                       
540001             IF CLAG-KDERS > 0                                            
550001               MOVE NEJ              TO INDATA-SW                         
560001               MOVE FEL5(SPRAK-IX)   TO MOD-TEMFSFEL                      
570001               MOVE MED11(SPRAK-IX)  TO MOD-TEMFSINF                      
580000             ELSE                                                         
590000               PERFORM S01-KOLLA-FINNS-PA-ARTREG-INPU                     
600000             END-IF                                                       
610000           END-IF                                                         
620000                                                                          
630000         ELSE                                                             
640000           PERFORM ECA-KOLLA-SAKN-PA-ARTREG-INPU                          
650000         END-IF                                                           
720000                                                                          
840000                                                                          
850000       ELSE                                                               
860000         MOVE NEJ               TO INDATA-SW                              
870001         MOVE FEL5(SPRAK-IX)    TO MOD-TEMFSFEL                           
880001         MOVE MED13(SPRAK-IX)   TO MOD-TEMFSINF                           
900000       END-IF                                                             
910000                                                                          
920000     ELSE                                                                 
930000       PERFORM MFS-LAES-IN-IGEN                                           
940000     END-IF                                                               
950000     .                                                                    
960000     EJECT                                                                
970000 ECA-KOLLA-SAKN-PA-ARTREG-INPU SECTION.                                   
980000*************************************************                         
990000* KOLL VID NYUPPL OCH IDARTNR SAKNAS PÅ ARTREG  *                         
000000*************************************************                         
010000                                                                          
030000                                                                          
330000                                                                          
340000       IF (MID-IDSTRTYP-IN = ALL '+')                                     
350000         MOVE NEJ                TO INDATA-SW                             
360000         MOVE MFS-ALFA-FAELT-FEL TO MOD-IDSTRTYP-IN-ATTR                  
370000       ELSE                                                               
380000         IF (MID-IDSTRTYP-IN = 'S' OR 'R' OR 'K')                         
390000           MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDSTRTYP-IN-ATTR              
400000         ELSE                                                             
410000           MOVE NEJ                TO INDATA-SW                           
420000           MOVE MFS-ALFA-FAELT-FEL TO MOD-IDSTRTYP-IN-ATTR                
430000         END-IF                                                           
440000       END-IF                                                             
450000     .                                                                    
460000     EJECT                                                                
470000 ED-KOLLA-KOPIERING-INPUT SECTION.                                        
480000                                                                          
490000     MOVE SAKNAS TO STRUKTURNR-NY-TYP                                     
500000                                                                          
520000     INSPECT IDARTNR2-WS REPLACING LEADING SPACE BY ZERO                  
530000     IF IDARTNR2-WS NUMERIC                                               
540000       IF (IDARTNR2-WS > 0 AND < 10000000)                                
550000         PERFORM EDA-KOLL-OM-KOPIERING-TILLATEN                           
560000         IF INDATA-OK                                                     
570000           MOVE IDARTNR2-WS TO IDARTNR-NYUPPL-WS                          
580000           COMPUTE IDARTNR-NYUPPL-KONVERTERAT-WS = 999999999 -            
590000                                               IDARTNR-NYUPPL-WS          
600000                                                                          
610000           MOVE IDARTNR-NYUPPL-WS TO W-IDARTNR                            
620000           PERFORM IMS-GET-SATB01                                         
630000           IF SEGMENT-FINNS                                               
640000             MOVE FINNS TO STRUKTURNR-NY-TYP                              
650000             PERFORM IMS-GET-SATB11                                       
660000             IF SEGMENT-FINNS                                             
670000*************  OM DET NYA STRUKTURNUMRET REDAN FINNS PÅ RASA              
680000*************  FÅR DET EJ HA NÅGRA RADER REGISTRERADE                     
690000               MOVE NEJ               TO INDATA-SW                        
700001               MOVE FEL2(SPRAK-IX)    TO MOD-TEMFSFEL                     
710001               MOVE MED4(SPRAK-IX)    TO MOD-TEMFSINF                     
730000             END-IF                                                       
740000           END-IF                                                         
750000                                                                          
760000           IF INDATA-OK                                                   
770000             MOVE IDARTNR-NYUPPL-KONVERTERAT-WS TO W-IDARTNR              
780000             PERFORM IMS-GET-SATB01                                       
790000             IF SEGMENT-FINNS                                             
800000*********** DET NYA STRUKTURNUMRET FÅR INTE REDAN FINNAS PÅ RASA          
810000*********** I 'KONVERTERAD FORM'                                          
820000               MOVE NEJ               TO INDATA-SW                        
830001               MOVE FEL2(SPRAK-IX)    TO MOD-TEMFSFEL                     
840001               MOVE MED10(SPRAK-IX)   TO MOD-TEMFSINF                     
860000             ELSE                                                         
880000                                                                          
890000               IF STRUKTURNR-FINNS-KONVERTERAT                            
900000*********** DET KOPIERADE STRUKTURNUMRET FÅR INTE FINNAS PÅ RASA          
910000*********** I 'KONVERTERAD FORM' UTAN ENDAST I 'RIKTIG' FORM              
920001                 MOVE NEJ              TO INDATA-SW                       
930001                 MOVE FEL5(SPRAK-IX)   TO MOD-TEMFSFEL                    
940001                 MOVE MED12(SPRAK-IX)  TO MOD-TEMFSINF                    
950000               END-IF                                                     
960000             END-IF                                                       
970000           END-IF                                                         
980000         ELSE                                                             
990001           MOVE FEL6(SPRAK-IX)  TO MOD-TEMFSFEL                           
000001           MOVE MED14(SPRAK-IX) TO MOD-TEMFSINF                           
010000         END-IF                                                           
020000       ELSE                                                               
030000         MOVE NEJ TO INDATA-SW                                            
040000       END-IF                                                             
050000     ELSE                                                                 
060001       MOVE NEJ             TO INDATA-SW                                  
070001       MOVE FEL3(SPRAK-IX)  TO MOD-TEMFSINF                               
080000     END-IF                                                               
090000                                                                          
100000     IF INDATA-OK                                                         
110000       IF STRUKTURNR-NY-FINNS                                             
120000*******  NYTT IDARTNR-NYUPPL FINNS REDAN SOM ROT PÅ RASA                  
130000*******  LYS UPP EV. FÖRSÖK TILL ÄNDRING AV ARTIKELUPPG                   
140000         PERFORM S09-LYS-UPP-INMATADE-FAELT                               
150000         IF INDATA-FEL                                                    
160001           MOVE FEL2(SPRAK-IX)   TO MOD-TEMFSFEL                          
170001           MOVE MED17(SPRAK-IX)  TO MOD-TEMFSINF                          
180000         END-IF                                                           
190000       END-IF                                                             
200000                                                                          
210000       IF INDATA-OK                                                       
220000         MOVE NEJ TO INGAAR-I-KOPIERAD-SATS-SW                            
230000         PERFORM EDB-KOLLA-OM-INGAAR-I-KOP-SATS                           
240000                                                                          
250000         IF INGAAR-EJ-I-KOPIERAD-SATS                                     
260000           MOVE IDARTNR-NYUPPL-WS TO W-IDARTNR                            
270000           PERFORM S07-KOLLA-INGAAR-I-ANNAN-SATS                          
280000           IF INDATA-OK                                                   
290000                                                                          
300000             MOVE IDARTNR-NYUPPL-WS TO W-IDARTNR                          
310000             PERFORM IMS-GET-ARTC01                                       
320000             IF SEGMENT-FINNS                                             
330000               MOVE FINNS TO STRUKTURNR-NY-PAA-ARTREG                     
340000                                                                          
350001               IF ART-KDERS-UTG > 0                                       
360001                 MOVE NEJ                  TO INDATA-SW                   
370001                 MOVE FEL5(SPRAK-IX)       TO MOD-TEMFSFEL                
380001                 MOVE MED15(SPRAK-IX)      TO MOD-TEMFSINF                
400000               ELSE                                                       
420001                 PERFORM IMS-GET-ARTC11                                   
430001                 IF CLAG-KDERS > 0                                        
440001                   MOVE NEJ                TO INDATA-SW                   
450001                   MOVE FEL5(SPRAK-IX)     TO MOD-TEMFSFEL                
460001                   MOVE MED11(SPRAK-IX)    TO MOD-TEMFSINF                
480000                 ELSE                                                     
490000                   PERFORM S01-KOLLA-FINNS-PA-ARTREG-INPU                 
500000                 END-IF                                                   
510000               END-IF                                                     
520000                                                                          
530000             ELSE                                                         
540000               PERFORM EDC-KOLLA-SAKN-PA-ARTREG-INPU                      
550000             END-IF                                                       
620000                                                                          
740000                                                                          
750000           ELSE                                                           
760000             MOVE NEJ               TO INDATA-SW                          
770001             MOVE FEL5(SPRAK-IX)    TO MOD-TEMFSFEL                       
780001             MOVE MED13(SPRAK-IX)   TO MOD-TEMFSINF                       
800000           END-IF                                                         
810000                                                                          
820000         ELSE                                                             
830000           MOVE NEJ               TO INDATA-SW                            
840001           MOVE FEL5(SPRAK-IX)    TO MOD-TEMFSFEL                         
850001           MOVE MED9(SPRAK-IX)    TO MOD-TEMFSINF                         
870000         END-IF                                                           
880000       END-IF                                                             
890000                                                                          
900000     ELSE                                                                 
910000       PERFORM MFS-LAES-IN-IGEN                                           
960000     END-IF                                                               
970000     .                                                                    
980000     EJECT                                                                
990000 EDA-KOLL-OM-KOPIERING-TILLATEN SECTION.                                  
000000*****************************************************                     
010000* KOPIERING ENDAST TILLÅTEN OM 'SIGNON-USERID':     *                     
020000* ¤ INTE HAR NÅGON GÄLLANDE LÅSNING PÅ WDR5 OCH     *                     
030000*   INTE HAR NÅGRA GÄLLANDE KONVERTERADE STRUKTURER.*                     
040000*****************************************************                     
050000                                                                          
060000     PERFORM S08-KOLLA-OM-USER-HAR-LAASNING                               
070000                                                                          
080000     IF INDATA-OK                                                         
090000       PERFORM IMS-GET-SATB01-DSEQ                                        
100000       PERFORM UNTIL SEGMENT-SAKNAS                                       
110000         IF SEGMENT-FINNS                                                 
120000                                                                          
130001           MOVE 001                 TO WORK-KDCALL                        
131001           MOVE WC-CDC-SE           TO WORK-IDDC                          
140001           MOVE SATB01-STR-TIREGDAT TO WORK-TIAAMMDD-FOM                  
150001           MOVE DAGENS-DATUM        TO WORK-TIAAMMDD-TOM                  
160001           CALL WORKDAY USING WORK-KDCALL                                 
170001                              WORK-DATE-AREA                              
180001                              WORK-KDSVAR                                 
190001           IF (WORK-KVWORKD > 2) OR (WORK-KDSVAR-FEL)                     
200000***********  OM MAN TRÄFFAR PÅ EN KOVERTERAD STRUKTUR                     
210000***********  SOM ÄR ÄLDRE ÄN 2 IGNORERAS DEN.                             
220000***********  ARB-KDSVAR-FEL INNEBÄR ATT ANTALET DAGAR > 999.              
230000                                                                          
240000             PERFORM IMS-GET-SATB01-DSEQ                                  
250000           ELSE                                                           
260000             MOVE 'GE' TO STATUS-WS                                       
270000             MOVE NEJ  TO INDATA-SW                                       
280000           END-IF                                                         
290000         END-IF                                                           
300000       END-PERFORM                                                        
310000     END-IF                                                               
320000     .                                                                    
330000     EJECT                                                                
340000 EDB-KOLLA-OM-INGAAR-I-KOP-SATS SECTION.                                  
350000**********************************************************                
360000*   KONTROLL ATT DET NYA STRUKTURNUMRET EJ INGÅR SOM RAD *                
370000*   I KOPIERAD STRUKTUR ELLER I "SATS-I-SATS".VID PÅ-    *                
380000*   TRÄFFADE AV EN SATS ELLER KOMPLETTENHET LÄGGS DETTA  *                
390000*   ARTIKELNR I EN TABELL .MAN LÄSER SEDAN ARTIKELNUMREN *                
400000*   I TABELLEN OCH KOLLAR DESSA O.S.V.                   *                
410000**********************************************************                
420000                                                                          
430000     MOVE +1 TO INDX                                                      
440000                INDX2                                                     
450000     MOVE NEJ            TO SATSTABELL-SLUT-SW                            
470000     MOVE IDARTNR-WS     TO W-IDARTNR                                     
480000     PERFORM UNTIL SATSTABELL-SLUT                                        
490000                                                                          
500000       PERFORM IMS-GET-SATB01                                             
510000       IF SEGMENT-FINNS                                                   
520000         PERFORM IMS-GET-SATB11                                           
530000         IF SEGMENT-FINNS                                                 
540000           PERFORM UNTIL (SEGMENT-SAKNAS) OR                              
550000                           (INGAAR-I-KOPIERAD-SATS)                       
550101             MOVE SATB11-RAD-TISTODAT   TO TMP1-YYMMDD                    
550201             MOVE DAGENS-DATUM          TO TMP2-YYMMDD                    
551001             PERFORM WY2000P1                                             
560001             IF TMP1-YYMMDD < TMP2-YYMMDD                                 
570000               PERFORM IMS-GET-SATB11                                     
580000             ELSE                                                         
590000               IF SATB11-RAD-IDARTNR = IDARTNR-NYUPPL-WS                  
600000                 MOVE JA TO INGAAR-I-KOPIERAD-SATS-SW                     
610000                            SATSTABELL-SLUT-SW                            
620000               ELSE                                                       
630000                 IF SATB11-RAD-IDSTRTYP = 'S' OR 'K'                      
640000*****************  OM INGÅENDE RAD ÄR EN SATS                             
650000*****************  ELLER EN KOMPLETTENHET                                 
660000                   MOVE SATB11-RAD-IDARTNR TO SATSNR(INDX)                
670000                   ADD +1 TO INDX                                         
680000                 END-IF                                                   
690000                 PERFORM IMS-GET-SATB11                                   
700000               END-IF                                                     
710000             END-IF                                                       
720000           END-PERFORM                                                    
730000         END-IF                                                           
740000       END-IF                                                             
750000                                                                          
760000       IF SATSNR(INDX2) = ZERO                                            
770000         MOVE JA TO SATSTABELL-SLUT-SW                                    
780000       ELSE                                                               
790000         MOVE SATSNR(INDX2) TO W-IDARTNR                                  
800000         ADD +1             TO INDX2                                      
810000       END-IF                                                             
820000     END-PERFORM                                                          
830000     .                                                                    
840000     EJECT                                                                
850000 EDC-KOLLA-SAKN-PA-ARTREG-INPU SECTION.                                   
860000*********************************************************                 
870000* KOLL VID KOPIERING OCH IDARTNR(NYTT) SAKNAS PÅ ARTREG *                 
880000*********************************************************                 
890000                                                                          
900000     PERFORM S05-KOLL-BEART-KDHOM-KOPIERING                               
910000                                                                          
190000                                                                          
200000     IF (MID-IDSTRTYP-IN = ALL '+')                                       
210000       CONTINUE                                                           
220000     ELSE                                                                 
230000       IF (MID-IDSTRTYP-IN = 'S' OR 'R' OR 'K')                           
240000         MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDSTRTYP-IN-ATTR                
250000       ELSE                                                               
260000         MOVE NEJ                TO INDATA-SW                             
270000         MOVE MFS-ALFA-FAELT-FEL TO MOD-IDSTRTYP-IN-ATTR                  
280000       END-IF                                                             
290000     END-IF                                                               
300000     .                                                                    
310000     EJECT                                                                
320000 F-UPPDATERA SECTION.                                                     
330000                                                                          
340000     IF MID-INPUT NOT = ALL '+'                                           
350000       IF UPDATE-BORTTAG                                                  
360000         PERFORM FA-TABORT-SATSSTRUKTUR                                   
370000       ELSE                                                               
380000         IF UPDATE-AENDRA                                                 
390000           PERFORM FB-AENDRA-SATSSTRUKTUR                                 
400000         ELSE                                                             
410000           IF UPDATE-NYUPPL                                               
420000             PERFORM FC-NYUPPL-SATSSTRUKTUR                               
430000           ELSE                                                           
440000             PERFORM FD-KOPIERA-SATSSTRUKTUR                              
450000           END-IF                                                         
460000         END-IF                                                           
470000       END-IF                                                             
480000       PERFORM MFS-FORM-ATTR                                              
490000                                                                          
500000       IF UPDATE-NYUPPL-KOPIERING                                         
510000         IF STRUKTURNR-NY-FINNS                                           
520001           MOVE MED18(SPRAK-IX) TO MOD-TEMFSINF                           
530000         ELSE                                                             
540001           MOVE MED2(SPRAK-IX)  TO MOD-TEMFSINF                           
550000         END-IF                                                           
560000         INSPECT IDARTNR-NYUPPL-WS                                        
570000            REPLACING LEADING ZERO BY SPACE                               
590000       ELSE                                                               
600001         MOVE MED5(SPRAK-IX)    TO MOD-TEMFSINF                           
610000       END-IF                                                             
620000     END-IF                                                               
630000                                                                          
640000     .                                                                    
650000     EJECT                                                                
660000 FA-TABORT-SATSSTRUKTUR SECTION.                                          
670000**************************************************                        
680000* BÅDE 'OKONVERTERAD' OCH EVENTUELL KONVERTERAD  *                        
690000* STRUKTUR TAS BORT                              *                        
700000**************************************************                        
710000                                                                          
720000     MOVE SAKNAS TO STRUKTURNR-TYP                                        
730000                                                                          
740000     MOVE IDARTNR-WS TO W-IDARTNR                                         
750000     PERFORM IMS-GET-SATB01                                               
760000     IF SEGMENT-FINNS                                                     
770000       PERFORM IMS-DLET-SATB                                              
780000     END-IF                                                               
790000                                                                          
800000     MOVE IDARTNR-KONVERTERAT-WS TO W-IDARTNR                             
810000     PERFORM IMS-GET-SATB01                                               
820000     IF SEGMENT-FINNS                                                     
830000       PERFORM IMS-DLET-SATB                                              
840000     END-IF                                                               
850000                                                                          
880000     .                                                                    
890000     EJECT                                                                
900000 FB-AENDRA-SATSSTRUKTUR SECTION.                                          
910000                                                                          
920000     IF STRUKTURNR-FINNS-KONVERTERAT                                      
930000       MOVE IDARTNR-KONVERTERAT-WS TO W-IDARTNR                           
940000     ELSE                                                                 
950000       MOVE IDARTNR-WS TO W-IDARTNR                                       
960000     END-IF                                                               
970000                                                                          
980000     PERFORM IMS-GET-SATB01                                               
990000                                                                          
290000                                                                          
300000     IF MID-IDSTRTYP-IN = ALL '+'                                         
310000       CONTINUE                                                           
320000     ELSE                                                                 
330000       MOVE MID-IDSTRTYP-IN TO SATB01-STR-IDSTRTYP                        
340000     END-IF                                                               
350000                                                                          
470000                                                                          
480000     PERFORM IMS-REPL-SATB                                                
490000     .                                                                    
500000     EJECT                                                                
510000 FC-NYUPPL-SATSSTRUKTUR SECTION.                                          
520000                                                                          
530000     IF STRUKTURNR-FINNS-PAA-ARTREG                                       
540000                                                                          
550000       MOVE IDARTNR-NYUPPL-WS TO W-IDARTNR                                
560000       PERFORM IMS-GET-ARTC01                                             
570001       IF ART-IDLEVNR = '1002 '                                           
580001         MOVE ART-IDLEVNR TO W-IDLEVNR                                    
590001                             SATB01I-STR-IDLEVNR                          
600000       ELSE                                                               
610001         MOVE SPACE TO SATB01I-STR-IDLEVNR                                
620000       END-IF                                                             
630000                                                                          
640000       MOVE ZERO TO  SATB01I-STR-KDBENHOM                                 
650000                     SATB01I-STR-KDPRODSL                                 
660000                     SATB01I-STR-IDFKNGRP                                 
670000       MOVE SPACE TO SATB01I-STR-BEART-SVE                                
680000     ELSE                                                                 
690001       MOVE SPACE           TO SATB01I-STR-IDLEVNR                        
700000       MOVE BEART-SVE-SPAR  TO SATB01I-STR-BEART-SVE                      
720000       MOVE KDPRODSL-WS     TO SATB01I-STR-KDPRODSL                       
740000       MOVE IDFKNGRP-WS     TO SATB01I-STR-IDFKNGRP                       
741003       MOVE ZERO            TO SATB01I-STR-KDBENHOM                       
750000                                                                          
810000     END-IF                                                               
820000                                                                          
830000     MOVE IDARTNR-NYUPPL-WS TO SATB01I-STR-IDARTNR                        
840000     MOVE SPACE             TO SATB01I-STR-FLEXFORP                       
850000     MOVE 'N'               TO SATB01I-STR-FLFORPQ                        
860000     MOVE MID-IDSTRTYP-IN   TO SATB01I-STR-IDSTRTYP                       
870000     MOVE MSG-SIGNON-USERID TO SATB01I-STR-IDUSER                         
880000     MOVE DAGENS-DATUM      TO SATB01I-STR-TIREGDAT                       
890000     MOVE ZERO              TO SATB01I-STR-TIUPPDAT                       
900000                               SATB01I-STR-TIBORT                         
910000                               SATB01I-STR-KVBYGMIN                       
920000                                                                          
940001     MOVE SPACE             TO SATB01I-STR-IDTSPEC                        
980000                                                                          
100000                                                                          
110000     PERFORM IMS-ISRT-SATB01                                              
120000     MOVE FINNS TO STRUKTURNR-TYP                                         
130000     .                                                                    
140000     EJECT                                                                
150000 FD-KOPIERA-SATSSTRUKTUR SECTION.                                         
160000                                                                          
170000     MOVE IDARTNR-NYUPPL-WS TO W-IDARTNR                                  
180000     PERFORM IMS-GET-SATB01                                               
190000     IF SEGMENT-FINNS                                                     
200000****   STRUKTUR FINNS REDAN SOM ROT                                       
210000****   KOPIERA BEFINTLIGT STRUKTURHUVUD                                   
220000                                                                          
230000       MOVE IO-AREA                       TO IO-AREA2                     
240000       MOVE IDARTNR-NYUPPL-KONVERTERAT-WS TO SATB01I-STR-IDARTNR          
250000       MOVE SPACE                         TO SATB01I-STR-FLEXFORP         
260000       MOVE 'N'                           TO SATB01I-STR-FLFORPQ          
270000       MOVE MSG-SIGNON-USERID             TO SATB01I-STR-IDUSER           
280000       MOVE DAGENS-DATUM                  TO SATB01I-STR-TIREGDAT         
290000       MOVE ZERO                          TO SATB01I-STR-TIUPPDAT         
300000                                             SATB01I-STR-TIBORT           
301001                                             SATB01I-STR-KVBYGMIN         
310000                                                                          
320000       PERFORM IMS-ISRT-SATB01                                            
330000                                                                          
340000*****  LÄS IN ROT PÅ STRUKTUR SOM SKALL KOPIERAS                          
350000       MOVE IDARTNR-WS TO W-IDARTNR                                       
360000       PERFORM IMS-GET-SATB01                                             
370000     ELSE                                                                 
380000       MOVE IDARTNR-WS TO W-IDARTNR                                       
390000       PERFORM IMS-GET-SATB01                                             
400000                                                                          
410000************   KOPIERA STRUKTURHUVUD                                      
420000                                                                          
430000       MOVE IO-AREA                       TO IO-AREA2                     
440000       MOVE IDARTNR-NYUPPL-KONVERTERAT-WS TO SATB01I-STR-IDARTNR          
450000       MOVE SPACE                         TO SATB01I-STR-FLEXFORP         
460000       MOVE 'N'                           TO SATB01I-STR-FLFORPQ          
470000       MOVE MSG-SIGNON-USERID             TO SATB01I-STR-IDUSER           
480000       MOVE DAGENS-DATUM                  TO SATB01I-STR-TIREGDAT         
490000       MOVE ZERO                          TO SATB01I-STR-TIUPPDAT         
500000                                             SATB01I-STR-TIBORT           
510000                                             SATB01I-STR-KVBYGMIN         
520000                                                                          
530000       IF STRUKTURNR-NY-FINNS-PAA-ARTREG                                  
540000******   OM DET NYA STRUKTURNUMRET FINNS PÅ ARTREG                        
550000         PERFORM FDA-NOLLA-ARTUPPG                                        
560000         MOVE IDARTNR-NYUPPL-WS TO W-IDARTNR                              
570000         PERFORM IMS-GET-ARTC01                                           
580001         IF ART-IDLEVNR = '1002 '                                         
590001           MOVE ART-IDLEVNR TO SATB01I-STR-IDLEVNR                        
600000         ELSE                                                             
610001           MOVE SPACE TO SATB01I-STR-IDLEVNR                              
620000         END-IF                                                           
630000                                                                          
640000       ELSE                                                               
650000         IF STRUKTURNR-FINNS-PAA-ARTREG                                   
660000******   OM DET KOPIERADE STRUKTURNUMRET FINNS PÅ ARTREG                  
670000           MOVE IDARTNR-WS TO W-IDARTNR                                   
680000           PERFORM FDB-HAMTA-ARTUPPG                                      
690001           MOVE SPACE TO SATB01I-STR-IDLEVNR                              
700000                                                                          
950000         ELSE                                                             
970000                                                                          
980001           MOVE SPACE TO SATB01-STR-IDLEVNR                               
990000                                                                          
230000                                                                          
240000         END-IF                                                           
250000       END-IF                                                             
260000                                                                          
300001       MOVE SPACE TO SATB01I-STR-IDTSPEC                                  
320000                                                                          
330000       IF MID-IDSTRTYP-IN = ALL '+'                                       
340000         CONTINUE                                                         
350000       ELSE                                                               
360000         MOVE MID-IDSTRTYP-IN TO SATB01I-STR-IDSTRTYP                     
370000       END-IF                                                             
380000                                                                          
500000                                                                          
510000       PERFORM IMS-ISRT-SATB01                                            
520000     END-IF                                                               
530000                                                                          
540000                                                                          
550000************ KOPIERA STRUKTURRADER                                        
560000     MOVE IDARTNR-NYUPPL-KONVERTERAT-WS TO W-IDARTNR-I                    
570000                                                                          
580000     PERFORM IMS-GET-SATB11                                               
590000     IF SEGMENT-FINNS                                                     
600000       PERFORM UNTIL RADSEGMENT-SLUT                                      
610000                                                                          
611001         MOVE SATB11-RAD-TISTODAT  TO TMP1-YYMMDD                         
612001         MOVE DAGENS-DATUM         TO TMP2-YYMMDD                         
613001         PERFORM WY2000P1                                                 
620000         IF (SATB11-RAD-KDSTRRAD NOT = '0') OR                            
630000             (SATB11-RAD-KDISATS = ('E' OR 'U') ) OR                      
640001              (TMP1-YYMMDD < TMP2-YYMMDD)                                 
650000           CONTINUE                                                       
660000         ELSE                                                             
670000           PERFORM FDC-FLYTTA-RADINFO                                     
680000           PERFORM IMS-ISRT-SATB11                                        
690000                                                                          
700000************   KOPIERA STRUKTURNOTERINGAR                                 
710000                                                                          
720000           PERFORM IMS-GET-SATB22                                         
730000           IF SEGMENT-FINNS                                               
740000             PERFORM UNTIL NOTSEGMENT-SLUT                                
750000               PERFORM FDE-FLYTTA-NOTINFO                                 
760000               PERFORM IMS-ISRT-SATB22                                    
770000               PERFORM IMS-GET-SATB22                                     
780000               MOVE SATB-STATUS-CODE TO STATUS-NOT-WS                     
790000             END-PERFORM                                                  
800000           END-IF                                                         
810000         END-IF                                                           
820000                                                                          
830000         PERFORM IMS-GET-SATB11                                           
840000         MOVE SATB-STATUS-CODE TO STATUS-ART-WS                           
850000       END-PERFORM                                                        
860000                                                                          
870000     END-IF                                                               
880000                                                                          
890000**** SWITCHAR SÄTTS RÄTT FÖR ATT FUNKA VID G-LAES...                      
900000**** DEN NYA STRUKTUREN (KONVERTERADE) SKALL VISAS                        
910000     MOVE IDARTNR-NYUPPL-WS             TO IDARTNR-WS                     
920000     MOVE IDARTNR-NYUPPL-KONVERTERAT-WS TO IDARTNR-KONVERTERAT-WS         
930000**** DEN NYA STRUKTUREN FINNS                                             
940000     MOVE FINNS TO STRUKTURNR-TYP                                         
950000     MOVE NEJ   TO FINNS-OKONVERTERAT-SW                                  
960000     MOVE JA    TO FINNS-KONVERTERAT-SW                                   
970000     IF STRUKTURNR-NY-FINNS-PAA-ARTREG                                    
980000       MOVE FINNS TO STRUKTURNR-PAA-ARTREG                                
990000     ELSE                                                                 
000000       MOVE SAKNAS TO STRUKTURNR-PAA-ARTREG                               
010000     END-IF                                                               
020000     .                                                                    
030000     EJECT                                                                
040000 FDA-NOLLA-ARTUPPG SECTION.                                               
050000                                                                          
060000     MOVE SPACE TO SATB01I-STR-BEART-SVE                                  
070000     MOVE ZERO  TO SATB01I-STR-IDFKNGRP                                   
080000                   SATB01I-STR-KDBENHOM                                   
090000                   SATB01I-STR-KDPRODSL                                   
100000     .                                                                    
110000                                                                          
120000 FDB-HAMTA-ARTUPPG SECTION.                                               
130000                                                                          
140000     MOVE IDARTNR-WS TO W-IDARTNR                                         
150001     PERFORM IMS-GET-ARTC01                                               
160001     MOVE ART-IDFKNGRP    TO SATB01I-STR-IDFKNGRP                         
170001     MOVE ART-KDPRODSL    TO SATB01I-STR-KDPRODSL                         
180000     MOVE BEART-SVE-SPAR  TO SATB01I-STR-BEART-SVE                        
190000     MOVE KDBENHOM-SPAR   TO SATB01I-STR-KDBENHOM                         
200000     .                                                                    
210000     EJECT                                                                
220000 FDC-FLYTTA-RADINFO SECTION.                                              
230000                                                                          
240000     MOVE SATB11-RAD-KDSTRRAD  TO SATB11I-RAD-KDSTRRAD                    
250000                                  W-KDSTRRAD                              
260000     MOVE SATB11-RAD-IDRADNR   TO SATB11I-RAD-IDRADNR                     
270000                                  W-IDRADNR                               
280000     MOVE SATB11-RAD-IDLEVNR   TO SATB11I-RAD-IDLEVNR                     
290000     MOVE SATB11-RAD-BELEVART  TO SATB11I-RAD-BELEVART                    
300000     MOVE SATB11-RAD-IDARTNR   TO SATB11I-RAD-IDARTNR                     
310000     MOVE SATB11-RAD-BEART-SVE TO SATB11I-RAD-BEART-SVE                   
320000     MOVE SPACE                TO SATB11I-RAD-IDAO-STA                    
330000                                  SATB11I-RAD-IDAO-STO                    
340000     MOVE SATB11-RAD-IDSTRTYP  TO SATB11I-RAD-IDSTRTYP                    
350000     MOVE SATB11-RAD-KDBENHOM  TO SATB11I-RAD-KDBENHOM                    
360000     MOVE 'N'                  TO SATB11I-RAD-KDISATS                     
370000     MOVE SATB11-RAD-KDSORT    TO SATB11I-RAD-KDSORT                      
380000     MOVE SATB11-RAD-REANTPSA  TO SATB11I-RAD-REANTPSA                    
390000     MOVE DAGENS-DATUM         TO SATB11I-RAD-TIREGDAT                    
400000                                                                          
400101     MOVE SATB11-RAD-TISTADAT   TO TMP1-YYMMDD                            
400201     MOVE DAGENS-DATUM          TO TMP2-YYMMDD                            
401001     PERFORM WY2000P1                                                     
410001     IF TMP1-YYMMDD > TMP2-YYMMDD                                         
420000       MOVE SATB11-RAD-TISTADAT TO SATB11I-RAD-TISTADAT                   
430000     ELSE                                                                 
440000       MOVE DAGENS-DATUM      TO SATB11I-RAD-TISTADAT                     
450000     END-IF                                                               
460000                                                                          
470000     MOVE +999999            TO SATB11I-RAD-TISTODAT                      
480000                                                                          
490000     .                                                                    
500000                                                                          
510000 FDE-FLYTTA-NOTINFO SECTION.                                              
520000                                                                          
530000     MOVE SATB22-NOT-IDSTRNOT    TO SATB22I-NOT-IDSTRNOT                  
540000     MOVE SATB22-NOT-TESTRNOT(1) TO SATB22I-NOT-TESTRNOT(1)               
550000     MOVE SATB22-NOT-TESTRNOT(2) TO SATB22I-NOT-TESTRNOT(2)               
560000                                                                          
570000     .                                                                    
580000     EJECT                                                                
590000 G-LAES-VISA-STRUKTUR SECTION.                                            
600000                                                                          
610000     MOVE IDARTNR-WS TO W-IDARTNR                                         
620000     IF STRUKTURNR-SAKNAS                                                 
630000       IF STRUKTURNR-FINNS-PAA-ARTREG                                     
640000         PERFORM GA-VIS-INF-FRAN-ARTC-BENA-WDF5                           
650000       ELSE                                                               
670000         PERFORM MFS-RENSA-FAELT-UT                                       
690000       END-IF                                                             
700000     ELSE                                                                 
710000       IF STRUKTURNR-FINNS-PAA-ARTREG                                     
720000         PERFORM GB-INF-FRA-ARTC-BENA-WDF5-SATB                           
730000       ELSE                                                               
740000         PERFORM GC-VISA-ALL-INF-FRAN-SATB                                
750000       END-IF                                                             
760000     END-IF                                                               
770000                                                                          
790000                                                                          
800000     IF STRUKTURNR-FINNS                                                  
810000       PERFORM MFS-FORM-ATTR                                              
820000     ELSE                                                                 
830000       PERFORM MFS-FORM-ATTR                                              
840000       PERFORM S06-STAENG-EVENTUELLA-FAELT                                
850000     END-IF                                                               
860000                                                                          
870000     .                                                                    
880000     SKIP2                                                                
890000 GA-VIS-INF-FRAN-ARTC-BENA-WDF5 SECTION.                                  
900000                                                                          
910000     PERFORM MFS-RENSA-FAELT-UT                                           
920001     PERFORM S02-INF-FRA-ARTC-BEN-WDF5                                    
930000     .                                                                    
940000     SKIP2                                                                
950000 GB-INF-FRA-ARTC-BENA-WDF5-SATB SECTION.                                  
960000                                                                          
970001     PERFORM S02-INF-FRA-ARTC-BEN-WDF5                                    
980000                                                                          
990000     IF (STRUKTURNR-FINNS-KONVERTERAT) AND                                
000000         (NOT STRUKTURNR-SPAERRAT)                                        
010000****** OM STRUKTUNUMRET FINNS I KONVERTERAD FORM OCH ÄR                   
020000****** 'TILLÅTEN', VISAS INFO FRÅN DEN KOVERTERADE FORMEN                 
030000       MOVE IDARTNR-KONVERTERAT-WS TO W-IDARTNR                           
040000     ELSE                                                                 
050000****** VISAS INFO FRÅN DEN 'RIKTIGA' FORMEN                               
060000       MOVE IDARTNR-WS TO W-IDARTNR                                       
070000     END-IF                                                               
080000                                                                          
090000     PERFORM IMS-GET-SATB01                                               
100000     MOVE SATB01-STR-IDSTRTYP    TO MOD-IDSTRTYP-UT                       
250000     .                                                                    
260000     SKIP2                                                                
270000 GC-VISA-ALL-INF-FRAN-SATB SECTION.                                       
280000                                                                          
290000     IF (STRUKTURNR-FINNS-KONVERTERAT) AND                                
300000         (NOT STRUKTURNR-SPAERRAT)                                        
310000****** OM STRUKTUNUMRET FINNS I KONVERTERAD FORM OCH ÄR                   
320000****** 'TILLÅTEN', VISAS INFO FRÅN DEN KOVERTERADE FORMEN                 
330000       MOVE IDARTNR-KONVERTERAT-WS TO W-IDARTNR                           
340000     ELSE                                                                 
350000****** ANNARS VISAS INFO FRÅN DEN 'RIKTIGA' FORMEN                        
360000       MOVE IDARTNR-WS TO W-IDARTNR                                       
370000     END-IF                                                               
380000                                                                          
390000     PERFORM IMS-GET-SATB01                                               
430000     MOVE SATB01-STR-IDSTRTYP    TO MOD-IDSTRTYP-UT                       
490000                                                                          
910000     .                                                                    
920000     EJECT                                                                
930000 H-SAMMA-SIDA SECTION.                                                    
940000                                                                          
090000     IF MID-IDSTRTYP-IN = SPACE                                           
100000****** BEHANDLAS SOM EJ IFYLLD                                            
110000       MOVE '+' TO MID-IDSTRTYP-IN                                        
120000     END-IF                                                               
130000                                                                          
140000     IF MID-INPUT NOT = ALL '+'                                           
150001       MOVE MED6(SPRAK-IX) TO MOD-TEMFSFEL                                
170000       PERFORM MFS-LAES-IN-IGEN                                           
180000     END-IF                                                               
190000                                                                          
200000     PERFORM S06-STAENG-EVENTUELLA-FAELT                                  
210000     .                                                                    
220000     EJECT                                                                
230000 S01-KOLLA-FINNS-PA-ARTREG-INPU SECTION.                                  
240000                                                                          
520000                                                                          
530000     IF UPDATE-AENDRA                                                     
540000       PERFORM S011-KOLLA-IDSTRTYP-AENDRA                                 
550000     ELSE                                                                 
560000       IF UPDATE-NYUPPL                                                   
570000         PERFORM S012-KOLLA-IDSTRTYP-NYUPPL                               
580000       ELSE                                                               
590000********* KOPIERING                                                       
600000         PERFORM S013-KOLLA-IDSTRTYP-NYUPPL-KOP                           
610000       END-IF                                                             
620000     END-IF                                                               
630000     .                                                                    
640000     EJECT                                                                
650000 S011-KOLLA-IDSTRTYP-AENDRA SECTION.                                      
660000***********************************************                           
670000* EVENTUELL INMATAD STRUKTURTYP MÅSTE 'STÄMMA'*                           
680000* MED SORT PÅ ARTREG.                         *                           
690000***********************************************                           
700000                                                                          
710000     IF (MID-IDSTRTYP-IN = ALL '+')                                       
720000       CONTINUE                                                           
730000     ELSE                                                                 
740000       IF (MID-IDSTRTYP-IN = 'S')                                         
750000         MOVE IDARTNR-WS TO W-IDARTNR                                     
760001         PERFORM IMS-GET-ARTC01                                           
770001         IF ART-KDSORT = 'SA' OR 'TM'                                     
780000           MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDSTRTYP-IN-ATTR              
790000         ELSE                                                             
800000           MOVE NEJ                TO INDATA-SW                           
810000           MOVE MFS-ALFA-FAELT-FEL TO MOD-IDSTRTYP-IN-ATTR                
820000         END-IF                                                           
830000       ELSE                                                               
840000         IF (MID-IDSTRTYP-IN = 'R' OR 'K')                                
850000           MOVE IDARTNR-WS TO W-IDARTNR                                   
860001           PERFORM IMS-GET-ARTC01                                         
870001           IF ART-KDSORT = 'ST'                                           
880000             MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDSTRTYP-IN-ATTR            
890000           ELSE                                                           
900000             MOVE NEJ                TO INDATA-SW                         
910000             MOVE MFS-ALFA-FAELT-FEL TO MOD-IDSTRTYP-IN-ATTR              
920000           END-IF                                                         
930000         ELSE                                                             
940000           MOVE NEJ                TO INDATA-SW                           
950000           MOVE MFS-ALFA-FAELT-FEL TO MOD-IDSTRTYP-IN-ATTR                
960000         END-IF                                                           
970000       END-IF                                                             
980000     END-IF                                                               
990000     .                                                                    
000000     EJECT                                                                
010000 S012-KOLLA-IDSTRTYP-NYUPPL SECTION.                                      
020000***********************************************                           
030000* IMATAD STRUKTURTYP MÅSTE 'STÄMMA' MED SORT  *                           
040000* PÅ ARTREG.                                  *                           
050000***********************************************                           
060000                                                                          
070000     IF (MID-IDSTRTYP-IN = ALL '+')                                       
080000       MOVE NEJ                TO INDATA-SW                               
090000       MOVE MFS-ALFA-FAELT-FEL TO MOD-IDSTRTYP-IN-ATTR                    
100000     ELSE                                                                 
110000       IF (MID-IDSTRTYP-IN = 'S')                                         
120000         MOVE IDARTNR-WS TO W-IDARTNR                                     
130001         PERFORM IMS-GET-ARTC01                                           
140001         IF ART-KDSORT = 'SA' OR 'TM'                                     
150000           MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDSTRTYP-IN-ATTR              
160000         ELSE                                                             
170000           MOVE NEJ                TO INDATA-SW                           
180000           MOVE MFS-ALFA-FAELT-FEL TO MOD-IDSTRTYP-IN-ATTR                
190000         END-IF                                                           
200000       ELSE                                                               
210000         IF (MID-IDSTRTYP-IN = 'R' OR 'K')                                
220000           MOVE IDARTNR-WS TO W-IDARTNR                                   
230001           PERFORM IMS-GET-ARTC01                                         
240001           IF ART-KDSORT = 'ST'                                           
250000             MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDSTRTYP-IN-ATTR            
260000           ELSE                                                           
270000             MOVE NEJ                TO INDATA-SW                         
280000             MOVE MFS-ALFA-FAELT-FEL TO MOD-IDSTRTYP-IN-ATTR              
290000           END-IF                                                         
300000         ELSE                                                             
310000           MOVE NEJ                TO INDATA-SW                           
320000           MOVE MFS-ALFA-FAELT-FEL TO MOD-IDSTRTYP-IN-ATTR                
330000         END-IF                                                           
340000       END-IF                                                             
350000     END-IF                                                               
360000     .                                                                    
370000     EJECT                                                                
380000 S013-KOLLA-IDSTRTYP-NYUPPL-KOP SECTION.                                  
390000***********************************************                           
400000* IMATAD IDSTRTYP MÅSTE 'STÄMMA' MED SORT     *                           
410000* PÅ ARTREG. OM EJ INMATAD STRUKTURTYP MÅSTE  *                           
420000* DEN KOPIERADE STRUKTURENS IDSTRTYP 'STÄMMA' *                           
430000* MED SORT PÅ ARTREG.                         *                           
440000***********************************************                           
450000                                                                          
460000     IF (MID-IDSTRTYP-IN = ALL '+')                                       
470000*****  KOLLA KOPIERAD STRUKTURS IDSTRTYP                                  
480000       MOVE IDARTNR-WS TO W-IDARTNR                                       
490000       PERFORM IMS-GET-SATB01                                             
500000       IF SATB01-STR-IDSTRTYP = 'S'                                       
510000         MOVE IDARTNR-NYUPPL-WS TO W-IDARTNR                              
520001         PERFORM IMS-GET-ARTC01                                           
530001         IF ART-KDSORT = 'SA' OR 'TM'                                     
540000           MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDSTRTYP-IN-ATTR              
550000         ELSE                                                             
560000           MOVE NEJ                TO INDATA-SW                           
570000           MOVE MFS-ALFA-FAELT-FEL TO MOD-IDSTRTYP-IN-ATTR                
580000         END-IF                                                           
590000       ELSE                                                               
600000******* IDSTRTYP = R ELLER K                                              
610000         MOVE IDARTNR-NYUPPL-WS TO W-IDARTNR                              
620001         PERFORM IMS-GET-ARTC01                                           
630001         IF ART-KDSORT = 'ST'                                             
640000           MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDSTRTYP-IN-ATTR              
650000         ELSE                                                             
660000           MOVE NEJ                TO INDATA-SW                           
670000           MOVE MFS-ALFA-FAELT-FEL TO MOD-IDSTRTYP-IN-ATTR                
680000         END-IF                                                           
690000       END-IF                                                             
700000     ELSE                                                                 
710000*****  KOLLA INMATAD IDSTRTYP                                             
720000       MOVE IDARTNR-NYUPPL-WS TO W-IDARTNR                                
730000       IF (MID-IDSTRTYP-IN = 'S')                                         
740001         PERFORM IMS-GET-ARTC01                                           
750001         IF ART-KDSORT = 'SA' OR 'TM'                                     
760000           MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDSTRTYP-IN-ATTR              
770000         ELSE                                                             
780000           MOVE NEJ                TO INDATA-SW                           
790000           MOVE MFS-ALFA-FAELT-FEL TO MOD-IDSTRTYP-IN-ATTR                
800000         END-IF                                                           
810000       ELSE                                                               
820000         IF (MID-IDSTRTYP-IN = 'R' OR 'K')                                
830000           MOVE IDARTNR-NYUPPL-WS TO W-IDARTNR                            
840001           PERFORM IMS-GET-ARTC01                                         
850001           IF ART-KDSORT = 'ST'                                           
860000             MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDSTRTYP-IN-ATTR            
870000           ELSE                                                           
880000             MOVE NEJ                TO INDATA-SW                         
890000             MOVE MFS-ALFA-FAELT-FEL TO MOD-IDSTRTYP-IN-ATTR              
900000           END-IF                                                         
910000         ELSE                                                             
920000           MOVE NEJ                TO INDATA-SW                           
930000           MOVE MFS-ALFA-FAELT-FEL TO MOD-IDSTRTYP-IN-ATTR                
940000         END-IF                                                           
950000       END-IF                                                             
960000     END-IF                                                               
970000     .                                                                    
980000     EJECT                                                                
990001 S02-INF-FRA-ARTC-BEN-WDF5 SECTION.                                       
000000******************************************                                
010000*     HÄMTA UPPGIFTER FRÅN ARTC          *                                
020000******************************************                                
030000                                                                          
040000     PERFORM IMS-GET-ARTC01                                               
050001     MOVE ART-IDLEVNR  TO IDLEVNR-ART-WS                                  
119401                                                                          
119501******************************************                                
119601                                                                          
119701     PERFORM IMS-GET-ARTC11                                               
121000                                                                          
130000     MOVE +3 TO W-KDNOTTYP                                                
140001     PERFORM IMS-GET-ARTC25                                               
200000                                                                          
210001     MOVE +7 TO W-KDNOTTYP                                                
220001     PERFORM IMS-GET-ARTC25                                               
280001                                                                          
420000*     HÄMTA UPPGIFTER FRÅN BENA          *                                
430000******************************************                                
440000                                                                          
450000     PERFORM IMS-GET-BENA01-BSEQ                                          
460000     IF SEGMENT-FINNS                                                     
480000       MOVE IDSKYLT-WS           TO W-IDSKYLT                             
490000       PERFORM IMS-GET-BENA11-BSEQ                                        
550000     ELSE                                                                 
571001       CONTINUE                                                           
580000     END-IF                                                               
590000******************************************                                
600000*     HÄMTA UPPGIFTER FRÅN WDF5          *                                
610000******************************************                                
620000                                                                          
630000     PERFORM IMS-GET-WDF501                                               
640000     IF SEGMENT-FINNS                                                     
650001       IF IDLEVNR-ART-WS NOT = SPACE                                      
660001         MOVE IDLEVNR-ART-WS TO W-IDLEVNR                                 
680000*****    HÄMTA BENÄMNING MED HÖGST BENÄMNINGSNR                           
690000         PERFORM IMS-GET-WDF502-LAST                                      
750000       ELSE                                                               
760000*****    HÄMTA BENÄMNING MED HÖGST BENÄMNINGSNR + LEVNR                   
770000*****    FRÅN FÖRSTA PÅTRÄFFADE LEVERANTÖR                                
780000         PERFORM IMS-GET-WDF502-OKVAL                                     
790000         IF SEGMENT-FINNS                                                 
800000           MOVE XLEV-IDLEVNR TO W-IDLEVNR                                 
840000           PERFORM IMS-GET-WDF502-LAST                                    
890000         END-IF                                                           
900000       END-IF                                                             
910000     ELSE                                                                 
921001       CONTINUE                                                           
930000     END-IF                                                               
940000     .                                                                    
950000     EJECT                                                                
900000 S05-KOLL-BEART-KDHOM-KOPIERING SECTION.                                  
910000                                                                          
920000     IF STRUKTURNR-FINNS-PAA-ARTREG                                       
930000****** OM DET KOPIERADE STRUKTURNUMRET FINNS PÅ ARTREG                    
940000                                                                          
950000       MOVE IDSKYLT-WS TO W-IDSKYLT                                       
960000       MOVE IDARTNR-WS TO W-IDARTNR                                       
970000       PERFORM IMS-GET-BENA01-BSEQ                                        
980000       MOVE BENA01-BEN-KDHOMONYM TO KDBENHOM-WS                           
990000                                    KDBENHOM-SPAR                         
000000       MOVE 'S  ' TO W-IDSKYLT                                            
010000       PERFORM IMS-GET-BENA11-BSEQ                                        
020000       MOVE BENA11-TEXT-BEART TO W-BEART                                  
030000                                 BEART-SVE-SPAR                           
040000     ELSE                                                                 
050000****** OM DET KOPIERADE STRUKTURNUMRET SAKNAS PÅ ARTREG                   
060000                                                                          
070000       MOVE BEART-RASA-SPAR    TO W-BEART                                 
080000                                  BEART-SVE-SPAR                          
090000       MOVE KDBENHOM-RASA-SPAR TO KDBENHOM-WS                             
100000                                  KDBENHOM-SPAR                           
110000     END-IF                                                               
120000                                                                          
130000**** KONTROLL AV BEART + KDHOM VID ÄNDRING INNAN KOPIERING                
150000     .                                                                    
160000     EJECT                                                                
170000 S06-STAENG-EVENTUELLA-FAELT SECTION.                                     
180000                                                                          
190000     IF STRUKTURNR-SAKNAS                                                 
231001       CONTINUE                                                           
240000     ELSE                                                                 
250000       IF (STRUKTURNR-SPAERRAT OR STRUKTURNR-BORTTAGET)                   
260000         PERFORM MFS-STAENG-FAELT-IN                                      
270000       END-IF                                                             
280000     END-IF                                                               
290000     .                                                                    
300000     EJECT                                                                
310000 S07-KOLLA-INGAAR-I-ANNAN-SATS SECTION.                                   
320000*********************************************************                 
330000* KOLL ATT STRUKTURNR EJ INGÅR SOM RAD I ANNAN STRUKTUR *                 
340000*********************************************************                 
350000                                                                          
360001     MOVE SPACE TO W-IDLEVNR                                              
370001                   W-BELEVART                                             
380000                                                                          
390000     PERFORM IMS-GET-SATB-CSEQ-OKONV-UNIK                                 
400000     IF SEGMENT-FINNS                                                     
410000       PERFORM UNTIL SEGMENT-SAKNAS                                       
420000         IF SATB01C-STR-TIBORT > 0                                        
430000**********   STRUKTUR BORTTAGSMÄRKT                                       
440000           PERFORM IMS-GET-SATB-CSEQ-OKONV-NEXT                           
450000         ELSE                                                             
450101           MOVE SATB11C-RAD-TISTODAT   TO TMP1-YYMMDD                     
450201           MOVE DAGENS-DATUM           TO TMP2-YYMMDD                     
451001           PERFORM WY2000P1                                               
460001           IF TMP1-YYMMDD > TMP2-YYMMDD                                   
470000**********   STRUKTURNR FINNS SOM GÄLLANDE RAD I ANNAN STRUKTUR           
480000             MOVE 'GE' TO STATUS-WS                                       
490000             MOVE NEJ TO INDATA-SW                                        
500000           ELSE                                                           
510000             PERFORM IMS-GET-SATB-CSEQ-OKONV-NEXT                         
520000           END-IF                                                         
530000         END-IF                                                           
540000       END-PERFORM                                                        
550000     END-IF                                                               
560000                                                                          
570000     IF INDATA-OK                                                         
580000       PERFORM IMS-GET-SATB-CSEQ-KONV-UNIK                                
590000       PERFORM UNTIL SEGMENT-SAKNAS                                       
600000         IF SEGMENT-FINNS                                                 
610001           MOVE 001                  TO WORK-KDCALL                       
611001           MOVE WC-CDC-SE            TO WORK-IDDC                         
620001           MOVE SATB01C-STR-TIREGDAT TO WORK-TIAAMMDD-FOM                 
630001           MOVE DAGENS-DATUM         TO WORK-TIAAMMDD-TOM                 
640001           CALL WORKDAY USING WORK-KDCALL                                 
650001                              WORK-DATE-AREA                              
660001                              WORK-KDSVAR                                 
670001           IF (WORK-KVWORKD > 2) OR (WORK-KDSVAR-FEL)                     
680000***********  OM MAN TRÄFFAR PÅ EN KOVERTERAD STRUKTUR                     
690000***********  SOM ÄR ÄLDRE ÄN 2 IGNORERAS DEN.                             
700000***********  ARB-KDSVAR-FEL INNEBÄR ATT ANTALET DAGAR > 999.              
710000             PERFORM IMS-GET-SATB-CSEQ-KONV-NEXT                          
720000           ELSE                                                           
730000***********  STRUKTUR FINNS SOM RAD I GÄLLANDE KONV. STRUKTUR             
740000             MOVE 'GE' TO STATUS-WS                                       
750000             MOVE NEJ TO INDATA-SW                                        
760000           END-IF                                                         
770000         END-IF                                                           
780000       END-PERFORM                                                        
790000     END-IF                                                               
800000     .                                                                    
810000     EJECT                                                                
820000 S08-KOLLA-OM-USER-HAR-LAASNING SECTION.                                  
830000**************************************                                    
840000* KOLL OM IDUSER HAR LÅSNING PÅ WDR5 *                                    
850000**************************************                                    
860000                                                                          
870000     MOVE '1151'    TO W-IDHTYP                                           
880000     MOVE LOW-VALUE TO W-LOW-VALUE                                        
890000     PERFORM IMS-GET-XXAZ01                                               
900000                                                                          
910000     MOVE MSG-SIGNON-USERID TO W-IDUSER                                   
920000     PERFORM IMS-GET-XXAZ11-IDUSER                                        
930000     PERFORM UNTIL SEGMENT-SAKNAS                                         
940000       IF SEGMENT-FINNS                                                   
950000                                                                          
960001         MOVE 001                  TO WORK-KDCALL                         
961001         MOVE WC-CDC-SE            TO WORK-IDDC                           
970001         MOVE XXAZ11-1152-TIREGDAT TO WORK-TIAAMMDD-FOM                   
980001         MOVE DAGENS-DATUM         TO WORK-TIAAMMDD-TOM                   
990001         CALL WORKDAY USING WORK-KDCALL                                   
000001                            WORK-DATE-AREA                                
010001                            WORK-KDSVAR                                   
020001         IF (WORK-KVWORKD > 2) OR (WORK-KDSVAR-FEL)                       
030000********** OM MAN TRÄFFAR PÅ EN 'LÅSNING'                                 
040000********** SOM ÄR ÄLDRE ÄN 2 DAGAR TAS DEN BORT.                          
050000********** ARB-KDSVAR-FEL INNEBÄR ATT ANTALET DAGAR > 999.                
060000           PERFORM IMS-DLET-XXAZ11                                        
070000           PERFORM IMS-GET-XXAZ11-IDUSER                                  
080000         ELSE                                                             
090000           MOVE 'GE' TO STATUS-WS                                         
100000           MOVE NEJ  TO INDATA-SW                                         
110000         END-IF                                                           
120000       END-IF                                                             
130000     END-PERFORM                                                          
140000     .                                                                    
150000     EJECT                                                                
160000 S09-LYS-UPP-INMATADE-FAELT SECTION.                                      
170000                                                                          
590000                                                                          
600000     IF (MID-IDSTRTYP-IN = ALL '+' OR SPACE)                              
610000       CONTINUE                                                           
620000     ELSE                                                                 
630000       MOVE NEJ TO INDATA-SW                                              
640000       MOVE MFS-ALFA-FAELT-FEL TO MOD-IDSTRTYP-IN-ATTR                    
650000     END-IF                                                               
660000                                                                          
800000     .                                                                    
810000     EJECT                                                                
820000 MFS-LAES-IN-IGEN SECTION.                                                
830000                                                                          
310000                                                                          
320000     IF (MID-IDSTRTYP-IN = ALL '+')                                       
330000       CONTINUE                                                           
340000     ELSE                                                                 
350000       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDSTRTYP-IN-ATTR                 
360000     END-IF                                                               
370000                                                                          
490000                                                                          
500000     .                                                                    
510000     EJECT                                                                
520000 MFS-RENSA-FAELT-UT SECTION.                                              
530000                                                                          
540001     MOVE MFS-RENSA-FAELT TO                                              
590000                             MOD-IDSTRTYP-UT                              
700000     .                                                                    
710000     SKIP2                                                                
910000 MFS-ROER-EJ-FAELT-UT  SECTION.                                           
920001     MOVE MFS-ROER-EJ-FAELT TO                                            
970000                               MOD-IDSTRTYP-UT                            
070000     .                                                                    
080000     SKIP2                                                                
280000     EJECT                                                                
290000 MFS-STAENG-FAELT-IN  SECTION.                                            
300001     MOVE MFS-STAENG-FAELT-NOMOD TO                                       
360000                                    MOD-IDSTRTYP-IN-ATTR                  
400000     .                                                                    
410000     SKIP2                                                                
420000 MFS-FORM-ATTR SECTION.                                                   
430000                                                                          
440001     MOVE MFS-FORMATETS-ATTR TO                                           
500000                                MOD-IDSTRTYP-IN-ATTR                      
540000     .                                                                    
550000     EJECT                                                                
560000* IMS SEKTIONER                                                           
570000     SKIP3                                                                
580000 IMS-GET-MSG SECTION.                                                     
590000                                                                          
600000     MOVE '  QC' TO GODK-STATUSKODER                                      
610000     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
620000     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
630000     PERFORM IMS-STATUSKONTROLL                                           
640000     .                                                                    
650000     SKIP3                                                                
660000 IMS-INSERT-MSG SECTION.                                                  
670000                                                                          
710000     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
720000     MOVE SPACE TO GODK-STATUSKODER                                       
730000     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
740000     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
750000     PERFORM IMS-STATUSKONTROLL                                           
760000     .                                                                    
770000     EJECT                                                                
880000 IMS-GET-ARTC01 SECTION.                                                  
890000                                                                          
900000     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
910000          DELIMITED BY SIZE INTO SSA1                                     
920000     MOVE '  GE' TO GODK-STATUSKODER                                      
930000     CALL CBLTDLI USING GU ARTC-PCB DLI-IO-AREA SSA1                      
940000     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
950000     PERFORM IMS-STATUSKONTROLL                                           
960000     .                                                                    
970000     SKIP3                                                                
980001 IMS-GET-ARTC25 SECTION.                                                  
990000                                                                          
000001     MOVE 'WLARTC11 ' TO SSA1                                             
010001     STRING 'WLARTC25(KDNOTTYP =' W-KDNOTTYP-X ')'                        
020001          DELIMITED BY SIZE INTO SSA2                                     
030000     MOVE '  GE' TO GODK-STATUSKODER                                      
040001     CALL CBLTDLI USING GNP ARTC-PCB DLI-IO-AREA SSA1 SSA2                
050000     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
060000     PERFORM IMS-STATUSKONTROLL                                           
070000     .                                                                    
080000     SKIP3                                                                
090001 IMS-GET-ARTC11 SECTION.                                                  
100000                                                                          
110001     MOVE 'WLARTC11 ' TO SSA1                                             
140000     MOVE '  GE' TO GODK-STATUSKODER                                      
150001     CALL CBLTDLI USING GNP ARTC-PCB DLI-IO-AREA SSA1                     
160000     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
170000     PERFORM IMS-STATUSKONTROLL                                           
180000     .                                                                    
190000     EJECT                                                                
200000 IMS-GET-BENA01-ASEQ SECTION.                                             
210000                                                                          
220000     STRING 'WLBENA01(WDD3ASEQ =' W-IDSKYLT-X                             
230000                                  W-BEART-X ')'                           
240000          DELIMITED BY SIZE INTO SSA1                                     
250000     MOVE '  GE' TO GODK-STATUSKODER                                      
260000     CALL CBLTDLI USING GU BENA-A-PCB DLI-IO-AREA SSA1                    
270000     MOVE BENA-A-STATUS-CODE TO STATUS-WS                                 
280000     PERFORM IMS-STATUSKONTROLL                                           
290000     .                                                                    
300000     SKIP3                                                                
310000 IMS-GET-BENA01-ASEQ-NEXT SECTION.                                        
320000                                                                          
330000     STRING 'WLBENA01(WDD3ASEQ =' W-IDSKYLT-X                             
340000                                  W-BEART-X ')'                           
350000          DELIMITED BY SIZE INTO SSA1                                     
360000     MOVE '  GE' TO GODK-STATUSKODER                                      
370000     CALL CBLTDLI USING GN BENA-A-PCB DLI-IO-AREA SSA1                    
380000     MOVE BENA-A-STATUS-CODE TO STATUS-WS                                 
390000     PERFORM IMS-STATUSKONTROLL                                           
400000     .                                                                    
410000     SKIP3                                                                
420000 IMS-GET-BENA11-ASEQ SECTION.                                             
430000                                                                          
440000     STRING 'WLBENA11(IDSKYLT  =' W-IDSKYLT-X ')'                         
450000          DELIMITED BY SIZE INTO SSA1                                     
460000     MOVE '  GE' TO GODK-STATUSKODER                                      
470000     CALL CBLTDLI USING GNP BENA-A-PCB DLI-IO-AREA SSA1                   
480000     MOVE BENA-A-STATUS-CODE TO STATUS-WS                                 
490000     PERFORM IMS-STATUSKONTROLL                                           
500000     .                                                                    
510000     EJECT                                                                
520000 IMS-GET-BENA01-BSEQ SECTION.                                             
530000                                                                          
540000     STRING 'WLBENA01(WDD3BSEQ =' W-IDARTNR-X ')'                         
550000          DELIMITED BY SIZE INTO SSA1                                     
560000     MOVE '  GE' TO GODK-STATUSKODER                                      
570000     CALL CBLTDLI USING GU BENA-B-PCB DLI-IO-AREA SSA1                    
580000     MOVE BENA-B-STATUS-CODE TO STATUS-WS                                 
590000     PERFORM IMS-STATUSKONTROLL                                           
600000     .                                                                    
610000     SKIP3                                                                
620000 IMS-GET-BENA11-BSEQ SECTION.                                             
630000                                                                          
640000     STRING 'WLBENA11(IDSKYLT  =' W-IDSKYLT-X ')'                         
650000          DELIMITED BY SIZE INTO SSA1                                     
660000     MOVE '  GE' TO GODK-STATUSKODER                                      
670000     CALL CBLTDLI USING GNP BENA-B-PCB DLI-IO-AREA SSA1                   
680000     MOVE BENA-B-STATUS-CODE TO STATUS-WS                                 
690000     PERFORM IMS-STATUSKONTROLL                                           
700000     .                                                                    
710000     EJECT                                                                
720000 IMS-GET-WDF501 SECTION.                                                  
730000                                                                          
740000     STRING 'WDF501  (IDARTNR  =' W-IDARTNR-X ')'                         
750000          DELIMITED BY SIZE INTO SSA1                                     
760000     MOVE '  GE' TO GODK-STATUSKODER                                      
770000     CALL CBLTDLI USING GU WDF5-PCB DLI-IO-AREA SSA1                      
780000     MOVE WDF5-STATUS-CODE TO STATUS-WS                                   
790000     PERFORM IMS-STATUSKONTROLL                                           
800000     .                                                                    
810000     SKIP3                                                                
820000 IMS-GET-WDF502-LAST SECTION.                                             
830000                                                                          
840000     STRING 'WDF502  *L(IDLEVNR  =' W-IDLEVNR-X ')'                       
850000          DELIMITED BY SIZE INTO SSA1                                     
860000     MOVE '  GE' TO GODK-STATUSKODER                                      
870000     CALL CBLTDLI USING GNP WDF5-PCB DLI-IO-AREA SSA1                     
880000     MOVE WDF5-STATUS-CODE TO STATUS-WS                                   
890000     PERFORM IMS-STATUSKONTROLL                                           
900000     .                                                                    
910000     SKIP3                                                                
920000 IMS-GET-WDF502-OKVAL SECTION.                                            
930000                                                                          
940000     MOVE 'WDF502  ' TO SSA1                                              
950000     MOVE '  GE' TO GODK-STATUSKODER                                      
960000     CALL CBLTDLI USING GNP WDF5-PCB DLI-IO-AREA SSA1                     
970000     MOVE WDF5-STATUS-CODE TO STATUS-WS                                   
980000     PERFORM IMS-STATUSKONTROLL                                           
990000     .                                                                    
000000     EJECT                                                                
010000 IMS-DLET-SATB SECTION.                                                   
020000                                                                          
030000     MOVE '  ' TO GODK-STATUSKODER                                        
040000     CALL CBLTDLI USING DLET SATB-PCB DLI-IO-AREA                         
050000     MOVE SATB-STATUS-CODE TO STATUS-WS                                   
060000     PERFORM IMS-STATUSKONTROLL                                           
070000     .                                                                    
080000     SKIP3                                                                
090000 IMS-GET-SATB01 SECTION.                                                  
100000                                                                          
110000     STRING 'WLSATB01(IDARTNR  =' W-IDARTNR-X ')'                         
120000          DELIMITED BY SIZE INTO SSA1                                     
130000     MOVE '  GE' TO GODK-STATUSKODER                                      
140000     CALL CBLTDLI USING GHU SATB-PCB DLI-IO-AREA SSA1                     
150000     MOVE SATB-STATUS-CODE TO STATUS-WS                                   
160000     PERFORM IMS-STATUSKONTROLL                                           
170000     .                                                                    
180000     SKIP3                                                                
190000 IMS-GET-SATB11 SECTION.                                                  
200000                                                                          
210000     MOVE 'WLSATB11 ' TO SSA1                                             
220000     MOVE '  GE' TO GODK-STATUSKODER                                      
230000     CALL CBLTDLI USING GNP SATB-PCB DLI-IO-AREA SSA1                     
240000     MOVE SATB-STATUS-CODE TO STATUS-WS                                   
250000     PERFORM IMS-STATUSKONTROLL                                           
260000     .                                                                    
270000     SKIP3                                                                
280000 IMS-GET-SATB22 SECTION.                                                  
290000                                                                          
300000     STRING 'WLSATB11(WDJ111KY =' W-KDSTRRAD-X                            
310000                                  W-IDRADNR-X ')'                         
320000          DELIMITED BY SIZE INTO SSA1                                     
330000     MOVE 'WLSATB22 ' TO SSA2                                             
340000     MOVE '  GE' TO GODK-STATUSKODER                                      
350000     CALL CBLTDLI USING GNP SATB-PCB DLI-IO-AREA SSA1 SSA2                
360000     MOVE SATB-STATUS-CODE TO STATUS-WS                                   
370000     PERFORM IMS-STATUSKONTROLL                                           
380000     .                                                                    
390000     EJECT                                                                
400000 IMS-ISRT-SATB01 SECTION.                                                 
410000                                                                          
420000     MOVE 'WLSATB01' TO SSA1                                              
430000     MOVE '  ' TO GODK-STATUSKODER                                        
440000     CALL CBLTDLI USING ISRT SATB-I-PCB DLI-IO-AREA2 SSA1                 
450000     MOVE SATB-I-STATUS-CODE TO STATUS-WS                                 
460000     PERFORM IMS-STATUSKONTROLL                                           
470000     .                                                                    
480000     SKIP3                                                                
490000 IMS-ISRT-SATB11 SECTION.                                                 
500000                                                                          
510000     STRING 'WLSATB01(IDARTNR  =' W-IDARTNR-I-X ')'                       
520000          DELIMITED BY SIZE INTO SSA1                                     
530000     MOVE 'WLSATB11 ' TO SSA2                                             
540000     MOVE '  ' TO GODK-STATUSKODER                                        
550000     CALL CBLTDLI USING ISRT SATB-I-PCB DLI-IO-AREA2 SSA1                 
560000                                                     SSA2                 
570000     MOVE SATB-I-STATUS-CODE TO STATUS-WS                                 
580000     PERFORM IMS-STATUSKONTROLL                                           
590000     .                                                                    
600000     SKIP3                                                                
610000 IMS-ISRT-SATB22 SECTION.                                                 
620000                                                                          
630000     STRING 'WLSATB01(IDARTNR  =' W-IDARTNR-I-X ')'                       
640000          DELIMITED BY SIZE INTO SSA1                                     
650000     STRING 'WLSATB11(WDJ111KY =' W-KDSTRRAD-X                            
660000                                  W-IDRADNR-X ')'                         
670000          DELIMITED BY SIZE INTO SSA2                                     
680000     MOVE 'WLSATB22 ' TO SSA3                                             
690000     MOVE '  ' TO GODK-STATUSKODER                                        
700000     CALL CBLTDLI USING ISRT SATB-I-PCB DLI-IO-AREA2 SSA1                 
710000                                                     SSA2                 
720000                                                     SSA3                 
730000     MOVE SATB-I-STATUS-CODE TO STATUS-WS                                 
740000     PERFORM IMS-STATUSKONTROLL                                           
750000     .                                                                    
760000     EJECT                                                                
770000 IMS-REPL-SATB SECTION.                                                   
780000                                                                          
790000     MOVE '  ' TO GODK-STATUSKODER                                        
800000     CALL CBLTDLI USING REPL SATB-PCB DLI-IO-AREA                         
810000     MOVE SATB-STATUS-CODE TO STATUS-WS                                   
820000     PERFORM IMS-STATUSKONTROLL                                           
830000     .                                                                    
840000     EJECT                                                                
850000 IMS-GET-SATB-CSEQ-OKONV-UNIK SECTION.                                    
860000* OBS! ENDAST EJ KONVERTERADE STRUKTURER 'TAS IN'                         
870000                                                                          
880000     STRING 'WLSATB11*D(WDJ1CSEQ =' W-IDLEVNR-X                           
890000                                    W-BELEVART-X                          
900000                                    W-IDARTNR-X ')'                       
910000          DELIMITED BY SIZE INTO SSA1                                     
920000     STRING 'WLSATB01(IDARTNR >=' W-IDARTNR-MIN-X                         
930000                    '&IDARTNR <=' W-IDARTNR-MAX-X ')'                     
940000          DELIMITED BY SIZE INTO SSA2                                     
950000     MOVE '  GE' TO GODK-STATUSKODER                                      
960000     CALL CBLTDLI USING GU SATB-C-PCB DLI-IO-AREA SSA1 SSA2               
970000     MOVE SATB-C-STATUS-CODE TO STATUS-WS                                 
980000     PERFORM IMS-STATUSKONTROLL                                           
990000     .                                                                    
000000     SKIP3                                                                
010000 IMS-GET-SATB-CSEQ-OKONV-NEXT SECTION.                                    
020000* OBS! ENDAST EJ KONVERTERADE STRUKTURER 'TAS IN'                         
030000                                                                          
040000     STRING 'WLSATB11*D(WDJ1CSEQ =' W-IDLEVNR-X                           
050000                                    W-BELEVART-X                          
060000                                    W-IDARTNR-X ')'                       
070000          DELIMITED BY SIZE INTO SSA1                                     
080000     STRING 'WLSATB01(IDARTNR >=' W-IDARTNR-MIN-X                         
090000                    '&IDARTNR <=' W-IDARTNR-MAX-X ')'                     
100000          DELIMITED BY SIZE INTO SSA2                                     
110000     MOVE '  GE' TO GODK-STATUSKODER                                      
120000     CALL CBLTDLI USING GN SATB-C-PCB DLI-IO-AREA SSA1 SSA2               
130000     MOVE SATB-C-STATUS-CODE TO STATUS-WS                                 
140000     PERFORM IMS-STATUSKONTROLL                                           
150000     .                                                                    
160000     SKIP3                                                                
170000 IMS-GET-SATB-CSEQ-KONV-UNIK SECTION.                                     
180000* OBS! ENDAST KONVERTERADE STRUKTURER 'TAS IN'                            
190000                                                                          
200000     STRING 'WLSATB11*D(WDJ1CSEQ =' W-IDLEVNR-X                           
210000                                    W-BELEVART-X                          
220000                                    W-IDARTNR-X ')'                       
230000          DELIMITED BY SIZE INTO SSA1                                     
240000     STRING 'WLSATB01(IDARTNR >=' W-IDARTNR-KONV-MIN-X                    
250000                    '&IDARTNR <=' W-IDARTNR-KONV-MAX-X ')'                
260000          DELIMITED BY SIZE INTO SSA2                                     
270000     MOVE '  GE' TO GODK-STATUSKODER                                      
280000     CALL CBLTDLI USING GHU SATB-C-PCB DLI-IO-AREA SSA1 SSA2              
290000     MOVE SATB-C-STATUS-CODE TO STATUS-WS                                 
300000     PERFORM IMS-STATUSKONTROLL                                           
310000     .                                                                    
320000     SKIP3                                                                
330000 IMS-GET-SATB-CSEQ-KONV-NEXT SECTION.                                     
340000* OBS! ENDAST KONVERTERADE STRUKTURER 'TAS IN'                            
350000                                                                          
360000     STRING 'WLSATB11*D(WDJ1CSEQ =' W-IDLEVNR-X                           
370000                                    W-BELEVART-X                          
380000                                    W-IDARTNR-X ')'                       
390000          DELIMITED BY SIZE INTO SSA1                                     
400000     STRING 'WLSATB01(IDARTNR >=' W-IDARTNR-KONV-MIN-X                    
410000                    '&IDARTNR <=' W-IDARTNR-KONV-MAX-X ')'                
420000          DELIMITED BY SIZE INTO SSA2                                     
430000     MOVE '  GE' TO GODK-STATUSKODER                                      
440000     CALL CBLTDLI USING GHN SATB-C-PCB DLI-IO-AREA SSA1 SSA2              
450000     MOVE SATB-C-STATUS-CODE TO STATUS-WS                                 
460000     PERFORM IMS-STATUSKONTROLL                                           
470000     .                                                                    
480000     EJECT                                                                
490000 IMS-GET-SATB01-DSEQ SECTION.                                             
500000* OBS! ENDAST KONVERTERADE STRUKTURER 'TAS IN'                            
510000                                                                          
520000     STRING 'WLSATB01(WDJ1DSEQ>=' W-IDUSER-X                              
530000                                  W-IDARTNR-KONV-MIN-X                    
540000                    '&WDJ1DSEQ<=' W-IDUSER-X                              
550000                                  W-IDARTNR-KONV-MAX-X ')'                
560000          DELIMITED BY SIZE INTO SSA1                                     
570000     MOVE '  GE' TO GODK-STATUSKODER                                      
580000     CALL CBLTDLI USING GHN SATB-D-PCB DLI-IO-AREA SSA1                   
590000     MOVE SATB-D-STATUS-CODE TO STATUS-WS                                 
600000     PERFORM IMS-STATUSKONTROLL                                           
610000     .                                                                    
620000     EJECT                                                                
630000 IMS-GET-XXAZ01 SECTION.                                                  
640000                                                                          
650000     STRING 'WLXXAZ01(WDGXKEY  =' W-WDGXKEY-X ')'                         
660000          DELIMITED BY SIZE INTO SSA1                                     
670000     MOVE '    ' TO GODK-STATUSKODER                                      
680000     CALL CBLTDLI USING GU XXAZ-PCB DLI-IO-AREA SSA1                      
690000     MOVE XXAZ-STATUS-CODE TO STATUS-WS                                   
700000     PERFORM IMS-STATUSKONTROLL                                           
710000     .                                                                    
720000     SKIP3                                                                
730000 IMS-DLET-XXAZ11 SECTION.                                                 
740000                                                                          
750000     MOVE '  ' TO GODK-STATUSKODER                                        
760000     CALL CBLTDLI USING DLET XXAZ-PCB DLI-IO-AREA                         
770000     MOVE XXAZ-STATUS-CODE TO STATUS-WS                                   
780000     PERFORM IMS-STATUSKONTROLL                                           
790000     .                                                                    
800000     SKIP3                                                                
810000 IMS-GET-XXAZ11-IDUSER SECTION.                                           
820000                                                                          
830000     STRING 'WLXXAZ11(IDUSER   =' W-IDUSER-X ')'                          
840000          DELIMITED BY SIZE INTO SSA1                                     
850000     MOVE '  GE' TO GODK-STATUSKODER                                      
860000     CALL CBLTDLI USING GHNP XXAZ-PCB DLI-IO-AREA SSA1                    
870000     MOVE XXAZ-STATUS-CODE TO STATUS-WS                                   
880000     PERFORM IMS-STATUSKONTROLL                                           
890000     .                                                                    
900000     EJECT                                                                
910000 IMS-STATUSKONTROLL SECTION.                                              
920000                                                                          
930000     SET STATUS-IX TO 1                                                   
940000     SEARCH GODK-STATUS                                                   
950000       AT END CALL FELLOG                                                 
960000       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
970000     END-SEARCH                                                           
980000     .                                                                    
990000     EJECT                                                                
991001     EJECT                                                                
000001*    -COPY WY2000P1                                                       
