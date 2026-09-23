000100 ID DIVISION.                                                             
000200*                                                                         
000300 PROGRAM-ID.             W2216D00.                                        
000400 AUTHOR.                 JOHAN NIHLBLAD                                   
000500 DATE-WRITTEN.           17/11/2011.                                      
000600                                                                          
000700     REMARKS.                                                             
000800*                                                                         
000900*                                                                         
001000*    FUNKTION:                                                            
001100*                                                                         
001200*            DETTA ÄR EN MODIFIERAD KOPIA PÅ PGM W2216200.                
001300*                                                                         
001400*            INFILEN (W2216C), SOM INNEHÅLLER ARTIKLAR SOM                
001500*            SKA SÄNDAS TILL LEVERANTÖR, SORTERAS M.H.A                   
001600*            STD.SORT PÅ LEVNR, GK ,GATE (ADINPORT) OCH ARTNR.            
001700*                                                                         
001800*            GK ÄR 1                                                      
001900*            ALLA ARTIKLARS AVROP LÄSES PÅ WDD9.                          
002000*            INFORMATION HÄMTAS ÄVEN FRÅN WDF1 OCH WDL6 OCH WDK7.         
002100*                                                                         
002200*            UTFILEN BESTÅR AV ETT ANTAL OLIKA DELINS-SEGMENT.            
002300*            BRYTNING FÖR NY FIL SKER FÖR LEVERANTÖR OCH DC               
002400*                                                                         
002500*    ÄNDRINGAR:                                                           
002600*    2012-08-29 E-TRACKER 10143273 LOCAL SOURCING CHINA                   
002700*                                                                         
002800*    2014-11-12 E-TRACKER 10244262 FLYTTA CROSS.INFO PÅ EDI (ARD)         
002900*               FLYTTAT BELEVART FRÅN POS 62 TILL POS 48.                 
003000*               BELEVART14 TILLAGT I POS 48 ARD-POST (E- SECTION)         
003100*               BELEVART35 BYTT TILL SPACE I POS 62 ARD-POST.             
003200*                                                                         
003300*    2015-06-05 E-TRACKER 10209749 TAG BORT EXTRALEVERANSER               
003400*                                  SEGMENT WDD903.                        
003500*                                                                         
003510*                                                                         
003520*    2016-10-17 E-TRACKER 10261999 (10282597 IHOPSLAGEN)                  
003530*               EDI: REMOVE STATUS 3,ONLY HAVE 1 & 4                      
003540*               EDI: CORRECTION DELFOR-PDN-SEGMENT                        
003550*                                                                         
003600*                                                                         
003700*    SUBPROGRAM:                                                          
003800*            WZ20DAYS                                                     
003900*            CBLTDLI                                                      
004000*                                                                         
004100     EJECT                                                                
004200 ENVIRONMENT DIVISION.                                                    
004300     SKIP2                                                                
004400 INPUT-OUTPUT SECTION.                                                    
004500                                                                          
004600 FILE-CONTROL.                                                            
004700     SKIP2                                                                
004800*- - - - - - - - - - - - - - INFILER:                                     
004900                                                                          
005000     SELECT  W2216C                   ASSIGN TO W2216DD1.                 
005100     SKIP2                                                                
005200*- - - - - - - - - - - - - - UTFILER:                                     
005300                                                                          
005400     SELECT  W2216D                   ASSIGN TO W2216DD2.                 
005500     EJECT                                                                
005600 DATA DIVISION.                                                           
005700     SKIP2                                                                
005800 FILE SECTION.                                                            
005900     SKIP3                                                                
006000 FD  W2216C                                                               
006100     LABEL RECORD STANDARD                                                
006200     RECORDING      F                                                     
006300     BLOCK CONTAINS 0.                                                    
006400     SKIP2                                                                
006500*01  -COPY W2216C                           -L.                           
006600     SKIP3                                                                
006700 FD  W2216D                                                               
006800     LABEL RECORD STANDARD                                                
006900     RECORDING   V                                                        
007000     BLOCK CONTAINS 0.                                                    
007100     SKIP2                                                                
007200 01  W22162-POST                 PIC X(1005).                             
007300*01  HEAD  -COPY W221001                      -L.                         
007400*01  UNB   -COPY W221UNB                      -L.                         
007500*01  UNH   -COPY W221UNH                      -L.                         
007600*01  MID   -COPY W221MID                      -L.                         
007700*01  SDT   -COPY W221SDT                      -L.                         
007800*01  BDT   -COPY W221BDT                      -L.                         
007900*01  ARI   -COPY W221ARI                      -L.                         
008000*01  CSG   -COPY W221CSG                      -L.                         
008100*01  ARD   -COPY W221ARD                      -L.                         
008200*01  PDI   -COPY W221PDI                      -L.                         
008300*01  SAD   -COPY W221SAD                      -L.                         
008400*01  DST   -COPY W221DST                      -L.                         
008500*01  PDN   -COPY W221PDN                      -L.                         
008600*01  DEL   -COPY W221DEL                      -L.                         
008700*01  TRAIL -COPY W221003                      -L.                         
008800     EJECT                                                                
008900 WORKING-STORAGE SECTION.                                                 
009000     SKIP2                                                                
009100*    -COPY WY2000W2                                                       
009200     SKIP3                                                                
009300*    -COPY WY2000W1                                                       
009400     SKIP3                                                                
009500*- - - - - - - - - - - - - -  GENERERAT PROGRAM-NAMN                      
009600 77   PROGRAM-NAMN           VALUE 'W2216D00'                             
009700                                 PIC X(8).                                
009800     SKIP2                                                                
009900*- - - - - - - - - - - - - -  GENERELLA KONSTANTER                        
010000                                                                          
010100 77  JA                          PIC X       VALUE 'J'.                   
010200 77  NEJ                         PIC X       VALUE 'N'.                   
010300 77  CURRENT-SECTION             PIC X(30)   VALUE SPACE.                 
010400 77  DBS-SECTION                 PIC X(30)   VALUE SPACE.                 
010500                                                                          
010600     SKIP2                                                                
010700*- - - - - - - - - - - - - -  END-OF-FILE SWITCHAR                        
010800 77  INFIL-EOF                   PIC X       VALUE 'N'.                   
010900     SKIP2                                                                
011000*- - - - - - - - - - - - - -  INDEX                                       
011100                                                                          
011200 77  IX                          PIC S9(3)   VALUE +0 COMP SYNC.          
011300 77  IX-DAG                      PIC S9(3)   VALUE +0 COMP SYNC.          
011310 77  IX-FTAG                     PIC S9(3)   VALUE +0 COMP SYNC.          
011320 77  IX-FTAG-MAX                 PIC S9(3)   VALUE +10 COMP SYNC.         
011400     SKIP2                                                                
011500*- - - - - - - - - - - - - -  LEV SOM VILL HA SENASTE INLEV.              
011600*                             I OMVÄND ORDNING, DVS ÄLDST FÖRST           
011601*                                                                         
011610*-DC71 ....SHANGHAI                                                       
011620*-DC72 ....BEIJING                                                        
011630*-DC73 ....GUANGZHO                                                       
011640*-DC74 ....CHENGDU                                                        
011700*- - - - - - - - - - - - - -  MOTTAGNINGSADRESSER                         
011800 01  FILLER                      PIC X(8)    VALUE 'ADRESSER'.            
011801 01  ADRESSER.                                                            
011807   03  WS-BEFTAG-71              PIC X(37)   VALUE                        
011808       '71VOLVO CAR DISTR.(SHANGHAI) CO LTD  '.                           
011809   03  WS-BEFTAG-72              PIC X(37)   VALUE                        
011811       '72VOLVO CAR DISTR.(SHANGHAI) CO LTD  '.                           
011812   03  WS-BEFTAG-73              PIC X(37)   VALUE                        
011814       '73VOLVO CAR DISTR.(SHANGHAI) CO LTD  '.                           
011815   03  WS-BEFTAG-74              PIC X(37)   VALUE                        
011817       '74VOLVO CAR DISTR.(SHANGHAI) CO LTD  '.                           
011818   03  WS-BEFTAG-41              PIC X(37)   VALUE                        
011819       '41VOLVO CAR USA,LLC, RUTHERFORD      '.                           
011820   03  WS-BEFTAG-43              PIC X(37)   VALUE                        
011821       '43VOLVO CAR USA,LLC, ONTARIO         '.                           
011822   03  WS-BEFTAG-44              PIC X(37)   VALUE                        
011823       '44VOLVO CAR USA,LLC, AUBURN          '.                           
011824   03  WS-BEFTAG-45              PIC X(37)   VALUE                        
011825       '45VOLVO CAR USA,LLC, BOLINGBROOK     '.                           
011826   03  WS-BEFTAG-46              PIC X(37)   VALUE                        
011827       '46VOLVO CAR USA,LLC, JACKSONVILLE    '.                           
011828   03  WS-BEFTAG-47              PIC X(37)   VALUE                        
011829       '47VOLVO CAR USA,LLC, DALLAS          '.                           
011830 01 TAB-ADRESSER REDEFINES ADRESSER.                                      
011831   03  TAB-ADRESS OCCURS 10.                                              
011832     05 TAB-IDDC                 PIC X(02).                               
011833     05 TAB-BEFTAG               PIC X(35).                               
011840                                                                          
012600*- - - - - - - - - - - - - -  ARBETS-FÄLT                                 
012700 01  FILLER                      PIC X(16)   VALUE ' ARBETSFÄLT'.         
012710 01  WS-KDGODSM.                                                          
012720   03  FILLER                    PIC X(02)   VALUE 'DC'.                  
012730   03  WS-KDGODSM-DC             PIC X(02).                               
012800 01  ARBETS-FAELT.                                                        
012900   03  SPAR-KDGK                 PIC S9      VALUE ZERO COMP-3.           
013000   03  SPAR-IDLEVNR              PIC X(5)    VALUE SPACE.                 
013100   03  SPAR-IDDC                 PIC X(2)    VALUE ZERO.                  
013200   03  SAVE-IDLEVNR              PIC X(5)    VALUE SPACE.                 
013300   03  SAVE-IDDC                 PIC X(2)    VALUE ZERO.                  
013400   03  SAVE-IDARTNR              PIC S9(9)   VALUE +0  COMP-3.            
013500   03  SPAR-IDLEVNR-AKTUELL      PIC X(5)    VALUE SPACE.                 
013700   03  SPAR-DAINLEV              PIC 9(16)   VALUE ZERO.                  
013800   03  SPAR-ADINPORT             PIC X(08)   VALUE SPACE.                 
013900   03  TRAFF                     PIC X       VALUE 'N'.                   
014000   03  SKAPA-TRAILER             PIC X       VALUE 'N'.                   
014100   03  DEL-SEGMENT               PIC X       VALUE 'N'.                   
014200   03  W-COUNT-W221DEL           PIC S9(5)   VALUE ZERO COMP-3.           
014300   03  TOT-ANTAL                 PIC S9(9)   VALUE ZERO COMP-3.           
014400   03  RAKNARE                   PIC S9(1)   VALUE ZERO COMP-3.           
014600   03  SPAR-KVVECKOR-LT          PIC S9(3)   VALUE ZERO COMP-3.           
014800   03  SW-EFT-SLAP               PIC X       VALUE 'N'.                   
015000   03  WS-IDBEST                 PIC S9(12)  VALUE ZERO COMP-3.           
015100   03  WS-OLD-IDARTNR            PIC 9(9)    VALUE ZERO COMP-3.           
015200   03  WS-IDLPLAN                PIC 9(7)    VALUE ZERO.                  
015300   03  WS-IDARTNR                PIC X(9)    VALUE ZERO.                  
015400   03  WS-IDLEVNR                PIC X(5)    VALUE SPACE.                 
015500   03  WS-IDLEVNR-NUM            PIC 9(5)    VALUE ZERO.                  
015600   03  WS-IDOVERFNR              PIC X(5)    VALUE SPACE.                 
015700   03  WS-MESSAGE-REF            PIC S9(3)   VALUE ZERO.                  
015800   03  WS-TIAAVVD                PIC S9(5)   VALUE ZERO.                  
015900   03  WS-TIAVROP-AVS-AAVVD      PIC 9(5)    VALUE ZERO.                  
016000   03  W-DAAVROP-AVS             PIC 9(6).                                
016100   03  FILLER REDEFINES W-DAAVROP-AVS.                                    
016200       05  FILLER                PIC 9(2).                                
016300       05  W-TIAVROP-AVS         PIC 9(4).                                
016400   03  DAINLEV-NYCKEL            PIC 9(16)   VALUE ZERO.                  
016500   03  DAINLEV-MAX             PIC 9(16) VALUE 9999999999999999.          
016600   03  ANT-PALL                  PIC S9(7)   VALUE ZERO COMP-3.           
016700   03  PALL-PER-DAG              PIC S9(7)   VALUE ZERO COMP-3.           
016800   03  REST                      PIC S9(7)   VALUE ZERO COMP-3.           
016900   03  ANTAL-PER-DAG             PIC S9(7)   VALUE ZERO COMP-3.           
017000   03  ANT-VECKODAG OCCURS 5     PIC S9(7)              COMP-3.           
017100   03  WS-KDGODSM-DET.                                                    
017200       05  WS-SUFFIX             PIC X(3)    VALUE SPACE.                 
017300       05  FILLER                PIC X(1)    VALUE SPACE.                 
017400       05  WS-ADINPORT           PIC X(8)    VALUE SPACE.                 
017500       05  FILLER                PIC X(5)    VALUE SPACE.                 
017600   03  W-ANTAL-VECKOR            PIC S9(3)               COMP-3.          
017700   03  WS-TIAAVV-FRYS            PIC S9(5)               COMP-3.          
017800   03  WS-DAREGDAT               PIC 9(8)    VALUE ZERO.                  
017900   03  WS-TIREGDAT               PIC 9(6)    VALUE ZERO.                  
018000   03  WS-IDINLEV-REGDAT         PIC 9(6)    VALUE ZERO.                  
018100   03  WS-DAINLEV                PIC 9(16)   VALUE ZERO.                  
018200     EJECT                                                                
018300*- - - - - - - - - - - - - -  DATUM- OCH TID-AREA                         
018400 01  MASKINENS-DATUM             PIC 9(6).                                
018500 01  DAGENS-DATUM                PIC 9(6).                                
018600 01  FILLER REDEFINES DAGENS-DATUM.                                       
018700   03  DAT-AA                    PIC 9(2).                                
018800   03  DAT-MM                    PIC 9(2).                                
018900   03  DAT-DD                    PIC 9(2).                                
019000                                                                          
019100 01  INNEVARANDE-AAVVD                    PIC 9(5).                       
019200 01  FILLER REDEFINES INNEVARANDE-AAVVD.                                  
019300   03  INNEV-AA            PIC 9(2).                                      
019400   03  INNEV-VV            PIC 9(2).                                      
019500   03  INNEV-D             PIC 9(1).                                      
019600                                                                          
019700 01  INNEVARANDE-DAT             PIC 9(16).                               
019800 01  INNEVARANDE-DAT-R  REDEFINES  INNEVARANDE-DAT.                       
019900   03  INNEVARANDE-SEKEL         PIC 9(2).                                
020000   03  INNEVARANDE-AAR           PIC 9(2).                                
020100   03  INNEVARANDE-NOLLOR        PIC 9(12).                               
020200 01  MASKINENS-TID               PIC 9(8).                                
020300                                                                          
020400                                                                          
020500 01  KONTR-DATUM                 PIC 9(5).                                
020600 01  FILLER REDEFINES KONTR-DATUM.                                        
020700   03  KONTR-TIAAVV              PIC 9(4).                                
020800   03  KONTR-TID                 PIC 9(1).                                
020900                                                                          
021000     EJECT                                                                
021100*- - - - - - - - - - - - - -  HJÄLPFÄLT VID FILLÄSNINGARNA                
021200 01  FILLER.                                                              
021300   03  INFIL-ID.                                                          
021400      05  IN-IDLEVNR-ID          PIC X(5)    VALUE SPACE.                 
021500      05  IN-IDARTNR-ID          PIC 9(9)    VALUE ZERO COMP-3.           
021600     SKIP2                                                                
021700 01  MAX-VARDE                   PIC S9(14)  VALUE                        
021800                                             +99999999999999.             
021900     SKIP3                                                                
022000 01  INFIL-TRANSID.                                                       
022100     03  INFIL-FDNAMN            PIC X(6)    VALUE 'W2216C'.              
022200     03  INFIL-DDNAMN            PIC X(8)    VALUE 'W2216DD1'.            
022300     03  INFIL-TRANSTYP          PIC X(4)    VALUE SPACE.                 
022400     SKIP3                                                                
022500 01  W2216D-TRANSID.                                                      
022600     03  6D-FDNAMN               PIC X(6)    VALUE 'W2216D'.              
022700     03  6D-DDNAMN               PIC X(8)    VALUE 'W2216DD2'.            
022800     03  6D-TRANSTYP             PIC X(4)    VALUE SPACE.                 
022900     EJECT                                                                
023000 01  DYNAMISKA-SUBPROGRAM.                                                
024000   03  ABEND                     PIC X(8)    VALUE 'ABEND   '.            
024100   03  POSTSUM                   PIC X(8)    VALUE 'POSTSUM '.            
024200   03  DATKORT                   PIC X(8)    VALUE 'DATKORT'.             
024300   03  CBLTDLI                   PIC X(8)    VALUE 'CBLTDLI'.             
024400   03  FELLOG                    PIC X(8)    VALUE 'FELLOG '.             
024500   03  WZ20DAYS                  PIC X(8)    VALUE 'WZ20DAYS'.            
024600   03  W009VADD                  PIC X(8)    VALUE 'W009VADD'.            
024700     SKIP3                                                                
024800*- - - - - - - - - - - - - -  PARAMETRAR TILL ABEND                       
024900                                                                          
025000 01  RETURKODER.                                                          
025100   03  RKOD-ABEND-UTAN-DUMP      PIC S9(4)   VALUE +16  COMP SYNC.        
025200   03  RKOD                      PIC S9(4)   VALUE +0   COMP SYNC.        
025300     SKIP2                                                                
025400*- - - - - - - - - - - - - -  PARAMETRAR TILL POSTSUM                     
025500                                                                          
025600*01  -COPY W0005       -PRE POSTSUM-.                                     
025700     EJECT                                                                
025800*- - - - - - - - - - - - - -  PARAMETRAR TILL DATUMKORT                   
025900                                                                          
026000 01  DATUMKORT-ID                PIC X(6)    VALUE 'WDATUM'.              
026100     SKIP2                                                                
026200*    -COPY WDATKORT                                                       
026300     EJECT                                                                
026400*- - - - - - - - - - - - - -  PARAMETRAR TILL WZ20DAYS                    
026500 01  FILLER                      PIC X(8)    VALUE 'WZ20DAYS'.            
026600     SKIP2                                                                
026700*01  -COPY WZ20DAYS.                                                      
026800     EJECT                                                                
026900*- - - - - - - - - - - - - -  INPOSTERNA                                  
027000                                                                          
027100 01  FILLER                      PIC X(16)   VALUE 'INFIL'.               
027200     SKIP2                                                                
027300*    -COPY W2216C        -PRE IN-.                                        
027400     EJECT                                                                
027500                                                                          
027600*- - - - - - - - - - - - - -  UTPOSTERNA                                  
027700                                                                          
027800 01  FILLER                      PIC X(16)   VALUE 'W221001'.             
027900     SKIP2                                                                
028000*    -COPY W221001       -PRE HEAD-                                       
028100     EJECT                                                                
028200 01  FILLER                      PIC X(16)   VALUE 'W221UNB'.             
028300     SKIP2                                                                
028400*    -COPY W221UNB       -PRE UNB-                                        
028500     EJECT                                                                
028600 01  FILLER                      PIC X(16)   VALUE 'W221UNH'.             
028700     SKIP2                                                                
028800*    -COPY W221UNH       -PRE UNH-                                        
028900     EJECT                                                                
029000 01  FILLER                      PIC X(16)   VALUE 'W221MID'.             
029100     SKIP2                                                                
029200*    -COPY W221MID       -PRE MID-                                        
029300     EJECT                                                                
029400 01  FILLER                      PIC X(16)   VALUE 'W221SDT'.             
029500     SKIP2                                                                
029600*    -COPY W221SDT       -PRE SDT-                                        
029700     EJECT                                                                
029800 01  FILLER                      PIC X(16)   VALUE 'W221BDT'.             
029900     SKIP2                                                                
030000*    -COPY W221BDT       -PRE BDT-                                        
030100     EJECT                                                                
030200 01  FILLER                      PIC X(16)   VALUE 'W221ARI'.             
030300     SKIP2                                                                
030400*    -COPY W221ARI       -PRE ARI-                                        
030500     EJECT                                                                
030600 01  FILLER                      PIC X(16)   VALUE 'W221CSG'.             
030700     SKIP2                                                                
030800*    -COPY W221CSG       -PRE CSG-                                        
030900     EJECT                                                                
031000 01  FILLER                      PIC X(16)   VALUE 'W221ARD'.             
031100     SKIP2                                                                
031200*    -COPY W221ARD       -PRE ARD-                                        
031300     EJECT                                                                
031400 01  FILLER                      PIC X(16)   VALUE 'W221PDI'.             
031500     SKIP2                                                                
031600*    -COPY W221PDI       -PRE PDI-                                        
031700     EJECT                                                                
031800 01  FILLER                      PIC X(16)   VALUE 'W221SAD'.             
031900     SKIP2                                                                
032000*    -COPY W221SAD       -PRE SAD-                                        
032100     EJECT                                                                
032200 01  FILLER                      PIC X(16)   VALUE 'W221DST'.             
032300     SKIP2                                                                
032400*    -COPY W221DST       -PRE DST-                                        
032500     EJECT                                                                
032600 01  FILLER                      PIC X(16)   VALUE 'W221PDN'.             
032700     SKIP2                                                                
032800*    -COPY W221PDN       -PRE PDN-                                        
032900     EJECT                                                                
033000 01  FILLER                      PIC X(16)   VALUE 'W221DEL'.             
033100     SKIP2                                                                
033200*    -COPY W221DEL       -PRE DEL-                                        
033300     EJECT                                                                
033400 01  FILLER                      PIC X(16)   VALUE 'W221003'.             
033500     SKIP2                                                                
033600*    -COPY W221003       -PRE TRAIL-                                      
033700     EJECT                                                                
033800*- - - - - - - - - - - - - -  NYCKLAR TILL DLI                            
033900                                                                          
034000 01  NYCKLAR-TILL-DLI.                                                    
034100   03  W-IDARTNR-X.                                                       
034200     05  W-IDARTNR               PIC S9(9)  COMP-3.                       
034300   03  W-IDLEVNR-X.                                                       
034400     05  W-IDLEVNR               PIC X(5).                                
034500   03  W-DAINLEV-X.                                                       
034600     05  W-DAINLEV               PIC 9(16).                               
034700   03  W-KDSEGKEY-X.                                                      
034800     05  W-KDSEGKEY              PIC X      VALUE '1'.                    
034900   03  W-IDDC-B6-X.                                                       
035000     05  W-IDDC-B6               PIC X(2)   VALUE SPACE.                  
035100                                                                          
035200   03  W-IDDC-K7-X.                                                       
035300     05  W-IDDC-K7               PIC X(2)   VALUE SPACE.                  
035400                                                                          
035500   03  W-IDARTNR-K7-X.                                                    
035600     05  W-IDARTNR-K7            PIC S9(9)  COMP-3.                       
035700                                                                          
035800   03  W-WDF502-MIN.                                                      
035900     05  W-IDLEVNR-MIN  PIC X(5).                                         
036000     05  FILLER         PIC S9(1)   VALUE ZERO COMP-3.                    
036100   03  W-WDF502-MAX.                                                      
036200     05  W-IDLEVNR-MAX  PIC X(5).                                         
036300     05  FILLER         PIC S9(1)   VALUE +9 COMP-3.                      
036400                                                                          
036500   03  W-WDD901KY-X.                                                      
036600     05  W-IDARTNR-D9   PIC S9(9)  VALUE ZERO COMP-3.                     
036700     05  W-IDDC-D9      PIC X(2)   VALUE SPACE.                           
036800                                                                          
036900   03  W-IDLEVNR-D9-X.                                                    
037000     05  W-IDLEVNR-D9   PIC X(5)   VALUE SPACE.                           
037100                                                                          
037200                                                                          
037300*- - - - - - - - - - - - - -  ARBETSAREOR TILL IMS-SEKTIONERNA            
037400*                                                                         
037500 01  IMS-WS.                                                              
037600   03  FILLER           PIC X(8) VALUE 'IMS-WS  '.                        
037700*- - - - - - - - - - - - - -  STATUSKOD FRÅN IMS                          
037800   03  STATUS-WS        PIC X(2).                                         
037900      88  SEGMENT-FINNS          VALUE '  '.                              
038000      88  SEGMENT-SAKNAS         VALUE 'GE'.                              
038100                                                                          
038200   03  GODK-STATUSKODER.                                                  
038300      05  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.               
038400                                                                          
038500   03  SSA1             PIC X(64).                                        
038600   03  SSA2             PIC X(64).                                        
038700   03  SSA3             PIC X(64).                                        
038800     EJECT                                                                
038900*01   -COPY W0003.                                                        
039000     EJECT                                                                
039100*    ---  DLI INPUT-OUTPUT AREA                                           
039200                                                                          
039620 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK611'.                      
039630 01  DLI-IO-WDK611.                                                       
039640*    03  -COPY WDK611                                                     
039700     EJECT                                                                
039800 01  DLI-IO-AREA-WDF5.                                                    
039900   03  IO-WDF5          PIC X(200) VALUE SPACE.                           
040000                                                                          
040100*  03  WDF501    -COPY WDF501  -RED IO-WDF5.                              
040200     EJECT                                                                
040300*  03  WDF502    -COPY WDF502  -RED IO-WDF5.                              
040400     EJECT                                                                
040500 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
040600 01   DLI-IO-AREA-B601.                                                   
040700*     03  -COPY WDB601                                                    
040800 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDL601'.                      
040900 01  DLI-IO-WDL601.                                                       
041000*    03  -COPY WDL601                                                     
042000*                                                                         
043000 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDL611'.                      
044000 01  DLI-IO-WDL611.                                                       
044100*    03  -COPY WDL611                                                     
044200*                                                                         
044300 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD902'.                      
044400 01  DLI-IO-WDD902.                                                       
044500*    03  -COPY WDD902 -PRE WDD9-                                          
044600     EJECT                                                                
045100 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD905'.                      
045200 01  DLI-IO-WDD905.                                                       
045300*    03  -COPY WDD905 -PRE WDD9-                                          
045400     EJECT                                                                
045500 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK711'.                      
045600 01  DLI-IO-WDK711.                                                       
045700*    03  -COPY WDK711                                                     
045800     EJECT                                                                
045900 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK722'.                      
046000 01  DLI-IO-WDK722.                                                       
046100*    03  -COPY WDK722                                                     
046200     EJECT                                                                
046300                                                                          
046400 LINKAGE SECTION.                                                         
046500     SKIP3                                                                
046600*01  -COPY W0008  -PRE WLARTC-                                            
046700     05  FILLER        PIC X(5).                                          
046710     EJECT                                                                
046800*01  -COPY W0008  -PRE WDF5-                                              
046900     05  FILLER        PIC X(5).                                          
046910     EJECT                                                                
047000*01  -COPY W0008  -PRE WDB6-                                              
047100     05  FILLER        PIC X(5).                                          
047110     EJECT                                                                
047200*01  -COPY W0008  -PRE WDL6-                                              
047300     05  FILLER        PIC X(5).                                          
047400     EJECT                                                                
047500*01  -COPY W0008  -PRE WDD9-                                              
047600     05  FILLER        PIC X.                                             
047700     EJECT                                                                
047800*01  -COPY W0008  -PRE WDK7-                                              
047900     05  FILLER        PIC X.                                             
048000                                                                          
048100     EJECT                                                                
048200 PROCEDURE DIVISION USING  WLARTC-PCB WDF5-PCB WDB6-PCB                   
048300                           WDL6-PCB WDD9-PCB WDK7-PCB.                    
048400     ENTRY 'DLITCBL' USING WLARTC-PCB WDF5-PCB WDB6-PCB                   
048500                           WDL6-PCB WDD9-PCB WDK7-PCB.                    
048600     SKIP2                                                                
048700     PERFORM A-INIT                                                       
048800     PERFORM S01-LAS-INFIL                                                
048900                                                                          
049000     IF INFIL-EOF = NEJ                                                   
049100       PERFORM S03-SKAPA-HEADER                                           
049200       MOVE JA TO SKAPA-TRAILER                                           
049300     END-IF                                                               
049400                                                                          
049500     PERFORM UNTIL INFIL-EOF = JA                                         
049600                                                                          
049700       MOVE IN-IDLEVNR TO SPAR-IDLEVNR                                    
049800       MOVE IN-IDDC    TO SPAR-IDDC                                       
049900                          W-IDDC-B6                                       
050000       PERFORM IMS-GU-WDB601                                              
050100       IF SEGMENT-FINNS                                                   
050200         PERFORM B-SKAPA-UNB                                              
050300         PERFORM C-SKAPA-UNH-MID-SDT-BDT-ARI                              
050400         PERFORM UNTIL INFIL-EOF    = JA           OR                     
050500                       IN-IDLEVNR NOT = SPAR-IDLEVNR OR                   
050600                       IN-IDDC  NOT = SPAR-IDDC                           
050700           MOVE IN-KDGK-SORT TO SPAR-KDGK                                 
050800           MOVE IN-ADINPORT TO SPAR-ADINPORT                              
050900           PERFORM D-SKAPA-CSG                                            
051000           PERFORM UNTIL INFIL-EOF        = JA            OR              
051100                         IN-IDLEVNR   NOT = SPAR-IDLEVNR  OR              
051200                         IN-IDDC      NOT = SPAR-IDDC     OR              
051300                         IN-ADINPORT  NOT = SPAR-ADINPORT OR              
051400                         IN-KDGK-SORT NOT = SPAR-KDGK                     
051500             PERFORM E-SKAPA-ARD-PDI-SAD-DST                              
051600             PERFORM F-SKAPA-OCH-SKRIV-PDN                                
051700             PERFORM G-SKAPA-OCH-SKRIV-DEL                                
051800                                                                          
051900             MOVE IN-IDLEVNR TO SAVE-IDLEVNR                              
052000             MOVE IN-IDDC    TO SAVE-IDDC                                 
052100             MOVE IN-IDARTNR TO SAVE-IDARTNR                              
052200                                                                          
052300             PERFORM S01-LAS-INFIL                                        
052400           END-PERFORM                                                    
052500         END-PERFORM                                                      
052600       END-IF                                                             
052700     END-PERFORM                                                          
052800                                                                          
052900     IF SKAPA-TRAILER = JA                                                
053000       PERFORM S04-SKAPA-TRAILER                                          
053100     END-IF                                                               
053200                                                                          
053300     PERFORM Z-FINIT                                                      
053400     MOVE ZERO TO RETURN-CODE                                             
053500     GOBACK                                                               
053600     EJECT                                                                
053700     .                                                                    
053800 A-INIT SECTION.                                                          
053810     MOVE 'A-INIT  '  TO CURRENT-SECTION                                  
053900     SKIP2                                                                
054000     OPEN INPUT  W2216C                                                   
054100                                                                          
054200     OPEN OUTPUT W2216D                                                   
054300                                                                          
054400     MOVE PROGRAM-NAMN TO POSTSUM-PROGNAMN                                
054500                                                                          
054600     CALL DATKORT USING PROGRAM-NAMN DATUMKORT-ID DATUMKORT               
054700     MOVE D-AAR        TO DAT-AA INNEVARANDE-AAR INNEV-AA                 
054800     MOVE D-VECKA      TO INNEV-VV                                        
054900     MOVE D-MAANAD     TO DAT-MM                                          
055000     MOVE D-DAG        TO DAT-DD                                          
055100     MOVE D-DAGNR      TO INNEV-D                                         
055200                                                                          
055300     ACCEPT MASKINENS-DATUM FROM DATE                                     
055400     ACCEPT MASKINENS-TID FROM TIME                                       
055500                                                                          
055600     MOVE ZERO TO INNEVARANDE-NOLLOR                                      
055700     IF INNEVARANDE-AAR < 50                                              
055800       MOVE 20         TO INNEVARANDE-SEKEL                               
055900     ELSE                                                                 
056000       MOVE 19         TO INNEVARANDE-SEKEL                               
056100     END-IF                                                               
056200     COMPUTE DAINLEV-NYCKEL = DAINLEV-MAX - INNEVARANDE-DAT               
056300******************************************************************        
056400*                                                                *        
056500*  DAINLEV-NYCKEL RÄKNAS UT GENOM ATT SUBTRAHERA INNEVARANDE-DAT *        
056600*  FRÅN DAINLEV-MAX. RESULTATET GER DAINLEV-NYCKEL SOM ANVÄNDS   *        
056700*  VID LÄSNING AV WDL2. WDL211 ÄR LAGRAD PÅ FALLANDE NYCKEL      *        
056800*  DAINLEV,OCH EFTER VARJE 211-SEGMENT JÄMFÖRS DAINLEV MED       *        
056900*  DAINLEV-NYCKEL FÖR ATT BESTÄMMA OM INLEVERANSEN SKETT UNDER   *        
057000*  INNEVARANDE ÅR.                                               *        
057100******************************************************************        
057200                                                                          
057300     .                                                                    
057400     EJECT                                                                
057500 B-SKAPA-UNB SECTION.                                                     
057510     MOVE 'B-SKAPA-UNB  '  TO CURRENT-SECTION                             
057600     SKIP2                                                                
057700*--  NOLLA UNB-SEGMENTET                                                  
057800     PERFORM BA-NOLLA-UNB                                                 
057900                                                                          
058000     MOVE 'UNB' TO UNB-IDPT                                               
058100                   6D-TRANSTYP                                            
058101                                                                          
058110*--  USA SKALL HA PARTNER NO/PLANTAN SOM LEVNR                            
058120     IF DCS-NDC-CN                                                        
058200       MOVE DCS-IDLEVNR-DC  TO UNB-IDFROM                                 
058210     ELSE                                                                 
058211       MOVE DCS-IDLEVNR-EMB TO UNB-IDFROM                                 
058220     END-IF                                                               
058300                                                                          
058400     MOVE IN-IDLEVNR TO WS-IDLEVNR                                        
058500     IF WS-IDLEVNR (5:1) = SPACE                                          
058600        MOVE ZERO TO TALLY                                                
058700        INSPECT WS-IDLEVNR TALLYING TALLY                                 
058800                FOR CHARACTERS BEFORE INITIAL SPACE                       
058900        IF TALLY = ZERO                                                   
059000           MOVE ZERO TO WS-IDLEVNR-NUM                                    
059100        ELSE                                                              
059200           MOVE WS-IDLEVNR (1:TALLY) TO WS-IDLEVNR-NUM                    
059300        END-IF                                                            
059400        MOVE WS-IDLEVNR-NUM TO UNB-IDLEVNR                                
059500     ELSE                                                                 
059600        MOVE WS-IDLEVNR     TO UNB-IDLEVNR                                
059700     END-IF                                                               
059800                                                                          
059900     MOVE IN-IDOVERFNR-LOP TO WS-IDOVERFNR                                
060000     INSPECT WS-IDOVERFNR TALLYING RAKNARE FOR LEADING '0'                
060100     ADD +1 TO RAKNARE                                                    
060200     UNSTRING WS-IDOVERFNR INTO UNB-IDSNRF WITH POINTER RAKNARE           
060300     MOVE ZERO TO RAKNARE                                                 
060400                                                                          
060500     WRITE UNB FROM UNB-W221UNB                                           
060600     MOVE W2216D-TRANSID TO POSTSUM-TRANSID                               
060700     CALL POSTSUM USING POSTSUM-PARM                                      
060800                                                                          
060900     MOVE ZERO TO WS-MESSAGE-REF                                          
061000     MOVE SPACE TO 6D-TRANSTYP                                            
061100     .                                                                    
061200     EJECT                                                                
061300 BA-NOLLA-UNB SECTION.                                                    
061400     SKIP2                                                                
061500     MOVE SPACE TO UNB-IDPT                                               
061600                   UNB-IDFROM                                             
061700                   UNB-IDLEVNR                                            
061800                   UNB-IDSNRF                                             
061900     .                                                                    
062000     EJECT                                                                
062100 C-SKAPA-UNH-MID-SDT-BDT-ARI SECTION.                                     
062200     SKIP2                                                                
062300     PERFORM CB-SKAPA-OCH-SKRIV-UNH                                       
062400     PERFORM CC-SKAPA-OCH-SKRIV-MID                                       
062500     PERFORM CD-SKAPA-OCH-SKRIV-SDT                                       
062600     PERFORM CE-SKAPA-OCH-SKRIV-BDT                                       
062700     PERFORM CF-SKAPA-OCH-SKRIV-ARI                                       
062800     .                                                                    
062900     EJECT                                                                
063000                                                                          
063100 CB-SKAPA-OCH-SKRIV-UNH SECTION.                                          
063110     MOVE 'CB-SKAPA-OCH-SKRIV-UNH '  TO CURRENT-SECTION                   
063200     SKIP2                                                                
063300*--  NOLLA UNH-SEGMENTET                                                  
063400     PERFORM CBA-NOLLA-UNH                                                
063500                                                                          
063600     MOVE 'UNH' TO UNH-IDPT                                               
063700     MOVE 'DELINS' TO UNH-MESSAGE-ID-TYPE                                 
063800     MOVE '  3' TO UNH-MESSAGE-ID-VERSION                                 
063900                                                                          
064000     WRITE UNH FROM UNH-W221UNH                                           
064100     MOVE W2216D-TRANSID TO POSTSUM-TRANSID                               
064200     CALL POSTSUM USING POSTSUM-PARM                                      
064300                                                                          
064400     .                                                                    
064500     SKIP3                                                                
064600 CBA-NOLLA-UNH SECTION.                                                   
064700     SKIP2                                                                
064800     MOVE SPACE TO UNH-IDPT                                               
064900                   UNH-MESSAGE-ID-TYPE                                    
065000                   UNH-MESSAGE-ID-VERSION                                 
065100     .                                                                    
065200     EJECT                                                                
065300 CC-SKAPA-OCH-SKRIV-MID SECTION.                                          
065310     MOVE 'CC-SKAPA-OCH-SKRIV-MID '  TO CURRENT-SECTION                   
065400     SKIP2                                                                
065500*--  NOLLA MID-SEGMENTET                                                  
065600     MOVE SPACE TO MID-IDPT                                               
065700                   MID-IDLPLAN                                            
065800     MOVE ZERO  TO MID-TILPLAN                                            
065900                                                                          
066000     MOVE 'MID' TO MID-IDPT                                               
066100     MOVE IN-IDLPLAN-LEV TO WS-IDLPLAN                                    
066200     MOVE WS-IDLPLAN TO MID-IDLPLAN                                       
066300     MOVE DAGENS-DATUM TO MID-TILPLAN                                     
066400                                                                          
066500     WRITE MID FROM MID-W221MID                                           
066600     MOVE W2216D-TRANSID TO POSTSUM-TRANSID                               
066700     CALL POSTSUM USING POSTSUM-PARM                                      
066800     .                                                                    
066900     EJECT                                                                
067000 CD-SKAPA-OCH-SKRIV-SDT SECTION.                                          
067010     MOVE 'CD-SKAPA-OCH-SKRIV-SDT '  TO CURRENT-SECTION                   
067100     SKIP2                                                                
067200*--  NOLLA SDT-SEGMENTET                                                  
067300     MOVE SPACE TO SDT-IDPT                                               
067400                   SDT-IDLEVNR                                            
067500                                                                          
067600     MOVE 'SDT' TO SDT-IDPT                                               
067700     MOVE WS-IDLEVNR TO SDT-IDLEVNR                                       
067800     MOVE ZERO TO RAKNARE                                                 
067900                                                                          
068000     WRITE SDT FROM SDT-W221SDT                                           
068100     MOVE W2216D-TRANSID TO POSTSUM-TRANSID                               
068200     CALL POSTSUM USING POSTSUM-PARM                                      
068300     .                                                                    
068400     EJECT                                                                
068500 CE-SKAPA-OCH-SKRIV-BDT SECTION.                                          
068510     MOVE 'CE-SKAPA-OCH-SKRIV-BDT '  TO CURRENT-SECTION                   
068600     SKIP2                                                                
068700*--  NOLLA BDT-SEGMENTET                                                  
068800     MOVE SPACE TO BDT-IDPT                                               
068900                   BDT-IDBUYER                                            
069000                                                                          
069100     MOVE 'BDT' TO BDT-IDPT                                               
069200                                                                          
069300     MOVE IN-IDLEVKND TO BDT-IDBUYER                                      
069400                                                                          
069500     WRITE BDT FROM BDT-W221BDT                                           
069600     MOVE W2216D-TRANSID TO POSTSUM-TRANSID                               
069700     CALL POSTSUM USING POSTSUM-PARM                                      
069800                                                                          
069900     .                                                                    
070000     EJECT                                                                
070100 CF-SKAPA-OCH-SKRIV-ARI SECTION.                                          
070110     MOVE 'CF-SKAPA-OCH-SKRIV-ARI '  TO CURRENT-SECTION                   
070200     SKIP2                                                                
070300*--  NOLLA ARI-SEGMENTET                                                  
070400     MOVE SPACE TO ARI-IDPT                                               
070500     MOVE ZERO  TO ARI-KDRLSTID                                           
070600                   ARI-TIYYMMDD-LPFROM                                    
070700                                                                          
070800     MOVE 'ARI' TO ARI-IDPT                                               
070900     MOVE +1 TO ARI-KDRLSTID                                              
071000                                                                          
071100*--  LEVERANSPLAN BÖRJAR GÄLLA DAGENS-DATUM + 1,                          
071200*--  DVS DAGEN EFTER NATTKÖRNINGEN                                        
071300                                                                          
071400     MOVE DAGENS-DATUM TO DAYS-TIDATE1                                    
071500     MOVE 'YYMMDD'     TO DAYS-KDDATFMT1                                  
071600     MOVE 'YYMMDD'     TO DAYS-KDDATFMT2                                  
071700     MOVE SPACE        TO DAYS-TIDATE2                                    
071800                          DAYS-IDCALEND                                   
071900     MOVE +1           TO DAYS-KVDAYS                                     
072000                                                                          
072100     CALL WZ20DAYS USING DAYS-WZ20DAYS                                    
072200                                                                          
072300     MOVE DAYS-TIDATE2(1:6) TO ARI-TIYYMMDD-LPFROM                        
072400                                                                          
072500     WRITE ARI FROM ARI-W221ARI                                           
072600     MOVE W2216D-TRANSID TO POSTSUM-TRANSID                               
072700     CALL POSTSUM USING POSTSUM-PARM                                      
072800     .                                                                    
072900     EJECT                                                                
073000 D-SKAPA-CSG SECTION.                                                     
073007                                                                          
073010     MOVE 'D-SKAPA-CSG '  TO CURRENT-SECTION                              
073100     SKIP2                                                                
073200     PERFORM DA-NOLLA-CSG                                                 
073300     MOVE 'CSG' TO CSG-IDPT                                               
073400                                                                          
073440     MOVE IN-IDDC           TO WS-KDGODSM-DC                              
073450     MOVE WS-KDGODSM        TO CSG-KDGODSM                                
073460                               CSG-KDGODSM-DET                            
073461                                                                          
073462*--  USA SKALL HA PARTNER NO/PLANTAN SOM LEVNR                            
073463     IF DCS-NDC-CN                                                        
073470       MOVE DCS-IDLEVNR-DC    TO CSG-KDFORBR                              
073471     ELSE                                                                 
073472       MOVE DCS-IDLEVNR-EMB   TO CSG-KDFORBR                              
073473     END-IF                                                               
073474                                                                          
073480     MOVE +1                TO IX-FTAG                                    
073490     PERFORM UNTIL IX-FTAG > IX-FTAG-MAX                                  
073493       IF IN-IDDC = TAB-IDDC (IX-FTAG)                                    
073494          MOVE TAB-BEFTAG (IX-FTAG) TO CSG-BEFTAG                         
073495          MOVE +99          TO IX-FTAG                                    
073496       ELSE                                                               
073497          ADD +1            TO IX-FTAG                                    
073498       END-IF                                                             
073499     END-PERFORM                                                          
073500                                                                          
075600     WRITE CSG FROM CSG-W221CSG                                           
075700     MOVE W2216D-TRANSID TO POSTSUM-TRANSID                               
075800     CALL POSTSUM USING POSTSUM-PARM                                      
075900                                                                          
076000     .                                                                    
077000     EJECT                                                                
077100                                                                          
077200 DA-NOLLA-CSG SECTION.                                                    
077300     SKIP2                                                                
077400     MOVE SPACE TO CSG-IDPT                                               
077500                   CSG-BEFTAG                                             
077600                   CSG-KDGODSM                                            
077700                   CSG-KDGODSM-DET                                        
077800                   CSG-KDFORBR                                            
077900     .                                                                    
078000     EJECT                                                                
078100                                                                          
078200 E-SKAPA-ARD-PDI-SAD-DST SECTION.                                         
078210     MOVE 'E-SKAPA-ARD-PDI-SAD-DST '  TO CURRENT-SECTION                  
078300     SKIP2                                                                
078400     PERFORM EB-SKAPA-OCH-SKRIV-ARD                                       
078500     PERFORM EC-SKAPA-OCH-SKRIV-PDI                                       
078600     PERFORM ED-SKAPA-OCH-SKRIV-SAD                                       
078700     MOVE ZERO       TO TOT-ANTAL                                         
078800                                                                          
078900     PERFORM IMS-GU-WDL601                                                
079000     IF SEGMENT-FINNS                                                     
079100        PERFORM IMS-GNP-WDL611                                            
079200        MOVE NEJ TO TRAFF                                                 
079300        PERFORM UNTIL SEGMENT-SAKNAS OR                                   
079400                      INL-DAINLEV > DAINLEV-NYCKEL                        
079500          MOVE INL-DAINLEV TO SPAR-DAINLEV                                
079600                              W-DAINLEV                                   
079700            IF INL-IDLEVNR   = IN-IDLEVNR   AND                           
079800               INL-KDRT      = 0            AND                           
079900               INL-IDDC      = IN-IDDC      AND                           
080000               INL-FLMAKUL   = NEJ          AND                           
080100              (INL-IDPTYP    = 'R31' OR 'R32')                            
080200                IF INL-IDPTYP = 'R31'                                     
080300                   ADD INL-KVAVIS TO TOT-ANTAL                            
080400                ELSE                                                      
080500                   ADD INL-KVANTMOT TO TOT-ANTAL                          
080600                END-IF                                                    
080700            END-IF                                                        
080800            PERFORM IMS-GNP-WDL611                                        
080900        END-PERFORM                                                       
081000     END-IF                                                               
082000                                                                          
083000     PERFORM EE-SKAPA-OCH-SKRIV-DST                                       
084000                                                                          
085000     .                                                                    
086000     EJECT                                                                
087000 EB-SKAPA-OCH-SKRIV-ARD SECTION.                                          
087100     MOVE 'EB-SKAPA-OCH-SKRIV-ARD '  TO CURRENT-SECTION                   
088000     SKIP2                                                                
088100*--  NOLLA ARD-SEGMENT                                                    
088200     MOVE SPACE TO ARD-IDPT                                               
088300                   ARD-IDART                                              
088400                   ARD-BESTNR                                             
088500                   ARD-BELEVART14                                         
088600                                                                          
088700     MOVE 'ARD' TO ARD-IDPT                                               
088800     MOVE IN-IDARTNR TO WS-IDARTNR                                        
088900                        W-IDARTNR                                         
089000     MOVE IN-IDLEVNR TO W-IDLEVNR-MIN                                     
089100                        W-IDLEVNR-MAX                                     
089200     PERFORM IMS-GU-WDF501                                                
089300     IF SEGMENT-FINNS                                                     
089400        PERFORM IMS-GNP-WDF502                                            
089500        PERFORM UNTIL SEGMENT-SAKNAS                                      
089600                                                                          
089700          MOVE XLEV-BELEVART(1:14) TO ARD-BELEVART14                      
089800          PERFORM IMS-GNP-WDF502                                          
089900        END-PERFORM                                                       
090000     END-IF                                                               
090100                                                                          
090200     INSPECT WS-IDARTNR TALLYING RAKNARE FOR LEADING '0'                  
090300     ADD +1 TO RAKNARE                                                    
090400     UNSTRING WS-IDARTNR INTO ARD-IDART WITH POINTER RAKNARE              
090500     MOVE ZERO TO RAKNARE                                                 
090600     MOVE IN-IDBEST TO WS-IDBEST                                          
090700     MOVE WS-IDBEST TO ARD-BESTNR                                         
090800                                                                          
090900     WRITE ARD FROM ARD-W221ARD                                           
091000     MOVE W2216D-TRANSID TO POSTSUM-TRANSID                               
092000     CALL POSTSUM USING POSTSUM-PARM                                      
092100                                                                          
092200     .                                                                    
092300     EJECT                                                                
092400 EC-SKAPA-OCH-SKRIV-PDI SECTION.                                          
092410     MOVE 'EC-SKAPA-OCH-SKRIV-PDI '  TO CURRENT-SECTION                   
092500     SKIP2                                                                
092600     IF IN-IDLPLAN-ART > 0                                                
092700                                                                          
092800*--    NOLLA PDI-SEGMENT                                                  
092900       MOVE SPACE TO PDI-IDPT                                             
093000                     PDI-IDLPLAN                                          
093100                                                                          
093200       MOVE 'PDI' TO PDI-IDPT                                             
093300       MOVE IN-IDLPLAN-ART TO WS-IDLPLAN                                  
093400       MOVE WS-IDLPLAN TO PDI-IDLPLAN                                     
093500                                                                          
093600       WRITE PDI FROM PDI-W221PDI                                         
093700       MOVE W2216D-TRANSID TO POSTSUM-TRANSID                             
093800       CALL POSTSUM USING POSTSUM-PARM                                    
093900     END-IF                                                               
094000                                                                          
094100     .                                                                    
094200     EJECT                                                                
094300 ED-SKAPA-OCH-SKRIV-SAD SECTION.                                          
094310     MOVE 'ED-SKAPA-OCH-SKRIV-SAD '  TO CURRENT-SECTION                   
094400     SKIP2                                                                
094500*--  NOLLA SAD-SEGMENT                                                    
094600     MOVE SPACE TO SAD-IDPT                                               
094700     MOVE ZERO  TO SAD-KDRLSTYP                                           
094800                                                                          
094900     MOVE 'SAD' TO SAD-IDPT                                               
095000     MOVE 1 TO SAD-KDRLSTYP                                               
095100                                                                          
095200     WRITE SAD FROM SAD-W221SAD                                           
095300     MOVE W2216D-TRANSID TO POSTSUM-TRANSID                               
095400     CALL POSTSUM USING POSTSUM-PARM                                      
095500                                                                          
095600     .                                                                    
095700     EJECT                                                                
095800 EE-SKAPA-OCH-SKRIV-DST SECTION.                                          
095810     MOVE 'EE-SKAPA-OCH-SKRIV-DST '  TO CURRENT-SECTION                   
095900     SKIP2                                                                
096000*--  NOLLA DST-SEGMENT                                                    
096100     MOVE SPACE TO DST-IDPT                                               
096200                   DST-KVART-BREST-ALPHA                                  
096300     MOVE ZERO  TO DST-TIYYMMDD-AVST                                      
096400                   DST-KVINLAAR                                           
096500                                                                          
096600     MOVE 'DST' TO DST-IDPT                                               
096700     MOVE DAGENS-DATUM TO DST-TIYYMMDD-AVST                               
096800     IF TOT-ANTAL > 0                                                     
096900        MOVE TOT-ANTAL TO DST-KVINLAAR                                    
097000     END-IF                                                               
097100                                                                          
097200     MOVE +0        TO DST-KVART-BREST                                    
097300                                                                          
097400     WRITE DST FROM DST-W221DST                                           
097500     MOVE W2216D-TRANSID TO POSTSUM-TRANSID                               
097600     CALL POSTSUM USING POSTSUM-PARM                                      
097700                                                                          
097800     .                                                                    
097900     EJECT                                                                
098000 F-SKAPA-OCH-SKRIV-PDN SECTION.                                           
098100     SKIP2                                                                
098200     PERFORM FA-NOLLA-PDN                                                 
098300     PERFORM FC-FYLL-I-RATT-ORDNING                                       
098400     .                                                                    
098500     EJECT                                                                
098600 FA-NOLLA-PDN SECTION.                                                    
098610     MOVE 'FA-NOLLA-PDN '  TO CURRENT-SECTION                             
098700     SKIP2                                                                
098800     MOVE 'PDN' TO PDN-IDPT                                               
098900     MOVE SPACE TO PDN-TISENINL-ALPHA                                     
099000                   PDN-KVSENINL-ALPHA                                     
099100     MOVE ZERO  TO PDN-IDNRINL                                            
099200     .                                                                    
099300     EJECT                                                                
099400 FC-FYLL-I-RATT-ORDNING SECTION.                                          
099410     MOVE 'FC-FYLL-I-RATT-ORDNING '  TO CURRENT-SECTION                   
099500                                                                          
099600     MOVE +1 TO IX                                                        
099700     PERFORM IMS-GU-WDL601                                                
099800     IF SEGMENT-FINNS                                                     
099900        PERFORM IMS-GNP-WDL611                                            
100000        MOVE NEJ TO TRAFF                                                 
101000        PERFORM UNTIL SEGMENT-SAKNAS OR IX > 3                            
102000          MOVE INL-DAINLEV TO SPAR-DAINLEV                                
103000                              W-DAINLEV                                   
104000            IF INL-IDLEVNR   = IN-IDLEVNR   AND                           
104100               INL-KDRT      = 0            AND                           
104200               INL-IDDC      = IN-IDDC      AND                           
104300               INL-FLMAKUL   = NEJ          AND                           
104400              (INL-IDPTYP    = 'R31' OR 'R32')                            
104500                IF INL-IDPTYP = 'R31'                                     
104600                   MOVE INL-KVAVIS TO PDN-KVSENINL                        
104700                ELSE                                                      
104800                   MOVE INL-KVANTMOT TO PDN-KVSENINL                      
104900                END-IF                                                    
105000                MOVE INL-IDKUNDRF TO PDN-IDNRINL                          
105100                IF INL-TIAVIDAT > 0                                       
105200                  MOVE INL-TIAVIDAT TO PDN-TISENINL                       
105300                ELSE                                                      
105400                  MOVE INL-DAINLEV TO WS-DAINLEV                          
105500                  MOVE WS-DAINLEV (3:6)                                   
105600                                      TO WS-IDINLEV-REGDAT                
105700                  COMPUTE WS-TIREGDAT = 999999 - WS-IDINLEV-REGDAT        
105800                  MOVE WS-TIREGDAT TO PDN-TISENINL                        
105900                END-IF                                                    
106100                IF PDN-TISENINL = ZERO                                    
106200                   MOVE SPACE TO PDN-TISENINL-ALPHA                       
106300                END-IF                                                    
106400                                                                          
106700                WRITE PDN FROM PDN-W221PDN                                
106800                MOVE W2216D-TRANSID TO POSTSUM-TRANSID                    
106900                CALL POSTSUM USING POSTSUM-PARM                           
107000                                                                          
107100                ADD +1 TO IX                                              
107200                PERFORM FA-NOLLA-PDN                                      
107300            END-IF                                                        
107400          PERFORM IMS-GNP-WDL611                                          
107500        END-PERFORM                                                       
107600     END-IF                                                               
107700     .                                                                    
107800     EJECT                                                                
107900 G-SKAPA-OCH-SKRIV-DEL SECTION.                                           
108000     MOVE 'G-SKAPA-OCH-SKRIV-DEL '  TO CURRENT-SECTION                    
109000                                                                          
110000     PERFORM GA-NOLLA-DEL                                                 
110100                                                                          
110200     IF  IN-IDLEVNR = SAVE-IDLEVNR                                        
110300     AND IN-IDDC    = SAVE-IDDC                                           
110400     AND IN-IDARTNR = SAVE-IDARTNR                                        
110500        CONTINUE                                                          
110600     ELSE                                                                 
110700        MOVE +0 TO W-COUNT-W221DEL                                        
110800     END-IF                                                               
110900                                                                          
111000     MOVE IN-IDDC      TO W-IDDC-D9                                       
111100     MOVE IN-IDARTNR   TO W-IDARTNR-D9                                    
111200     MOVE IN-IDLEVNR   TO W-IDLEVNR-D9                                    
111300     PERFORM IMS-GU-WDD902                                                
111400                                                                          
111500     IF SEGMENT-FINNS                                                     
111600        MOVE NEJ TO DEL-SEGMENT                                           
111800                                                                          
111900        MOVE IN-IDARTNR   TO W-IDARTNR-K7                                 
112000        MOVE IN-IDDC      TO W-IDDC-K7                                    
112100        PERFORM IMS-GU-WDK711                                             
112200        PERFORM IMS-GNP-WDK722                                            
112300        IF SEGMENT-FINNS                                                  
112400          MOVE XLAG-KVVECKOR-LT   TO SPAR-KVVECKOR-LT                     
113400        ELSE                                                              
113600          MOVE ZERO    TO SPAR-KVVECKOR-LT                                
113700        END-IF                                                            
117700                                                                          
117800        PERFORM IMS-GNP-WDD905                                            
117900        PERFORM UNTIL SEGMENT-SAKNAS                                      
118000           IF WDD9-KDAVROP = +2                                           
118100              MOVE WDD9-DAAVROP-AVS TO W-DAAVROP-AVS                      
118200              COMPUTE WS-TIAAVVD = (W-TIAVROP-AVS * 10 ) +                
118300                                    WDD9-TILEVDAG                         
118400                                                                          
118500              MOVE WS-TIAAVVD TO WS-TIAVROP-AVS-AAVVD                     
118600              MOVE WS-TIAAVVD TO DAYS-TIDATE1                             
118700              MOVE 'YYWWD '   TO DAYS-KDDATFMT1                           
118800              MOVE 'YYMMDD'   TO DAYS-KDDATFMT2                           
118900              MOVE SPACE      TO DAYS-TIDATE2                             
119000                                 DAYS-IDCALEND                            
119100              MOVE ZERO       TO DAYS-KVDAYS                              
119200                                                                          
119300              CALL WZ20DAYS USING DAYS-WZ20DAYS                           
119400                                                                          
119500              MOVE DAYS-TIDATE2(1:6) TO DEL-TISTART                       
119600                                                                          
119700              MOVE WS-TIAVROP-AVS-AAVVD   TO TMP1-YYWWD                   
119800              MOVE INNEVARANDE-AAVVD      TO TMP2-YYWWD                   
119900              PERFORM WY2000P2                                            
120000              IF TMP1-YYWWD <= TMP2-YYWWD                                 
120100                 MOVE '3'   TO DEL-KDDELTYP                               
120200              ELSE                                                        
120300                 MOVE SPACE TO DEL-KDDELTYP                               
120400              END-IF                                                      
120500              MOVE WDD9-KVAVROP TO DEL-KVART-AVROP                        
120700                                                                          
120800              PERFORM GB-KOLLA-MOT-LEDTID                                 
120900              MOVE 'DEL' TO DEL-IDPT                                      
121000              PERFORM S09-SKRIV-DEL                                       
121100              MOVE JA TO DEL-SEGMENT                                      
121200              PERFORM GA-NOLLA-DEL                                        
121300           END-IF                                                         
121400           PERFORM IMS-GNP-WDD905                                         
121500        END-PERFORM                                                       
121600                                                                          
121700        IF DEL-SEGMENT = NEJ                                              
121800           MOVE 'DEL'        TO DEL-IDPT                                  
121900           MOVE ZERO         TO DEL-KVART-AVROP                           
122000           MOVE SPACE        TO DEL-KDDELTYP                              
122100           MOVE 4            TO DEL-KDDELIND                              
122200           PERFORM GC-DATUM-PLUS-ETT                                      
122300           PERFORM S09-SKRIV-DEL                                          
122400        END-IF                                                            
122500     ELSE                                                                 
122600        MOVE 'DEL'        TO DEL-IDPT                                     
122700        MOVE ZERO         TO DEL-KVART-AVROP                              
122800        MOVE SPACE        TO DEL-KDDELTYP                                 
122900        MOVE 4            TO DEL-KDDELIND                                 
123000        MOVE DAGENS-DATUM TO DEL-TISTART                                  
123100        PERFORM S09-SKRIV-DEL                                             
123200     END-IF                                                               
123500                                                                          
123600     .                                                                    
123700     EJECT                                                                
123800                                                                          
123900 GA-NOLLA-DEL SECTION.                                                    
124000     MOVE 'GA-NOLLA-DEL'  TO CURRENT-SECTION                              
124100                                                                          
124200     MOVE SPACE TO DEL-IDPT                                               
124300                   DEL-TISTOPP                                            
124400                   DEL-KDDELTYP                                           
124500     MOVE ZERO  TO DEL-TISTART                                            
124600                   DEL-KVART-AVROP                                        
124700                   DEL-KDDELIND                                           
124800     .                                                                    
124900     EJECT                                                                
125000 GB-KOLLA-MOT-LEDTID SECTION.                                             
125100     MOVE 'GB-KOLLA-MOT-LEDTID '  TO CURRENT-SECTION                      
125101*---                                                                      
125110*--- MAN SKALL JÄMFÖRA MOT HELA VECKOR OCH INTE MOT                       
125120*--- HEL VECKA+DAG SOM TIDIGARE FÖR ATT SÄTTA STATUS 1.                   
125200                                                                          
125300     MOVE SPAR-KVVECKOR-LT     TO W-ANTAL-VECKOR                          
125400     MOVE INNEVARANDE-AAVVD    TO KONTR-DATUM                             
125410     MOVE 7                    TO KONTR-TID                               
125500     MOVE KONTR-TIAAVV         TO WS-TIAAVV-FRYS                          
125600     CALL W009VADD USING WS-TIAAVV-FRYS W-ANTAL-VECKOR                    
125700     MOVE WS-TIAAVV-FRYS       TO KONTR-TIAAVV                            
125800                                                                          
125900     IF WS-TIAVROP-AVS-AAVVD > KONTR-DATUM                                
126000       MOVE 4                  TO DEL-KDDELIND                            
126100     ELSE                                                                 
126200       MOVE 1                  TO DEL-KDDELIND                            
126300     END-IF                                                               
126400                                                                          
126500     .                                                                    
126600     EJECT                                                                
126700 GC-DATUM-PLUS-ETT SECTION.                                               
126800     MOVE 'GC-DATUM-PLUS-ETT '   TO CURRENT-SECTION                       
126900                                                                          
127000*--  DATUM SKALL VARA DAGENS-DATUM + 1,                                   
127100*--  DVS DAGEN EFTER NATTKÖRNINGEN                                        
127200                                                                          
127300     MOVE DAGENS-DATUM TO DAYS-TIDATE1                                    
127400     MOVE 'YYMMDD'     TO DAYS-KDDATFMT1                                  
127500     MOVE 'YYMMDD'     TO DAYS-KDDATFMT2                                  
127600     MOVE SPACE        TO DAYS-TIDATE2                                    
127700                          DAYS-IDCALEND                                   
127800     MOVE +1           TO DAYS-KVDAYS                                     
127900                                                                          
128000     CALL WZ20DAYS USING DAYS-WZ20DAYS                                    
128100                                                                          
128200     MOVE DAYS-TIDATE2(1:6) TO DEL-TISTART                                
128300                                                                          
128400                                                                          
128500     .                                                                    
128600     EJECT                                                                
128700 S01-LAS-INFIL SECTION.                                                   
128710     MOVE 'S01-LAS-INFIL '  TO CURRENT-SECTION                            
128800     SKIP3                                                                
128900     READ W2216C INTO IN-W2216C AT END MOVE JA TO INFIL-EOF               
129000                                                                          
129100     NOT AT END                                                           
129200        MOVE 'INFIL' TO POSTSUM-FDNAMN                                    
129300        MOVE 'W2216DD1' TO POSTSUM-DDNAMN2                                
129400        CALL POSTSUM USING POSTSUM-PARM                                   
129500                                                                          
129600     END-READ                                                             
129700     .                                                                    
129800     EJECT                                                                
129900 S03-SKAPA-HEADER SECTION.                                                
129910     MOVE 'S03-SKAPA-HEADER '  TO CURRENT-SECTION                         
130000     SKIP2                                                                
131000     MOVE '001'              TO HEAD-IDPT                                 
132000                                6D-TRANSTYP                               
133000     MOVE 'WPAR'             TO HEAD-IDSECN                               
134000     MOVE 'AMOS'             TO HEAD-IDRECN                               
134100     MOVE 'WPAR3201'         TO HEAD-IDFILE                               
134200     MOVE MASKINENS-DATUM    TO HEAD-TIFILE-DAT                           
134300     MOVE MASKINENS-TID(1:6) TO HEAD-TIFILE-KL                            
134400                                                                          
134500     WRITE HEAD FROM HEAD-W221001                                         
134600     MOVE W2216D-TRANSID TO POSTSUM-TRANSID                               
134700     CALL POSTSUM USING POSTSUM-PARM                                      
134800                                                                          
134900     MOVE SPACE TO 6D-TRANSTYP                                            
135000     .                                                                    
135100     EJECT                                                                
135200 S04-SKAPA-TRAILER SECTION.                                               
135210     MOVE 'S04-SKAPA-TRAILER '  TO CURRENT-SECTION                        
135300     SKIP2                                                                
135400     MOVE '003' TO TRAIL-IDPT                                             
135500                   6D-TRANSTYP                                            
135600                                                                          
135700     WRITE TRAIL FROM TRAIL-W221003                                       
135800     MOVE W2216D-TRANSID TO POSTSUM-TRANSID                               
135900     CALL POSTSUM USING POSTSUM-PARM                                      
136000                                                                          
136100     MOVE SPACE TO 6D-TRANSTYP                                            
136200     .                                                                    
136300     EJECT                                                                
136400 S09-SKRIV-DEL SECTION.                                                   
136410     MOVE 'S09-SKRIV-DEL '  TO CURRENT-SECTION                            
136500                                                                          
136600     ADD +1                TO W-COUNT-W221DEL                             
136700                                                                          
136800     IF W-COUNT-W221DEL > +200                                            
136900        CONTINUE                                                          
137000     ELSE                                                                 
137100        WRITE DEL         FROM DEL-W221DEL                                
137200        MOVE W2216D-TRANSID TO POSTSUM-TRANSID                            
137300        CALL POSTSUM     USING POSTSUM-PARM                               
137400     END-IF                                                               
137500                                                                          
137600     .                                                                    
137700     EJECT                                                                
137800***********************IMS-SEKTIONER                                      
137900 IMS-GET-WDK611 SECTION.                                                  
137910     MOVE 'IMS-GET-WDK611 '  TO DBS-SECTION                               
137920                                                                          
138000     SKIP2                                                                
139000     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
140000     DELIMITED BY SIZE INTO SSA1                                          
141000     STRING 'WLARTC11(KDSEGKEY =' W-KDSEGKEY-X ')'                        
142000     DELIMITED BY SIZE INTO SSA2                                          
143000     MOVE '  GE' TO GODK-STATUSKODER                                      
144000     CALL CBLTDLI USING GU WLARTC-PCB DLI-IO-WDK611 SSA1 SSA2             
145000     MOVE WLARTC-STATUS-CODE TO STATUS-WS                                 
146000     PERFORM IMS-STATUSKONTROLL                                           
146100     SKIP3                                                                
146200     .                                                                    
146300 IMS-GU-WDF501 SECTION.                                                   
146310     MOVE 'IMS-GU-WDF501 '  TO DBS-SECTION                                
146320                                                                          
146400     STRING 'WDF501  (IDARTNR  =' W-IDARTNR-X ')'                         
146500          DELIMITED BY SIZE INTO SSA1                                     
146600     MOVE '  GE' TO GODK-STATUSKODER                                      
146700     CALL CBLTDLI USING GU WDF5-PCB DLI-IO-AREA-WDF5 SSA1                 
146800     MOVE WDF5-STATUS-CODE TO STATUS-WS                                   
146900     PERFORM IMS-STATUSKONTROLL                                           
147000     .                                                                    
147100     EJECT                                                                
147200 IMS-GNP-WDF502 SECTION.                                                  
147210     MOVE 'IMS-GNP-WDF502 '  TO DBS-SECTION                               
147220                                                                          
147300     STRING 'WDF502  (WDF5KEY =>' W-WDF502-MIN                            
147400                 '&WDF5KEY =<' W-WDF502-MAX ')'                           
147500          DELIMITED BY SIZE INTO SSA1                                     
147600     MOVE '  GE' TO GODK-STATUSKODER                                      
147700     CALL CBLTDLI USING GNP WDF5-PCB DLI-IO-AREA-WDF5 SSA1                
147800     MOVE WDF5-STATUS-CODE TO STATUS-WS                                   
147900     PERFORM IMS-STATUSKONTROLL                                           
148000     .                                                                    
148100     EJECT                                                                
148200 IMS-GU-WDB601    SECTION.                                                
148210     MOVE 'IMS-GU-WDB601 '  TO DBS-SECTION                                
148220                                                                          
148300     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
148400          DELIMITED BY SIZE INTO SSA1                                     
148500     MOVE '  ' TO GODK-STATUSKODER                                        
148600     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
148700     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
148800     PERFORM IMS-STATUSKONTROLL                                           
148900     .                                                                    
149000     EJECT                                                                
150000 IMS-GU-WDL601 SECTION.                                                   
150010     MOVE 'IMS-GU-WDL601 '  TO DBS-SECTION                                
150020                                                                          
150100     STRING 'WDL601  (IDARTNR  =' W-IDARTNR-X ')'                         
150200          DELIMITED BY SIZE INTO SSA1                                     
150300     MOVE '  GE' TO GODK-STATUSKODER                                      
150400     CALL CBLTDLI USING GU WDL6-PCB DLI-IO-WDL601 SSA1                    
150500     MOVE WDL6-STATUS-CODE TO STATUS-WS                                   
150600     PERFORM IMS-STATUSKONTROLL                                           
150700     .                                                                    
150800     SKIP2                                                                
150900 IMS-GNP-WDL611 SECTION.                                                  
150910     MOVE 'IMS-GNP-WDL611 '  TO DBS-SECTION                               
150920                                                                          
151000     STRING 'WDL611     '                                                 
151100          DELIMITED BY SIZE INTO SSA1                                     
151200     MOVE '  GE' TO GODK-STATUSKODER                                      
151300     CALL CBLTDLI USING GNP WDL6-PCB DLI-IO-WDL611 SSA1                   
151400     MOVE WDL6-STATUS-CODE TO STATUS-WS                                   
151500     PERFORM IMS-STATUSKONTROLL                                           
151600     .                                                                    
151700     SKIP2                                                                
151800 IMS-GU-WDD902 SECTION.                                                   
151900     MOVE 'IMS-GU-WDD902'   TO DBS-SECTION                                
152000                                                                          
152100     STRING 'WDD901  (WDD901KY =' W-WDD901KY-X ')'                        
152200     DELIMITED BY SIZE INTO SSA1                                          
152300     STRING 'WDD902  (IDLEVNR  =' W-IDLEVNR-D9-X ')'                      
152400     DELIMITED BY SIZE INTO SSA2                                          
152500     MOVE '  GE' TO GODK-STATUSKODER                                      
152600     CALL CBLTDLI USING GU WDD9-PCB DLI-IO-WDD902 SSA1 SSA2               
152700     MOVE WDD9-STATUS-CODE TO STATUS-WS                                   
152800     PERFORM IMS-STATUSKONTROLL                                           
152900     .                                                                    
153000     EJECT                                                                
154100 IMS-GNP-WDD905 SECTION.                                                  
154200     MOVE 'IMS-GNP-WDD905'  TO DBS-SECTION                                
154300                                                                          
154400     MOVE 'WDD905   ' TO SSA1                                             
154500     MOVE '  GE' TO GODK-STATUSKODER                                      
154600     CALL CBLTDLI USING GNP WDD9-PCB DLI-IO-WDD905 SSA1                   
154700     MOVE WDD9-STATUS-CODE TO STATUS-WS                                   
154800     PERFORM IMS-STATUSKONTROLL                                           
154900     .                                                                    
155000     EJECT                                                                
155100 IMS-GU-WDK711 SECTION.                                                   
155200     MOVE 'IMS-GU-WDK711'   TO DBS-SECTION                                
155300                                                                          
155400     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-K7-X ')'                      
155500     DELIMITED BY SIZE INTO SSA1                                          
155600     STRING 'WDK711  (IDDC     =' W-IDDC-K7-X ')'                         
155700     DELIMITED BY SIZE INTO SSA2                                          
155800                                                                          
155900     MOVE '  GE' TO GODK-STATUSKODER                                      
156000     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-WDK711 SSA1 SSA2               
156100     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
156200     PERFORM IMS-STATUSKONTROLL                                           
156300     .                                                                    
156400     EJECT                                                                
156500 IMS-GNP-WDK722 SECTION.                                                  
156600     MOVE 'IMS-GNP-WDK722'  TO DBS-SECTION                                
156700                                                                          
156800     MOVE 'WDK722   ' TO SSA1                                             
156900     MOVE '  GE' TO GODK-STATUSKODER                                      
157000     CALL CBLTDLI USING GNP WDK7-PCB DLI-IO-WDK722 SSA1                   
157100     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
157200     PERFORM IMS-STATUSKONTROLL                                           
157300     SKIP3                                                                
157400     .                                                                    
157500     EJECT                                                                
157600 IMS-STATUSKONTROLL SECTION.                                              
157700     SKIP2                                                                
157800     SET STATUS-IX TO 1                                                   
157900     SEARCH GODK-STATUS AT END CALL FELLOG                                
158000     WHEN GODK-STATUS(STATUS-IX) = STATUS-WS                              
158100     CONTINUE                                                             
158200     END-SEARCH                                                           
158300                                                                          
158400     .                                                                    
158500 Z-FINIT   SECTION.                                                       
158600     SKIP3                                                                
158700     CLOSE  W2216C                                                        
158800            W2216D                                                        
158900*- - - - - - - - - - - - - - -  SKRIV UT ANTAL LÄSTA OCH                  
159000*                               SKRIVNA POSTER                            
159100     MOVE 'S' TO POSTSUM-OPKOD                                            
159200     CALL POSTSUM USING POSTSUM-PARM                                      
159300     .                                                                    
159400     EJECT                                                                
159500*    -COPY WY2000P1                                                       
159600     EJECT                                                                
159700*    -COPY WY2000P2                                                       
