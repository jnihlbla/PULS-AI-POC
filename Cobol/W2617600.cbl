000100 PROCESS DYNAM                                                            
000200 ID DIVISION.                                                             
000300 PROGRAM-ID.     W2617600.                                                
000400 AUTHOR.         HELGEGREN PER-ANDERS.                                    
000500 DATE-WRITTEN.   09/12/16.                                                
000600 DATE-COMPILED.                                                           
000700                                                                          
000800                                                                          
000900*    FUNKTION:                                                            
001000*        (PROGRAMMET KOPIA/VIDAREUTV. AV W26174,EGENTLIGEN W26168)        
001100*        FRAMSTÄLLER UNDERLAG FÖR AUTO-SKROTNING 01-MÄRKTA ART.           
001200*                                                                         
001300*        PROGRAMMET UPPDATERAR WDK6                                       
001400*        PROGRAMMET LÄSER      WDK6                                       
001500*        PROGRAMMET LÄSER      WDK9                                       
001600*        PROGRAMMET UPPDATERAR WDR5                                       
001700*        PROGRAMMET LÄSER      WDR5                                       
001800*        PROGRAMMET LÄSER      WDN6                                       
001900*        PROGRAMMET LÄSER      WDQ4                                       
002000*        PROGRAMMET LÄSER      WDL8                                       
002100*        PROGRAMMET LÄSER      WDD7                                       
002200*        PROGRAMMET LÄSER      WDR2                                       
002300*        PROGRAMMET LÄSER      WDJ1C                                      
002400*        PROGRAMMET LÄSER      WDD9                                       
002500*        PROGRAMMET LÄSER      WDD3                                       
002600*                                                                         
002700*        THE PROGRAM READS   TABLE TP1KAMP                                
002800*        THE PROGRAM READS   TABLE TP1ARTK                                
002900*                                                                         
003000                                                                          
003100     SKIP3                                                                
003200 ENVIRONMENT DIVISION.                                                    
003300     SKIP2                                                                
003400 INPUT-OUTPUT SECTION.                                                    
003500                                                                          
003600 FILE-CONTROL.                                                            
003700     SKIP2                                                                
003800*          --- ARTIKELREGISTER / LAGERBAND                                
003900     SELECT W01160                     ASSIGN TO W26176D1.                
004000     EJECT                                                                
004100 DATA DIVISION.                                                           
004200     SKIP3                                                                
004300 FILE SECTION.                                                            
004400     SKIP3                                                                
004500 FD  W01160                                                               
004600     RECORDING       F                                                    
004700     BLOCK CONTAINS  0.                                                   
004800                                                                          
004900*01  -COPY W01160   -L.                                                   
005000     EJECT                                                                
005100 WORKING-STORAGE SECTION.                                                 
005200                                                                          
005300 77  IDPGM                       PIC X(8)    VALUE 'W2617600'.            
005400 01  CHKP-VAR.                                                            
005500     03 CHKP-MSG-IO-AREA-LENGTH  PIC S9(9)   VALUE +32 COMP SYNC.         
005600     03 CHKP-MSG-IO-AREA         PIC X(32)   VALUE SPACE.                 
005700     03 CHKP-AREA-LENGTH         PIC S9(9)   VALUE +32 COMP SYNC.         
005800     03 CHKP-AREA                PIC X(32)   VALUE SPACE.                 
005900     03 CHKP-ANT                 PIC S9(3)   VALUE +0   COMP-3.           
006000     03 CHKP-MAX                 PIC S9(3)   VALUE +200 COMP-3.           
006100 77  JA                          PIC X       VALUE 'J'.                   
006200 77  NEJ                         PIC X       VALUE 'N'.                   
006300 77  IX                          PIC 9(3)    VALUE ZERO.                  
006400 77  WS-IX                       PIC 9(2)    VALUE ZERO.                  
006500 77  IX-PAH                      PIC 9(2)    VALUE ZERO.                  
006600 77  IX-PAH2                     PIC 9(2)    VALUE ZERO.                  
006700 77  IX-TABB                     PIC 9(2)    VALUE ZERO.                  
006800 77  IX-TAB                      PIC S9(3)   VALUE ZERO COMP-3.           
006900     SKIP2                                                                
007000 01  FELTEXT.                                                             
007100     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
007200     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
007300 77  WS-SPAR-BEANST-GODK         PIC X(25)   VALUE SPACE.                 
007400 77  W01160-EOF-SW               PIC X       VALUE 'N'.                   
007500     88  END-OF-W01160                       VALUE 'J'.                   
007600 77  SW-KOLL                     PIC X       VALUE 'J'.                   
007700*    SW-KOLL = JA     =>  OK FÖR SKROT                                    
007800 77  SW-KAMPANJ                  PIC X       VALUE 'N'.                   
007900 77  SW-DEKAL                    PIC X       VALUE 'N'.                   
008000 77  SW-SATS-OK                  PIC X       VALUE 'N'.                   
008100     EJECT                                                                
008200*      --- VALID IDDC CODES                                               
008300*                                                                         
008400*01    -COPY WWDCKONS                                                     
008500     EJECT                                                                
008600 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
008700 01  FILLER REDEFINES DAGENS-DATUM.                                       
008800     03  DAGENS-DATUM-AAR        PIC 9(2).                                
008900     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
009000     03  DAGENS-DATUM-DAG        PIC 9(2).                                
009100                                                                          
009200 77  DAGENS-DATUM-Y2K            PIC 9(8)  VALUE ZERO.                    
009300     EJECT                                                                
009400 01  DYNAMISKA-SUBPROGRAM.                                                
009500*                                                                         
009600     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
009700     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
009800     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
009900     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
010000     03  W009LTXT                PIC X(8)    VALUE 'W009LTXT'.            
010100     SKIP3                                                                
010200 01  ARB.                                                                 
010300     03  WARTC-KVUTRS-C1         PIC S9(7)   COMP-3.                      
010400     03  WARTC-KVAKS-C1          PIC S9(7)   COMP-3.                      
010500     03  WARTC-KDERS-C1          PIC S9(3)   COMP-3.                      
010600     03  WARTC-KVLS-C1           PIC S9(7)   COMP-3.                      
010700     03  WARTC-KVRESS-C1         PIC S9(7)   COMP-3.                      
010800     03  WARTC-IDANSK            PIC S9(3)   COMP-3.                      
010900     03  WARTC-KVSPANT           PIC S9(7)   COMP-3.                      
011000     03  WS-KVOKS-C1             PIC S9(6)   VALUE ZERO.                  
011100     03  IDARTNR-WS              PIC S9(9)   VALUE ZERO COMP-3.           
011200     03  WS-FLERS                PIC X(1)    VALUE SPACE.                 
011300                                                                          
011400     03  W-SDC-KVLS              PIC S9(7)   VALUE ZERO COMP-3.           
011500     03  W-SDC-KVAKS             PIC S9(7)   VALUE ZERO COMP-3.           
011600     03  W-SDC-KVOKS             PIC S9(7)   VALUE ZERO COMP-3.           
011700                                                                          
011800     03  WS-AUTO-USERID          PIC X(8)    VALUE 'W2617600'.            
011900     03  WS-IDKUNDNR             PIC S9(7)   VALUE ZERO COMP-3.           
012000     03  WKVSKRANT               PIC S9(7).                               
012100     03  IX-RAD                  PIC S9(3)   VALUE ZERO COMP-3.           
012200     03  WS-TEMEMO               PIC X(25)   VALUE SPACE.                 
012300     03  WS-KVBR                 PIC S9(7)   VALUE ZERO COMP-3.           
012400     03  ANT-SKROTU              PIC S9(3)   VALUE ZERO COMP-3.           
012500     03  MAX-ANT-SKROTU          PIC S9(3)   VALUE 999  COMP-3.           
012600     03  WS-KVBEART              PIC S9(7)   VALUE ZERO COMP-3.           
012700     03  WS-MAX-SUBEL            PIC S9(7)   VALUE ZERO COMP-3.           
012800     03  WS-SUARTSTD             PIC 9(7)V9(2) VALUE ZERO.                
012900     03  WS-TIAAAA               PIC 9(4)    VALUE ZERO.                  
013000     03  FILLER  REDEFINES  WS-TIAAAA.                                    
013100         05  WS-TISEKEL          PIC 9(2).                                
013200         05  WS-TIAA-VECKA       PIC 9(2).                                
013300     03  WS-TIAAAA-1             PIC 9(4)    VALUE ZERO.                  
013400     03  WS-TIVV                 PIC S9(3)   VALUE ZERO  COMP-3.          
013500     03  WS-TIAAVVD-D            PIC  9(5)   VALUE ZERO.                  
013600     03  FILLER REDEFINES WS-TIAAVVD-D.                                   
013700         05  FILLER              PIC  9(4).                               
013800         05  WS-TID-D            PIC  9(1).                               
013900     03  WS-TIAAAAVVD            PIC  9(7)   VALUE ZERO.                  
014000     03  FILLER  REDEFINES WS-TIAAAAVVD.                                  
014100         05  WS-TISS             PIC 9(2).                                
014200         05  WS-TIAAVVD          PIC 9(5).                                
014300     03  A2-TIAAAAVVD            PIC  9(7)   VALUE ZERO.                  
014400     03  FILLER  REDEFINES A2-TIAAAAVVD.                                  
014500         05  A2-TISS             PIC 9(2).                                
014600         05  A2-TIFINLV          PIC 9(5).                                
014700     03  FILLER  REDEFINES A2-TIAAAAVVD.                                  
014800         05  FILLER              PIC 9(2).                                
014900         05  A2-TIAA             PIC 9(2).                                
015000         05  FILLER              PIC 9(3).                                
015100     03  WS-ANTAL-OI             PIC S9(7)   VALUE ZERO COMP-3.           
015200     03  WS-TEXT.                                                         
015300         05  WS-TEXT-GB          PIC X(25).                               
015400         05  WS-TEXT-SV          PIC X(25).                               
015500     03  WS-ARKOST               PIC S9(9)   VALUE ZERO COMP-3.           
015600     03  SUMOI-T                 PIC S9(9)   VALUE ZERO COMP-3.           
015700     03  SUMOI-X                 PIC S9(9)   VALUE ZERO COMP-3.           
015800     03  SUMOI-Y                 PIC S9(9)   VALUE ZERO COMP-3.           
015900     03  SUMOI-Z                 PIC S9(9)   VALUE ZERO COMP-3.           
016000     03  SUMOI-W                 PIC S9(9)   VALUE ZERO COMP-3.           
016100     03  INNEV-TIVV              PIC 9(2)    VALUE ZERO.                  
016200     03  INNEV-TIAAPP            PIC 9(4)    VALUE ZERO.                  
016300     03  INNEV-KVVIPER           PIC 9(1)    VALUE ZERO.                  
016400     03  INNEV-HIT-VV            PIC 9(1)    VALUE ZERO.                  
016500     03  INNEV-KVAR-VV           PIC 9(1)    VALUE ZERO.                  
016600     03  FOREG-TIAAPP            PIC 9(4)    VALUE ZERO.                  
016700     03  FILLER  REDEFINES FOREG-TIAAPP.                                  
016800         05  FOREG-TIAA          PIC 9(2).                                
016900         05  FOREG-TIPP          PIC 9(2).                                
017000     03  FOREG-KVVIPER           PIC 9(1)    VALUE ZERO.                  
017100     03  WS-TPO-NAESTA-INLEV     PIC S9(9)   VALUE ZERO COMP-3.           
017200     03  WS-SUTPO-TOT            PIC S9(9)   VALUE ZERO COMP-3.           
017300     03  W-TIAAAAVV.                                                      
017400       05 W-TISEKEL              PIC  9(2)   VALUE ZERO.                  
017500       05 W-TIAA                 PIC  9(2)   VALUE ZERO.                  
017600       05 W-TIVV                 PIC  9(2)   VALUE ZERO.                  
017700     03  W-DADISPIN              PIC  9(6)   VALUE ZERO.                  
017800     03  W-TIDISPIN              PIC  9(6)   VALUE ZERO.                  
017900                                                                          
018000                                                                          
018100 01  OMRADES-TAB.                                                         
018200*****KOSTNADER PER LAGEROMRÅDE                                            
018300***      OBS I G-KOLL... ANVÄNDS IX=5 FÖR LAGEROMRÅDE 20                  
018400     03  FILLER.                                                          
018500         05  FILLER          PIC S9(3) COMP-3  VALUE     10.              
018600         05  FILLER          PIC S9(7) COMP-3  VALUE    770.              
018700         05  FILLER          PIC S9(3) COMP-3  VALUE     11.              
018800         05  FILLER          PIC S9(7) COMP-3  VALUE    770.              
018900         05  FILLER          PIC S9(3) COMP-3  VALUE     12.              
019000         05  FILLER          PIC S9(7) COMP-3  VALUE    770.              
019100         05  FILLER          PIC S9(3) COMP-3  VALUE     16.              
019200         05  FILLER          PIC S9(7) COMP-3  VALUE    770.              
019300         05  FILLER          PIC S9(3) COMP-3  VALUE     20.              
019400         05  FILLER          PIC S9(7) COMP-3  VALUE   2650.              
019500         05  FILLER          PIC S9(3) COMP-3  VALUE     21.              
019600         05  FILLER          PIC S9(7) COMP-3  VALUE   2650.              
019700         05  FILLER          PIC S9(3) COMP-3  VALUE     22.              
019800         05  FILLER          PIC S9(7) COMP-3  VALUE   2650.              
019900         05  FILLER          PIC S9(3) COMP-3  VALUE     30.              
020000         05  FILLER          PIC S9(7) COMP-3  VALUE   8170.              
020100         05  FILLER          PIC S9(3) COMP-3  VALUE     42.              
020200         05  FILLER          PIC S9(7) COMP-3  VALUE   2650.              
020300         05  FILLER          PIC S9(3) COMP-3  VALUE     43.              
020400         05  FILLER          PIC S9(7) COMP-3  VALUE   2650.              
020500         05  FILLER          PIC S9(3) COMP-3  VALUE     72.              
020600         05  FILLER          PIC S9(7) COMP-3  VALUE   8170.              
020700         05  FILLER          PIC S9(3) COMP-3  VALUE     90.              
020800         05  FILLER          PIC S9(7) COMP-3  VALUE    174.              
020900                                                                          
021000 01  OMR-TAB  REDEFINES OMRADES-TAB.                                      
021100     03  TAB-LAGOMR-KOST OCCURS 12.                                       
021200         05  TAB-ADLAGOMR    PIC S9(3) COMP-3.                            
021300         05  TAB-ARKOST      PIC S9(7) COMP-3.                            
021400                                                                          
021500*****TABELLER                                                             
021600*****VILKA VECKOR HAR VILKEN PERIOD SISTA 3 ÅREN                          
021700*****      VI SKALL JU RÄKNA ANTAL PERIODER BAKÅT OCH                     
021800*****      I WDL811 ÄR JU OI PER VECKA                                    
021900 01  TAB0.                                                                
022000     03 FILLER OCCURS 53.                                                 
022100        05  TAB0-TIAAVV.                                                  
022200            07  FILLER       PIC 9(2).                                    
022300            07  TAB0-TIVV    PIC 9(2).                                    
022400        05  TAB0-TIAAPP.                                                  
022500            07  TAB0-TIAA    PIC 9(2).                                    
022600            07  TAB0-TIPP    PIC 9(2).                                    
022700        05  TAB0-KVVIPER     PIC 9(1).                                    
022800                                                                          
022900 01  TAB1.                                                                
023000     03 FILLER OCCURS 53.                                                 
023100        05  TAB1-TIAAVV.                                                  
023200            07  FILLER       PIC 9(2).                                    
023300            07  TAB1-TIVV    PIC 9(2).                                    
023400        05  TAB1-TIAAPP.                                                  
023500            07  TAB1-TIAA    PIC 9(2).                                    
023600            07  TAB1-TIPP    PIC 9(2).                                    
023700        05  TAB1-KVVIPER     PIC 9(1).                                    
023800                                                                          
023900 01  TAB2.                                                                
024000     03 FILLER OCCURS 53.                                                 
024100        05  TAB2-TIAAVV.                                                  
024200            07  FILLER       PIC 9(2).                                    
024300            07  TAB2-TIVV    PIC 9(2).                                    
024400        05  TAB2-TIAAPP.                                                  
024500            07  TAB2-TIAA    PIC 9(2).                                    
024600            07  TAB2-TIPP    PIC 9(2).                                    
024700        05  TAB2-KVVIPER     PIC 9(1).                                    
024800                                                                          
024900 01  TABB-OI-PERIODER.                                                    
025000*****OI PER PERIOD LÖPANDE BAKÅT                                          
025100     03  FILLER  OCCURS 25.                                               
025200         05 TABB-OI               PIC S9(7)   COMP-3.                     
025300         05 TABB-KVVIPER          PIC  9(1).                              
025400                                                                          
025500 01  TABKV-OI-KVPERIOD.                                                   
025600*****FRÅN BILD 2148  ANTAL PERIODER/PRODSL TILL OLIKA BERÄKNINGAR         
025700     03  FILLER  OCCURS 99.                                               
025800         05 TABKV-KDPRODSL        PIC S9(3)   COMP-3.                     
025900         05 TABKV-KVPERIOD-BERS   PIC S9(3)   COMP-3.                     
026000         05 TABKV-KVPERIOD-BTILLK PIC S9(3)   COMP-3.                     
026100         05 TABKV-KVPERIOD-VERS   PIC S9(3)   COMP-3.                     
026200         05 TABKV-KVPERIOD-HERS   PIC S9(3)   COMP-3.                     
026300                                                                          
026400 01  TIAAVV                  PIC 9(4).                                    
026500 01  FILLER   REDEFINES TIAAVV.                                           
026600     03  TIAA                PIC 9(2).                                    
026700     03  TIVV                PIC 9(2).                                    
026800                                                                          
026900 01  ARB.                                                                 
027000     03  ARB-TIAAAAPP.                                                    
027100         05  ARB-TISEKEL     PIC 9(2).                                    
027200         05  ARB-TIAAPP      PIC 9(4).                                    
027300         05  FILLER  REDEFINES ARB-TIAAPP.                                
027400             07  ARB-TIAA    PIC 9(2).                                    
027500             07  ARB-TIPP    PIC 9(2).                                    
027600     03  FILLER REDEFINES ARB-TIAAAAPP.                                   
027700         05  ARB-TIAAAA      PIC 9(4).                                    
027800         05  FILLER          PIC 9(2).                                    
027900                                                                          
028000 01  S-AAPP.                                                              
028100     03  S-AA                PIC 9(2).                                    
028200     03  S-PP                PIC 9(2).                                    
028300                                                                          
028400 01  W-DATUM.                                                             
028500     05  W-DATUM-DATE    PIC X(6).                                        
028600     EJECT                                                                
028700                                                                          
028800*01  -COPY WWPRODSL                                                       
028900                                                                          
029000*- - - - - - - - - - - - - -  PARAMETRAR TILL WDATKONV                    
029100                                                                          
029200*01  -COPY WDATAREA                                                       
029300     EJECT                                                                
029400*    --- PARAMETRAR TILL POSTSUM                                          
029500*                                                                         
029600*01  -COPY W0005   -PRE  POSTSUM-                                         
029700     EJECT                                                                
029800*    --- TEXTSÖKNING                                                      
029900*                                                                         
030000*01  -COPY W009W041                                                       
030100     EJECT                                                                
030200 01  IN-AREA-START               PIC X(24)   VALUE                        
030300                                             'IN-AREA-START'.             
030400     SKIP2                                                                
030500                                                                          
030600*01  AREA -COPY W01160     -PRE IN-                                       
030700*                                                                         
030800     EJECT                                                                
030900 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
031000     SKIP3                                                                
031100 01  NYCKLAR-TILL-DLI.                                                    
031200     03  W-IDARTNR-X.                                                     
031300         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
031400     03  W-WDD901KY-X.                                                    
031500         05  W-IDARTNR-D9        PIC S9(9)   VALUE ZERO COMP-3.           
031600         05  W-IDDC-D9           PIC X(2)    VALUE SPACE.                 
031700     03  W-KDSEGKEY-X.                                                    
031800         05  W-KDSEGKEY          PIC X(1)    VALUE '1'.                   
031900     03  W-DASKROT-X.                                                     
032000         05  W-DASKROT           PIC X(8)    VALUE SPACE.                 
032100     03  W-IDSKYLT-X.                                                     
032200         05  W-IDSKYLT           PIC X(3)    VALUE SPACE.                 
032300     03  W-DASKROT9-X.                                                    
032400         05  W-DASKROT9          PIC S9(8)   VALUE ZERO COMP-3.           
032500     03  W-KY6324-X.                                                      
032600         05  W-KY6324            PIC X(8)    VALUE SPACE.                 
032700     03  W-KY6328-X.                                                      
032800         05  W-KY6328            PIC X(15)    VALUE SPACE.                
032900     03  W-IDDC-X.                                                        
033000         05  W-IDDC              PIC X(2)    VALUE SPACE.                 
033100     03  W-WDN611KY-X.                                                    
033200         05  W-WDN611KY          PIC X(6)    VALUE SPACE.                 
033300     03  W-IDLEVNR-X.                                                     
033400         05  W-IDLEVNR           PIC X(5)    VALUE SPACE.                 
033500     03  W-WDJ1CSEQ-X.                                                    
033600         05  W-IDLEVNR-S         PIC X(5)    VALUE SPACE.                 
033700         05  W-BELEVART-S        PIC X(30)   VALUE SPACE.                 
033800         05  W-IDARTNR-S         PIC S9(9)   COMP-3 VALUE ZERO.           
033900                                                                          
034000     03  W-IDHTYP-X.                                                      
034100         05  W-4533-IDHTYP        PIC X(04)    VALUE '4533'.              
034200                                                                          
034300     03  W-WDGXKEY-4534-X.                                                
034400         05  W-4534-IDARTNR      PIC S9(09)   VALUE ZERO COMP-3.          
034500         05  W-4534-IDDC         PIC X(2)     VALUE '11'.                 
034600         05  W-4534-LOW-VALUE    PIC X(03)    VALUE LOW-VALUE.            
034700                                                                          
034800     03  W-WDQ4B1KY-MAX.                                                  
034900         05  W-IDARTNR-Q4B1-MAX  PIC S9(9)    COMP-3.                     
035000         05  FILLER              PIC X(32)    VALUE HIGH-VALUE.           
035100                                                                          
035200     03  W-WDQ4B1KY-MIN.                                                  
035300         05  W-IDARTNR-Q4B1-MIN  PIC S9(9)    COMP-3.                     
035400         05  FILLER              PIC X(32)    VALUE LOW-VALUE.            
035500                                                                          
035600     03  W-WDQ401KY-X.                                                    
035700         05  W-IDWDQ401          PIC X(20).                               
035800     03  FILLER   REDEFINES W-WDQ401KY-X.                                 
035900         05  FILLER              PIC X(18).                               
036000         05  W-IDLOPNR-Q4B1      PIC S9(3)    COMP-3.                     
036100                                                                          
036200     03  W-TIAAAA-X.                                                      
036300         05  W-TIAAAA            PIC 9(4).                                
036400                                                                          
036500 01  W-DABEHOV-MIN-X.                                                     
036600     03  W-DABEHOV-MIN        PIC   9(6)  VALUE ZERO.                     
036700 01  W-DABEHOV-MAX-X.                                                     
036800     03  W-DABEHOV-MAX        PIC   9(6)  VALUE ZERO.                     
036900                                                                          
037000 01  W-IDARTNR-STR-X.                                                     
037100     03  W-IDARTNR-STR        PIC S9(9) COMP-3 VALUE ZERO.                
037200                                                                          
037300 01  W-IDARTNR-2.                                                         
037400     03  W-IDARTNR-PCB2       PIC S9(9) COMP-3 VALUE ZERO.                
037500                                                                          
037600 01  W-IDSKYLT-KEY-X.                                                     
037700   03  W-IDSKYLT-KEY     PIC X(3)    VALUE SPACE.                         
037800                                                                          
037900                                                                          
038000 01  NYCKLAR-6321.                                                        
038100     03  W-WDGX6321-ROT-X.                                                
038200         05  FILLER              PIC X(04)  VALUE '6321'.                 
038300         05  W-KDARBTYP          PIC X(08)  VALUE 'ANSK    '.             
038400         05  FILLER              PIC X(18)  VALUE LOW-VALUE.              
038500     03  W-WDGX6322-KEY-X.                                                
038600         05  W-DASKROT9-BEORD    PIC 9(08)  VALUE ZERO.                   
038700     03  W-KY6324-KVAL-X.                                                 
038800         05  W-IDARTNR-KVAL      PIC S9(9)  VALUE ZERO COMP-3.            
038900         05  W-IDDC-KVAL         PIC X(2)   VALUE SPACE.                  
039000         05  W-KDSTASKR-KVAL     PIC S9     VALUE ZERO COMP-3.            
039100                                                                          
039200     03  W-WDGXKEY-X.                                                     
039300         05  FILLER             PIC X(4)    VALUE '6327'.                 
039400         05  W-KDARBTYP-6327    PIC X(8)    VALUE SPACE.                  
039500         05  W-IDDC-6327        PIC X(2)    VALUE SPACE.                  
039600         05  FILLER             PIC X(16)   VALUE LOW-VALUE.              
039700     03  W-IDUSER-GODK-X.                                                 
039800         05  W-IDUSER-GODK      PIC X(8)    VALUE SPACE.                  
039900                                                                          
040000*****   *NYCKEL TILL WDR2  (BILD 2148)                                    
040100     03  W-WDGX2243-ROT-X.                                                
040200         05  FILLER              PIC X(04)  VALUE '2243'.                 
040300         05  FILLER              PIC X(26)  VALUE LOW-VALUE.              
040400                                                                          
040500     03  W-KDPRODSL-X.                                                    
040600         05  W-KDPRODSL          PIC S9(3)   COMP-3.                      
040700     EJECT                                                                
040800*    --- STATUS-KOD FRÅN IMS                                              
040900 01  STATUS-WS                   PIC XX.                                  
041000     88  SEGMENT-FINNS                       VALUE '  '.                  
041100     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
041200     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
041300     88  SEGMENT-SLUT                        VALUE 'GB'.                  
041400     88  IMS-EJ-OK                           VALUE 'XD'.                  
041500     SKIP2                                                                
041600 01  GODK-STATUSKODER.                                                    
041700     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
041800     SKIP3                                                                
041900 01  SSA1                        PIC X(128).                              
042000 01  SSA2                        PIC X(64).                               
042100 01  SSA3                        PIC X(64).                               
042200 01  SSA4                        PIC X(64).                               
042300     EJECT                                                                
042400*    --- IMS FUNKTIONSKODER                                               
042500*01  -COPY W0003                                                          
042600     EJECT                                                                
042700*    ---  DLI INPUT-OUTPUT AREA                                           
042800                                                                          
042900 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK601'.                      
043000 01  DLI-IO-WDK601.                                                       
043100*    03  -COPY WDK601                                                     
043200     EJECT                                                                
043300 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK611'.                      
043400 01  DLI-IO-WDK611.                                                       
043500*    03  -COPY WDK611                                                     
043600     EJECT                                                                
043700 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK627'.                      
043800 01  DLI-IO-WDK627.                                                       
043900*    03  -COPY WDK627                                                     
044000 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK601-2'.                    
044100 01  DLI-IO-WDK601-2.                                                     
044200*    03  -COPY WDK601  -PRE ART2-                                         
044300     EJECT                                                                
044400 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK611-2'.                    
044500 01  DLI-IO-WDK611-2.                                                     
044600*    03  -COPY WDK611  -PRE CLAG2-                                        
044700     EJECT                                                                
044800 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK901'.                      
044900 01  DLI-IO-WDK901.                                                       
045000*    03  -COPY WDK901  -PRE ARTM-                                         
045100     EJECT                                                                
045200 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK911'.                      
045300 01  DLI-IO-WDK911.                                                       
045400*    03  -COPY WDK911  -PRE ARTM-                                         
045500     EJECT                                                                
045600 01  FILLER         PIC X(26) VALUE 'DLI-IO-WDR501-6321'.                 
045700 01  DLI-IO-WDR501-6321.                                                  
045800*    03  -COPY WDGX6321                                                   
045900     EJECT                                                                
046000 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX6322'.                    
046100 01  DLI-IO-WDGX6322.                                                     
046200*    03  -COPY WDGX6322                                                   
046300     EJECT                                                                
046400 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX6324'.                    
046500 01  DLI-IO-WDGX6324.                                                     
046600*    03  -COPY WDGX6324                                                   
046700 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX6325'.                    
046800 01  DLI-IO-WDGX6325.                                                     
046900*    03  -COPY WDGX6325                                                   
047000     EJECT                                                                
047100 01  FILLER         PIC X(26) VALUE 'DLI-IO-WDR501-6327'.                 
047200 01  DLI-IO-WDR501-6327.                                                  
047300*    03  -COPY WDGX6327                                                   
047400     EJECT                                                                
047500 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX6328'.                    
047600 01  DLI-IO-WDGX6328.                                                     
047700*    03  -COPY WDGX6328                                                   
047800 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDN601'.                      
047900 01  DLI-IO-WDN601.                                                       
048000*    03  -COPY WDN601                                                     
048100     EJECT                                                                
048200 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDN611'.                      
048300 01  DLI-IO-WDN611.                                                       
048400*    03  -COPY WDN611                                                     
048500     EJECT                                                                
048600 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDQ4B1'.                      
048700 01  DLI-IO-WDQ4B1.                                                       
048800*    03  -COPY WDQ4B1                                                     
048900     EJECT                                                                
049000 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDQ4'.                        
049100 01  DLI-IO-WDQ401.                                                       
049200*    03  -COPY WDQ401                                                     
049300     EJECT                                                                
049400 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDL801'.                      
049500 01  DLI-IO-WDL801.                                                       
049600*    03  -COPY WDL801                                                     
049700     EJECT                                                                
049800 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDL811'.                      
049900 01  DLI-IO-WDL811.                                                       
050000*    03  -COPY WDL811                                                     
050100     EJECT                                                                
050200 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD701'.                      
050300 01  DLI-IO-WDD701.                                                       
050400*    03  -COPY WDD701                                                     
050500     EJECT                                                                
050600 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD702'.                      
050700 01  DLI-IO-WDD702.                                                       
050800*    03  -COPY WDD702                                                     
050900     EJECT                                                                
051000 01  FILLER         PIC X(26) VALUE 'DLI-IO-WDR201-2243'.                 
051100 01  DLI-IO-WDR201-2243.                                                  
051200*    03  -COPY WDGX01                                                     
051300     EJECT                                                                
051400 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX2244'.                    
051500 01  DLI-IO-WDGX2244.                                                     
051600*    03  -COPY WDGX2244                                                   
051700     EJECT                                                                
051800 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDJ1'.                        
051900 01  DLI-IO-WDJ1.                                                         
052000*    03  -COPY WDJ111 -PRE SATS-                                          
052100*    03  -COPY WDJ101 -PRE SATS-                                          
052200     EJECT                                                                
052300 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD901'.                      
052400 01  DLI-IO-WDD901.                                                       
052500*    03  -COPY WDD901  -PRE   LEV-                                        
052600     EJECT                                                                
052700 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD902'.                      
052800 01  DLI-IO-WDD902.                                                       
052900*    03  -COPY WDD902  -PRE   LEV-                                        
053000     EJECT                                                                
053100 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD3'.                        
053200 01  DLI-IO-WDD3.                                                         
053300*03  WLBENA11  -COPY WDD311  -PRE  BENA11-                                
053400     EJECT                                                                
053500 01  FILLER                      PIC X(16)  VALUE 'SQLCA-AREA'.           
053600       EXEC SQL INCLUDE SQLCA END-EXEC.                                   
053700                                                                          
053800 01  FILLER                      PIC X(16)  VALUE 'SQLCODE-WS'.           
053900 01  DB2-WS.                                                              
054000     03  SQLCODE-WS              PIC 9(3)   VALUE ZERO.                   
054100         88  CURSOR-OK                      VALUE 000.                    
054200         88  LINES-FOUND                    VALUE 000.                    
054300         88  LINES-MISSING                  VALUE 100.                    
054400         88  RESOURCE-WRONG                 VALUE 904.                    
054500     03  GOOD-SQLCODECODES.                                               
054600         05  GOOD-SQLCODE OCCURS 5                                        
054700             INDEXED BY SQLCODE-IX PIC 9(3).                              
054800                                                                          
054900 01  WS.                                                                  
055000     03 WS-SECTION               PIC X(24)   VALUE SPACE.                 
055100     03 FILLER                   PIC X(16)   VALUE                        
055200                                             'WS-DB2-SEKTION'.            
055300     03 WS-DB2-SEKTION           PIC X(24)   VALUE SPACE.                 
055400     EJECT                                                                
055500 01  FILLER                      PIC X(16)  VALUE 'TP1KAMP-AREA'.         
055600                                                                          
055700*01  -COPY TP1KAMP -PRE TP1KAMP-                                          
055800     EJECT                                                                
055900 01  FILLER                      PIC X(16)  VALUE 'TP1ARTK-AREA'.         
056000                                                                          
056100*01  -COPY TP1ARTK -PRE TP1ARTK-                                          
056200     EJECT                                                                
056300     EXEC SQL INCLUDE TP1KAMP END-EXEC.                                   
056400     EJECT                                                                
056500     EXEC SQL INCLUDE TP1ARTK END-EXEC.                                   
056600     EJECT                                                                
056700 LINKAGE SECTION.                                                         
056800                                                                          
056900*01  -COPY W0009   -PRE MSG-                                              
057000                                                                          
057100*01  -COPY W0008  -PRE WDK6-                                              
057200     05  FILLER                  PIC X.                                   
057300                                                                          
057400*01  -COPY W0008  -PRE WDK62-                                             
057500     05  FILLER                  PIC X.                                   
057600                                                                          
057700*01  -COPY W0008  -PRE WDK9-                                              
057800     05  FILLER                  PIC X.                                   
057900                                                                          
058000*01  -COPY W0008  -PRE 6321-                                              
058100     05  FILLER                  PIC X.                                   
058200                                                                          
058300*01  -COPY W0008  -PRE 6327-                                              
058400     05  FILLER                  PIC X.                                   
058500                                                                          
058600*01  -COPY W0008  -PRE WDN6-                                              
058700     05  FILLER                  PIC X.                                   
058800                                                                          
058900*01  -COPY W0008  -PRE WDQ4B-                                             
059000     05  FILLER                  PIC X.                                   
059100                                                                          
059200*01  -COPY W0008  -PRE WDQ4-                                              
059300     05  FILLER                  PIC X.                                   
059400                                                                          
059500*01  -COPY W0008  -PRE WDL8-                                              
059600     05  FILLER                  PIC X.                                   
059700                                                                          
059800*01  -COPY W0008  -PRE WDD7-                                              
059900     05  FILLER                  PIC X.                                   
060000                                                                          
060100*01  -COPY W0008  -PRE 2243-                                              
060200     05  FILLER                  PIC X.                                   
060300                                                                          
060400*01  -COPY W0008  -PRE WDJ1C-                                             
060500     05  FILLER                  PIC X.                                   
060600                                                                          
060700*01  -COPY W0008  -PRE WDD9-                                              
060800     05  FILLER                  PIC X.                                   
060900                                                                          
061000*01  -COPY W0008  -PRE WDD3-                                              
061100     05  FILLER                  PIC X.                                   
061200     EJECT                                                                
061300 PROCEDURE DIVISION  USING MSG-PCB                                        
061400           WDK6-PCB WDK62-PCB  WDK9-PCB                                   
061500                     6321-PCB  6327-PCB                                   
061600           WDN6-PCB  WDQ4B-PCB                                            
061700           WDQ4-PCB  WDL8-PCB  WDD7-PCB 2243-PCB                          
061800           WDJ1C-PCB                                                      
061900           WDD9-PCB  WDD3-PCB.                                            
062000 MAIN SECTION.                                                            
062100     ENTRY 'DLITCBL' USING MSG-PCB                                        
062200           WDK6-PCB WDK62-PCB  WDK9-PCB                                   
062300                     6321-PCB  6327-PCB                                   
062400           WDN6-PCB  WDQ4B-PCB                                            
062500           WDQ4-PCB  WDL8-PCB  WDD7-PCB 2243-PCB                          
062600           WDJ1C-PCB                                                      
062700           WDD9-PCB  WDD3-PCB.                                            
062800                                                                          
062900     SKIP2                                                                
063000     PERFORM A-INIT                                                       
063100     PERFORM C-KOLLA-IDUSER                                               
063200     PERFORM S01-LAES-W01160                                              
063300     PERFORM UNTIL END-OF-W01160                                          
063400       IF CHKP-ANT > CHKP-MAX                                             
063500         PERFORM X-TAG-CHECKPOINT                                         
063600       END-IF                                                             
063700       MOVE IN-CLAG-IDARTNR TO W-IDARTNR                                  
063800                               IDARTNR-WS                                 
063810       MOVE IN-CLAG-KDPRODSL       TO TEST-KDPRODSL                       
063900       IF (ANT-SKROTU < MAX-ANT-SKROTU)        AND                        
064000          (IN-CLAG-KVLS > ZERO)                AND                        
064100          (IN-CLAG-KDERS-UTG = ZERO)           AND                        
064200          (IN-CLAG-KVROS = ZERO)               AND                        
064300          (IN-CLAG-KDERS = 01)                 AND                        
064400          (IN-CLAG-IDLEVNR NOT = 'BQ8VA')                                 
064410**        NOT KDPRODSL-LYNK                                               
064500                                                                          
064600          PERFORM IMS-GET-WDK601                                          
064700          PERFORM IMS-GET-WDK611                                          
064800          IF  SEGMENT-FINNS                    AND                        
064900              CLAG-TISKROT-AUTO < DAGENS-DATUM AND                        
065000              CLAG-FLSKROT-BEORD NOT = JA                                 
065100             MOVE JA TO SW-KOLL                                           
065200****        *LÄS IN ERSATT ARTIKELS OI I TABB-OI                          
065300             PERFORM F-LAES-OI                                            
065400             PERFORM G-KOLL-OI-ERSATT                                     
065500             IF SW-KOLL = JA                                              
065600                PERFORM IMS-GU-WDD701                                     
065700                IF SEGMENT-FINNS AND SW-KOLL = JA                         
065800                   PERFORM IMS-GNP-WDD702                                 
065900                   IF SEGMENT-SAKNAS OR SEGMENT-SLUT                      
066000                      MOVE NEJ TO SW-KOLL                                 
066100                   END-IF                                                 
066200                   PERFORM UNTIL SEGMENT-SAKNAS OR SEGMENT-SLUT           
066300                                 OR SW-KOLL = NEJ                         
066400*******             *GÖR KONTROLL FÖR VARJE TILLKOMMANDE ARTIKEL          
066500*******             *LÄS IN TILLK ARTIKELS OI I TAB                       
066600                     MOVE IDARTNR-TILLK TO W-IDARTNR                      
066700                     PERFORM F-LAES-OI                                    
066800                     PERFORM E-KONTROLL-TILLKOMMANDE                      
066900                     MOVE IDARTNR-WS    TO W-IDARTNR                      
067000                     PERFORM IMS-GNP-WDD702                               
067100                   END-PERFORM                                            
067200                                                                          
067300                   IF SW-KOLL = JA                                        
067400                      PERFORM B-LAES-SKROTINFO                            
067500                      PERFORM D-UPPDATERA                                 
067600                   END-IF                                                 
067700                END-IF                                                    
067800             END-IF                                                       
067900          END-IF                                                          
068000       END-IF                                                             
068100       PERFORM S01-LAES-W01160                                            
068200     END-PERFORM                                                          
068300                                                                          
068400                                                                          
068500     PERFORM Z-FINIT                                                      
068600                                                                          
068700     MOVE ZERO TO RETURN-CODE                                             
068800     GOBACK                                                               
068900     .                                                                    
069000     EJECT                                                                
069100 A-INIT SECTION.                                                          
069200     SKIP2                                                                
069300                                                                          
069400     PERFORM IMS-RESTART                                                  
069500                                                                          
069600     OPEN INPUT W01160                                                    
069700                                                                          
069800                                                                          
069900     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
070000                                                                          
070100     ACCEPT W-DATUM-DATE FROM DATE                                        
070200     ACCEPT DAGENS-DATUM FROM DATE                                        
070300                                                                          
070400     MOVE FUNCTION CURRENT-DATE (1:8) TO DAGENS-DATUM-Y2K                 
070500                                                                          
070600     MOVE 'IDAG' TO DAT-KDDATFORM                                         
070700     CALL WDATKONV USING DAT-KDDATFORM,                                   
070800                         DAT-I-TIDATUM,                                   
070900                         DAT-O-TIDATUM,                                   
071000                         DAT-KDSVAR                                       
071100                                                                          
071200     IF DAT-KDSVAR-FEL                                                    
071300        DISPLAY '****  FEL I WDATKONV  *******'                           
071400        CALL FELLOG                                                       
071500     END-IF                                                               
071600                                                                          
071700     MOVE DAT-TIVV       TO WS-TIVV                                       
071800                            INNEV-TIVV                                    
071900     MOVE DAT-TISEKEL    TO WS-TISEKEL                                    
072000     MOVE DAT-TIAA-VECKA TO WS-TIAA-VECKA                                 
072100     MOVE WS-TIAAAA      TO WS-TIAAAA-1                                   
072200     SUBTRACT +1         FROM WS-TIAAAA-1                                 
072300     MOVE DAT-TIAAVVD    TO WS-TIAAVVD-D                                  
072400     MOVE ZERO           TO WS-TID-D                                      
072500     MOVE WS-TIAAVVD-D   TO WS-TIAAVVD                                    
072600     MOVE DAT-TISEKEL    TO WS-TISS                                       
072700     MOVE DAT-TIAAPP     TO INNEV-TIAAPP                                  
072800     MOVE DAT-KVVIPER    TO INNEV-KVVIPER                                 
072900                                                                          
073000     PERFORM AA-INIT-PERIOD                                               
073100                                                                          
073200     PERFORM AB-INIT-KVPERIOD                                             
073300                                                                          
073400     PERFORM AC-JUST-INNEV-PERIOD                                         
073500     .                                                                    
073600     EJECT                                                                
073700 AA-INIT-PERIOD   SECTION.                                                
073800                                                                          
073900*****    *KNYT IHOP VECKA OCH PERIOD FÖR SISTA ÅRENS OI                   
074000*****    *VI SKALL SENARE ANVÄNDA OLIKA ANTAL PERIODERS OI BAKÅT          
074100*****    *I TABELLERNA TAB0- TAB1- OCH TAB2-                              
074200                                                                          
074300     MOVE DAT-TISEKEL TO ARB-TISEKEL                                      
074400     MOVE DAT-TIAAPP  TO ARB-TIAAPP                                       
074500*   *STARTPUNKT FÖR OI => ÅR= ARB-TIAA + PER= ARB-TIPP  OCH BACKA         
074600                                                                          
074700*   *LÄGG UPP TABELL MED VECKOR - PERIOD                                  
074800     MOVE 'AAVV '   TO DAT-KDDATFORM                                      
074900                                                                          
075000*   *ÅR 0                                                                 
075100     MOVE ARB-TIAA  TO TIAA                                               
075200     MOVE 1 TO IX-TAB                                                     
075300     PERFORM UNTIL IX-TAB > 52                                            
075400        MOVE IX-TAB    TO TIVV                                            
075500        MOVE TIAAVV    TO DAT-I-TIDATUM                                   
075600        CALL WDATKONV USING DAT-KDDATFORM,                                
075700                            DAT-I-TIDATUM,                                
075800                            DAT-O-TIDATUM,                                
075900                            DAT-KDSVAR                                    
076000        IF DAT-KDSVAR-FEL                                                 
076100           DISPLAY '****  FEL I WDATKONV 0   ****'                        
076200           CALL FELLOG                                                    
076300        END-IF                                                            
076400        MOVE DAT-TIAAVV-GRP TO TAB0-TIAAVV (IX-TAB)                       
076500        MOVE DAT-TIAAPP     TO TAB0-TIAAPP (IX-TAB)                       
076600        MOVE DAT-KVVIPER    TO TAB0-KVVIPER(IX-TAB)                       
076700        ADD  +1             TO IX-TAB                                     
076800     END-PERFORM                                                          
076900     MOVE IX-TAB    TO TIVV                                               
077000     MOVE TIAAVV    TO DAT-I-TIDATUM                                      
077100     CALL WDATKONV USING DAT-KDDATFORM,                                   
077200                         DAT-I-TIDATUM,                                   
077300                         DAT-O-TIDATUM,                                   
077400                         DAT-KDSVAR                                       
077500     IF DAT-KDSVAR-FEL                                                    
077600        MOVE ZERO           TO TAB0-TIAAVV (IX-TAB)                       
077700        MOVE ZERO           TO TAB0-TIAAPP (IX-TAB)                       
077800        MOVE ZERO           TO TAB0-KVVIPER(IX-TAB)                       
077900     ELSE                                                                 
078000        MOVE DAT-TIAAVV-GRP TO TAB0-TIAAVV (IX-TAB)                       
078100        MOVE DAT-TIAAPP     TO TAB0-TIAAPP (IX-TAB)                       
078200        MOVE DAT-KVVIPER    TO TAB0-KVVIPER(IX-TAB)                       
078300     END-IF                                                               
078400                                                                          
078500*   *ÅR -1                                                                
078600     SUBTRACT 1 FROM   TIAA                                               
078700     MOVE 1 TO IX-TAB                                                     
078800     PERFORM UNTIL IX-TAB > 52                                            
078900        MOVE IX-TAB    TO TIVV                                            
079000        MOVE TIAAVV    TO DAT-I-TIDATUM                                   
079100        CALL WDATKONV USING DAT-KDDATFORM,                                
079200                            DAT-I-TIDATUM,                                
079300                            DAT-O-TIDATUM,                                
079400                            DAT-KDSVAR                                    
079500        IF DAT-KDSVAR-FEL                                                 
079600           DISPLAY '****  FEL I WDATKONV -1  ****'                        
079700           CALL FELLOG                                                    
079800        END-IF                                                            
079900        MOVE DAT-TIAAVV-GRP TO TAB1-TIAAVV (IX-TAB)                       
080000        MOVE DAT-TIAAPP     TO TAB1-TIAAPP (IX-TAB)                       
080100        MOVE DAT-KVVIPER    TO TAB1-KVVIPER(IX-TAB)                       
080200        ADD  +1             TO IX-TAB                                     
080300     END-PERFORM                                                          
080400     MOVE IX-TAB    TO TIVV                                               
080500     MOVE TIAAVV    TO DAT-I-TIDATUM                                      
080600     CALL WDATKONV USING DAT-KDDATFORM,                                   
080700                         DAT-I-TIDATUM,                                   
080800                         DAT-O-TIDATUM,                                   
080900                         DAT-KDSVAR                                       
081000     IF DAT-KDSVAR-FEL                                                    
081100        MOVE ZERO           TO TAB1-TIAAVV (IX-TAB)                       
081200        MOVE ZERO           TO TAB1-TIAAPP (IX-TAB)                       
081300        MOVE ZERO           TO TAB1-KVVIPER(IX-TAB)                       
081400     ELSE                                                                 
081500        MOVE DAT-TIAAVV-GRP TO TAB1-TIAAVV (IX-TAB)                       
081600        MOVE DAT-TIAAPP     TO TAB1-TIAAPP (IX-TAB)                       
081700        MOVE DAT-KVVIPER    TO TAB1-KVVIPER(IX-TAB)                       
081800     END-IF                                                               
081900                                                                          
082000*   *ÅR -2                                                                
082100     SUBTRACT 1 FROM   TIAA                                               
082200     MOVE 1 TO IX-TAB                                                     
082300     PERFORM UNTIL IX-TAB > 52                                            
082400        MOVE IX-TAB    TO TIVV                                            
082500        MOVE TIAAVV    TO DAT-I-TIDATUM                                   
082600        CALL WDATKONV USING DAT-KDDATFORM,                                
082700                            DAT-I-TIDATUM,                                
082800                            DAT-O-TIDATUM,                                
082900                            DAT-KDSVAR                                    
083000        IF DAT-KDSVAR-FEL                                                 
083100           DISPLAY '****  FEL I WDATKONV -2  ****'                        
083200           CALL FELLOG                                                    
083300        END-IF                                                            
083400        MOVE DAT-TIAAVV-GRP TO TAB2-TIAAVV (IX-TAB)                       
083500        MOVE DAT-TIAAPP     TO TAB2-TIAAPP (IX-TAB)                       
083600        MOVE DAT-KVVIPER    TO TAB2-KVVIPER(IX-TAB)                       
083700        ADD  +1             TO IX-TAB                                     
083800     END-PERFORM                                                          
083900     MOVE IX-TAB    TO TIVV                                               
084000     MOVE TIAAVV    TO DAT-I-TIDATUM                                      
084100     CALL WDATKONV USING DAT-KDDATFORM,                                   
084200                         DAT-I-TIDATUM,                                   
084300                         DAT-O-TIDATUM,                                   
084400                         DAT-KDSVAR                                       
084500     IF DAT-KDSVAR-FEL                                                    
084600        MOVE ZERO           TO TAB2-TIAAVV (IX-TAB)                       
084700        MOVE ZERO           TO TAB2-TIAAPP (IX-TAB)                       
084800        MOVE ZERO           TO TAB2-KVVIPER(IX-TAB)                       
084900     ELSE                                                                 
085000        MOVE DAT-TIAAVV-GRP TO TAB2-TIAAVV (IX-TAB)                       
085100        MOVE DAT-TIAAPP     TO TAB2-TIAAPP (IX-TAB)                       
085200        MOVE DAT-KVVIPER    TO TAB2-KVVIPER(IX-TAB)                       
085300     END-IF                                                               
085400     .                                                                    
085500     EJECT                                                                
085600 AB-INIT-KVPERIOD   SECTION.                                              
085700                                                                          
085800*****    *FYLL TABELL FRÅN BILD 2148 MED ANT.PERIODER/PRODUKTSLAG         
085900*****    *DVS PRODUKTSLAG MED DIV. ANTAL PERIODER OI                      
086000*****    *NOLLA TABKV- FÖRST                                              
086100                                                                          
086200     MOVE +1                         TO IX                                
086300     PERFORM UNTIL IX > 99                                                
086400        MOVE ZERO                    TO TABKV-KDPRODSL       (IX)         
086500        MOVE ZERO                    TO TABKV-KVPERIOD-BERS  (IX)         
086600        MOVE ZERO                    TO TABKV-KVPERIOD-BTILLK(IX)         
086700        MOVE ZERO                    TO TABKV-KVPERIOD-VERS  (IX)         
086800        MOVE ZERO                    TO TABKV-KVPERIOD-HERS  (IX)         
086900        ADD +1                       TO IX                                
087000     END-PERFORM                                                          
087100                                                                          
087200     PERFORM IMS-GET-WDR201-2243                                          
087300     IF SEGMENT-FINNS                                                     
087400        MOVE +1                      TO IX                                
087500        PERFORM IMS-GET-WDGX2244                                          
087600        PERFORM UNTIL SEGMENT-SAKNAS                                      
087700           MOVE 2244-KDPRODSL        TO TABKV-KDPRODSL       (IX)         
087800           MOVE 2244-KVPERIOD-BERS   TO TABKV-KVPERIOD-BERS  (IX)         
087900           MOVE 2244-KVPERIOD-BTILLK TO TABKV-KVPERIOD-BTILLK(IX)         
088000           MOVE 2244-KVPERIOD-VERS   TO TABKV-KVPERIOD-VERS  (IX)         
088100           MOVE 2244-KVPERIOD-HERS   TO TABKV-KVPERIOD-HERS  (IX)         
088200           ADD +1                    TO IX                                
088300           PERFORM IMS-GET-WDGX2244                                       
088400        END-PERFORM                                                       
088500     END-IF                                                               
088600     .                                                                    
088700     EJECT                                                                
088800 AC-JUST-INNEV-PERIOD  SECTION.                                           
088900                                                                          
089000*   *VAR I INNEVARANDE PERIOD BEFINNER VI OSS                             
089100     MOVE 'AAPP '        TO DAT-KDDATFORM                                 
089200     MOVE INNEV-TIAAPP   TO DAT-I-TIDATUM                                 
089300     CALL WDATKONV USING DAT-KDDATFORM,                                   
089400                         DAT-I-TIDATUM,                                   
089500                         DAT-O-TIDATUM,                                   
089600                         DAT-KDSVAR                                       
089700     IF DAT-KDSVAR-FEL                                                    
089800        DISPLAY '****  FEL I WDATKONV INNEV **'                           
089900        CALL FELLOG                                                       
090000     END-IF                                                               
090100*    STARTVECKA I PERIODEN = DAT-TIVV                                     
090200*    ANTAL VECKOR HITTILLS I INNEV PER:                                   
090300     COMPUTE INNEV-HIT-VV = INNEV-TIVV - DAT-TIVV + 1                     
090400*    ANTAL VECKOR KVAR I PERIODEN:                                        
090500     COMPUTE INNEV-KVAR-VV = INNEV-KVVIPER - INNEV-HIT-VV                 
090600                                                                          
090700     .                                                                    
090800     EJECT                                                                
090900 B-LAES-SKROTINFO SECTION.                                                
091000                                                                          
091100        MOVE IDARTNR-WS       TO W-IDARTNR                                
091200                                                                          
091300        MOVE CLAG-KDERS       TO WARTC-KDERS-C1                           
091400        MOVE CLAG-KVLS        TO WARTC-KVLS-C1                            
091500        MOVE CLAG-KVRESS      TO WARTC-KVRESS-C1                          
091600        MOVE CLAG-IDANSK      TO WARTC-IDANSK                             
091700        MOVE CLAG-KVSPANT     TO WARTC-KVSPANT                            
091800                                                                          
091900        PERFORM IMS-GET-WDK901                                            
092000        IF SEGMENT-FINNS                                                  
092100                                                                          
092200           COMPUTE WS-KVOKS-C1 = ARTM-ART-KVOKS-BULK   +                  
092300                                 ARTM-ART-KVOKS-DAG    +                  
092400                                 ARTM-ART-KVOKS-VOR                       
092500        ELSE                                                              
092600           MOVE ZERO          TO WS-KVOKS-C1                              
092700        END-IF                                                            
092800     .                                                                    
092900     EJECT                                                                
093000 C-KOLLA-IDUSER SECTION.                                                  
093100                                                                          
093200     MOVE WC-CDC-SE              TO W-IDDC-6327                           
093300     MOVE 'ANSK'                 TO W-KDARBTYP-6327                       
093400     MOVE SPACE                  TO WS-SPAR-BEANST-GODK                   
093500     PERFORM IMS-GET-WDR501-6327                                          
093600     IF SEGMENT-FINNS                                                     
093700        MOVE WS-AUTO-USERID      TO W-IDUSER-GODK                         
093800        PERFORM IMS-GET-WDGX6328                                          
093900        IF SEGMENT-FINNS                                                  
094000           MOVE 6328-BEANST-GODK TO WS-SPAR-BEANST-GODK                   
094100           MOVE 6328-SUBEL       TO WS-MAX-SUBEL                          
094200        END-IF                                                            
094300     END-IF                                                               
094400     .                                                                    
094500     EJECT                                                                
094600 D-UPPDATERA SECTION.                                                     
094700                                                                          
094800     MOVE IDARTNR-WS TO W-IDARTNR                                         
094900                                                                          
095000     PERFORM DA-SKAPA-KVSKRANT                                            
095100     IF WKVSKRANT > ZERO                                                  
095200        PERFORM DB-UPPD-SKROTSPARR                                        
095300        PERFORM DC-SKAPA-HANDELSETR-6321                                  
095400        PERFORM DE-SKAPA-TEMEMO                                           
095500     END-IF                                                               
095600                                                                          
095700     .                                                                    
095800     EJECT                                                                
095900 DA-SKAPA-KVSKRANT SECTION.                                               
096000                                                                          
096100     COMPUTE WKVSKRANT =                                                  
096200             WARTC-KVLS-C1 - WARTC-KVRESS-C1                              
096300                           - WS-KVOKS-C1                                  
096400                                                                          
096500     MOVE 11                  TO 6324-IDKUNDNR                            
096600                                 WS-IDKUNDNR                              
096700     MOVE ZERO                TO WS-KVBEART                               
096800     MOVE W-IDARTNR           TO W-IDARTNR-Q4B1-MAX                       
096900                                 W-IDARTNR-Q4B1-MIN                       
097000     PERFORM IMS-GU-WDQ4B1                                                
097100     PERFORM UNTIL SEGMENT-SAKNAS                                         
097200        IF SEQB-IDDISTR = 81 AND                                          
097300          (SEQB-IDKUNDNR = 11 OR 111)                                     
097400           MOVE SEQB-IDWDQ401 TO W-IDWDQ401                               
097500           MOVE SEQB-IDLOPNR  TO W-IDLOPNR-Q4B1                           
097600           PERFORM IMS-GU-WDQ401                                          
097700           IF SEGMENT-FINNS                                               
097800              ADD ORAD-KVBEART-Q TO WS-KVBEART                            
097900           END-IF                                                         
098000        END-IF                                                            
098100        PERFORM IMS-GN-WDQ4B1                                             
098200     END-PERFORM                                                          
098300     IF WS-KVBEART > ZERO                                                 
098400        COMPUTE WKVSKRANT = WKVSKRANT - WS-KVBEART                        
098500     END-IF                                                               
098600                                                                          
098700     .                                                                    
098800     EJECT                                                                
098900 DB-UPPD-SKROTSPARR SECTION.                                              
099000                                                                          
099100     PERFORM IMS-GET-WDK601                                               
099200     PERFORM IMS-GET-WDK611                                               
099300     MOVE JA  TO CLAG-FLSKROT-BEORD                                       
099400     MOVE NEJ TO CLAG-FLSKROT-AUTO                                        
099500                                                                          
099600     PERFORM IMS-REPL-WDK611                                              
099700                                                                          
099800     PERFORM IMS-GET-WDK627                                               
099900                                                                          
100000     IF SEGMENT-FINNS                                                     
100100         MOVE W-DATUM-DATE            TO SKROT-TISKROT-BEORD              
100200                                                                          
100300         PERFORM IMS-REPL-WDK627                                          
100400     ELSE                                                                 
100500         MOVE ZERO                    TO SKROT-KVSKROT                    
100600         MOVE ZERO                    TO SKROT-DASKROT                    
100700         MOVE W-DATUM-DATE            TO SKROT-TISKROT-BEORD              
100800                                                                          
100900                                                                          
101000         PERFORM IMS-ISRT-WDK627                                          
101100     END-IF                                                               
101200     .                                                                    
101300     EJECT                                                                
101400 DC-SKAPA-HANDELSETR-6321 SECTION.                                        
101500                                                                          
101600     MOVE WC-CDC-SE         TO W-IDDC-KVAL                                
101700     MOVE 'ANSK'            TO W-KDARBTYP                                 
101800     PERFORM IMS-GU-WDR501-6321                                           
101900     IF SEGMENT-SAKNAS                                                    
102000        MOVE '6321'         TO 6321-IDHTYP                                
102100        MOVE 'ANSK'         TO 6321-KDARBTYP                              
102200        MOVE LOW-VALUE      TO 6321-LOW-VALUE                             
102300        PERFORM IMS-ISRT-WDR501-6321                                      
102400                                                                          
102500     END-IF                                                               
102600                                                                          
102700     COMPUTE W-DASKROT9-BEORD = 99999999 - DAGENS-DATUM-Y2K               
102800     MOVE W-DASKROT9-BEORD  TO 6322-DASKROT9-BEORD                        
102900     PERFORM IMS-ISRT-WDGX6322                                            
103000                                                                          
103100     MOVE IDARTNR-WS        TO 6324-IDARTNR                               
103200     MOVE WC-CDC-SE         TO 6324-IDDC                                  
103300     MOVE 1                 TO 6324-KDSTASKR                              
103400     MOVE NEJ               TO 6324-FLSKROT-GODK                          
103500     MOVE WARTC-IDANSK      TO 6324-IDPERSON                              
103600     MOVE 81                TO 6324-IDDISTR                               
103700     MOVE 11                TO 6324-IDKUNDNR                              
103800                                 WS-IDKUNDNR                              
103900     MOVE WS-AUTO-USERID    TO 6324-IDUSER                                
104000     MOVE ZERO              TO 6324-KDFRAKT                               
104100     MOVE 1                 TO 6324-KDORDKL                               
104200     MOVE WKVSKRANT         TO 6324-KVSKROT-BEORD                         
104300     MOVE SPACE             TO 6324-IDANALYS                              
104400                               6324-FLJUSTBUFF                            
104500                               6324-IDKST                                 
104600     MOVE ZERO              TO 6324-IDKONTO                               
104700                               6324-KVSKROT-KVAR                          
104800     MOVE ZERO              TO 6324-KVSKROT-ONDEM                         
104900     MOVE WS-SPAR-BEANST-GODK  TO 6324-BEANST                             
105000     COMPUTE WS-SUARTSTD ROUNDED =                                        
105100             6324-KVSKROT-BEORD * CLAG-PRARTSTD                           
105200                                                                          
105300     MOVE 'AUTOMATBEORDRAD' TO 6324-BELAGINS-DEL                          
105400*****                                                                     
105500     MOVE CLAG-KDERS        TO 6324-KDERS-UTG                             
105600                                                                          
105700     COMPUTE 6324-KVTILLG-CDC ROUNDED =                                   
105800            CLAG-KVLS     - CLAG-KVRESS                                   
105900                          - CLAG-KVROS                                    
106000                          - WS-KVOKS-C1                                   
106100                                                                          
106200     COMPUTE 6324-KVTILLG-SDC ROUNDED =                                   
106300            W-SDC-KVLS - W-SDC-KVOKS                                      
106400                                                                          
106500     COMPUTE 6324-KVAKS-CDC ROUNDED =                                     
106600          CLAG-KVAKS-CDC  + CLAG-KVAKS-PAV                                
106700                          + CLAG-KVAKS-T                                  
106800                                                                          
106900     COMPUTE 6324-KVAKS-SDC ROUNDED =                                     
107000          W-SDC-KVAKS                                                     
107100                                                                          
107200     PERFORM IMS-GET-WDK901                                               
107300     IF SEGMENT-FINNS                                                     
107400       COMPUTE 6324-SUTPO-TOT =                                           
107500                ARTM-ART-SUTPO-TOT                                        
107600     ELSE                                                                 
107700       MOVE ZERO              TO 6324-SUTPO-TOT                           
107800     END-IF                                                               
107900                                                                          
108000     PERFORM DCB-LAS-FLYTTA-WDN6                                          
108100*****                                                                     
108200                                                                          
108300     PERFORM IMS-ISRT-WDGX6324                                            
108400     ADD +1 TO ANT-SKROTU                                                 
108500*****                                                                     
108600     .                                                                    
108700     EJECT                                                                
108800 DCB-LAS-FLYTTA-WDN6 SECTION.                                             
108900     MOVE +1 TO IX                                                        
109000     PERFORM UNTIL IX > 20                                                
109100        MOVE SPACE        TO 6324-BEEMBLEM (IX)                           
109200        ADD +1        TO IX                                               
109300     END-PERFORM                                                          
109400     PERFORM IMS-GET-WDN601                                               
109500     IF SEGMENT-FINNS                                                     
109600        PERFORM IMS-GET-WDN611                                            
109700        MOVE +1 TO IX                                                     
109800        PERFORM UNTIL IX > 20 OR SEGMENT-SAKNAS                           
109900           MOVE KAT-BEEMBLEM TO 6324-BEEMBLEM (IX)                        
110000           ADD +1        TO IX                                            
110100           PERFORM IMS-GET-WDN611                                         
110200        END-PERFORM                                                       
110300     END-IF                                                               
110400     .                                                                    
110500     EJECT                                                                
110600 DE-SKAPA-TEMEMO    SECTION.                                              
110700                                                                          
110800     MOVE SPACE                      TO WS-TEMEMO                         
110900     MOVE ZERO                       TO IX-RAD                            
111000     ADD +1                       TO IX-RAD                               
111100     MOVE '01-MÄRKT      '        TO WS-TEMEMO                            
111200     PERFORM DEA-ISRT-6325                                                
111300                                                                          
111400     MOVE SPACE                      TO WS-TEMEMO                         
111500     IF CLAG-KVSPANT > ZERO                                               
111600        ADD +1                       TO IX-RAD                            
111700        MOVE 'SPÄRRAD KVANT '        TO WS-TEMEMO                         
111800        PERFORM DEA-ISRT-6325                                             
111900     END-IF                                                               
112000                                                                          
112100     IF ART-FLIART = JA                                                   
112200        ADD +1                       TO IX-RAD                            
112300        MOVE 'INGÅR I SATS '         TO WS-TEMEMO                         
112400***TESTA FÖRST OM SJÄLVA SATSEN ÄR 09-MÄRKT, ANNARS                       
112500        MOVE NEJ TO SW-SATS-OK                                            
112600        PERFORM DEB-KOLLA-SATS                                            
112700        IF SW-SATS-OK = JA                                                
112800           PERFORM DEA-ISRT-6325                                          
112900        END-IF                                                            
113000     END-IF                                                               
113100                                                                          
113200     MOVE ART-KDPRODSL               TO TEST-KDPRODSL                     
113300     IF KDPRODSL-VCBV-PARTS                                               
113400        ADD +1                       TO IX-RAD                            
113500        MOVE '300-/400-SERIEN '      TO WS-TEMEMO                         
113600        PERFORM DEA-ISRT-6325                                             
113700     END-IF                                                               
113800                                                                          
113900     IF KDPRODSL-BYTES                                                    
114000        ADD +1                       TO IX-RAD                            
114100        MOVE 'BYTES'                 TO WS-TEMEMO                         
114200        PERFORM DEA-ISRT-6325                                             
114300     END-IF                                                               
114400                                                                          
114500     IF KDPRODSL-ACC OR KDPRODSL-WHEELS OR                                
114600        KDPRODSL-BRANDON OR KDPRODSL-VCBV-WHEELS                          
114700        ADD +1                       TO IX-RAD                            
114800        MOVE 'TILLBEHÖR       '      TO WS-TEMEMO                         
114900        PERFORM DEA-ISRT-6325                                             
115000     END-IF                                                               
115100                                                                          
115200     MOVE ZERO TO WS-KVBR                                                 
115300     MOVE W-IDARTNR TO W-IDARTNR-D9                                       
115400     MOVE WC-CDC-SE TO W-IDDC-D9                                          
115500     PERFORM IMS-GET-WDD901                                               
115600     IF SEGMENT-FINNS                                                     
115700        PERFORM IMS-GET-WDD902                                            
115800        PERFORM UNTIL SEGMENT-SAKNAS                                      
115900           ADD LEV-KVBR    TO WS-KVBR                                     
116000           PERFORM IMS-GET-WDD902                                         
116100        END-PERFORM                                                       
116200     END-IF                                                               
116300     IF WS-KVBR > ZERO                                                    
116400        ADD +1                       TO IX-RAD                            
116500        MOVE 'BESTÄLLNINGSREST '     TO WS-TEMEMO                         
116600        PERFORM DEA-ISRT-6325                                             
116700     END-IF                                                               
116800                                                                          
116900     MOVE NEJ                        TO SW-KAMPANJ                        
117000     PERFORM DEC-KOLLA-KAMPANJ-DB2                                        
117100     IF SW-KAMPANJ = JA                                                   
117200        ADD +1                       TO IX-RAD                            
117300        MOVE 'KAMPANJ '              TO WS-TEMEMO                         
117400        PERFORM DEA-ISRT-6325                                             
117500     END-IF                                                               
117600                                                                          
117700     IF ART-IDFKNGRP > 1000 AND                                           
117800        ART-IDFKNGRP < 2000                                               
117900        ADD +1                       TO IX-RAD                            
118000        MOVE 'STANDARD'              TO WS-TEMEMO                         
118100        PERFORM DEA-ISRT-6325                                             
118200     END-IF                                                               
118300                                                                          
118400     IF ART-IDFKNGRP > 8840 AND                                           
118500        ART-IDFKNGRP < 8849                                               
118600        ADD +1                       TO IX-RAD                            
118700        MOVE 'SÄKERHETSPRODUKT  '    TO WS-TEMEMO                         
118800        PERFORM DEA-ISRT-6325                                             
118900     END-IF                                                               
119000                                                                          
119100     PERFORM DED-KOLLA-DEKAL                                              
119200     .                                                                    
119300     EJECT                                                                
119400 DEA-ISRT-6325      SECTION.                                              
119500                                                                          
119600     MOVE IX-RAD                  TO 6325-IDRADNR                         
119700     MOVE WS-TEMEMO               TO 6325-TEMEMO                          
119800     MOVE 6321-KDARBTYP           TO W-KDARBTYP                           
119900     MOVE 6322-DASKROT9-BEORD     TO W-DASKROT9-BEORD                     
120000     MOVE 6324-IDARTNR            TO W-IDARTNR-KVAL                       
120100     MOVE 6324-KDSTASKR           TO W-KDSTASKR-KVAL                      
120200     MOVE 6324-IDDC               TO W-IDDC-KVAL                          
120300     PERFORM IMS-ISRT-WDGX6325                                            
120400     .                                                                    
120500     EJECT                                                                
120600 DEB-KOLLA-SATS     SECTION.                                              
120700                                                                          
120800*LÄS WDJ1 FÖR ATT FÅ FRAM VILKA SATSER ARTIKELN INGÅR I                   
120900*LÄS DÄREFTER WDK6 FÖR SATSNUMRET FÖR ATT FÅ FRAM OM 09-MÄRKT             
121000     MOVE W-IDARTNR TO W-IDARTNR-S                                        
121100     PERFORM IMS-GET-WDJ1-CSEQ-NEXT                                       
121200     PERFORM UNTIL SEGMENT-SAKNAS OR SW-SATS-OK = JA                      
121300       IF SEGMENT-FINNS                                                   
121400         MOVE SATS-STR-IDARTNR TO W-IDARTNR-PCB2                          
121500         PERFORM IMS-GET-WDK611-2                                         
121600         IF SEGMENT-FINNS AND CLAG2-CLAG-KDERS NOT = 09                   
121700           MOVE JA TO SW-SATS-OK                                          
121800         END-IF                                                           
121900       END-IF                                                             
122000       PERFORM IMS-GET-WDJ1-CSEQ-NEXT                                     
122100     END-PERFORM                                                          
122200     .                                                                    
122300     EJECT                                                                
122400 DEC-KOLLA-KAMPANJ-DB2 SECTION.                                           
122500                                                                          
122600     PERFORM DB2-DCL-OPN-TP1ARTK-CRS                                      
122700                                                                          
122800     IF SQLCODE-WS = ZERO                                                 
122900********READ TP1ARTK AND TP1KAMP                                          
123000        PERFORM DB2-FETCH-TP1ARTK-CRS                                     
123100        IF LINES-FOUND                                                    
123200           MOVE JA   TO SW-KAMPANJ                                        
123300        ELSE                                                              
123400           CONTINUE                                                       
123500        END-IF                                                            
123600     END-IF                                                               
123700                                                                          
123800     PERFORM DB2-CLOSE-TP1ARTK-CRS                                        
123900     .                                                                    
124000     EJECT                                                                
124100 DED-KOLLA-DEKAL       SECTION.                                           
124200                                                                          
124300     MOVE NEJ                 TO SW-DEKAL                                 
124400     MOVE SPACE               TO WS-TEXT                                  
124500     MOVE 'GB '               TO W-IDSKYLT                                
124600     MOVE SPACE               TO BENA11-TEXT-BEART                        
124700     PERFORM IMS-GET-BENA11-BSEQ                                          
124800     MOVE BENA11-TEXT-BEART   TO WS-TEXT-GB                               
124900     MOVE 'S  '               TO W-IDSKYLT                                
125000     MOVE SPACE               TO BENA11-TEXT-BEART                        
125100     PERFORM IMS-GET-BENA11-BSEQ                                          
125200     MOVE BENA11-TEXT-BEART   TO WS-TEXT-SV                               
125300     MOVE WS-TEXT             TO W041-BESTEXT                             
125400     MOVE 50                  TO W041-DIFAELT                             
125500                                                                          
125600     MOVE 'DEKAL'             TO W041-BESORD                              
125700     CALL W009LTXT USING W041-W009W041                                    
125800     IF W041-OK                                                           
125900        MOVE JA               TO SW-DEKAL                                 
126000     END-IF                                                               
126100                                                                          
126200     MOVE 'DECAL'             TO W041-BESORD                              
126300     CALL W009LTXT USING W041-W009W041                                    
126400     IF W041-OK                                                           
126500        MOVE JA               TO SW-DEKAL                                 
126600     END-IF                                                               
126700                                                                          
126800     IF SW-DEKAL = JA                                                     
126900        ADD +1                TO IX-RAD                                   
127000        MOVE 'DEKAL   '       TO WS-TEMEMO                                
127100        PERFORM DEA-ISRT-6325                                             
127200     END-IF                                                               
127300     .                                                                    
127400     EJECT                                                                
127500 E-KONTROLL-TILLKOMMANDE SECTION.                                         
127600                                                                          
127700     MOVE IDARTNR-TILLK    TO W-IDARTNR-PCB2                              
127800     PERFORM IMS-GET-WDK601-2                                             
127900     IF SEGMENT-SAKNAS                                                    
128000        MOVE NEJ           TO SW-KOLL                                     
128100        MOVE ZERO          TO ART2-ART-TIFINLV                            
128200     END-IF                                                               
128300     PERFORM IMS-GET-WDK611-2                                             
128400     IF SEGMENT-SAKNAS                                                    
128500        MOVE NEJ           TO SW-KOLL                                     
128600     END-IF                                                               
128700     MOVE ART2-ART-TIFINLV TO A2-TIFINLV                                  
128800     IF A2-TIAA > 50                                                      
128900        MOVE 19            TO A2-TISS                                     
129000     ELSE                                                                 
129100        MOVE 20            TO A2-TISS                                     
129200     END-IF                                                               
129300     IF SW-KOLL = JA                 AND                                  
129400        A2-TIAAAAVVD  < WS-TIAAAAVVD AND                                  
129500        CLAG2-CLAG-KVROS   = ZERO                                         
129600                                                                          
129700***    *X    KVPERIOD-BERS     ANT.PER. BEHOV  ERSATT                     
129800***    *Y    KVPERIOD-BTILLK   ANT.PER. BEHOV  TILLKOMMANDE               
129900                                                                          
130000        MOVE ZERO TO WS-IX                                                
130100        MOVE +1 TO IX                                                     
130200        PERFORM UNTIL IX > 99 OR WS-IX > ZERO                             
130300***********IF TABKV-KDPRODSL (IX) = ART2-ART-KDPRODSL                     
130400           IF TABKV-KDPRODSL (IX) = ART-KDPRODSL                          
130500              MOVE IX TO WS-IX                                            
130600           END-IF                                                         
130700           ADD +1 TO IX                                                   
130800        END-PERFORM                                                       
130900        IF WS-IX = ZERO                                                   
131000***       *KDPRODSL MÅSTE FINNAS PÅ BILD 2148                             
131100           MOVE NEJ TO SW-KOLL                                            
131200        END-IF                                                            
131300                                                                          
131400        IF SW-KOLL = JA                                                   
131500           MOVE ZERO          TO SUMOI-Y                                  
131600           MOVE +1            TO IX-TABB                                  
131700           PERFORM UNTIL IX-TABB > TABKV-KVPERIOD-BTILLK (WS-IX)          
131800              ADD TABB-OI (IX-TABB) TO SUMOI-Y                            
131900              ADD +1          TO IX-TABB                                  
132000           END-PERFORM                                                    
132100*         *JUSTERA MED NÄSTKOMMANDE (ÄLDRE) PERIOD DVS                    
132200*         *FYLL PÅ MED NÄSTA PERIOD MOTSV VAD SOM FATTAS I INNEV.         
132300           IF TABB-KVVIPER (IX-TABB) > ZERO                               
132400              COMPUTE SUMOI-Y ROUNDED = SUMOI-Y +                         
132500              (TABB-OI (IX-TABB) / TABB-KVVIPER (IX-TABB))                
132600              * INNEV-KVAR-VV                                             
132700           END-IF                                                         
132800                                                                          
132900           PERFORM EA-LAES-WDK9                                           
133000                                                                          
133100           COMPUTE SUMOI-T =  SUMOI-X * DIERS-TILLK                       
133200                                                                          
133300           IF (SUMOI-T + SUMOI-Y) < (CLAG2-CLAG-KVLS      +               
133400                                     CLAG2-CLAG-KVAKS-CDC +               
133500                                     CLAG2-CLAG-KVAKS-T   +               
133600                                     CLAG2-CLAG-KVAKS-PAV -               
133700                                     CLAG2-CLAG-KVRESS    -               
133800                                     CLAG2-CLAG-KVROS     -               
133900                                     WS-KVOKS-C1          -               
134000                                     WS-TPO-NAESTA-INLEV)                 
134100              CONTINUE                                                    
134200           ELSE                                                           
134300              MOVE NEJ        TO SW-KOLL                                  
134400           END-IF                                                         
134500        END-IF                                                            
134600     ELSE                                                                 
134700        MOVE NEJ              TO SW-KOLL                                  
134800     END-IF                                                               
134900     .                                                                    
135000     EJECT                                                                
135100 EA-LAES-WDK9 SECTION.                                                    
135200                                                                          
135300     PERFORM IMS-GET-WDK901                                               
135400     IF SEGMENT-FINNS                                                     
135500                                                                          
135600        COMPUTE WS-KVOKS-C1 = ARTM-ART-KVOKS-BULK   +                     
135700                              ARTM-ART-KVOKS-DAG    +                     
135800                              ARTM-ART-KVOKS-VOR                          
135900                                                                          
136000        MOVE ZERO                  TO WS-TPO-NAESTA-INLEV                 
136100        MOVE ARTM-ART-SUTPO-TOT    TO WS-SUTPO-TOT                        
136200        IF CLAG2-CLAG-TIDISPIN = ZERO                                     
136300          MOVE WS-SUTPO-TOT        TO WS-TPO-NAESTA-INLEV                 
136400        ELSE                                                              
136500          MOVE ZERO                TO W-DABEHOV-MIN                       
136600          MOVE CLAG2-CLAG-TIDISPIN TO W-TIDISPIN                          
136700          PERFORM S02-KONV-TIDISPIN                                       
136800          MOVE W-DADISPIN          TO W-DABEHOV-MAX                       
136900          PERFORM IMS-GET-WDK911                                          
137000          PERFORM UNTIL SEGMENT-SAKNAS                                    
137100              COMPUTE WS-TPO-NAESTA-INLEV =  WS-TPO-NAESTA-INLEV          
137200                                           + ARTM-ANT-SUTPO-PB            
137300                                           + ARTM-ANT-SUTPO-EJPB          
137400              PERFORM IMS-GET-WDK911                                      
137500          END-PERFORM                                                     
137600        END-IF                                                            
137700     ELSE                                                                 
137800        MOVE ZERO          TO WS-KVOKS-C1                                 
137900                              WS-TPO-NAESTA-INLEV                         
138000     END-IF                                                               
138100     .                                                                    
138200     EJECT                                                                
138300 F-LAES-OI SECTION.                                                       
138400                                                                          
138500*****SKAPA TABELL MED SUMMA OI PER PERIOD BAKÅT I TIDEN                   
138600*****      UNDER 3 KALENDERÅR (MAX 24 PER ANVÄNDS,                        
138700*****      MEN IBLAND TITTAR MAN ÄVEN I DEN 25:E)                         
138800                                                                          
138900     MOVE +1              TO IX-TABB                                      
139000     PERFORM UNTIL IX-TABB > 25                                           
139100        MOVE ZERO         TO TABB-OI      (IX-TABB)                       
139200        MOVE ZERO         TO TABB-KVVIPER (IX-TABB)                       
139300        ADD +1            TO IX-TABB                                      
139400     END-PERFORM                                                          
139500     MOVE +1              TO IX-TABB                                      
139600                                                                          
139700     MOVE ARB-TIAAAA      TO WS-TIAAAA                                    
139800*ÅR0                                                                      
139900     MOVE ARB-TIAAPP      TO S-AAPP                                       
140000     MOVE 53              TO WS-IX                                        
140100     IF TAB0-TIPP (53)    =  ZERO                                         
140200        MOVE 52           TO WS-IX                                        
140300     END-IF                                                               
140400     MOVE WS-TIAAAA       TO W-TIAAAA                                     
140500     PERFORM IMS-GU-WDL811                                                
140600     IF SEGMENT-FINNS                                                     
140700        PERFORM UNTIL WS-IX = ZERO OR IX-TABB > 25                        
140800          IF TAB0-TIPP (WS-IX) > S-PP                                     
140900           CONTINUE                                                       
141000          ELSE                                                            
141100           IF TAB0-TIPP (WS-IX) = S-PP                                    
141200             MOVE TAB0-KVVIPER   (WS-IX) TO TABB-KVVIPER(IX-TABB)         
141300             ADD AAR-KVOI-PROG   (WS-IX) TO TABB-OI (IX-TABB)             
141400             ADD AAR-KVOI-DIV    (WS-IX) TO TABB-OI (IX-TABB)             
141500             ADD AAR-KVOI-SATS   (WS-IX) TO TABB-OI (IX-TABB)             
141600             ADD AAR-KVOI-REFILL (WS-IX) TO TABB-OI (IX-TABB)             
141700           ELSE                                                           
141800             MOVE TAB0-TIPP (WS-IX)      TO S-PP                          
141900             ADD +1                      TO IX-TABB                       
142000             IF IX-TABB NOT > 25                                          
142100             MOVE TAB0-KVVIPER   (WS-IX) TO TABB-KVVIPER(IX-TABB)         
142200             ADD AAR-KVOI-PROG   (WS-IX) TO TABB-OI (IX-TABB)             
142300             ADD AAR-KVOI-DIV    (WS-IX) TO TABB-OI (IX-TABB)             
142400             ADD AAR-KVOI-SATS   (WS-IX) TO TABB-OI (IX-TABB)             
142500             ADD AAR-KVOI-REFILL (WS-IX) TO TABB-OI (IX-TABB)             
142600             END-IF                                                       
142700           END-IF                                                         
142800          END-IF                                                          
142900          SUBTRACT +1   FROM WS-IX                                        
143000        END-PERFORM                                                       
143100     ELSE                                                                 
143200        PERFORM UNTIL WS-IX = ZERO OR IX-TABB > 25                        
143300          IF TAB0-TIPP (WS-IX) > S-PP                                     
143400           CONTINUE                                                       
143500          ELSE                                                            
143600           IF TAB0-TIPP (WS-IX) = S-PP                                    
143700             CONTINUE                                                     
143800           ELSE                                                           
143900             MOVE TAB0-TIPP (WS-IX) TO S-PP                               
144000             ADD +1       TO IX-TABB                                      
144100           END-IF                                                         
144200          END-IF                                                          
144300          SUBTRACT +1   FROM WS-IX                                        
144400        END-PERFORM                                                       
144500     END-IF                                                               
144600*ÅR1                                                                      
144700     ADD +1               TO IX-TABB                                      
144800     MOVE 12              TO S-PP                                         
144900     SUBTRACT +1        FROM WS-TIAAAA                                    
145000     MOVE WS-TIAAAA       TO W-TIAAAA                                     
145100     MOVE 53              TO WS-IX                                        
145200     IF TAB1-TIPP (53)    =  ZERO                                         
145300        MOVE 52           TO WS-IX                                        
145400     END-IF                                                               
145500     PERFORM IMS-GU-WDL811                                                
145600     IF SEGMENT-FINNS                                                     
145700        PERFORM UNTIL WS-IX = ZERO OR IX-TABB > 25                        
145800          IF TAB1-TIPP (WS-IX) > S-PP                                     
145900           CONTINUE                                                       
146000          ELSE                                                            
146100           IF TAB1-TIPP (WS-IX) = S-PP                                    
146200             MOVE TAB1-KVVIPER   (WS-IX) TO TABB-KVVIPER(IX-TABB)         
146300             ADD AAR-KVOI-PROG   (WS-IX) TO TABB-OI (IX-TABB)             
146400             ADD AAR-KVOI-DIV    (WS-IX) TO TABB-OI (IX-TABB)             
146500             ADD AAR-KVOI-SATS   (WS-IX) TO TABB-OI (IX-TABB)             
146600             ADD AAR-KVOI-REFILL (WS-IX) TO TABB-OI (IX-TABB)             
146700           ELSE                                                           
146800             MOVE TAB1-TIPP (WS-IX) TO S-PP                               
146900             ADD +1       TO IX-TABB                                      
147000             IF IX-TABB NOT > 25                                          
147100             MOVE TAB1-KVVIPER   (WS-IX) TO TABB-KVVIPER(IX-TABB)         
147200             ADD AAR-KVOI-PROG   (WS-IX) TO TABB-OI (IX-TABB)             
147300             ADD AAR-KVOI-DIV    (WS-IX) TO TABB-OI (IX-TABB)             
147400             ADD AAR-KVOI-SATS   (WS-IX) TO TABB-OI (IX-TABB)             
147500             ADD AAR-KVOI-REFILL (WS-IX) TO TABB-OI (IX-TABB)             
147600             END-IF                                                       
147700           END-IF                                                         
147800          END-IF                                                          
147900          SUBTRACT +1   FROM WS-IX                                        
148000        END-PERFORM                                                       
148100     ELSE                                                                 
148200        PERFORM UNTIL WS-IX = ZERO OR IX-TABB > 25                        
148300          IF TAB1-TIPP (WS-IX) > S-PP                                     
148400           CONTINUE                                                       
148500          ELSE                                                            
148600           IF TAB1-TIPP (WS-IX) = S-PP                                    
148700             CONTINUE                                                     
148800           ELSE                                                           
148900             MOVE TAB1-TIPP (WS-IX) TO S-PP                               
149000             ADD +1       TO IX-TABB                                      
149100           END-IF                                                         
149200          END-IF                                                          
149300          SUBTRACT +1   FROM WS-IX                                        
149400        END-PERFORM                                                       
149500     END-IF                                                               
149600*ÅR2                                                                      
149700     MOVE 12              TO S-PP                                         
149800     ADD +1               TO IX-TABB                                      
149900     SUBTRACT +1        FROM WS-TIAAAA                                    
150000     MOVE WS-TIAAAA       TO W-TIAAAA                                     
150100     MOVE 53              TO WS-IX                                        
150200     IF TAB2-TIPP (53)    =  ZERO                                         
150300        MOVE 52           TO WS-IX                                        
150400     END-IF                                                               
150500     PERFORM IMS-GU-WDL811                                                
150600     IF SEGMENT-FINNS                                                     
150700        PERFORM UNTIL WS-IX = ZERO OR IX-TABB > 25                        
150800          IF TAB2-TIPP (WS-IX) > S-PP                                     
150900           CONTINUE                                                       
151000          ELSE                                                            
151100           IF TAB2-TIPP (WS-IX) = S-PP                                    
151200             MOVE TAB2-KVVIPER   (WS-IX) TO TABB-KVVIPER(IX-TABB)         
151300             ADD AAR-KVOI-PROG   (WS-IX) TO TABB-OI (IX-TABB)             
151400             ADD AAR-KVOI-DIV    (WS-IX) TO TABB-OI (IX-TABB)             
151500             ADD AAR-KVOI-SATS   (WS-IX) TO TABB-OI (IX-TABB)             
151600             ADD AAR-KVOI-REFILL (WS-IX) TO TABB-OI (IX-TABB)             
151700           ELSE                                                           
151800             MOVE TAB2-TIPP (WS-IX) TO S-PP                               
151900             ADD +1       TO IX-TABB                                      
152000             IF IX-TABB NOT > 25                                          
152100             MOVE TAB2-KVVIPER   (WS-IX) TO TABB-KVVIPER(IX-TABB)         
152200             ADD AAR-KVOI-PROG   (WS-IX) TO TABB-OI (IX-TABB)             
152300             ADD AAR-KVOI-DIV    (WS-IX) TO TABB-OI (IX-TABB)             
152400             ADD AAR-KVOI-SATS   (WS-IX) TO TABB-OI (IX-TABB)             
152500             ADD AAR-KVOI-REFILL (WS-IX) TO TABB-OI (IX-TABB)             
152600             END-IF                                                       
152700           END-IF                                                         
152800          END-IF                                                          
152900          SUBTRACT +1   FROM WS-IX                                        
153000        END-PERFORM                                                       
153100     ELSE                                                                 
153200        PERFORM UNTIL WS-IX = ZERO OR IX-TABB > 25                        
153300          IF TAB2-TIPP (WS-IX) > S-PP                                     
153400           CONTINUE                                                       
153500          ELSE                                                            
153600           IF TAB2-TIPP (WS-IX) = S-PP                                    
153700             CONTINUE                                                     
153800           ELSE                                                           
153900             MOVE TAB2-TIPP (WS-IX) TO S-PP                               
154000             ADD +1       TO IX-TABB                                      
154100           END-IF                                                         
154200          END-IF                                                          
154300          SUBTRACT +1   FROM WS-IX                                        
154400        END-PERFORM                                                       
154500     END-IF                                                               
154600     .                                                                    
154700     EJECT                                                                
154800 G-KOLL-OI-ERSATT SECTION.                                                
154900                                                                          
155000***    *X    KVPERIOD-BERS   ANT.PER. BEHOV    ERSATT                     
155100***    *W    KVPERIOD-VERS   ANT.PER. VÄRDE    ERSATT                     
155200***    *Z    KVPERIOD-HERS   ANT.PER. HISTORIK ERSATT                     
155300                                                                          
155400***    *TA FRAM WS-IX FÖR KDPRODSL   (RAD? PÅ BILD 2148)                  
155500                                                                          
155600        MOVE ZERO             TO WS-IX                                    
155700        MOVE +1               TO IX                                       
155800        PERFORM UNTIL IX > 99 OR WS-IX > ZERO                             
155900           IF TABKV-KDPRODSL (IX) = ART-KDPRODSL                          
156000              MOVE IX         TO WS-IX                                    
156100           END-IF                                                         
156200           ADD +1             TO IX                                       
156300        END-PERFORM                                                       
156400        IF WS-IX = ZERO                                                   
156500***       *KDPRODSL MÅSTE FINNAS PÅ BILD 2148                             
156600           MOVE NEJ           TO SW-KOLL                                  
156700        END-IF                                                            
156800                                                                          
156900        IF SW-KOLL = JA                                                   
157000***       *SUMOI-X  =>  SPARAS  (TILL E-KONTROLL-TILLKOMMANDE)            
157100           MOVE ZERO          TO SUMOI-X                                  
157200           MOVE +1            TO IX-TABB                                  
157300           PERFORM UNTIL IX-TABB > TABKV-KVPERIOD-BERS (WS-IX)            
157400              ADD TABB-OI (IX-TABB) TO SUMOI-X                            
157500              ADD +1          TO IX-TABB                                  
157600           END-PERFORM                                                    
157700*         *JUSTERA MED NÄSTKOMMANDE (ÄLDRE) PERIOD DVS                    
157800*         *FYLL PÅ MED NÄSTA PERIOD MOTSV VAD SOM FATTAS I INNEV.         
157900           IF TABB-KVVIPER (IX-TABB) > ZERO                               
158000              COMPUTE SUMOI-X ROUNDED = SUMOI-X +                         
158100              (TABB-OI (IX-TABB) / TABB-KVVIPER (IX-TABB))                
158200              * INNEV-KVAR-VV                                             
158300           END-IF                                                         
158400                                                                          
158500           MOVE ZERO          TO SUMOI-Z                                  
158600           MOVE +1            TO IX-TABB                                  
158700           PERFORM UNTIL IX-TABB > TABKV-KVPERIOD-HERS (WS-IX)            
158800              ADD TABB-OI (IX-TABB) TO SUMOI-Z                            
158900              ADD +1          TO IX-TABB                                  
159000           END-PERFORM                                                    
159100*         *JUSTERA MED NÄSTKOMMANDE (ÄLDRE) PERIOD DVS                    
159200*         *FYLL PÅ MED NÄSTA PERIOD MOTSV VAD SOM FATTAS I INNEV.         
159300           IF TABB-KVVIPER (IX-TABB) > ZERO                               
159400              COMPUTE SUMOI-Z ROUNDED = SUMOI-Z +                         
159500              (TABB-OI (IX-TABB) / TABB-KVVIPER (IX-TABB))                
159600              * INNEV-KVAR-VV                                             
159700           END-IF                                                         
159800                                                                          
159900           PERFORM GA-LAES-WDK9                                           
160000                                                                          
160100           IF SUMOI-Z < CLAG-KVLS      +                                  
160200                        CLAG-KVAKS-CDC +                                  
160300                        CLAG-KVAKS-T   +                                  
160400                        CLAG-KVAKS-PAV -                                  
160500                        CLAG-KVRESS    -                                  
160600                        CLAG-KVROS     -                                  
160700                        WS-KVOKS-C1    -                                  
160800                        WS-TPO-NAESTA-INLEV                               
160900              CONTINUE                                                    
161000           ELSE                                                           
161100              MOVE NEJ        TO SW-KOLL                                  
161200           END-IF                                                         
161300                                                                          
161400           IF SW-KOLL = JA                                                
161500              MOVE ZERO          TO SUMOI-W                               
161600              MOVE +1            TO IX-TABB                               
161700              PERFORM UNTIL IX-TABB > TABKV-KVPERIOD-VERS (WS-IX)         
161800                 ADD TABB-OI (IX-TABB) TO SUMOI-W                         
161900                 ADD +1          TO IX-TABB                               
162000              END-PERFORM                                                 
162100*         *JUSTERA MED NÄSTKOMMANDE (ÄLDRE) PERIOD DVS                    
162200*         *FYLL PÅ MED NÄSTA PERIOD MOTSV VAD SOM FATTAS I INNEV.         
162300              IF TABB-KVVIPER (IX-TABB) > ZERO                            
162400                 COMPUTE SUMOI-W ROUNDED = SUMOI-W +                      
162500                 (TABB-OI (IX-TABB) / TABB-KVVIPER (IX-TABB))             
162600                 * INNEV-KVAR-VV                                          
162700              END-IF                                                      
162800                                                                          
162900              MOVE ZERO          TO WS-ARKOST                             
163000              MOVE +1            TO IX                                    
163100              PERFORM UNTIL IX > 12 OR WS-ARKOST > ZERO                   
163200                 IF TAB-ADLAGOMR (IX) = CLAG-ADLAGOMR                     
163300                    MOVE TAB-ARKOST (IX) TO WS-ARKOST                     
163400                 END-IF                                                   
163500                 ADD +1          TO IX                                    
163600              END-PERFORM                                                 
163700              IF WS-ARKOST = ZERO                                         
163800******          *FINNS INTE LAGEROMR I TABELL  TAG LAGEROMR = 20          
163900                 MOVE TAB-ARKOST (5) TO WS-ARKOST                         
164000              END-IF                                                      
164100                                                                          
164200              IF (SUMOI-W * CLAG-PRARTSTD) < WS-ARKOST                    
164300                 CONTINUE                                                 
164400              ELSE                                                        
164500                 MOVE NEJ        TO SW-KOLL                               
164600              END-IF                                                      
164700           END-IF                                                         
164800        END-IF                                                            
164900     .                                                                    
165000     EJECT                                                                
165100 GA-LAES-WDK9 SECTION.                                                    
165200                                                                          
165300     PERFORM IMS-GET-WDK901                                               
165400     IF SEGMENT-FINNS                                                     
165500                                                                          
165600        COMPUTE WS-KVOKS-C1 = ARTM-ART-KVOKS-BULK   +                     
165700                              ARTM-ART-KVOKS-DAG    +                     
165800                              ARTM-ART-KVOKS-VOR                          
165900                                                                          
166000        MOVE ZERO               TO WS-TPO-NAESTA-INLEV                    
166100        MOVE ARTM-ART-SUTPO-TOT TO WS-SUTPO-TOT                           
166200        IF CLAG-TIDISPIN = ZERO                                           
166300          MOVE WS-SUTPO-TOT     TO WS-TPO-NAESTA-INLEV                    
166400        ELSE                                                              
166500          MOVE ZERO             TO W-DABEHOV-MIN                          
166600          MOVE CLAG-TIDISPIN    TO W-TIDISPIN                             
166700          PERFORM S02-KONV-TIDISPIN                                       
166800          MOVE W-DADISPIN       TO W-DABEHOV-MAX                          
166900          PERFORM IMS-GET-WDK911                                          
167000          PERFORM UNTIL SEGMENT-SAKNAS                                    
167100              COMPUTE WS-TPO-NAESTA-INLEV =  WS-TPO-NAESTA-INLEV          
167200                                           + ARTM-ANT-SUTPO-PB            
167300                                           + ARTM-ANT-SUTPO-EJPB          
167400              PERFORM IMS-GET-WDK911                                      
167500          END-PERFORM                                                     
167600        END-IF                                                            
167700     ELSE                                                                 
167800        MOVE ZERO          TO WS-KVOKS-C1                                 
167900                              WS-TPO-NAESTA-INLEV                         
168000     END-IF                                                               
168100     .                                                                    
168200     EJECT                                                                
168300 Z-FINIT SECTION.                                                         
168400                                                                          
168500                                                                          
168600     CLOSE W01160                                                         
168700     SKIP2                                                                
168800     MOVE 'S' TO POSTSUM-OPKOD                                            
168900     CALL POSTSUM USING POSTSUM-PARM                                      
169000     .                                                                    
169100     EJECT                                                                
169200 S01-LAES-W01160  SECTION.                                                
169300     SKIP2                                                                
169400     READ W01160 INTO IN-AREA                                             
169500     AT END                                                               
169600        SET END-OF-W01160 TO TRUE                                         
169700                                                                          
169800     NOT AT END                                                           
169900        MOVE 'W01160'   TO POSTSUM-FDNAMN                                 
170000        MOVE 'W26176D1' TO POSTSUM-DDNAMN2                                
170100        MOVE 'IN'       TO POSTSUM-TRANSTYP                               
170200        CALL POSTSUM USING POSTSUM-PARM                                   
170300                                                                          
170400     END-READ                                                             
170500     .                                                                    
170600     EJECT                                                                
170700 S02-KONV-TIDISPIN SECTION.                                               
170800                                                                          
170900     MOVE 'AAMMDD'         TO DAT-KDDATFORM                               
171000     MOVE W-TIDISPIN       TO DAT-I-TIDATUM                               
171100     CALL WDATKONV USING DAT-KDDATFORM                                    
171200                         DAT-I-TIDATUM                                    
171300                         DAT-O-TIDATUM                                    
171400                         DAT-KDSVAR                                       
171500     IF DAT-KDSVAR-OK                                                     
171600       MOVE DAT-TIAA-VECKA TO W-TIAA                                      
171700       MOVE DAT-TIVV       TO W-TIVV                                      
171800       MOVE DAT-TISEKEL    TO W-TISEKEL                                   
171900       MOVE W-TIAAAAVV     TO W-DADISPIN                                  
172000     ELSE                                                                 
172100       MOVE ZERO           TO W-DADISPIN                                  
172200     END-IF                                                               
172300     .                                                                    
172400     EJECT                                                                
172500 X-TAG-CHECKPOINT   SECTION.                                              
172600                                                                          
172700* --- VID CHECKPOINTTAGGNING SÅ TAPPAR MAN GN-POSITION I BASEN            
172800* --- SPARA DATABASNYCKLAR OM DET BEHÖVS                                  
172900     PERFORM IMS-CHECKPOINT                                               
173000     MOVE ZERO TO CHKP-ANT                                                
173100* --- LÄS OM DATABAS OM DET BEHÖVS                                        
173200     .                                                                    
173300     EJECT                                                                
173400* --- IMS SEKTIONER ---                                                   
173500                                                                          
173600 IMS-GET-WDK601 SECTION.                                                  
173700                                                                          
173800     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
173900          DELIMITED BY SIZE INTO SSA1                                     
174000     MOVE '  GE' TO GODK-STATUSKODER                                      
174100     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-WDK601 SSA1                    
174200     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
174300     PERFORM IMS-STATUSKONTROLL                                           
174400     .                                                                    
174500     EJECT                                                                
174600 IMS-GET-WDK611 SECTION.                                                  
174700                                                                          
174800     STRING 'WDK611  (KDSEGKEY =' W-KDSEGKEY-X ')'                        
174900          DELIMITED BY SIZE INTO SSA1                                     
175000     MOVE '  GE' TO GODK-STATUSKODER                                      
175100     CALL CBLTDLI USING GHNP WDK6-PCB DLI-IO-WDK611 SSA1                  
175200     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
175300     PERFORM IMS-STATUSKONTROLL                                           
175400     .                                                                    
175500     SKIP3                                                                
175600 IMS-REPL-WDK611 SECTION.                                                 
175700                                                                          
175800     MOVE '  ' TO GODK-STATUSKODER                                        
175900     CALL CBLTDLI USING REPL WDK6-PCB DLI-IO-WDK611                       
176000     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
176100     PERFORM IMS-STATUSKONTROLL                                           
176200     ADD +1  TO CHKP-ANT                                                  
176300     .                                                                    
176400     EJECT                                                                
176500 IMS-REPL-WDK627 SECTION.                                                 
176600                                                                          
176700     MOVE '  ' TO GODK-STATUSKODER                                        
176800     CALL CBLTDLI USING REPL WDK6-PCB DLI-IO-WDK627                       
176900     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
177000     PERFORM IMS-STATUSKONTROLL                                           
177100     ADD +1  TO CHKP-ANT                                                  
177200     .                                                                    
177300     SKIP3                                                                
177400 IMS-GET-WDK627 SECTION.                                                  
177500*                      LÄSNING AV SKROTNINGSSEGMENT WDK627                
177600     MOVE   'WDK611  *F'         TO SSA1                                  
177700     MOVE   'WDK627   '          TO SSA2                                  
177800     MOVE   '  GE'               TO GODK-STATUSKODER                      
177900     SKIP2                                                                
178000     CALL CBLTDLI USING GHNP WDK6-PCB DLI-IO-WDK627 SSA1                  
178100                                               SSA2                       
178200     MOVE WDK6-STATUS-CODE       TO STATUS-WS                             
178300     PERFORM IMS-STATUSKONTROLL                                           
178400     EJECT                                                                
178500     SKIP3                                                                
178600     .                                                                    
178700 IMS-ISRT-WDK627 SECTION.                                                 
178800*                        INSERT AV SKROTNINGSSEGMENT WDK627               
178900     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
179000             DELIMITED BY SIZE INTO SSA1                                  
179100     MOVE   'WDK611   '          TO SSA2                                  
179200     MOVE   'WDK627   '          TO SSA3                                  
179300     MOVE   '  '  TO GODK-STATUSKODER                                     
179400                                                                          
179500     CALL CBLTDLI USING ISRT WDK6-PCB DLI-IO-WDK627 SSA1                  
179600                                          SSA2 SSA3                       
179700     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
179800     PERFORM IMS-STATUSKONTROLL                                           
179900     ADD +1  TO CHKP-ANT                                                  
180000     .                                                                    
180100     EJECT                                                                
180200 IMS-GET-WDK601-2 SECTION.                                                
180300                                                                          
180400     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-2 ')'                         
180500          DELIMITED BY SIZE INTO SSA1                                     
180600     MOVE '  GE' TO GODK-STATUSKODER                                      
180700     CALL CBLTDLI USING GU WDK62-PCB DLI-IO-WDK601-2 SSA1                 
180800     MOVE WDK62-STATUS-CODE TO STATUS-WS                                  
180900     PERFORM IMS-STATUSKONTROLL                                           
181000     .                                                                    
181100     EJECT                                                                
181200 IMS-GET-WDK611-2 SECTION.                                                
181300                                                                          
181400     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-2 ')'                         
181500          DELIMITED BY SIZE INTO SSA1                                     
181600     STRING 'WDK611  (KDSEGKEY =' W-KDSEGKEY-X ')'                        
181700          DELIMITED BY SIZE INTO SSA2                                     
181800     MOVE '  GE' TO GODK-STATUSKODER                                      
181900     CALL CBLTDLI USING GU WDK62-PCB DLI-IO-WDK611-2 SSA1 SSA2            
182000     MOVE WDK62-STATUS-CODE TO STATUS-WS                                  
182100     PERFORM IMS-STATUSKONTROLL                                           
182200     .                                                                    
182300     EJECT                                                                
182400 IMS-GET-WDK901 SECTION.                                                  
182500                                                                          
182600     STRING 'WDK901  (IDARTNR  =' W-IDARTNR-X ')'                         
182700          DELIMITED BY SIZE INTO SSA1                                     
182800     MOVE '  GE' TO GODK-STATUSKODER                                      
182900     CALL CBLTDLI USING GU WDK9-PCB DLI-IO-WDK901 SSA1                    
183000     MOVE WDK9-STATUS-CODE TO STATUS-WS                                   
183100     PERFORM IMS-STATUSKONTROLL                                           
183200     .                                                                    
183300     SKIP3                                                                
183400 IMS-GET-WDK911 SECTION.                                                  
183500                                                                          
183600     STRING 'WDK901  (IDARTNR  =' W-IDARTNR-X ')'                         
183700          DELIMITED BY SIZE INTO SSA1                                     
183800     STRING 'WDK911  (DABEHOV >=' W-DABEHOV-MIN-X                         
183900                    '&DABEHOV <=' W-DABEHOV-MAX-X ')'                     
184000          DELIMITED BY SIZE INTO SSA2                                     
184100     MOVE '  GE' TO GODK-STATUSKODER                                      
184200     CALL CBLTDLI USING GNP WDK9-PCB DLI-IO-WDK911 SSA1 SSA2              
184300     MOVE WDK9-STATUS-CODE TO STATUS-WS                                   
184400     PERFORM IMS-STATUSKONTROLL                                           
184500     .                                                                    
184600     EJECT                                                                
184700 IMS-GU-WDR501-6321 SECTION.                                              
184800                                                                          
184900     STRING 'WDR501  (WDGXKEY  =' W-WDGX6321-ROT-X ')'                    
185000            DELIMITED BY SIZE INTO SSA1                                   
185100     MOVE 'GE  '                TO GODK-STATUSKODER                       
185200     CALL CBLTDLI USING GU   6321-PCB DLI-IO-WDR501-6321 SSA1             
185300     MOVE 6321-STATUS-CODE      TO STATUS-WS                              
185400     PERFORM IMS-STATUSKONTROLL                                           
185500     .                                                                    
185600     SKIP3                                                                
185700 IMS-ISRT-WDR501-6321 SECTION.                                            
185800                                                                          
185900     STRING 'WDR501     '                                                 
186000            DELIMITED BY SIZE INTO SSA1                                   
186100     MOVE '  '                  TO GODK-STATUSKODER                       
186200     CALL CBLTDLI USING ISRT 6321-PCB DLI-IO-WDR501-6321 SSA1             
186300     MOVE 6321-STATUS-CODE      TO STATUS-WS                              
186400     PERFORM IMS-STATUSKONTROLL                                           
186500     ADD +1  TO CHKP-ANT                                                  
186600     .                                                                    
186700     EJECT                                                                
186800 IMS-ISRT-WDGX6322 SECTION.                                               
186900                                                                          
187000     STRING 'WDR501  (WDGXKEY  =' W-WDGX6321-ROT-X ')'                    
187100            DELIMITED BY SIZE INTO SSA1                                   
187200     MOVE 'WDGX6322'            TO SSA2                                   
187300     MOVE '  II'                TO GODK-STATUSKODER                       
187400     CALL CBLTDLI USING ISRT 6321-PCB DLI-IO-WDGX6322 SSA1 SSA2           
187500     MOVE 6321-STATUS-CODE      TO STATUS-WS                              
187600     PERFORM IMS-STATUSKONTROLL                                           
187700     ADD +1  TO CHKP-ANT                                                  
187800     SKIP3                                                                
187900     .                                                                    
188000     SKIP3                                                                
188100 IMS-ISRT-WDGX6324 SECTION.                                               
188200                                                                          
188300     STRING 'WDR501  (WDGXKEY  =' W-WDGX6321-ROT-X ')'                    
188400            DELIMITED BY SIZE INTO SSA1                                   
188500     STRING 'WDGX6322(DASKROT9 =' W-WDGX6322-KEY-X ')'                    
188600            DELIMITED BY SIZE INTO SSA2                                   
188700     MOVE 'WDGX6324'            TO SSA3                                   
188800     MOVE '  '                  TO GODK-STATUSKODER                       
188900     CALL CBLTDLI USING ISRT 6321-PCB DLI-IO-WDGX6324                     
189000                                      SSA1 SSA2 SSA3                      
189100     MOVE 6321-STATUS-CODE      TO STATUS-WS                              
189200     PERFORM IMS-STATUSKONTROLL                                           
189300     ADD +1  TO CHKP-ANT                                                  
189400     SKIP3                                                                
189500     .                                                                    
189600     EJECT                                                                
189700 IMS-ISRT-WDGX6325 SECTION.                                               
189800                                                                          
189900     STRING 'WDR501  (WDGXKEY  =' W-WDGX6321-ROT-X ')'                    
190000            DELIMITED BY SIZE INTO SSA1                                   
190100     STRING 'WDGX6322(DASKROT9 =' W-WDGX6322-KEY-X ')'                    
190200            DELIMITED BY SIZE INTO SSA2                                   
190300     STRING 'WDGX6324(KY6324   =' W-KY6324-KVAL-X ')'                     
190400          DELIMITED BY SIZE INTO SSA3                                     
190500     MOVE 'WDGX6325 '           TO SSA4                                   
190600     MOVE '  II'                TO GODK-STATUSKODER                       
190700     CALL CBLTDLI USING ISRT 6321-PCB DLI-IO-WDGX6325                     
190800                                      SSA1 SSA2 SSA3 SSA4                 
190900     MOVE 6321-STATUS-CODE      TO STATUS-WS                              
191000     PERFORM IMS-STATUSKONTROLL                                           
191100     ADD +1  TO CHKP-ANT                                                  
191200     .                                                                    
191300     EJECT                                                                
191400 IMS-GET-WDR501-6327 SECTION.                                             
191500     STRING 'WDR501  (WDGXKEY  =' W-WDGXKEY-X ')'                         
191600          DELIMITED BY SIZE INTO SSA1                                     
191700     MOVE '  GE' TO GODK-STATUSKODER                                      
191800     CALL CBLTDLI USING GU 6327-PCB DLI-IO-WDR501-6327 SSA1               
191900     MOVE 6327-STATUS-CODE TO STATUS-WS                                   
192000     PERFORM IMS-STATUSKONTROLL                                           
192100     .                                                                    
192200     SKIP3                                                                
192300 IMS-GET-WDGX6328 SECTION.                                                
192400     STRING 'WDGX6328(IDUSERGK= ' W-IDUSER-GODK ')'                       
192500          DELIMITED BY SIZE INTO SSA1                                     
192600     MOVE '  GE' TO GODK-STATUSKODER                                      
192700     CALL CBLTDLI USING GHNP 6327-PCB DLI-IO-WDGX6328 SSA1                
192800     MOVE 6327-STATUS-CODE TO STATUS-WS                                   
192900     PERFORM IMS-STATUSKONTROLL                                           
193000     .                                                                    
193100     EJECT                                                                
193200 IMS-GET-WDR201-2243 SECTION.                                             
193300                                                                          
193400     STRING 'WDR201  (WDGXKEY  =' W-WDGX2243-ROT-X ')'                    
193500            DELIMITED BY SIZE INTO SSA1                                   
193600     MOVE 'GE  '                TO GODK-STATUSKODER                       
193700     CALL CBLTDLI USING GU   2243-PCB DLI-IO-WDR201-2243 SSA1             
193800     MOVE 2243-STATUS-CODE      TO STATUS-WS                              
193900     PERFORM IMS-STATUSKONTROLL                                           
194000     .                                                                    
194100     SKIP3                                                                
194200 IMS-GET-WDGX2244 SECTION.                                                
194300                                                                          
194400*****STRING 'WDGX2244(KDPRODSL= ' W-KDPRODSL-X ')'                        
194500     STRING 'WDGX2244           '                                         
194600          DELIMITED BY SIZE INTO SSA1                                     
194700     MOVE '  GE' TO GODK-STATUSKODER                                      
194800     CALL CBLTDLI USING GNP 2243-PCB DLI-IO-WDGX2244 SSA1                 
194900     MOVE 2243-STATUS-CODE TO STATUS-WS                                   
195000     PERFORM IMS-STATUSKONTROLL                                           
195100     .                                                                    
195200     EJECT                                                                
195300 IMS-GET-WDN601 SECTION.                                                  
195400                                                                          
195500     STRING 'WDN601  (IDARTNR  =' W-IDARTNR-X ')'                         
195600          DELIMITED BY SIZE INTO SSA1                                     
195700     MOVE '  GE' TO GODK-STATUSKODER                                      
195800     CALL CBLTDLI USING GU WDN6-PCB DLI-IO-WDN601 SSA1                    
195900     MOVE WDN6-STATUS-CODE TO STATUS-WS                                   
196000     PERFORM IMS-STATUSKONTROLL                                           
196100     .                                                                    
196200     EJECT                                                                
196300 IMS-GET-WDN611 SECTION.                                                  
196400                                                                          
196500     STRING 'WDN611  (WDN611KY =' W-WDN611KY-X ')'                        
196600          DELIMITED BY SIZE INTO SSA1                                     
196700     MOVE '  GE' TO GODK-STATUSKODER                                      
196800     CALL CBLTDLI USING GNP WDN6-PCB DLI-IO-WDN611 SSA1                   
196900     MOVE WDN6-STATUS-CODE TO STATUS-WS                                   
197000     PERFORM IMS-STATUSKONTROLL                                           
197100     .                                                                    
197200     EJECT                                                                
197300 IMS-GU-WDQ4B1 SECTION.                                                   
197400                                                                          
197500     STRING 'WDQ4B1  (WDQ4B1KY >' W-WDQ4B1KY-MIN                          
197600                    '&WDQ4B1KY <' W-WDQ4B1KY-MAX ')'                      
197700          DELIMITED BY SIZE INTO SSA1                                     
197800     MOVE '  GEGB' TO GODK-STATUSKODER                                    
197900     CALL CBLTDLI USING GU WDQ4B-PCB DLI-IO-WDQ4B1 SSA1                   
198000     MOVE WDQ4B-STATUS-CODE TO STATUS-WS                                  
198100     PERFORM IMS-STATUSKONTROLL                                           
198200     .                                                                    
198300     SKIP3                                                                
198400 IMS-GN-WDQ4B1 SECTION.                                                   
198500                                                                          
198600     STRING 'WDQ4B1  (WDQ4B1KY >' W-WDQ4B1KY-MIN                          
198700                    '&WDQ4B1KY <' W-WDQ4B1KY-MAX ')'                      
198800          DELIMITED BY SIZE INTO SSA1                                     
198900     MOVE '  GEGB' TO GODK-STATUSKODER                                    
199000     CALL CBLTDLI USING GN WDQ4B-PCB DLI-IO-WDQ4B1 SSA1                   
199100     MOVE WDQ4B-STATUS-CODE TO STATUS-WS                                  
199200     PERFORM IMS-STATUSKONTROLL                                           
199300     .                                                                    
199400     SKIP3                                                                
199500 IMS-GU-WDQ401 SECTION.                                                   
199600                                                                          
199700     STRING 'WDQ401  (WDQ401KY =' W-WDQ401KY-X ')'                        
199800          DELIMITED BY SIZE INTO SSA1                                     
199900     MOVE '  GEGB' TO GODK-STATUSKODER                                    
200000     CALL CBLTDLI USING GU WDQ4-PCB DLI-IO-WDQ401 SSA1                    
200100     MOVE WDQ4-STATUS-CODE TO STATUS-WS                                   
200200     PERFORM IMS-STATUSKONTROLL                                           
200300     .                                                                    
200400     EJECT                                                                
200500 IMS-GU-WDL811 SECTION.                                                   
200600                                                                          
200700     STRING 'WDL801  (IDARTNR  =' W-IDARTNR-X ')'                         
200800          DELIMITED BY SIZE INTO SSA1                                     
200900     STRING 'WDL811  (TIAAAA   =' W-TIAAAA-X ')'                          
201000          DELIMITED BY SIZE INTO SSA2                                     
201100     MOVE '  GE' TO GODK-STATUSKODER                                      
201200     CALL CBLTDLI USING GU WDL8-PCB DLI-IO-WDL811 SSA1 SSA2               
201300     MOVE WDL8-STATUS-CODE TO STATUS-WS                                   
201400     PERFORM IMS-STATUSKONTROLL                                           
201500     .                                                                    
201600     EJECT                                                                
201700 IMS-GU-WDD701 SECTION.                                                   
201800                                                                          
201900     STRING 'WDD701  (IDARTNR  =' W-IDARTNR-X ')'                         
202000          DELIMITED BY SIZE INTO SSA1                                     
202100     MOVE '  GE' TO GODK-STATUSKODER                                      
202200     CALL CBLTDLI USING GU WDD7-PCB DLI-IO-WDD701 SSA1                    
202300     MOVE WDD7-STATUS-CODE TO STATUS-WS                                   
202400     PERFORM IMS-STATUSKONTROLL                                           
202500     .                                                                    
202600     EJECT                                                                
202700 IMS-GNP-WDD702 SECTION.                                                  
202800                                                                          
202900     STRING 'WDD701  (IDARTNR  =' W-IDARTNR-X ')'                         
203000          DELIMITED BY SIZE INTO SSA1                                     
203100     STRING 'WDD702       '                                               
203200          DELIMITED BY SIZE INTO SSA2                                     
203300     MOVE '  GE' TO GODK-STATUSKODER                                      
203400     CALL CBLTDLI USING GNP WDD7-PCB DLI-IO-WDD702 SSA1 SSA2              
203500     MOVE WDD7-STATUS-CODE TO STATUS-WS                                   
203600     PERFORM IMS-STATUSKONTROLL                                           
203700     .                                                                    
203800     EJECT                                                                
203900 IMS-GET-WDJ1-CSEQ-NEXT SECTION.                                          
204000     STRING 'WDJ111  *D(WDJ1CSEQ =' W-WDJ1CSEQ-X ')'                      
204100             DELIMITED BY SIZE INTO SSA1                                  
204200     MOVE 'WDJ101   ' TO SSA2                                             
204300     MOVE '  GEGB' TO GODK-STATUSKODER                                    
204400     CALL CBLTDLI USING GN WDJ1C-PCB DLI-IO-WDJ1                          
204500                SSA1 SSA2                                                 
204600     MOVE WDJ1C-STATUS-CODE TO STATUS-WS                                  
204700     PERFORM IMS-STATUSKONTROLL                                           
204800     .                                                                    
204900     EJECT                                                                
205000 IMS-GET-WDD901 SECTION.                                                  
205100                                                                          
205200     STRING 'WDD901  (WDD901KY =' W-WDD901KY-X ')'                        
205300          DELIMITED BY SIZE INTO SSA1                                     
205400     MOVE '  GE' TO GODK-STATUSKODER                                      
205500     CALL CBLTDLI USING GU WDD9-PCB DLI-IO-WDD901 SSA1                    
205600     MOVE WDD9-STATUS-CODE TO STATUS-WS                                   
205700     PERFORM IMS-STATUSKONTROLL                                           
205800     .                                                                    
205900     EJECT                                                                
206000 IMS-GET-WDD902 SECTION.                                                  
206100                                                                          
206200     STRING 'WDD902     '                                                 
206300          DELIMITED BY SIZE INTO SSA1                                     
206400     MOVE '  GE' TO GODK-STATUSKODER                                      
206500     CALL CBLTDLI USING GNP WDD9-PCB DLI-IO-WDD902 SSA1                   
206600     MOVE WDD9-STATUS-CODE TO STATUS-WS                                   
206700     PERFORM IMS-STATUSKONTROLL                                           
206800     .                                                                    
206900     EJECT                                                                
207000 IMS-GET-BENA11-BSEQ SECTION.                                             
207100                                                                          
207200     STRING 'WDD301  (WDD3BSEQ =' W-IDARTNR-X ')'                         
207300             DELIMITED BY SIZE INTO SSA1                                  
207400     STRING 'WDD311  (IDSKYLT  =' W-IDSKYLT-X ')'                         
207500             DELIMITED BY SIZE INTO SSA2                                  
207600     MOVE '  GE' TO GODK-STATUSKODER                                      
207700     CALL CBLTDLI USING GU WDD3-PCB DLI-IO-WDD3 SSA1 SSA2                 
207800     MOVE WDD3-STATUS-CODE TO STATUS-WS                                   
207900     PERFORM IMS-STATUSKONTROLL                                           
208000     .                                                                    
208100     EJECT                                                                
208200 IMS-RESTART SECTION.                                                     
208300     SKIP2                                                                
208400     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
208500     MOVE '  ' TO GODK-STATUSKODER                                        
208600     CALL CBLTDLI USING XRST MSG-PCB                                      
208700                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
208800                        CHKP-AREA-LENGTH CHKP-AREA                        
208900     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
209000     PERFORM IMS-STATUSKONTROLL                                           
209100     .                                                                    
209200     SKIP3                                                                
209300 IMS-CHECKPOINT SECTION.                                                  
209400     SKIP2                                                                
209500     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
209600     MOVE '  XD' TO GODK-STATUSKODER                                      
209700     CALL CBLTDLI USING CHKP MSG-PCB                                      
209800                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
209900                        CHKP-AREA-LENGTH CHKP-AREA                        
210000     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
210100     PERFORM IMS-STATUSKONTROLL                                           
210200                                                                          
210300     IF IMS-EJ-OK                                                         
210400       MOVE 'IMS-KONTROLLREGION EJ TILLGÄNGLIG' TO FELTEXT-STR            
210500       DISPLAY FELTEXT                                                    
210600       CALL FELLOG                                                        
210700     END-IF                                                               
210800     .                                                                    
210900     EJECT                                                                
211000 IMS-STATUSKONTROLL SECTION.                                              
211100     SKIP2                                                                
211200     SET STATUS-IX TO 1                                                   
211300     SEARCH GODK-STATUS                                                   
211400       AT END                                                             
211500         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
211600           DELIMITED BY SIZE INTO FELTEXT                                 
211700         DISPLAY FELTEXT                                                  
211800         CALL FELLOG                                                      
211900       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
212000         CONTINUE                                                         
212100     END-SEARCH                                                           
212200     .                                                                    
212300     EJECT                                                                
212400 DB2-DCL-OPN-TP1ARTK-CRS  SECTION.                                        
212500     MOVE 'DB2-DCL-OPN-TP1ARTK   ' TO  WS-DB2-SEKTION                     
212600                                                                          
212700     MOVE 000100  TO GOOD-SQLCODECODES                                    
212800                                                                          
212900     EXEC SQL                                                             
213000         DECLARE TP1ARTK-CRS CURSOR FOR                                   
213100           SELECT  A.IDKAMP                                               
213200                  ,A.IDARTNR                                              
213300                  ,B.TISTADAT_KAMP                                        
213400                  ,B.TISTODAT_KAMP                                        
213500                  ,B.KDKAMP                                               
213600                  ,B.IDKAMP_GRP                                           
213700                                                                          
213800           FROM    TP1ARTK A                                              
213900                  ,TP1KAMP B                                              
214000                                                                          
214100           WHERE   A.IDARTNR = :W-IDARTNR                                 
214200               AND A.IDKAMP  =  B.IDKAMP                                  
214300                                                                          
214400           ORDER BY B.IDKAMP                                              
214500     END-EXEC                                                             
214600                                                                          
214700     MOVE 000100  TO GOOD-SQLCODECODES                                    
214800     MOVE SQLCODE TO SQLCODE-WS                                           
214900     EXEC SQL OPEN TP1ARTK-CRS END-EXEC                                   
215000     .                                                                    
215100     SKIP3                                                                
215200 DB2-FETCH-TP1ARTK-CRS  SECTION.                                          
215300     MOVE 'DB2-FETCH-TP1ARTK   ' TO  WS-DB2-SEKTION                       
215400     SKIP2                                                                
215500     MOVE 000100  TO GOOD-SQLCODECODES                                    
215600     EXEC SQL                                                             
215700         FETCH TP1ARTK-CRS INTO                                           
215800                    :TP1KAMP-IDKAMP                                       
215900                   ,:TP1ARTK-IDARTNR                                      
216000                   ,:TP1KAMP-TISTADAT-KAMP                                
216100                   ,:TP1KAMP-TISTODAT-KAMP                                
216200                   ,:TP1KAMP-KDKAMP                                       
216300                   ,:TP1KAMP-IDKAMP-GRP                                   
216400     END-EXEC                                                             
216500                                                                          
216600     MOVE SQLCODE TO SQLCODE-WS                                           
216700     PERFORM DB2-STATUS-CHECK                                             
216800     .                                                                    
216900     SKIP3                                                                
217000 DB2-CLOSE-TP1ARTK-CRS  SECTION.                                          
217100     MOVE 'DB2-CLOSE-TP1ARTK   ' TO  WS-DB2-SEKTION                       
217200                                                                          
217300     EXEC SQL CLOSE TP1ARTK-CRS END-EXEC                                  
217400     .                                                                    
217500     EJECT                                                                
217600 DB2-STATUS-CHECK  SECTION.                                               
217700                                                                          
217800     SET SQLCODE-IX TO 1                                                  
217900     SEARCH GOOD-SQLCODE                                                  
218000       AT END                                                             
218100*         STRING 'INVALID DB2 SQL STATUS CODE: ' SQLCODE-WS               
218200*         DELIMITED BY SIZE INTO ERROR-TEXT                               
218300          CALL FELLOG                                                     
218400       WHEN GOOD-SQLCODE (SQLCODE-IX) = SQLCODE-WS CONTINUE               
218500     END-SEARCH                                                           
218600     .                                                                    
