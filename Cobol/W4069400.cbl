000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W4069400.                                                
000400 AUTHOR.         SVANTE BJÖRKBERG.                                        
000500 DATE-WRITTEN.   SEPT 87.                                                 
000600                                                                          
000700     REMARKS.                                                             
000800*                                                                         
000900*    FUNKTION.                                                            
001000*        PROGRAMMET SKRIVER FRAKTSEDLAR FÖR BULKORDER SVERIGE.            
001100*                                                                         
001200*    INDATA.                                                              
001300*        TRANSAKTION: W4T611U    ELLER                                    
001600*                     W4T694U    ELLER                                    
001700*                     W4T694                                              
001800*        MID:         W4I61101   ELLER                                    
002000*                     W4I69401                                            
002100*                                                                         
002200*    UTDATA.                                                              
002300*        MOD:         W4O61101   ELLER                                    
002500*                     W4O69401                                            
002600*        LISTA:       L4O69401                                            
002700*    SKIP3                                                                
002800 ENVIRONMENT DIVISION.                                                    
002900     SKIP3                                                                
003000 DATA DIVISION.                                                           
003100     EJECT                                                                
003200 WORKING-STORAGE SECTION.                                                 
003300                                                                          
003400*    -- CHECKED BY WY2000                                                 
003500 77    PROGRAM-NAMN          PIC X(8)    VALUE 'W4069400'.                
003600 77    FELTEXT               PIC X(8)    VALUE SPACE.                     
003700 77    PGM-POS               PIC X(16)   VALUE SPACE.                     
003800 77    JA                    PIC X       VALUE 'J'.                       
003900 77    NEJ                   PIC X       VALUE 'N'.                       
003910 77    SVS                   PIC X       VALUE 'S'.                       
004000 77    FL-INDATA-OK          PIC X       VALUE 'J'.                       
004100 77    FRAKTS-SKALL-SKRIVAS  PIC X       VALUE 'N'.                       
004200 77    QUIT                  PIC X       VALUE 'Q'.                       
004300 77    SKRIVARE-NORDENKONTORET PIC X     VALUE 'N'.                       
004400 77    SKRIVARE-SVERIGE2     PIC X       VALUE 'N'.                       
004700 77    SKRIVARE-LANDROVER    PIC X       VALUE 'N'.                       
004710 77    SKRIVARE-SVSLEGOPACK  PIC X       VALUE 'N'.                       
004800 77    FILLERX4              PIC X(04).                                   
004900 77    FILLERX5              PIC X(05).                                   
005000 77    FILLERX6              PIC X(06).                                   
005100 77    FILLERX10             PIC X(10).                                   
005200 01    FILLERX60.                                                         
005300   03  FILLER                PIC X(30)   VALUE SPACE.                     
005400   03  FILLERX60-DEL2        PIC X(30).                                   
005500 77    FILLER                PIC X(8)    VALUE ALL 'A'.                   
005600 77    FILLERN8              PIC 9(08).                                   
005700 77    FILLERN9              PIC 9(09).                                   
005800 77    DUMMY-AREA            PIC X(50)   VALUE SPACE.                     
005900 77    FILLER                PIC X(8)    VALUE ALL 'B'.                   
006000 77    MAX-MOD2-LAENGD       PIC S9(4)   VALUE  +444 COMP SYNC.           
006100 77    FILLER                PIC X(8)    VALUE ALL 'C'.                   
006200 77    INDX                  PIC S9(2).                                   
006300 77    KOLLI-TAB-IX          PIC S9(9)   VALUE +0    COMP SYNC.           
006400 77    MOD-TAB-IX            PIC S9(9)   VALUE +0    COMP SYNC.           
006500 77    FILLER                PIC X(8)    VALUE ALL 'D'.                   
006600 77    RAD-IX                PIC S9(2).                                   
006700 77    RAD-IX2               PIC S9(2).                                   
006800 77    LIST-RAD-IX           PIC S9(2).                                   
006900 77    FILLER                PIC X(8)    VALUE ALL 'E'.                   
007000 77    RAD-MAX-P-1           PIC S9(2)   VALUE +14.                       
007100 77    IX                    PIC S9(2).                                   
007200 77    IX2                   PIC S9(2).                                   
007300 77    ANT-KOLLI-T-FSEDEL    PIC S9(2).                                   
007400 77    EMB-RADIX             PIC S9(2).                                   
007500 77    SUM-EMB-RADIX         PIC S9(2).                                   
007600 77    REMB-RADIX            PIC S9(2).                                   
007700 77    SUM-REMB-RADIX        PIC S9(2).                                   
007710 77    FILLER                PIC X(8)    VALUE ALL 'F'.                   
007720 77    RKOD-ABEND-MED-DUMP   PIC S9(4)   VALUE +33 COMP SYNC.             
007800 77    MAX-ANTAL-I-FRA-TAB   PIC S9(4)  VALUE +47.                        
007900 77    KOLLI-IX              PIC S9(9)   VALUE +0    COMP SYNC.           
008000 77    KOLLI-IX-MAX          PIC S9(4)   VALUE +160.                      
008100 77    FRAKEYACC             PIC S9(2)   VALUE +0.                        
008200 77    LIST-ACC-AVV          PIC S9(2).                                   
008300 77    LIST-MAX-AVV          PIC S9(2)   VALUE +21.                       
008400 77    LIST-MAX-AVV-P-1      PIC S9(2)   VALUE +22.                       
008500 77    TAB-ACC-AVV           PIC S9(2).                                   
008600 77    TAB-MAX-AVV           PIC S9(2)   VALUE +42.                       
008700 77    TAB-MAX-AVV-P-1       PIC S9(2)   VALUE +43.                       
008800 77    MAX-ANT-ORD-RAD-PA-FRAKTSEDEL PIC S9(2) VALUE +20.                 
008900 77    MAX-ANT-FRAKTSRAD-PLUS1       PIC S9(2) VALUE +61.                 
008910 77    FILLER                PIC X(8)    VALUE ALL 'G'.                   
009000 77    ANT-ORDER-RADER       PIC S9(2).                                   
009100 77    FRAKTS-NORDEN         PIC X(8)    VALUE '4FRNOR  '.                
009200 77    FRAKTS-SVERIGE2       PIC X(8)    VALUE '4FRSV2  '.                
009500 77    FRAKTS-LANDROVER      PIC X(8)    VALUE '4FRLR1  '.                
009510 77    FRAKTS-SVSLEGOPACK    PIC X(8)    VALUE '4FRSVS  '.                
009600 77    WS-IDPRINTER          PIC X(8)    VALUE '        '.                
009700     SKIP2                                                                
009800 77    NY-RAD-SW             PIC X.                                       
009900   88     EXTRA-LISTRAD-BEHOVS        VALUE 'J'.                          
009910 77    FILLER                PIC X(8)    VALUE ALL 'H'.                   
010000                                                                          
010100 77    WS-LIST3-TIUTSKR-SEK  PIC 9(8).                                    
010200*                                                                         
010300 01    DYNAMISKA-SUBPROGRAM.                                              
010400   03  W006PRR1              PIC X(8) VALUE 'W006PRR1'.                   
010600   03  CBLTDLI               PIC X(8) VALUE 'CBLTDLI '.                   
010700   03  FELLOG                PIC X(8) VALUE 'FELLOG  '.                   
010710   03  ABEND                 PIC X(8) VALUE 'ABEND   '.                   
010800     SKIP2                                                                
010900 01    WS-KDKOLLI.                                                        
011000   03  WS-KDKOLLI-1          PIC X.                                       
011100   03  WS-KDKOLLI-2          PIC X.                                       
011200   03  FILLER                PIC X(6).                                    
011300     SKIP2                                                                
011400 01  WS-KVRAM-X.                                                          
011500   03  WS-KVRAM              PIC 9.                                       
011600     SKIP2                                                                
011700 77  SW-NYTT-PRODNR          PIC X(01).                                   
011800   88  NYTT-PRODNR                      VALUE 'J'.                        
011900     SKIP2                                                                
011910 77    FILLER                PIC X(8)    VALUE ALL 'I'.                   
012000 77    WS-IDTRANS            PIC X(04).                                   
012100   88  WS-SAMMA-BILD             VALUE '4694'.                            
012200   88  WS-GODKAND-BILD           VALUE '4694' '4611'.                     
012400     SKIP2                                                                
012500 77    TRANSTYP              PIC 9.                                       
012600   88  ENTER-EGEN-BILD                  VALUE 1.                          
012700   88  KOMPLETTERING                    VALUE 2.                          
012800   88  UTSKRIFT                         VALUE 3.                          
012900     SKIP2                                                                
013000 77  WS-ENTER-EGEN-BILD      PIC 9(1)   VALUE 1.                          
013100 77  WS-KOMPLETTERING        PIC 9(1)   VALUE 2.                          
013200 77  WS-UTSKRIFT             PIC 9(1)   VALUE 3.                          
013300                                                                          
013400     EJECT                                                                
013900                                                                          
014000 01  FILLER.                                                              
014100   03  FILLER OCCURS 7.                                                   
014200     05  WSFRA-KVKOLLI       PIC S9(5)   COMP-3.                          
014300     05  WSFRA-VKORDBTO      PIC S9(6)V9 COMP-3.                          
014400     05  WSFRA-VLORDBTO      PIC S9(4)V9(3) COMP-3.                       
014500                                                                          
014600 01    WSFRA-KDFRAKT         PIC S9(3)   COMP-3.                          
014700     SKIP2                                                                
014800 01    FILLER                PIC X(8)    VALUE 'KOLLITAB'.                
014900 01  KOLLI-TAB.                                                           
015000   03  WSTAB-KOLLI   OCCURS 47.                                           
015100       05 WSTAB-IDKOLLI     PIC S9(5).                                    
015200*                                                                         
015300 01    FILLER                PIC X(8)    VALUE 'WS-TIME '.                
015400 01    WS-TIME.                                                           
015500   03  WS-TIME-POS1-6        PIC 9(6).                                    
015600   03  FILLER                PIC X(2).                                    
015700     SKIP2                                                                
015800                                                                          
015900 01    WS-BELISTID.                                                       
016000   03  FILLER                PIC X(01)   VALUE SPACE.                     
016100   03  WS-IDDISTR-BEL        PIC X(04).                                   
016200   03  FILLER                PIC X(02)   VALUE SPACE.                     
016300   03  WS-KDFRAKT-BEL        PIC X(02).                                   
016400   03  FILLER                PIC X(01)   VALUE SPACE.                     
016500     SKIP2                                                                
016600                                                                          
016700 01    WS-IDKUNDNR           PIC 9(6).                                    
016800 01    WS-KDFRAKT            PIC 9(3).                                    
016900 01    WS-IDDISTR            PIC 9(4).                                    
017000 01    WS-IDKUNDNR-NUM6      PIC 9(6).                                    
017100                                                                          
017200 01    WHELP.                                                             
017300   03  WHELP-MOD             PIC X(8)   VALUE 'W0050401'.                 
017400   03  WHELP-MOD-LL          PIC S9(4)  VALUE +85 COMP SYNC.              
017500     SKIP3                                                                
017600                                                                          
017700 01  SPAR-AREOR.                                                          
017800   03  SPAR-IDDISTR          PIC S9(5) COMP-3.                            
017900   03  SPAR-IDKUNDNR         PIC S9(7) COMP-3.                            
018000   03  SPAR-KDFRAKT          PIC S9(3) COMP-3.                            
018100     EJECT                                                                
018200                                                                          
018300 01  FILLER                  PIC X(16) VALUE                              
018400                             'NYCKLAR-TILL-DLI'.                          
018500*                                                                         
018600 01  W-WDE4D1KY-MIN-X.                                                    
018700   03  W-FLFYSAVV-MIN        PIC X            VALUE 'J'.                  
018800   03  W-IDDISTR-MIN         PIC S9(5) COMP-3.                            
018900   03  W-IDKUNDNR-MIN        PIC S9(7) COMP-3.                            
019000   03  W-IDKUNDRF-MIN        PIC X(10) VALUE SPACE.                       
019100   03  FILLER                PIC X(9)  VALUE LOW-VALUE.                   
019200*                                                                         
019300 01  W-WDE4D1KY-MAX-X.                                                    
019400   03  W-FLFYSAVV-MAX        PIC X            VALUE 'J'.                  
019500   03  W-IDDISTR-MAX         PIC S9(5) COMP-3.                            
019600   03  W-IDKUNDNR-MAX        PIC S9(7) COMP-3.                            
019700   03  W-IDKUNDRF-MAX        PIC X(10) VALUE SPACE.                       
019800   03  FILLER                PIC X(9)  VALUE HIGH-VALUE.                  
019900*                                                                         
020000 01    W-IDPRODNR-X.                                                      
020100   05  W-IDPRODNR            PIC S9(7)   VALUE ZERO  COMP-3.              
020200*                                                                         
020300 01    W-IDKOLLI-X.                                                       
020400   05  W-IDKOLLI                PIC S9(5)   VALUE ZERO  COMP-3.           
020500     EJECT                                                                
020600*                                                                         
020700 01  4732-WDGXKEY-X.                                                      
020800   05  4732-IDHTYP              PIC X(4)    VALUE '4732'.                 
020900   05  4732-KDFRAKT             PIC S9(3)   COMP-3.                       
021000   05  4732-LOWVALUE            PIC X(24)   VALUE LOW-VALUE.              
021100*                                                                         
021200 01  4732-KDSEGKEY-X.                                                     
021300   05  4732-KDSEGKEY            PIC X(1)    VALUE '1'.                    
021400*                                                                         
021500 01    W-WDQ2CSEQ-X.                                                      
021600   05  W-IDDISTR                PIC S9(5)   VALUE ZERO COMP-3.            
021700   05  W-IDKUNDNR               PIC S9(7)   VALUE ZERO COMP-3.            
021800   05  W-IDKUNDRF.                                                        
021900     07 W-IDORDNR7              PIC 9(7)    VALUE ZERO.                   
022000     07 FILLER                  PIC X(3)    VALUE SPACE.                  
022100*                                                                         
022200 01    W-IDDC-X.                                                          
022300   05  W-IDDC                   PIC X(2)    VALUE SPACE.                  
022400     EJECT                                                                
022900 01  FILLER                      PIC X(16) VALUE 'WWDIST-AREA'.           
023000 01  TEST-IDDISTR                PIC 9(5)                COMP-3.          
023100*01  FILLER -COPY WWDIST80       -RED TEST-IDDISTR.                       
023200     EJECT                                                                
023300*01    -COPY WWFRAKT1                                                     
023400    SKIP2                                                                 
023500 01    FELMEDDELANDE.                                                     
023600*                                                                         
023700   03    FEL-1.                                                           
023800     05  FILLER                  PIC X(40)   VALUE                        
023900        'UPPLYSTA FÄLT FEL                       '.                       
024000     05  FILLER                  PIC X(40)   VALUE                        
024100        'LIGHTENED FIELDS ARE WRONG              '.                       
024200   03    FILLER  REDEFINES  FEL-1.                                        
024300     05  FEL1        OCCURS 2    PIC X(40).                               
024400     SKIP2                                                                
024500   03    FEL-2.                                                           
024600     05  FILLER                  PIC X(40)   VALUE                        
024700        'FEL BILD VALD                           '.                       
024800     05  FILLER                  PIC X(40)   VALUE                        
024900        'WRONG PICTURE SELECTED                  '.                       
025000   03    FILLER  REDEFINES  FEL-2.                                        
025100     05  FEL2         OCCURS 2   PIC X(40).                               
025200     SKIP2                                                                
025300   03    FEL-3.                                                           
025400     05  FILLER                  PIC X(40)   VALUE                        
025500        'ANGE GODSMOTTAGARE                      '.                       
025600     05  FILLER                  PIC X(40)   VALUE                        
025700        'GIVE GOODS RECEIVER                     '.                       
025800   03    FILLER  REDEFINES  FEL-3.                                        
025900     05  FEL3         OCCURS 2   PIC X(40).                               
026000     EJECT                                                                
026100*                                                                         
026200 01  FILLER                      PIC X(8) VALUE 'KOLI-TAB'.               
026300 01  KOLLI-TAB.                                                           
026400   03  FILLER    OCCURS 10.                                               
026500     05  WS-IDKOLLI                PIC 9(05).                             
026600*                                                                         
026700 01  KOLLIKODER-POS1             PIC X(5) VALUE 'LKFGH'.                  
026800 01  FILLER    REDEFINES KOLLIKODER-POS1.                                 
026900   03  FILLER OCCURS 5.                                                   
027000     05  REMB-KDKOLLI-POS1         PIC X.                                 
027100     SKIP3                                                                
027200 01  EMBALLAGETYPER.                                                      
027300  03  FILLER             PIC X(10) VALUE 'CONT LÅDA '.                    
027400  03  FILLER             PIC X(10) VALUE '     PAKET'.                    
027500  03  FILLER             PIC X(10) VALUE 'BUNT STYCK'.                    
027600  03  FILLER             PIC X(10) VALUE '     HÄCK '.                    
027700  03  FILLER             PIC X(10) VALUE '     PALL '.                    
027800     SKIP3                                                                
027900 01  FILLER    REDEFINES EMBALLAGETYPER.                                  
028000   03  FILLER OCCURS 5.                                                   
028100     05  EMB-EMBTYP-1            PIC X(05).                               
028200     05  EMB-EMBTYP-2            PIC X(05).                               
028300     SKIP3                                                                
028400 01  SUM-EMB-TAB.                                                         
028500   03  FILLER    OCCURS 5.                                                
028600     05  SUM-EMB-KVEMBTYP      PIC 9(03).                                 
028700     05  SUM-EMB-EMBTYP-1        PIC X(05).                               
028800     05  SUM-EMB-EMBTYP-2        PIC X(05).                               
028900     05  SUM-EMB-VKORDBTO-EMBT   PIC 9(6)V9(1).                           
029000     05  SUM-EMB-VLORDBTO-EMBT   PIC 9(4)V9(3).                           
029100     SKIP3                                                                
029200 01  SUM-REMB-TAB.                                                        
029300   03  FILLER    OCCURS 5.                                                
029400     05  SUM-REMB-KDKOLLI-POS1   PIC X(01).                               
029500     05  SUM-REMB-KVPALL         PIC 9(03).                               
029600     05  SUM-REMB-KVRAM          PIC 9(03).                               
029700     05  SUM-REMB-KVLOCK         PIC 9(03).                               
029800     EJECT                                                                
029900 01  EMB-TAB.                                                             
030000   03  FILLER    OCCURS 5.                                                
030100     05  EMB-KVEMBTYP            PIC 9(03).                               
030200     05  EMB-VKORDBTO-EMBT       PIC 9(6)V9(1).                           
030300     05  EMB-VLORDBTO-EMBT       PIC 9(4)V9(3).                           
030400     05  EMB-PEKA-PA-SRAD        PIC 9(01).                               
030500     SKIP2                                                                
030600 01  REMB-TAB.                                                            
030700   03  FILLER    OCCURS 5.                                                
030800     05  REMB-KVPALL             PIC 9(03).                               
030900     05  REMB-KVRAM              PIC 9(03).                               
031000     05  REMB-KVLOCK             PIC 9(03).                               
031100     05  REMB-PEKA-PA-SRAD       PIC 9(01).                               
031200     SKIP2                                                                
031300 01  AVV-TAB.                                                             
031400   03  FILLER    OCCURS 42.                                               
031500     05  AVV-IDORDNR             PIC 9(05).                               
031600     05  AVV-IDARTNR             PIC 9(09).                               
031700     05  AVV-REKSIFFR            PIC 9(01).                               
031800     05  AVV-KVLEVART            PIC 9(07).                               
031900     SKIP2                                                                
032000 01  RAD-SKIP-KODER.                                                      
032100   03  FILLER PIC X(20) VALUE '02--0101010104------'.                     
032200   03  FILLER PIC X(20) VALUE '01010102--11--------'.                     
032300   03  FILLER PIC X(20) VALUE '------------01010101'.                     
032400   03  FILLER PIC X(20) VALUE '010101010104------01'.                     
032500   03  FILLER PIC X(20) VALUE '01010101010101010101'.                     
032600   03  FILLER PIC X(20) VALUE '01010101010101010101'.                     
032700 01  FILLER REDEFINES RAD-SKIP-KODER.                                     
032800  03  FILLER OCCURS 60.                                                   
032900    05  LIST-RADSKIP             PIC 9(02).                               
033000     EJECT                                                                
033100 01  RAD-SKIP-KODER-LIST3.                                                
033200   03  FILLER PIC X(20) VALUE '02--0101010101010101'.                     
033300   03  FILLER PIC X(20) VALUE '01010102--11--------'.                     
033400   03  FILLER PIC X(20) VALUE '------------01010101'.                     
033500   03  FILLER PIC X(20) VALUE '01010101010101010101'.                     
033600   03  FILLER PIC X(20) VALUE '01010101010101010101'.                     
033700   03  FILLER PIC X(20) VALUE '01010101010101010101'.                     
033800 01  FILLER REDEFINES RAD-SKIP-KODER-LIST3.                               
033900  03  FILLER OCCURS 60.                                                   
034000    05  RADSKIP-LIST3            PIC 9(02).                               
034100     EJECT                                                                
034200 01  LIST-TEXTER.                                                         
034300       03  WS-SPECIALTEXT        PIC X(17)                                
034400              VALUE 'EXP  -  592790   '.                                  
034500       03  WS-LVNAMN-RAD1        PIC X(16)                                
034600              VALUE 'VOLVO TRUCK     '.                                   
034700       03  WS-PVNAMN-RAD1        PIC X(16)                                
034800              VALUE 'VOLVO CAR CORP.,'.                                   
034900       03  WS-LVNAMN-RAD2        PIC X(11)                                
035000              VALUE 'PARTS CORP.'.                                        
035100       03  WS-PVNAMN-RAD2        PIC X(11)                                
035200              VALUE 'CUSTOMER SE'.                                        
035300       03  WS-ADRESS-RAD3        PIC X(15)                                
035400              VALUE '405 31 GÖTEBORG'.                                    
035500     EJECT                                                                
035600 01  FILLER                      PIC X(8) VALUE 'LIST3RAD'.               
035700*    LIST 3 FÖR SVERIGE-KOLLI DAGORDER                                    
035800 01  LIST3-RADER.                                                         
035900   03  FILLER OCCURS 60.                                                  
036000     05  LIST3-RAD                PIC X(80).                              
036100 01  LIST3-TEXT-RADER   REDEFINES LIST3-RADER.                            
036200     03 LIST3-RAD-1.                                                      
036300        05 FILLER             PIC X(53).                                  
036400        05 LIST3-BEFRAKT      PIC X(15).                                  
036500        05 LIST3-LEDTEXT      PIC X(03).                                  
036600        05 LIST3-KDFRAKT      PIC Z(02).                                  
036700        05 FILLER             PIC X(7).                                   
036800     03 LIST3-RAD-2.                                                      
036900        05 FILLER             PIC X(80).                                  
037000     03 LIST3-RAD-3.                                                      
037100        05 FILLER              PIC X(22).                                 
037200        05 LIST3-GODSADNR-0    PIC X(17).                                 
037300        05 FILLER              PIC X(41).                                 
037400     03 LIST3-RAD-4.                                                      
037500        05 FILLER              PIC X(5).                                  
037600        05 LIST3-GODSAVS-1     PIC X(16).                                 
037700        05 FILLER              PIC X(1).                                  
037800        05 LIST3-GODSADNR-1    PIC X(17).                                 
037900        05 FILLER              PIC X(08).                                 
038000        05 LIST3-TIUTSKR       PIC 9(6).                                  
038100        05 FILLER              PIC X(1).                                  
038200        05 LIST3-TIUTSTID      PIC 9(2).9(2).                             
038300        05 FILLER              PIC X(21).                                 
038400     03 LIST3-RAD-5.                                                      
038500        05 FILLER              PIC X(5).                                  
038600        05 LIST3-GODSAVS-2     PIC X(11).                                 
038700        05 FILLER              PIC X(6).                                  
038800        05 LIST3-GODSADNR-2    PIC X(17).                                 
038900        05 FILLER              PIC X(41).                                 
039000     03 LIST3-RAD-6.                                                      
039100        05 FILLER              PIC X(5).                                  
039200        05 LIST3-GODSAVSADR-3  PIC X(15).                                 
039300        05 FILLER              PIC X(2).                                  
039400        05 LIST3-GODSADNR-3    PIC X(17).                                 
039500        05 FILLER              PIC X(2).                                  
039600        05 LIST3-KRYSS-IDAVD  PIC X(1).                                   
039700        05 FILLER            PIC X(6).                                    
039800        05 LIST3-IDAVD        PIC X(5).                                   
039900        05 FILLER            PIC X(04).                                   
040000        05 LIST3-KRYSS-DAGORD PIC X(1).                                   
040100        05 FILLER            PIC X(22).                                   
040200     03 LIST3-RAD-7.                                                      
040300        05 FILLER            PIC X(41).                                   
040400        05 LIST3-KRYSS-DIST   PIC X(1).                                   
040500        05 FILLER            PIC X(10).                                   
040600        05 LIST3-IDDISTR-BET  PIC X(4).                                   
040700        05 FILLER            PIC X(24).                                   
040800     03 LIST3-RAD-8-10.                                                   
040900        05 FILLER            OCCURS 3 TIMES.                              
041000           07 FILLER         PIC X(80).                                   
041100     03 LIST3-RAD-11.                                                     
041200        05 FILLER            PIC X(5).                                    
041300        05 LIST3-BEGMT-RAD1  PIC X(35).                                   
041400        05 FILLER            PIC X(40).                                   
041500     03 LIST3-RAD-12.                                                     
041600        05 FILLER            PIC X(5).                                    
041700        05 LIST3-BEGMT-RAD2  PIC X(35).                                   
041800        05 FILLER            PIC X(40).                                   
041900     03 LIST3-RAD-13.                                                     
042000        05 FILLER            PIC X(5).                                    
042100        05 LIST3-ADGMT-RAD1  PIC X(35).                                   
042200        05 FILLER            PIC X(40).                                   
042300     03 LIST3-RAD-14.                                                     
042400        05 FILLER            PIC X(5).                                    
042500        05 LIST3-ADGMT-RAD2  PIC X(27).                                   
042600        05 LIST3-IDDISTR-GDM  PIC Z(4).                                   
042700        05 FILLER            PIC X(5).                                    
042800        05 LIST3-KRYSS-AVSBET PIC X.                                      
042900        05 FILLER            PIC X(38).                                   
043000     03 LIST3-RAD-15.                                                     
043100        05 FILLER            PIC X(80).                                   
043200     03 LIST3-RAD-16.                                                     
043300        05 FILLER            PIC X(5).                                    
043400        05 LIST3-BEGMRK      PIC X(35).                                   
043500        05 FILLER            PIC X(1).                                    
043600        05 LIST3-KRYSS-MOTBET PIC X.                                      
043700        05 FILLER            PIC X(38).                                   
043800     03 LIST3-RAD-17-26.                                                  
043900        05 FILLER            OCCURS 10 TIMES.                             
044000           07 FILLER         PIC X(80).                                   
044100     03 LIST3-RAD-27-36.                                                  
044200        05 FILLER            OCCURS 5 TIMES.                              
044300           07 LIST3-EMB-RAD-1.                                            
044400              09 FILLER      PIC X(26).                                   
044500              09 LIST3-EMBTYP-1                                           
044600                             PIC X(5).                                    
044700              09 FILLER      PIC X(49).                                   
044800           07 LIST3-EMB-RAD-2.                                            
044900              09 FILLER      PIC X(5).                                    
045000              09 LIST3-IDDISTR-EMB                                        
045100                             PIC Z(5).                                    
045200              09 FILLER      PIC X(2).                                    
045300              09 LIST3-IDKUNDNR-EMB                                       
045400                             PIC Z(6).                                    
045500              09 FILLER      PIC X(4).                                    
045600              09 LIST3-KVEMBTYP                                           
045700                             PIC Z(3).                                    
045800              09 FILLER      PIC X.                                       
045900              09 LIST3-EMBTYP-2                                           
046000                             PIC X(5).                                    
046100              09 FILLER      PIC X.                                       
046200              09 LIST3-KDKOLLI                                            
046300                             PIC X(8).                                    
046400              09 FILLER      PIC X(17).                                   
046500              09 LIST3-VKORDBTO-EMBT                                      
046600                             PIC Z(6).Z.                                  
046700              09 FILLER      PIC X(01).                                   
046800              09 LIST3-VLORDBTO-EMBT                                      
046900                             PIC Z(4).Z(3).                               
047000              09 FILLER      PIC X(6).                                    
047100*    03 LIST3-RAD-37-38.                                                  
047200*       05 FILLER            OCCURS 2 TIMES.                              
047300*          07 FILLER         PIC X(80).                                   
047400*    03 LIST3-RAD-37.                                                     
047500*       05 FILLER           PIC X(80).                                    
047600     03 LIST3-RAD-38.                                                     
047700        05 LIST3-RAD-38-1   PIC X(11).                                    
047800        05 LIST3-RAD-38-2   PIC X(13).                                    
047900        05 LIST3-RAD-38-3   PIC X(8).                                     
048000        05 LIST3-RAD-38-4   PIC X(22).                                    
048100        05 LIST3-RAD-38-5   PIC X(26).                                    
048200     03 LIST3-RAD-39.                                                     
048300        05 LIST3-RAD-39-1   PIC X(11).                                    
048400        05 LIST3-RAD-39-2   PIC X(5).                                     
048500        05 LIST3-RAD-39-3   PIC X(7).                                     
048600        05 LIST3-RAD-39-4   PIC X(1).                                     
048700        05 LIST3-RAD-39-5   PIC X(8).                                     
048800        05 LIST3-RAD-39-6   PIC X(6).                                     
048900        05 LIST3-RAD-39-7   PIC X(6).                                     
049000        05 LIST3-RAD-39-8   PIC X(7).                                     
049100        05 LIST3-RAD-39-9   PIC X(1).                                     
049200        05 LIST3-RAD-39-10  PIC X(8).                                     
049300        05 LIST3-RAD-39-11  PIC X(20).                                    
049400     03 LIST3-RAD-40-60.                                                  
049500        05 LIST3-KOLLI-RAD    OCCURS 20 TIMES.                            
049600           07 FILLER         PIC X(4).                                    
049700           07 LIST3-IDORDNR  PIC Z(5).                                    
049800           07 LIST3-KOLLI1   PIC Z(5).                                    
049900           07 LIST3-KOLLI2   PIC Z(5).                                    
050000           07 LIST3-KOLLI3   PIC Z(5).                                    
050100           07 LIST3-KOLLI4   PIC Z(5).                                    
050200           07 FILLER-KOLLI.                                               
050300              09 FILLER         PIC X.                                    
050400              09 LIST3-KOLLI5   PIC Z(5).                                 
050500              09 LIST3-KOLLI6   PIC Z(5).                                 
050600              09 LIST3-KOLLI7   PIC Z(5).                                 
050700              09 LIST3-KOLLI8   PIC Z(5).                                 
050800           07 FILLER         PIC X(9).                                    
050900           07 LIST3-KDKOLLI-POS1  PIC X.                                  
051000           07 FILLER         PIC X.                                       
051100           07 LIST3-KVPALL   PIC Z(3).                                    
051200           07 FILLER         PIC X.                                       
051300           07 LIST3-KVRAM    PIC Z(3).                                    
051400           07 FILLER         PIC X.                                       
051500           07 LIST3-KVLOCK   PIC Z(3).                                    
051600           07 FILLER         PIC X(8).                                    
051700 01    FILLER                PIC X(16) VALUE 'SLUT-TABELLIST3'.           
051800*SLUT-TABELL-LIST3                                                        
051900     EJECT                                                                
052000*01  -COPY W006PRAR                                                       
052100     EJECT                                                                
052500******************************************************************        
052600*                                                                *        
052700*                AREOR FÖR MFS OCH SKÄRMHANTERING                *        
052800*                                                                *        
052900******************************************************************        
053000 01    FILLER                 PIC X(16) VALUE 'MID W4I34201MID1'.         
053100     SKIP3                                                                
053200*01    MID -COPY W4I69401  -PRE MID1-.                                    
053300     EJECT                                                                
053400 01    FILLER                 PIC X(16) VALUE 'MID W4I34201MID2'.         
053500     SKIP3                                                                
053600*01    MID -COPY W4I69402  -PRE MID2-.                                    
053700     EJECT                                                                
053800*01    -COPY WMSGAREA                                                     
053900     EJECT                                                                
054000*  03    MOD -COPY W4O69401  -RED MSG-AREA  -PRE MOD2-.                   
054100     EJECT                                                                
054200   03  FEL-MOD REDEFINES MSG-AREA.                                        
054300     05  MODFEL-IDTRANS          PIC X(04).                               
054400     05  MODFEL-TEMFSFEL         PIC X(40).                               
054500 01    FILLER                    PIC X(16) VALUE 'ALT1-IO-AREA'.          
054600     SKIP3                                                                
054700 01    ALT1-IO-AREA.                                                      
054800   03  ALT1-LL                   PIC S9(4) VALUE +360 COMP SYNC.          
054900   03  ALT1-Z1                   PIC X(01) VALUE LOW-VALUE.               
055000   03  ALT1-Z2                   PIC X(01) VALUE HIGH-VALUE.              
055100   03  ALT1-TRANSKOD             PIC X(06) VALUE 'W4T611'.                
055200   03  FILLER                    PIC X(02) VALUE SPACE.                   
055300   03  FILLER                    PIC X(04) VALUE '4694'.                  
055400   03  ALT1-KDMFSFOR             PIC X(01) VALUE '1'.                     
055500*03    -COPY W4I61101 -PRE ALT1-                                          
055600     EJECT                                                                
056900 01    FILLER                    PIC X(16)   VALUE 'MFS-WS'.              
057000     SKIP3                                                                
057100*01    -COPY WMFSAREA                                                     
057200     EJECT                                                                
057300******************************************************************        
057400*                                                                         
057500*        ARBETS-AREOR TILL IMS-SEKTIONERNA                                
057600*                                                                         
057700 01    IMS-WS.                                                            
057800   03    FILLER                  PIC X(16)   VALUE 'IMS-WS     '.         
057900     SKIP3                                                                
058000*                        **** STATUS-KOD FRÅN IMS                         
058100   03    STATUS-WS               PIC XX.                                  
058200     88    SEGMENT-FINNS                     VALUE '  '.                  
058300     88    SEGMENT-SAKNAS                    VALUE 'GE'.                  
058400     88    SEGMENT-FINNS-REDAN               VALUE 'II'.                  
058500     88    SEGMENT-INDEX-DUBBLET             VALUE 'NI'.                  
058600     SKIP3                                                                
058700   03    GODK-STATUSKODER.                                                
058800     05    GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.              
058900     SKIP3                                                                
059000 01    FILLER    PIC X(24)  VALUE 'DLI SSA AREA'.                         
059100 01    SSA1                      PIC X(160).                              
059200 01    SSA2                      PIC X(96).                               
059300     EJECT                                                                
059400*                            IMS FUNKTIONSKODER                           
059500*01    -COPY W0003                                                        
059600     EJECT                                                                
059700*                            DLI INPUT-OUTPUT AREA                        
059800 01    FILLER    PIC X(16)  VALUE 'DLI-IO-E601'.                          
059900 01    DLI-IO-E601.                                                       
060000*  03    -COPY WDE601                                                     
060100     EJECT                                                                
060200 01    FILLER    PIC X(16)  VALUE 'DLI-IO-E611'.                          
060300 01    DLI-IO-E611.                                                       
060400*  03    -COPY WDE611                                                     
060500     EJECT                                                                
060600 01    FILLER    PIC X(16)  VALUE 'DLI-IO-E4D1'.                          
060700 01    DLI-IO-E4D1.                                                       
060800*  03    -COPY WDE4D1                                                     
060900     EJECT                                                                
061500 01    FILLER    PIC X(24)  VALUE 'DLI INPUT-OUTPUT AREA2'.               
061600 01    DLI-IO-AREA-2.                                                     
061700   03    IO-AREA-2               PIC X(300)  VALUE SPACE.                 
061800     SKIP3                                                                
061900*  03    WL473201 -COPY WDGX473B           -RED IO-AREA-2.                
062000     EJECT                                                                
062100*  03    WL473211 -COPY WDGX4732           -RED IO-AREA-2.                
062200     EJECT                                                                
062300*                                                                         
062400 01  FILLER                      PIC X(16)   VALUE 'WDQ201-AREA'.         
062500 01  DLI-IO-AREA-OHUV.                                                    
062600*  03  -COPY WDQ201                                                       
062700     EJECT                                                                
062800 01  FILLER                      PIC X(16)   VALUE 'WDQ212-AREA'.         
062900 01  DLI-IO-AREA-ARB.                                                     
063000*  03  -COPY WDQ212                                                       
063100     EJECT                                                                
063200 LINKAGE SECTION.                                                         
063300*01    -COPY W0009     -PRE MSG-                                          
063400     EJECT                                                                
063500*01    -COPY W0009     -PRE ALT1-                                         
063600     EJECT                                                                
063700*01    -COPY W0009     -PRE ALT2-                                         
063800     EJECT                                                                
063900*01    -COPY W0009     -PRE ALT3-                                         
064000     EJECT                                                                
064300*01    -COPY W0008     -PRE LISB-                                         
064400     05  FILLER                  PIC X.                                   
064500     EJECT                                                                
064600*01    -COPY W0008     -PRE WDE4D-                                        
064700     05  FILLER                  PIC X.                                   
064800     EJECT                                                                
064900*01    -COPY W0008     -PRE WDE6-                                         
065000     05  FILLER                  PIC X.                                   
065100     EJECT                                                                
065200*01    -COPY W0008     -PRE 4732-                                         
065300     05  FILLER                  PIC X.                                   
065400     EJECT                                                                
065800*01    -COPY W0008     -PRE WDQ2-                                         
065900     05  FILLER                  PIC X.                                   
066000     EJECT                                                                
066100 PROCEDURE DIVISION USING  MSG-PCB  ALT1-PCB ALT2-PCB ALT3-PCB            
066200                           LISB-PCB WDE4D-PCB                             
066300                           WDE6-PCB 4732-PCB WDQ2-PCB.                    
066400                                                                          
066500     ENTRY 'DLITCBL' USING MSG-PCB  ALT1-PCB ALT2-PCB ALT3-PCB            
066510                           LISB-PCB WDE4D-PCB                             
066520                           WDE6-PCB 4732-PCB WDQ2-PCB.                    
066800                                                                          
066900     PERFORM IMS-GET-MSG                                                  
067000                                                                          
067100     IF SEGMENT-FINNS                                                     
067200       PERFORM A-INIT                                                     
067300       MOVE MFS-IDTRANS TO WS-IDTRANS                                     
067400                                                                          
067500       IF WS-GODKAND-BILD                                                 
067600         PERFORM B-KOLLA-TRANS                                            
067700                                                                          
067800         EVALUATE TRANSTYP                                                
067900           WHEN WS-ENTER-EGEN-BILD                                        
068000             PERFORM S01-KOLLA-INDATA-EGEN-BILD                           
068100           WHEN WS-KOMPLETTERING                                          
068200             PERFORM D-KOMPLETTERING                                      
068300           WHEN WS-UTSKRIFT                                               
068400             PERFORM E-UTSKRIFT                                           
068500         END-EVALUATE                                                     
068600       ELSE                                                               
068700                                                                          
068800         PERFORM F-FELAKTIGT-ANROP                                        
068900       END-IF                                                             
069000     END-IF                                                               
069100                                                                          
069200     MOVE ZERO TO RETURN-CODE                                             
069300     GOBACK                                                               
069400     .                                                                    
069500     EJECT                                                                
069600 A-INIT             SECTION.                                              
069700     MOVE 'START A-INIT'                  TO PGM-POS                      
069800                                                                          
069900     IF MSG-DUBBLA-TRANSKODER                                             
070000       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO   MID1-W4I69401-CTX          
070100                                               MID2-W4I69402-CTX          
070200       MOVE MSG-IDTRANS-2                 TO   MFS-IDTRANS                
070300                                               WS-IDTRANS                 
070400       MOVE MSG-KDMFSFOR-2                TO   MFS-KDMFSFOR               
070500       MOVE MSG-KDTRTYP                   TO   MFS-KDTRTYP                
070600     ELSE                                                                 
070700       MOVE MSG-INDATA-MINUS-1-TRANSKOD   TO   MID1-W4I69401-CTX          
070800                                               MID2-W4I69402-CTX          
070900       MOVE MSG-IDTRANS-1                 TO   MFS-IDTRANS                
071000                                               WS-IDTRANS                 
071100       MOVE MSG-KDMFSFOR-1                TO   MFS-KDMFSFOR               
071200       MOVE ' '                           TO   MFS-KDTRTYP                
071300     END-IF                                                               
071400                                                                          
071500     MOVE LOW-VALUE                       TO   MSG-AREA                   
071600     MOVE MFS-IDTRANS                     TO   MOD2-IDTRANS               
071700                                                                          
071800     MOVE NEJ                           TO SKRIVARE-SVERIGE2              
071900                                           SKRIVARE-NORDENKONTORET        
072200                                           SKRIVARE-LANDROVER             
072210                                           SKRIVARE-SVSLEGOPACK           
072300                                                                          
072400     MOVE +1                TO KOLLI-TAB-IX                               
072500     PERFORM UNTIL KOLLI-TAB-IX > MAX-ANTAL-I-FRA-TAB                     
072600                                                                          
072700       MOVE +00000                      TO                                
072800            WSTAB-IDKOLLI(KOLLI-TAB-IX)                                   
072900       ADD +1               TO KOLLI-TAB-IX                               
073000     END-PERFORM                                                          
073100                                                                          
073200     IF ENGLISH-TEXT                                                      
073300         MOVE +2                          TO   INDX                       
073400     ELSE                                                                 
073500         MOVE +1                          TO   INDX                       
073600     END-IF                                                               
073700                                                                          
073800     MOVE MFS-RENSA-FAELT                 TO MOD2-TEMFSFEL                
073900                                             MOD2-BEGMT-RAD1              
074000                                             MOD2-ADGMT-RAD1              
074100                                             MOD2-ADGMT-RAD2              
074200                                             MOD2-IDAVD                   
074300                                             MOD2-IDDISTR-BET             
074400                                             MOD2-FLMOTBET                
074500                                             MOD2-TEMFSINF                
074600     MOVE ZERO               TO FILLERN9                                  
074700                                FILLERN8                                  
074800                                WS-TIME                                   
074900     MOVE 'END A-INIT'                  TO PGM-POS                        
075000     .                                                                    
075100     EJECT                                                                
075200 B-KOLLA-TRANS     SECTION.                                               
075300     MOVE 'START B-KOLLA'                 TO PGM-POS                      
075400                                                                          
075500     IF WS-SAMMA-BILD                                                     
075600       IF MFS-UPDATE                                                      
075700         MOVE WS-UTSKRIFT                 TO TRANSTYP                     
075800       ELSE                                                               
075900         MOVE WS-ENTER-EGEN-BILD          TO TRANSTYP                     
076000       END-IF                                                             
076100     ELSE                                                                 
076200       IF MID1-IDPRODNR-KOMPL = ZERO                                      
076300         MOVE WS-UTSKRIFT                 TO TRANSTYP                     
076400       ELSE                                                               
076500         MOVE WS-KOMPLETTERING            TO TRANSTYP                     
076600       END-IF                                                             
076700     END-IF                                                               
076800     .                                                                    
076900     EJECT                                                                
077000 D-KOMPLETTERING      SECTION.                                            
077100     MOVE 'START D-KOMPL'                 TO PGM-POS                      
077200                                                                          
077300     PERFORM DA-KLARMARKERA                                               
077400     MOVE MID1-IDDISTR-UT                 TO MOD2-IDDISTR-UT              
077500     MOVE MID1-IDKUNDNR-UT                TO MOD2-IDKUNDNR-UT             
077600     MOVE MID1-KDFRAKT-UT                 TO MOD2-KDFRAKT-UT              
077700     MOVE MID1-IDDC-UT                    TO MOD2-IDDC-UT                 
077720     MOVE MID1-FLLANDROVER                TO MOD2-FLLANDROVER             
077800     MOVE 1                               TO RAD-IX                       
077900                                             RAD-IX2                      
078000                                                                          
078100     PERFORM UNTIL RAD-IX = RAD-MAX-P-1                                   
078200       IF MID1-RAD-KDUPPTYP (RAD-IX) = 'K'                                
078300         PERFORM DB-GODSM-INFO-FRAN-WDQ2                                  
078400         MOVE MID1-RAD-IDDISTR  (RAD-IX)  TO MOD2-IDDISTR                 
078500         MOVE MID1-RAD-IDKUNDNR (RAD-IX)  TO MOD2-IDKUNDNR                
078600         MOVE MID1-RAD-KDFRAKT  (RAD-IX)  TO MOD2-KDFRAKT                 
078700         MOVE MID1-RAD-IDORDNR  (RAD-IX)  TO                              
078800              MOD2-IDORDNR      (RAD-IX2)                                 
078900         MOVE MID1-RAD-IDPRODNR (RAD-IX)  TO                              
079000              MOD2-IDPRODNR     (RAD-IX2)                                 
079100                                                                          
079200         MOVE MID1-RAD-IDDISTR  (RAD-IX)  TO WS-IDDISTR                   
079300         MOVE MID1-RAD-IDKUNDNR (RAD-IX)  TO WS-IDKUNDNR-NUM6             
079400         MOVE MID1-RAD-KDFRAKT  (RAD-IX)  TO WS-KDFRAKT                   
079500         ADD +1                           TO RAD-IX2                      
079600       END-IF                                                             
079700                                                                          
079800       ADD +1                             TO RAD-IX                       
079900     END-PERFORM                                                          
080000                                                                          
080100     INSPECT MOD2-IDDISTR-UT  REPLACING LEADING ZERO BY SPACE             
080200     INSPECT MOD2-IDKUNDNR-UT REPLACING LEADING ZERO BY SPACE             
080300     INSPECT MOD2-KDFRAKT-UT  REPLACING LEADING ZERO BY SPACE             
080400                                                                          
080500     MOVE '4694'                          TO MOD2-IDTRANS                 
080600                                                                          
080700     MOVE MAX-MOD2-LAENGD                 TO MSG-KVLL                     
080800     MOVE 'W4O69401'                      TO MFS-IDMOD                    
080900     PERFORM IMS-INSERT-MSG                                               
081000     MOVE 'END D-KOMPL'                 TO PGM-POS                        
081100     .                                                                    
081200     EJECT                                                                
081300 DA-KLARMARKERA       SECTION.                                            
081400     MOVE 'START DA-KLAR'                 TO PGM-POS                      
081500                                                                          
081600     MOVE 1                               TO RAD-IX                       
081700                                                                          
081800     PERFORM UNTIL RAD-IX = RAD-MAX-P-1                                   
081900       IF MID1-RAD-KDUPPTYP (RAD-IX) = 'B'                                
082000         MOVE MID1-RAD-IDPRODNR (RAD-IX)  TO W-IDPRODNR                   
082100         PERFORM IMS-GHU-WDE601                                           
082200                                                                          
082300         EVALUATE TRUE                                                    
082400         WHEN VORD-KVKOLLI = +00001                                       
082500           MOVE NEJ                     TO VORD-FLFRAKTS                  
082600           PERFORM IMS-REPL-WDE601                                        
082700         WHEN VORD-KVKOLLI NOT = VORD-KVKOLPAC                            
082800             CONTINUE                                                     
082900         WHEN OTHER                                                       
083000           MOVE NEJ                     TO VORD-FLFRAKTS                  
083100           PERFORM IMS-REPL-WDE601                                        
083200         END-EVALUATE                                                     
083300       END-IF                                                             
083400                                                                          
083500       ADD +1  TO RAD-IX                                                  
083600     END-PERFORM                                                          
083700     MOVE 'END DA-KLAR'                 TO PGM-POS                        
083800     .                                                                    
083900     EJECT                                                                
084000 DB-GODSM-INFO-FRAN-WDQ2    SECTION.                                      
084100     MOVE 'START DB-GODSM'                TO PGM-POS                      
084200                                                                          
084300     MOVE MID1-RAD-IDDISTR (RAD-IX)       TO W-IDDISTR                    
084400     MOVE MID1-RAD-IDKUNDNR (RAD-IX)      TO W-IDKUNDNR                   
084500     MOVE MID1-RAD-IDORDNR (RAD-IX)       TO W-IDORDNR7                   
084600                                                                          
084700     PERFORM IMS-GU-WDQ2-CSEQ                                             
084800                                                                          
084900     MOVE OHUV-BEGMT-RAD1                 TO MOD2-BEGMT-RAD1              
085000     MOVE OHUV-BEGMT-RAD2                 TO MOD2-BEGMT-RAD2              
085100     MOVE OHUV-ADGMT-GATA                 TO MOD2-ADGMT-RAD1              
085200     MOVE OHUV-ADGMT-PADR                 TO MOD2-ADGMT-RAD2              
085300     MOVE OHUV-ADGMT-PADR                 TO MOD2-ADGMT-RAD2              
085400                                                                          
085500     MOVE MID1-IDDC-UT                    TO W-IDDC                       
085600                                                                          
085700     PERFORM IMS-GNP-WDQ212                                               
085800                                                                          
085900     IF ARB-BEGMRK-RAD1 > SPACE                                           
086000       MOVE ARB-BEGMRK-RAD1               TO MOD2-BEGMRK-RAD1             
086100     ELSE                                                                 
086200       MOVE ARB-BEGMRK-RAD2               TO MOD2-BEGMRK-RAD1             
086300     END-IF                                                               
086400                                                                          
086500     MOVE 'END DB-GODSM'                  TO PGM-POS                      
086600     .                                                                    
086700     EJECT                                                                
086800 E-UTSKRIFT           SECTION.                                            
086900     MOVE 'START E-UTSKRIFT'              TO PGM-POS                      
087000                                                                          
087100     IF WS-SAMMA-BILD                                                     
087200       PERFORM S01-KOLLA-INDATA-EGEN-BILD                                 
087300     END-IF                                                               
087400                                                                          
087500     IF FL-INDATA-OK = JA                                                 
087600       IF WS-SAMMA-BILD                                                   
087700         PERFORM EH-INGANG-FRAN-4694                                      
087800       END-IF                                                             
087900                                                                          
088000       MOVE +1     TO RAD-IX                                              
088100                      KOLLI-TAB-IX                                        
088200       PERFORM UNTIL RAD-IX = RAD-MAX-P-1                                 
088300        IF (MID1-RAD-KDUPPTYP (RAD-IX) = '+')                             
088400        OR (MID1-RAD-KDUPPTYP (RAD-IX) = 'V'                              
088500        AND   MID1-IDPRODNR-KOMPL = 0)                                    
088600        OR (MID1-RAD-KDUPPTYP (RAD-IX) = 'K')                             
088700          MOVE MID1-RAD-KDFRAKT(RAD-IX) TO FRAK01-KDFRAKT                 
088800          MOVE MID1-RAD-IDDISTR(RAD-IX) TO TEST-IDDISTR                   
088900                                                                          
089010          IF MID1-FLLANDROVER = SVS                                       
089100            MOVE JA TO SKRIVARE-SVSLEGOPACK                               
089200          ELSE                                                            
089210            IF MID1-FLLANDROVER = JA                                      
089220              MOVE JA TO SKRIVARE-LANDROVER                               
089230            ELSE                                                          
089900               IF (FRAK01-KDFRAKT21 OR FRAK01-KDFRAKT62)                  
090000                  IF DIST80-FRAKTS-NORDEN                                 
090100                     MOVE JA TO SKRIVARE-NORDENKONTORET                   
090200                  ELSE                                                    
090300                     MOVE JA TO SKRIVARE-SVERIGE2                         
090400                  END-IF                                                  
090500               ELSE                                                       
090600                  IF FRAK01-NORDEN                                        
090700                     MOVE JA TO SKRIVARE-NORDENKONTORET                   
090800                  ELSE                                                    
090900                     MOVE JA TO SKRIVARE-SVERIGE2                         
091000                  END-IF                                                  
091100               END-IF                                                     
091400            END-IF                                                        
091410          END-IF                                                          
091500        END-IF                                                            
091600        ADD +1                         TO RAD-IX                          
091700       END-PERFORM                                                        
091800                                                                          
091900       IF SKRIVARE-NORDENKONTORET = JA                                    
092000         CALL W006PRR1 USING PRT-SPOOL-A4S                                
092100                             PRT-OPEN                                     
092200                             FRAKTS-NORDEN                                
092300                             ALT2-PCB                                     
092400                             LISB-PCB                                     
092500                             DUMMY-AREA                                   
092600                             DUMMY-AREA                                   
092700                             DUMMY-AREA                                   
092800                                                                          
092900       END-IF                                                             
093000       IF SKRIVARE-SVERIGE2 = JA                                          
093100         CALL W006PRR1 USING PRT-SPOOL-A4S                                
093200                             PRT-OPEN                                     
093300                             FRAKTS-SVERIGE2                              
093400                             ALT3-PCB                                     
093500                             LISB-PCB                                     
093600                             DUMMY-AREA                                   
093700                             DUMMY-AREA                                   
093800                             DUMMY-AREA                                   
093900                                                                          
094000       END-IF                                                             
097310       IF SKRIVARE-SVSLEGOPACK = JA                                       
097320       CALL W006PRR1 USING PRT-SPOOL-A4S                                  
097330                           PRT-OPEN                                       
097340                           FRAKTS-SVSLEGOPACK                             
097350                           ALT3-PCB                                       
097360                           LISB-PCB                                       
097370                           DUMMY-AREA                                     
097380                           DUMMY-AREA                                     
097390                           DUMMY-AREA                                     
097391                                                                          
097392       END-IF                                                             
097400                                                                          
097500       MOVE +1                            TO RAD-IX                       
097600                                                                          
097700       PERFORM S04C-INITIERA-LISTAN                                       
097800       PERFORM UNTIL RAD-IX = RAD-MAX-P-1                                 
097900         MOVE +1     TO ANT-ORDER-RADER                                   
098000         IF WS-SAMMA-BILD                                                 
098100           PERFORM EAB-GODSM-INFO-FRAN-MID                                
098200         ELSE                                                             
098300           IF MID1-RAD-IDDISTR  (RAD-IX) = 0000                           
098400             CONTINUE                                                     
098500           ELSE                                                           
098600             PERFORM EAA-GODSM-INFO-FRAN-WDQ2                             
098700           END-IF                                                         
098800         END-IF                                                           
098900                                                                          
099000         MOVE NEJ TO FRAKTS-SKALL-SKRIVAS                                 
099100         MOVE MID1-RAD-IDDISTR  (RAD-IX)    TO TEST-IDDISTR               
099200         IF DIST80-FRAKTS-NORDEN                                          
099300            MOVE MID1-RAD-IDKUNDNR (RAD-IX) TO WS-IDKUNDNR                
099400            MOVE WS-IDKUNDNR                TO WS-IDDISTR-BEL             
099500         ELSE                                                             
099600            MOVE MID1-RAD-IDDISTR (RAD-IX)  TO WS-IDDISTR-BEL             
099700         END-IF                                                           
099800         MOVE MID1-RAD-IDDISTR  (RAD-IX)    TO SPAR-IDDISTR               
099900         MOVE MID1-RAD-IDKUNDNR (RAD-IX)    TO SPAR-IDKUNDNR              
100000         MOVE MID1-RAD-KDFRAKT  (RAD-IX)    TO SPAR-KDFRAKT               
100100                                               WS-KDFRAKT-BEL             
100200         INSPECT WS-IDDISTR-BEL REPLACING LEADING ZERO BY SPACE           
100300         INSPECT WS-KDFRAKT-BEL REPLACING LEADING ZERO BY SPACE           
100400                                                                          
100500     MOVE +1                TO KOLLI-TAB-IX                               
100600     PERFORM UNTIL KOLLI-TAB-IX > MAX-ANTAL-I-FRA-TAB                     
100700                                                                          
100800       MOVE +00000                      TO                                
100900            WSTAB-IDKOLLI(KOLLI-TAB-IX)                                   
101000       ADD +1               TO KOLLI-TAB-IX                               
101100     END-PERFORM                                                          
101200                                                                          
101300     MOVE +1                TO KOLLI-TAB-IX                               
101400                                                                          
101500         PERFORM UNTIL RAD-IX = RAD-MAX-P-1                  OR           
101600            NOT (MID1-RAD-IDDISTR (RAD-IX)  = SPAR-IDDISTR   AND          
101700                 MID1-RAD-IDKUNDNR (RAD-IX) = SPAR-IDKUNDNR  AND          
101800                 MID1-RAD-KDFRAKT  (RAD-IX) = SPAR-KDFRAKT)  OR           
101900                 ANT-ORDER-RADER = MAX-ANT-ORD-RAD-PA-FRAKTSEDEL          
102000           IF MID1-RAD-KDUPPTYP (RAD-IX) NOT = QUIT                       
102100             PERFORM EC-NOLLSTALL-TABELLERNA                              
102200                                                                          
102300             MOVE MID1-RAD-IDPRODNR (RAD-IX)    TO W-IDPRODNR             
102400             PERFORM IMS-GHU-WDE601                                       
102500              PERFORM S05C-FRAKTSATT-TILL-LISTAN                          
102600                                                                          
102700             IF MID1-RAD-KDUPPTYP (RAD-IX) = 'B'                          
102800               PERFORM ED-KLARMARKERA                                     
102900             ELSE                                                         
103000               IF (MID1-RAD-KDUPPTYP (RAD-IX) = '+') OR                   
103100                  (MID1-RAD-KDUPPTYP (RAD-IX) = 'V'       AND             
103200                   MID1-IDPRODNR-KOMPL = 0)          OR                   
103300                  (MID1-RAD-KDUPPTYP (RAD-IX) = 'K')                      
103400                 MOVE JA  TO FRAKTS-SKALL-SKRIVAS                         
103500                 PERFORM ED-KLARMARKERA                                   
103600                 PERFORM EE-HAMTA-INFO-WDE611                             
103700                 PERFORM EF-ORDERN-SLUT                                   
103800               END-IF                                                     
103900             END-IF                                                       
104000           END-IF                                                         
104100                                                                          
104200           ADD +1                         TO RAD-IX                       
104300         END-PERFORM                                                      
104400                                                                          
104500         IF FRAKTS-SKALL-SKRIVAS = JA                                     
104700             PERFORM S02C-SKRIV-FRAKTSEDEL                                
104800             PERFORM S04C-INITIERA-LISTAN                                 
104900         END-IF                                                           
105000       END-PERFORM                                                        
105100                                                                          
105200       IF SKRIVARE-NORDENKONTORET = JA                                    
105300         CALL W006PRR1 USING PRT-SPOOL-A4S                                
105400                             PRT-CLOSE                                    
105500                             FRAKTS-NORDEN                                
105600                             ALT3-PCB                                     
105700                             LISB-PCB                                     
105800                             WS-BELISTID                                  
105900                             DUMMY-AREA                                   
106000                             DUMMY-AREA                                   
106100       END-IF                                                             
106200       IF SKRIVARE-SVERIGE2 = JA                                          
106300         CALL W006PRR1 USING PRT-SPOOL-A4S                                
106400                             PRT-CLOSE                                    
106500                             FRAKTS-SVERIGE2                              
106600                             ALT3-PCB                                     
106700                             LISB-PCB                                     
106800                             WS-BELISTID                                  
106900                             DUMMY-AREA                                   
107000                             DUMMY-AREA                                   
107100       END-IF                                                             
110110       IF SKRIVARE-SVSLEGOPACK = JA                                       
110120       CALL W006PRR1 USING PRT-SPOOL-A4S                                  
110130                           PRT-CLOSE                                      
110140                           FRAKTS-SVSLEGOPACK                             
110150                           ALT3-PCB                                       
110160                           LISB-PCB                                       
110170                           WS-BELISTID                                    
110180                           DUMMY-AREA                                     
110190                           DUMMY-AREA                                     
110191       END-IF                                                             
110200                                                                          
110300       PERFORM EG-FLYTTA-TILL-ALTMOD                                      
110700       PERFORM IMS-INSERT-ALTMSG                                          
110900     END-IF                                                               
111000     MOVE 'END E-UTSKRIFT'              TO PGM-POS                        
111100     .                                                                    
111200     EJECT                                                                
111300 EAA-GODSM-INFO-FRAN-WDQ2    SECTION.                                     
111400     MOVE 'START EAA-GODSM'               TO PGM-POS                      
111500                                                                          
111600                                                                          
111700     MOVE MID1-RAD-IDDISTR (RAD-IX)       TO W-IDDISTR                    
111800     MOVE MID1-RAD-IDKUNDNR (RAD-IX)      TO W-IDKUNDNR                   
111900     MOVE MID1-RAD-IDORDNR (RAD-IX)       TO W-IDORDNR7                   
112000                                                                          
112100     PERFORM IMS-GU-WDQ2-CSEQ                                             
112200                                                                          
112300     MOVE OHUV-BEGMT-RAD1                 TO LIST3-BEGMT-RAD1             
112400     MOVE OHUV-BEGMT-RAD2                 TO LIST3-BEGMT-RAD2             
112500     MOVE OHUV-ADGMT-GATA                 TO LIST3-ADGMT-RAD1             
112600     MOVE OHUV-ADGMT-PADR                 TO LIST3-ADGMT-RAD2             
112700     MOVE OHUV-ADGMT-PADR                 TO LIST3-ADGMT-RAD2             
112800                                                                          
112900     MOVE MID1-IDDC-UT                    TO W-IDDC                       
113000                                                                          
113100     PERFORM IMS-GNP-WDQ212                                               
113200                                                                          
113300     IF ARB-BEGMRK-RAD1 > SPACE                                           
113400       MOVE ARB-BEGMRK-RAD1               TO LIST3-BEGMRK                 
113500     ELSE                                                                 
113600       MOVE ARB-BEGMRK-RAD2               TO LIST3-BEGMRK                 
113700     END-IF                                                               
113800                                                                          
113900     MOVE 'END EAA-GODSM'                 TO PGM-POS                      
114000     .                                                                    
114100     EJECT                                                                
114200 EAB-GODSM-INFO-FRAN-MID     SECTION.                                     
114300     MOVE 'START EAB-GODSM'               TO PGM-POS                      
114400                                                                          
114500     MOVE MID2-BEGMT-RAD1                 TO LIST3-BEGMT-RAD1             
114600     MOVE MID2-BEGMT-RAD2                 TO LIST3-BEGMT-RAD2             
114700     MOVE MID2-ADGMT-RAD1                 TO LIST3-ADGMT-RAD1             
114800     MOVE MID2-ADGMT-RAD2                 TO LIST3-ADGMT-RAD2             
114900     MOVE MID2-BEGMRK-RAD1                TO LIST3-BEGMRK                 
115000     MOVE 'END EAB-GODSM'               TO PGM-POS                        
115100     .                                                                    
115200     EJECT                                                                
115300 EC-NOLLSTALL-TABELLERNA    SECTION.                                      
115400     MOVE 'START EC-NOLLST'              TO PGM-POS                       
115500                                                                          
115600     MOVE 1                          TO IX                                
115700     PERFORM UNTIL IX = 6                                                 
115800       MOVE ZERO                     TO EMB-KVEMBTYP       (IX)           
115900                                        EMB-VKORDBTO-EMBT  (IX)           
116000                                        EMB-VLORDBTO-EMBT  (IX)           
116100       ADD 1 TO IX                                                        
116200     END-PERFORM                                                          
116300                                                                          
116400     MOVE 1                          TO IX                                
116500     PERFORM UNTIL IX = 6                                                 
116600       MOVE ZERO                     TO REMB-KVPALL        (IX)           
116700                                        REMB-KVRAM         (IX)           
116800                                        REMB-KVLOCK        (IX)           
116900       ADD 1 TO IX                                                        
117000     END-PERFORM                                                          
117100                                                                          
117200     MOVE 1                          TO IX                                
117300     PERFORM UNTIL IX = TAB-MAX-AVV-P-1                                   
117400       MOVE ZERO                     TO AVV-IDORDNR        (IX)           
117500                                        AVV-IDARTNR        (IX)           
117600                                        AVV-REKSIFFR       (IX)           
117700                                        AVV-KVLEVART       (IX)           
117800       ADD 1 TO IX                                                        
117900     END-PERFORM                                                          
118000     MOVE 'END EC-NOLLST'              TO PGM-POS                         
118100     .                                                                    
118200     EJECT                                                                
118300 ED-KLARMARKERA       SECTION.                                            
118400     MOVE 'START ED-KLARM'              TO PGM-POS                        
118500                                                                          
118600     EVALUATE TRUE                                                        
118700     WHEN VORD-KVKOLLI = +00001                                           
118800       MOVE NEJ                         TO VORD-FLFRAKTS                  
118900       PERFORM IMS-REPL-WDE601                                            
119000     WHEN VORD-KVKOLLI NOT = VORD-KVKOLPAC                                
119100         CONTINUE                                                         
119200     WHEN OTHER                                                           
119300       MOVE NEJ                         TO VORD-FLFRAKTS                  
119400       PERFORM IMS-REPL-WDE601                                            
119500     END-EVALUATE                                                         
119600     .                                                                    
119700     EJECT                                                                
119800 EE-HAMTA-INFO-WDE611    SECTION.                                         
119900     MOVE 'START EE-HAMTA'              TO PGM-POS                        
120000                                                                          
120100        MOVE JA TO SW-NYTT-PRODNR                                         
120200*FÖR LANDROVER ENDAST KOLLI MED IDKOLLI 350 - 399.                        
120210*FÖR SVSLEGOPACK ENDAST KOLLI MED IDKOLLI 150 - 199.                      
120300        PERFORM IMS-GHNP-WDE611                                           
120400        MOVE 1                 TO ANT-KOLLI-T-FSEDEL                      
120500        MOVE 1                 TO KOLLI-IX                                
120600                                                                          
120700        MOVE MID1-RAD-IDORDNR (RAD-IX) TO                                 
120800             LIST3-IDORDNR(ANT-ORDER-RADER)                               
120900                                                                          
121000        PERFORM UNTIL SEGMENT-SAKNAS                                      
121100        OR KOLLI-IX > KOLLI-IX-MAX                                        
121200        OR ANT-ORDER-RADER = MAX-ANT-ORD-RAD-PA-FRAKTSEDEL                
121300                                                                          
121400           MOVE KOLLI-IDKOLLI  TO W-IDKOLLI                               
121500           IF (KOLLI-FLFRSUTS NOT = 'J' AND                               
121600              KOLLI-KDKOLSTA > 0)                                         
121700              OR                                                          
121800              (KOLLI-FLFRSUTS NOT = 'J' AND                               
121900              KOLLI-KDKOLSTA =  0   AND                                   
122000              KOLLI-IDKOLLI  >  99000)                                    
122100                IF MID1-FLLANDROVER = JA                                  
122200                  IF KOLLI-IDKOLLI > +349                                 
122300                  AND KOLLI-IDKOLLI < +400                                
122400                                                                          
122500                    PERFORM EEA-ADDERA-EMB-TAB                            
122600                    PERFORM EEB-ADDERA-REMB-TAB                           
122700                    IF KOLLI-IDKOLLI < 99000                              
122800                      PERFORM EEC-MOVE-KOLLI-T-LISTRAD                    
122900                                                                          
123000                      MOVE JA     TO KOLLI-FLFRSUTS                       
123100                      IF KOLLI-TAB-IX < MAX-ANTAL-I-FRA-TAB               
123200                        IF NYTT-PRODNR AND                                
123300                          MID1-RAD-IDPRODNR (RAD-IX) NOT = ALL '+'        
123400                        MOVE +99999 TO WSTAB-IDKOLLI(KOLLI-TAB-IX)        
123500                          ADD +1   TO KOLLI-TAB-IX                        
123600                          MOVE KOLLI-IDKOLLI TO                           
123700                          WSTAB-IDKOLLI(KOLLI-TAB-IX)                     
123800                        ELSE                                              
123900                          MOVE KOLLI-IDKOLLI TO                           
124000                          WSTAB-IDKOLLI(KOLLI-TAB-IX)                     
124100                        END-IF                                            
124200                      END-IF                                              
124300                      MOVE NEJ     TO SW-NYTT-PRODNR                      
124400                      ADD +1       TO KOLLI-TAB-IX                        
124500                      PERFORM IMS-REPL-WDE611                             
124600                  MOVE MID1-RAD-IDPRODNR (RAD-IX) TO W-IDPRODNR           
124700                      PERFORM IMS-GHU-WDE601                              
124800                    END-IF                                                
124910                  END-IF                                                  
125000                ELSE                                                      
125001*FÖR SVSLEGOPACK ENDAST KOLLI MED IDKOLLI 150 - 199.                      
125010                  IF MID1-FLLANDROVER = SVS                               
125020                    IF KOLLI-IDKOLLI > +149                               
125030                    AND KOLLI-IDKOLLI < +200                              
125040                                                                          
125050                      PERFORM EEA-ADDERA-EMB-TAB                          
125060                      PERFORM EEB-ADDERA-REMB-TAB                         
125070                      IF KOLLI-IDKOLLI < 99000                            
125080                        PERFORM EEC-MOVE-KOLLI-T-LISTRAD                  
125090                                                                          
125091                        MOVE JA   TO KOLLI-FLFRSUTS                       
125092                        IF KOLLI-TAB-IX < MAX-ANTAL-I-FRA-TAB             
125093                          IF NYTT-PRODNR AND                              
125094                          MID1-RAD-IDPRODNR (RAD-IX) >= ZERO              
125096                        MOVE +99999 TO WSTAB-IDKOLLI(KOLLI-TAB-IX)        
125097                            ADD +1 TO KOLLI-TAB-IX                        
125098                            MOVE KOLLI-IDKOLLI TO                         
125099                            WSTAB-IDKOLLI(KOLLI-TAB-IX)                   
125100                          ELSE                                            
125101                            MOVE KOLLI-IDKOLLI TO                         
125102                            WSTAB-IDKOLLI(KOLLI-TAB-IX)                   
125103                          END-IF                                          
125104                        END-IF                                            
125105                        MOVE NEJ   TO SW-NYTT-PRODNR                      
125106                        ADD +1     TO KOLLI-TAB-IX                        
125107                        PERFORM IMS-REPL-WDE611                           
125108                    MOVE MID1-RAD-IDPRODNR (RAD-IX) TO W-IDPRODNR         
125109                        PERFORM IMS-GHU-WDE601                            
125110                      END-IF                                              
125111                    END-IF                                                
125112                  ELSE                                                    
125120                  PERFORM EEA-ADDERA-EMB-TAB                              
125200                  PERFORM EEB-ADDERA-REMB-TAB                             
125300                  IF KOLLI-IDKOLLI < 99000                                
125400                    IF  KOLLI-IDKOLLI < +149                              
125500                    AND KOLLI-IDKOLLI > +200                              
125510                    OR  KOLLI-IDKOLLI < +349                              
125520                    AND KOLLI-IDKOLLI > +400                              
125600                      CONTINUE                                            
125700                    ELSE                                                  
125800                                                                          
125900                      PERFORM EEC-MOVE-KOLLI-T-LISTRAD                    
126000                                                                          
126100                      MOVE JA     TO KOLLI-FLFRSUTS                       
126200                      IF KOLLI-TAB-IX < MAX-ANTAL-I-FRA-TAB               
126300                        IF NYTT-PRODNR AND                                
126400                          MID1-RAD-IDPRODNR (RAD-IX) NOT = ALL '+'        
126500                        MOVE +99999 TO WSTAB-IDKOLLI(KOLLI-TAB-IX)        
126600                          ADD +1   TO KOLLI-TAB-IX                        
126700                          MOVE KOLLI-IDKOLLI TO                           
126800                          WSTAB-IDKOLLI(KOLLI-TAB-IX)                     
126900                        ELSE                                              
127000                          MOVE KOLLI-IDKOLLI TO                           
127100                          WSTAB-IDKOLLI(KOLLI-TAB-IX)                     
127200                        END-IF                                            
127300                      END-IF                                              
127400                      MOVE NEJ     TO SW-NYTT-PRODNR                      
127500                      ADD +1       TO KOLLI-TAB-IX                        
127600                      PERFORM IMS-REPL-WDE611                             
127700                  MOVE MID1-RAD-IDPRODNR (RAD-IX) TO W-IDPRODNR           
127800                      PERFORM IMS-GHU-WDE601                              
127900                    END-IF                                                
128000                  END-IF                                                  
128010                  END-IF                                                  
128100                END-IF                                                    
128200           END-IF                                                         
128300                                                                          
128400           PERFORM IMS-GHNP-WDE611-KVAL                                   
128500           IF SEGMENT-FINNS                                               
128600             ADD +1                  TO KOLLI-IX                          
128700           END-IF                                                         
128800           IF SEGMENT-FINNS AND EXTRA-LISTRAD-BEHOVS                      
128900             ADD +1                  TO ANT-ORDER-RADER                   
129000           END-IF                                                         
129100        END-PERFORM                                                       
129200        ADD +1                  TO ANT-ORDER-RADER                        
129300                                                                          
129400     IF KOLLI-IX > KOLLI-IX-MAX                                           
129500     OR ANT-ORDER-RADER >= MAX-ANT-ORD-RAD-PA-FRAKTSEDEL                  
129600                                                                          
129700       MOVE MID1-RAD-IDPRODNR (RAD-IX) TO W-IDPRODNR                      
129800       PERFORM IMS-GHU-WDE601                                             
129900       MOVE JA                      TO VORD-FLFRAKTS                      
130000       PERFORM IMS-REPL-WDE601                                            
130100     END-IF                                                               
130200                                                                          
130300     MOVE MID1-RAD-IDDISTR   (RAD-IX)     TO W-IDDISTR-MIN                
130400                                             W-IDDISTR-MAX                
130500     MOVE MID1-RAD-IDKUNDNR  (RAD-IX)     TO W-IDKUNDNR-MIN               
130600                                             W-IDKUNDNR-MAX               
130700     MOVE MID1-RAD-IDORDNR   (RAD-IX)     TO W-IDKUNDRF-MIN               
130800                                             W-IDKUNDRF-MAX               
130900     PERFORM IMS-GN-WDE4D1                                                
131000                                                                          
131100     IF SEGMENT-SAKNAS                                                    
131200       MOVE MID1-RAD-IDORDNR  (RAD-IX)    TO AVV-IDORDNR  (1)             
131300     END-IF                                                               
131400                                                                          
131500     MOVE 1                               TO IX                           
131600                                                                          
131700     PERFORM UNTIL NOT SEGMENT-FINNS OR IX = TAB-MAX-AVV-P-1              
131800       MOVE MID1-RAD-IDORDNR  (RAD-IX)    TO AVV-IDORDNR  (IX)            
131900       MOVE SEQD-IDARTNR                  TO AVV-IDARTNR  (IX)            
132000       MOVE SEQD-REKSIFFR                 TO AVV-REKSIFFR (IX)            
132100       MOVE SEQD-KVLEVART                 TO AVV-KVLEVART (IX)            
132200       PERFORM IMS-GN-WDE4D1                                              
132300       ADD +1 TO IX                                                       
132400     END-PERFORM                                                          
132500                                                                          
132600     COMPUTE TAB-ACC-AVV = IX - 1                                         
132700     MOVE 'END EE-HAMTA'              TO PGM-POS                          
132800     .                                                                    
132900     EJECT                                                                
133000 EEA-ADDERA-EMB-TAB     SECTION.                                          
133100     MOVE 'START EEA-ADDERA'            TO PGM-POS                        
133200                                                                          
133300     EVALUATE TRUE                                                        
133400       WHEN KOLLI-KDEMBTYP = 1 OR 6                                       
133500*1=LÅDA ELLER 6=LÅDA                                                      
133600         MOVE 1                           TO IX                           
133700       WHEN KOLLI-KDEMBTYP = 2 OR 8                                       
133800*2=PAKET ELLER 8=PAKET                                                    
133900         MOVE 2                           TO IX                           
134000       WHEN KOLLI-KDEMBTYP = 3 OR 5                                       
134100*3=BUNT ELLER 5=STYCK                                                     
134200         MOVE 3                           TO IX                           
134300       WHEN KOLLI-KDEMBTYP = 4                                            
134400*4=HÄCK                                                                   
134500         MOVE 4                           TO IX                           
134600       WHEN KOLLI-KDEMBTYP = 7                                            
134700*7=PALL                                                                   
134800         MOVE 5                           TO IX                           
134900       WHEN OTHER                                                         
135000        CONTINUE                                                          
135100*       MOVE 'FEL: ENDAST KDEMBTYP 1 - 7 ÄR GODKÄNDA' TO FELTEXT          
135200     END-EVALUATE                                                         
135300                                                                          
135400     IF KOLLI-KDEMBTYP > 0 AND < 9                                        
135500       ADD 1                   TO EMB-KVEMBTYP (IX)                       
135600       IF KOLLI-KDEMBTYP = 8                                              
135700         ADD 1                 TO WSFRA-KVKOLLI (2)                       
135800       ELSE                                                               
135900         ADD 1                 TO WSFRA-KVKOLLI (KOLLI-KDEMBTYP)          
136000       END-IF                                                             
136100                                                                          
136200                                                                          
136300       COMPUTE EMB-VLORDBTO-EMBT (1) = EMB-VLORDBTO-EMBT (1) +            
136400                                   KOLLI-VLORDBTO-KOLLI                   
136500                                                                          
136600       COMPUTE EMB-VKORDBTO-EMBT (1) = EMB-VKORDBTO-EMBT (1) +            
136700                                   KOLLI-VKORDBTO-KOLLI                   
136800                                                                          
136900       IF KOLLI-KDEMBTYP = 8                                              
137000         COMPUTE WSFRA-VLORDBTO (2) =                                     
137100            WSFRA-VLORDBTO (2) + KOLLI-VLORDBTO-KOLLI                     
137200       ELSE                                                               
137300         COMPUTE WSFRA-VLORDBTO (KOLLI-KDEMBTYP) =                        
137400            WSFRA-VLORDBTO (KOLLI-KDEMBTYP) + KOLLI-VLORDBTO-KOLLI        
137500       END-IF                                                             
137600                                                                          
137700       IF KOLLI-KDEMBTYP = 8                                              
137800         COMPUTE WSFRA-VKORDBTO (2) =                                     
137900            WSFRA-VKORDBTO (2) + KOLLI-VKORDBTO-KOLLI                     
138000       ELSE                                                               
138100         COMPUTE WSFRA-VKORDBTO (KOLLI-KDEMBTYP) =                        
138200            WSFRA-VKORDBTO (KOLLI-KDEMBTYP) + KOLLI-VKORDBTO-KOLLI        
138300       END-IF                                                             
138400     END-IF                                                               
138500     MOVE 'END EEA-ADDERA'            TO PGM-POS                          
138600     .                                                                    
138700     EJECT                                                                
138800                                                                          
138900 EEB-ADDERA-REMB-TAB   SECTION.                                           
139000     MOVE 'START EEB-ADDERA'            TO PGM-POS                        
139100                                                                          
139200     MOVE KOLLI-KDKOLLI          TO WS-KDKOLLI                            
139300                                                                          
139400     IF   WS-KDKOLLI = 'FLEN    '                                         
139500       ADD +1                    TO REMB-KVPALL (1)                       
139600       ADD +2                    TO REMB-KVPALL (2)                       
139700       ADD +8                    TO REMB-KVRAM (2)                        
139800       ADD +1                    TO REMB-KVLOCK (1)                       
139900       ADD +2                    TO REMB-KVLOCK (2)                       
140000     ELSE                                                                 
140100                                                                          
140200       EVALUATE TRUE                                                      
140300         WHEN WS-KDKOLLI-1 = 'L'                                          
140400           ADD +1                TO REMB-KVPALL (1)                       
140500           IF WS-KDKOLLI-2 NUMERIC                                        
140600             ADD +1            TO REMB-KVLOCK (1)                         
140700             MOVE WS-KDKOLLI-2 TO WS-KVRAM-X                              
140800             ADD WS-KVRAM      TO REMB-KVRAM (1)                          
140900           END-IF                                                         
141000                                                                          
141100         WHEN WS-KDKOLLI-1 = 'K'                                          
141200           ADD +1                TO REMB-KVPALL (2)                       
141300           IF WS-KDKOLLI-2 NUMERIC                                        
141400             ADD +1            TO REMB-KVLOCK (2)                         
141500             MOVE WS-KDKOLLI-2 TO WS-KVRAM-X                              
141600             ADD WS-KVRAM      TO REMB-KVRAM (2)                          
141700           END-IF                                                         
141800                                                                          
141900         WHEN WS-KDKOLLI-1 = 'F'                                          
142000           ADD +1                TO REMB-KVPALL (3)                       
142100           IF WS-KDKOLLI-2 NUMERIC                                        
142200             ADD +1            TO REMB-KVLOCK (3)                         
142300             MOVE WS-KDKOLLI-2 TO WS-KVRAM-X                              
142400             ADD WS-KVRAM      TO REMB-KVRAM (3)                          
142500           END-IF                                                         
142600                                                                          
142700         WHEN WS-KDKOLLI-1 = 'G'                                          
142800           ADD +1                TO REMB-KVPALL (4)                       
142900           IF WS-KDKOLLI-2 NUMERIC                                        
143000             ADD +1            TO REMB-KVLOCK (4)                         
143100             MOVE WS-KDKOLLI-2 TO WS-KVRAM-X                              
143200             ADD WS-KVRAM      TO REMB-KVRAM (4)                          
143300           END-IF                                                         
143400                                                                          
143500         WHEN WS-KDKOLLI-1 = 'H'                                          
143600           ADD +1                TO REMB-KVPALL (5)                       
143700           IF WS-KDKOLLI-2 NUMERIC                                        
143800             ADD +1            TO REMB-KVLOCK (5)                         
143900             MOVE WS-KDKOLLI-2 TO WS-KVRAM-X                              
144000             ADD WS-KVRAM      TO REMB-KVRAM (5)                          
144100           END-IF                                                         
144200         WHEN OTHER                                                       
144300           CONTINUE                                                       
144400       END-EVALUATE                                                       
144500     END-IF                                                               
144600     MOVE 'END EEB-ADDERA'            TO PGM-POS                          
144700     .                                                                    
144800     EJECT                                                                
144900 EEC-MOVE-KOLLI-T-LISTRAD    SECTION.                                     
145000     MOVE 'START EEC-MOVE'            TO PGM-POS                          
145100                                                                          
145200     IF KOLLI-IDKOLLI = +1                                                
145300       CONTINUE                                                           
145400     ELSE                                                                 
145500       ADD 1           TO ANT-KOLLI-T-FSEDEL                              
145600       EVALUATE TRUE                                                      
145700         WHEN  ANT-KOLLI-T-FSEDEL = 1                                     
145800           MOVE KOLLI-IDKOLLI    TO LIST3-KOLLI1(ANT-ORDER-RADER)         
145900         WHEN  ANT-KOLLI-T-FSEDEL = 2                                     
146000           MOVE KOLLI-IDKOLLI    TO LIST3-KOLLI2(ANT-ORDER-RADER)         
146100         WHEN  ANT-KOLLI-T-FSEDEL = 3                                     
146200           MOVE KOLLI-IDKOLLI    TO LIST3-KOLLI3(ANT-ORDER-RADER)         
146300         WHEN  ANT-KOLLI-T-FSEDEL = 4                                     
146400           MOVE KOLLI-IDKOLLI    TO LIST3-KOLLI4(ANT-ORDER-RADER)         
146500         WHEN  ANT-KOLLI-T-FSEDEL = 5                                     
146600           MOVE KOLLI-IDKOLLI    TO LIST3-KOLLI5(ANT-ORDER-RADER)         
146700         WHEN  ANT-KOLLI-T-FSEDEL = 6                                     
146800           MOVE KOLLI-IDKOLLI    TO LIST3-KOLLI6(ANT-ORDER-RADER)         
146900         WHEN  ANT-KOLLI-T-FSEDEL = 7                                     
147000           MOVE KOLLI-IDKOLLI    TO LIST3-KOLLI7(ANT-ORDER-RADER)         
147100         WHEN  ANT-KOLLI-T-FSEDEL = 8                                     
147200           MOVE KOLLI-IDKOLLI    TO LIST3-KOLLI8(ANT-ORDER-RADER)         
147300       END-EVALUATE                                                       
147400                                                                          
147500       IF ANT-KOLLI-T-FSEDEL = 8                                          
147600         MOVE JA                 TO NY-RAD-SW                             
147700         MOVE ZERO               TO ANT-KOLLI-T-FSEDEL                    
147800       ELSE                                                               
147900         MOVE NEJ                TO NY-RAD-SW                             
148000       END-IF                                                             
148100     END-IF                                                               
148200     MOVE 'END EEC-MOVE'            TO PGM-POS                            
148300     .                                                                    
148400     EJECT                                                                
148500 EF-ORDERN-SLUT             SECTION.                                      
148600     MOVE 'START EF-ORDER'            TO PGM-POS                          
148700                                                                          
148800     IF (LIST-ACC-AVV + TAB-ACC-AVV - 1) < TAB-MAX-AVV-P-1                
148900       PERFORM EFA-FLYTTA-TAB-TILL-SUMTAB                                 
149000     ELSE                                                                 
149200       PERFORM S02C-SKRIV-FRAKTSEDEL                                      
149300       PERFORM S03C-NOLLA-LISTAN-EJ-HUV                                   
149400       PERFORM EFA-FLYTTA-TAB-TILL-SUMTAB                                 
149500     END-IF                                                               
149600     MOVE 'END EF-ORDER'            TO PGM-POS                            
149700     .                                                                    
149800     EJECT                                                                
149900 EFA-FLYTTA-TAB-TILL-SUMTAB   SECTION.                                    
150000     MOVE 'START EFA-FLYTTA'          TO PGM-POS                          
150100                                                                          
150200     PERFORM EFAA-SUMMERA-EMB-TAB                                         
150300     PERFORM EFAB-SUMMERA-REMB-TAB                                        
150400     MOVE 'END EFA-FLYTTA'          TO PGM-POS                            
150500     .                                                                    
150600     SKIP3                                                                
150700 EFAA-SUMMERA-EMB-TAB            SECTION.                                 
150800     MOVE 'START EFAA-SUM'          TO PGM-POS                            
150900                                                                          
151000     MOVE 1                     TO EMB-RADIX                              
151100     ADD EMB-VKORDBTO-EMBT  (1)        TO                                 
151200         SUM-EMB-VKORDBTO-EMBT (1)                                        
151300                                                                          
151400     ADD EMB-VLORDBTO-EMBT  (1)        TO                                 
151500         SUM-EMB-VLORDBTO-EMBT (1)                                        
151600                                                                          
151700     PERFORM UNTIL EMB-RADIX = 6                                          
151800       IF EMB-KVEMBTYP   (EMB-RADIX) > 0                                  
151900         IF EMB-PEKA-PA-SRAD (EMB-RADIX) = 0                              
152000           MOVE 1                 TO SUM-EMB-RADIX                        
152100                                                                          
152200           PERFORM UNTIL SUM-EMB-KVEMBTYP (SUM-EMB-RADIX) = 0 OR          
152300                   SUM-EMB-RADIX = 5                                      
152400             ADD 1 TO SUM-EMB-RADIX                                       
152500           END-PERFORM                                                    
152600                                                                          
152700           MOVE SUM-EMB-RADIX     TO EMB-PEKA-PA-SRAD (EMB-RADIX)         
152800         END-IF                                                           
152900                                                                          
153000         MOVE EMB-PEKA-PA-SRAD (EMB-RADIX) TO IX2                         
153100         ADD EMB-KVEMBTYP  (EMB-RADIX)     TO                             
153200             SUM-EMB-KVEMBTYP (IX2)                                       
153300         MOVE EMB-EMBTYP-1 (EMB-RADIX)     TO                             
153400              SUM-EMB-EMBTYP-1(IX2)                                       
153500         MOVE EMB-EMBTYP-2 (EMB-RADIX)     TO                             
153600              SUM-EMB-EMBTYP-2(IX2)                                       
153700       END-IF                                                             
153800                                                                          
153900       ADD 1 TO EMB-RADIX                                                 
154000     END-PERFORM                                                          
154100     MOVE 'END EFAA-SUM'          TO PGM-POS                              
154200     .                                                                    
154300     EJECT                                                                
154400 EFAB-SUMMERA-REMB-TAB           SECTION.                                 
154500     MOVE 'START EFAB-SUM'          TO PGM-POS                            
154600                                                                          
154700     MOVE 1                     TO REMB-RADIX                             
154800                                                                          
154900     PERFORM UNTIL REMB-RADIX = 6                                         
155000       IF REMB-KVPALL    (REMB-RADIX) > 0  OR                             
155100          REMB-KVRAM     (REMB-RADIX) > 0  OR                             
155200          REMB-KVLOCK    (REMB-RADIX) > 0                                 
155300         IF REMB-PEKA-PA-SRAD (REMB-RADIX) = 0                            
155400           MOVE 1                 TO SUM-REMB-RADIX                       
155500                                                                          
155600           PERFORM UNTIL (SUM-REMB-KVPALL (SUM-REMB-RADIX) = 0 AND        
155700                          SUM-REMB-KVRAM (SUM-REMB-RADIX) = 0 AND         
155800                          SUM-REMB-KVLOCK (SUM-REMB-RADIX) = 0) OR        
155900                          SUM-REMB-RADIX = 5                              
156000             ADD 1 TO SUM-REMB-RADIX                                      
156100           END-PERFORM                                                    
156200                                                                          
156300           MOVE SUM-REMB-RADIX TO REMB-PEKA-PA-SRAD (REMB-RADIX)          
156400         END-IF                                                           
156500                                                                          
156600         MOVE REMB-PEKA-PA-SRAD (REMB-RADIX) TO IX2                       
156700         ADD REMB-KVPALL (REMB-RADIX)  TO SUM-REMB-KVPALL (IX2)           
156800         ADD REMB-KVRAM  (REMB-RADIX)  TO SUM-REMB-KVRAM (IX2)            
156900         ADD REMB-KVLOCK (REMB-RADIX)  TO SUM-REMB-KVLOCK (IX2)           
157000         MOVE REMB-KDKOLLI-POS1  (REMB-RADIX)  TO                         
157100              SUM-REMB-KDKOLLI-POS1 (IX2)                                 
157200       END-IF                                                             
157300       ADD 1 TO REMB-RADIX                                                
157400     END-PERFORM                                                          
157500     MOVE 'END EFAB-SUM'          TO PGM-POS                              
157600     .                                                                    
157700     EJECT                                                                
157800 EG-FLYTTA-TILL-ALTMOD      SECTION.                                      
157900     MOVE 'START EG-FLYTTA'       TO PGM-POS                              
158000                                                                          
158100     IF MFS-IDTRANS = '4611'                                              
158200       MOVE MID1-IDDISTR-UT               TO ALT1-MID-IDDISTR-IN          
158300       MOVE MID1-IDKUNDNR-UT              TO ALT1-MID-IDKUNDNR-IN         
158400       MOVE MID1-KDFRAKT-UT               TO ALT1-MID-KDFRAKT-IN          
158500       MOVE MID1-IDDC-UT                  TO ALT1-MID-IDDC-IN             
158600     ELSE                                                                 
159300       MOVE MID2-IDDISTR-UT               TO ALT1-MID-IDDISTR-IN          
159400       MOVE MID2-IDKUNDNR-UT              TO ALT1-MID-IDKUNDNR-IN         
159500       MOVE MID2-KDFRAKT-UT               TO ALT1-MID-KDFRAKT-IN          
159600       MOVE MID2-IDDC-UT                  TO ALT1-MID-IDDC-IN             
159700       INSPECT ALT1-MID-IDDISTR-IN                                        
159800               REPLACING LEADING SPACE BY ZERO                            
159900       INSPECT ALT1-MID-IDKUNDNR-IN                                       
160000               REPLACING LEADING SPACE BY ZERO                            
160100       INSPECT ALT1-MID-KDFRAKT-IN                                        
160200               REPLACING LEADING SPACE BY ZERO                            
160400     END-IF                                                               
160500     MOVE 'END EG-FLYTTA'       TO PGM-POS                                
160600     .                                                                    
160700     EJECT                                                                
160800                                                                          
160900 EH-INGANG-FRAN-4694        SECTION.                                      
161000     MOVE 'START EH-INGANG'       TO PGM-POS                              
161100                                                                          
161200     MOVE ALL '+'                   TO MID1-W4I69401-CTX                  
161210     MOVE MID2-FLLANDROVER          TO MID1-FLLANDROVER                   
161300     MOVE 1                         TO RAD-IX                             
161400                                                                          
161500     PERFORM UNTIL RAD-IX = RAD-MAX-P-1            OR                     
161600                   MID2-IDORDNR (RAD-IX) = SPACE                          
161700       MOVE MID2-IDDISTR            TO MID1-RAD-IDDISTR  (RAD-IX)         
161800       MOVE MID2-IDKUNDNR           TO MID1-RAD-IDKUNDNR (RAD-IX)         
161900       MOVE MID2-KDFRAKT            TO MID1-RAD-KDFRAKT  (RAD-IX)         
162000       MOVE MID2-IDORDNR  (RAD-IX)  TO MID1-RAD-IDORDNR  (RAD-IX)         
162100       MOVE MID2-IDPRODNR (RAD-IX)  TO MID1-RAD-IDPRODNR (RAD-IX)         
162200       MOVE 'K'                     TO MID1-RAD-KDUPPTYP (RAD-IX)         
162300       ADD +1                       TO RAD-IX                             
162400     END-PERFORM                                                          
162500                                                                          
162600     PERFORM UNTIL RAD-IX = RAD-MAX-P-1                                   
162700       MOVE QUIT                    TO MID1-RAD-KDUPPTYP (RAD-IX)         
162800       MOVE ZERO                    TO MID1-RAD-IDDISTR  (RAD-IX)         
162900                                       MID1-RAD-IDKUNDNR (RAD-IX)         
163000                                       MID1-RAD-KDFRAKT  (RAD-IX)         
163100                                       MID1-RAD-KDORDKL  (RAD-IX)         
163200                                       MID1-RAD-IDORDNR  (RAD-IX)         
163300                                       MID1-RAD-IDPRODNR (RAD-IX)         
163400       ADD +1                       TO RAD-IX                             
163500     END-PERFORM                                                          
163600     MOVE 'END EH-INGANG'       TO PGM-POS                                
163700     .                                                                    
163800     EJECT                                                                
163900                                                                          
175500 F-FELAKTIGT-ANROP    SECTION.                                            
175600     MOVE 'START F-FELAKTIG'      TO PGM-POS                              
175700                                                                          
175800     MOVE MFS-IDTRANS                     TO MODFEL-IDTRANS               
175900     MOVE FEL2 (INDX)                     TO MODFEL-TEMFSFEL              
176000     MOVE MSG-MOD-NAME                    TO MFS-IDMOD                    
176100     INSPECT MFS-IDMOD   REPLACING FIRST 'I' BY 'O'                       
176200     MOVE +48                             TO MSG-KVLL                     
176300     PERFORM IMS-INSERT-MSG                                               
176400                                                                          
176500     MOVE 'END F-FELAKTIG'      TO PGM-POS                                
176600     .                                                                    
176700     EJECT                                                                
176800 S01-KOLLA-INDATA-EGEN-BILD     SECTION.                                  
176900     MOVE 'START S01-KOLLA'      TO PGM-POS                               
177000                                                                          
177100     MOVE JA                              TO FL-INDATA-OK                 
177200     INSPECT MID2-IDAVD       REPLACING LEADING SPACE BY ZERO             
177300     INSPECT MID2-IDDISTR-BET REPLACING LEADING SPACE BY ZERO             
177400                                                                          
177500     IF MID2-FLMOTBET = JA   OR                                           
177600        MID2-FLMOTBET = NEJ                                               
177700       MOVE MFS-ALFA-FAELT-RAETT            TO MOD2-FLMOTBET-ATTR         
177800     ELSE                                                                 
177900       MOVE MFS-ALFA-FAELT-FEL              TO MOD2-FLMOTBET-ATTR         
178000       MOVE FEL1(INDX)                      TO MOD2-TEMFSFEL              
178100       MOVE NEJ                             TO FL-INDATA-OK               
178200     END-IF                                                               
178300                                                                          
178400     IF MID2-IDAVD NUMERIC                                                
178500       MOVE MFS-NUM-FAELT-RAETT           TO MOD2-IDAVD-ATTR              
178600     ELSE                                                                 
178700       IF FL-INDATA-OK = JA                                               
178800         MOVE MFS-NUM-FAELT-FEL           TO MOD2-IDAVD-ATTR              
178900         MOVE FEL1(INDX)                  TO MOD2-TEMFSFEL                
179000         MOVE NEJ                         TO FL-INDATA-OK                 
179100       END-IF                                                             
179200     END-IF                                                               
179300                                                                          
179400     IF MID2-IDDISTR-BET NUMERIC                                          
179500       MOVE MFS-NUM-FAELT-RAETT           TO MOD2-IDDISTR-BET-ATTR        
179600     ELSE                                                                 
179700       IF FL-INDATA-OK = JA                                               
179800         MOVE MFS-NUM-FAELT-FEL           TO MOD2-IDDISTR-BET-ATTR        
179900         MOVE FEL1(INDX)                  TO MOD2-TEMFSFEL                
180000         MOVE NEJ                         TO FL-INDATA-OK                 
180100       END-IF                                                             
180200     END-IF                                                               
180300                                                                          
180400     IF FL-INDATA-OK = JA                                                 
180500       EVALUATE TRUE                                                      
180600         WHEN MID2-IDAVD > ZERO   AND                                     
180700             (MID2-IDDISTR-BET > ZERO OR MID2-FLMOTBET = JA)              
180800           MOVE MFS-NUM-FAELT-FEL         TO MOD2-IDAVD-ATTR              
180900           MOVE FEL1(INDX)                TO MOD2-TEMFSFEL                
181000           MOVE NEJ                       TO FL-INDATA-OK                 
181100                                                                          
181200         WHEN MID2-IDDISTR-BET > ZERO   AND                               
181300              MID2-FLMOTBET = JA                                          
181400           MOVE MFS-NUM-FAELT-FEL    TO MOD2-IDDISTR-BET-ATTR             
181500           MOVE FEL1(INDX)                TO MOD2-TEMFSFEL                
181600           MOVE NEJ                       TO FL-INDATA-OK                 
181700                                                                          
181800         WHEN (MID2-BEGMT-RAD1 = SPACE      AND                           
181900               MID2-BEGMT-RAD2 = SPACE)     OR                            
182000              (MID2-ADGMT-RAD1 = SPACE      AND                           
182100               MID2-ADGMT-RAD2 = SPACE)                                   
182200           MOVE FEL3 (INDX)               TO MOD2-TEMFSFEL                
182300           MOVE NEJ                       TO FL-INDATA-OK                 
182400       END-EVALUATE                                                       
182500     END-IF                                                               
182600                                                                          
182700     IF ENTER-EGEN-BILD       OR                                          
182800        FL-INDATA-OK = NEJ                                                
182900       MOVE MID2-IDDISTR-UT               TO MOD2-IDDISTR-UT              
183000       MOVE MID2-IDDISTR                  TO MOD2-IDDISTR                 
183100       MOVE MID2-IDKUNDNR-UT              TO MOD2-IDKUNDNR-UT             
183200       MOVE MID2-IDKUNDNR                 TO MOD2-IDKUNDNR                
183300       MOVE MID2-KDFRAKT-UT               TO MOD2-KDFRAKT-UT              
183400       MOVE MID2-KDFRAKT                  TO MOD2-KDFRAKT                 
183410       MOVE MID2-FLLANDROVER              TO MOD2-FLLANDROVER             
183500       MOVE 1                             TO RAD-IX                       
183600                                                                          
183700       PERFORM UNTIL RAD-IX = RAD-MAX-P-1                                 
183800         MOVE MID2-IDORDNR  (RAD-IX)      TO                              
183900              MOD2-IDORDNR  (RAD-IX)                                      
184000         MOVE MID2-IDPRODNR (RAD-IX)      TO                              
184100              MOD2-IDPRODNR (RAD-IX)                                      
184200         ADD 1                            TO RAD-IX                       
184300       END-PERFORM                                                        
184400                                                                          
184500       MOVE MID2-BEGMT-RAD1               TO MOD2-BEGMT-RAD1              
184600       MOVE MID2-BEGMT-RAD2               TO MOD2-BEGMT-RAD2              
184700       MOVE MID2-ADGMT-RAD1               TO MOD2-ADGMT-RAD1              
184800       MOVE MID2-ADGMT-RAD2               TO MOD2-ADGMT-RAD2              
184900       MOVE MID2-BEGMRK-RAD1              TO MOD2-BEGMRK-RAD1             
185000       MOVE MID2-IDAVD                    TO MOD2-IDAVD                   
185100       MOVE MID2-IDDISTR-BET              TO MOD2-IDDISTR-BET             
185200       MOVE MID2-FLMOTBET                 TO MOD2-FLMOTBET                
185300                                                                          
185400       INSPECT MOD2-IDDISTR-UT  REPLACING LEADING ZERO BY SPACE           
185500       INSPECT MOD2-IDKUNDNR-UT REPLACING LEADING ZERO BY SPACE           
185600       INSPECT MOD2-KDFRAKT-UT  REPLACING LEADING ZERO BY SPACE           
185700       INSPECT MOD2-IDAVD       REPLACING LEADING ZERO BY SPACE           
185800       INSPECT MOD2-IDDISTR-BET REPLACING LEADING ZERO BY SPACE           
185900                                                                          
186000       MOVE MAX-MOD2-LAENGD               TO MSG-KVLL                     
186100       MOVE 'W4O69401'                    TO MFS-IDMOD                    
186200       PERFORM IMS-INSERT-MSG                                             
186300     END-IF                                                               
186400     MOVE 'END S01-KOLLA'      TO PGM-POS                                 
186500     .                                                                    
186600     EJECT                                                                
186700 S02C-SKRIV-FRAKTSEDEL         SECTION.                                   
186800     MOVE 'START S02C-SKRIV'      TO PGM-POS                              
186900                                                                          
187000     MOVE 1                               TO IX                           
187100     MOVE SUM-EMB-VKORDBTO-EMBT (1) TO LIST3-VKORDBTO-EMBT (1)            
187200     MOVE SUM-EMB-VLORDBTO-EMBT (1) TO LIST3-VLORDBTO-EMBT (1)            
187300                                                                          
187400     PERFORM UNTIL IX = 6                                                 
187500       IF SUM-EMB-KVEMBTYP (IX) > 0                                       
187600         MOVE SUM-EMB-KVEMBTYP (IX)       TO LIST3-KVEMBTYP (IX)          
187700         MOVE SUM-EMB-EMBTYP-1 (IX)       TO LIST3-EMBTYP-1 (IX)          
187800         MOVE SUM-EMB-EMBTYP-2 (IX)       TO LIST3-EMBTYP-2 (IX)          
187900       END-IF                                                             
188000       ADD +1 TO IX                                                       
188100     END-PERFORM                                                          
188200                                                                          
188300     MOVE 1                               TO IX                           
188400                                                                          
188500     PERFORM UNTIL IX = 6                                                 
188600       IF (SUM-REMB-KVPALL (IX) > 0 OR                                    
188700           SUM-REMB-KVRAM (IX) > 0 OR                                     
188800           SUM-REMB-KVLOCK (IX) > 0)                                      
188900         MOVE SUM-REMB-KDKOLLI-POS1 (IX) TO LIST3-KDKOLLI-POS1(IX)        
189000         MOVE SUM-REMB-KVPALL    (IX)     TO LIST3-KVPALL    (IX)         
189100         MOVE SUM-REMB-KVRAM     (IX)     TO LIST3-KVRAM     (IX)         
189200         MOVE SUM-REMB-KVLOCK    (IX)     TO LIST3-KVLOCK    (IX)         
189300       END-IF                                                             
189400       ADD +1 TO IX                                                       
189500     END-PERFORM                                                          
189600                                                                          
189700     MOVE 1                               TO IX                           
189800     MOVE PRT-NYSIDA-RAD4                 TO PRT-RADSKIP                  
189900                                                                          
190210     IF MID1-FLLANDROVER = SVS                                            
190220       MOVE FRAKTS-SVSLEGOPACK            TO WS-IDPRINTER                 
190230     ELSE                                                                 
190240       IF MID1-FLLANDROVER = JA                                           
190250         MOVE FRAKTS-LANDROVER            TO WS-IDPRINTER                 
190260       ELSE                                                               
190900         IF SKRIVARE-NORDENKONTORET = JA                                  
191000            MOVE FRAKTS-NORDEN         TO WS-IDPRINTER                    
191100         ELSE                                                             
191200            MOVE FRAKTS-SVERIGE2       TO WS-IDPRINTER                    
191300         END-IF                                                           
191600       END-IF                                                             
191601     END-IF                                                               
191700                                                                          
191800     PERFORM UNTIL IX = MAX-ANT-FRAKTSRAD-PLUS1                           
191900       CALL W006PRR1 USING PRT-SPOOL-A4S                                  
192000                           PRT-WRITE                                      
192100                           WS-IDPRINTER                                   
192200                           ALT3-PCB                                       
192300                           LISB-PCB                                       
192400                           WS-BELISTID                                    
192500                           PRT-RADSKIP                                    
192600                           LIST3-RAD (IX)                                 
192700       MOVE RADSKIP-LIST3 (IX)            TO PRT-RADSKIP                  
192800       ADD  RADSKIP-LIST3 (IX)            TO IX                           
192900     END-PERFORM                                                          
193000     MOVE 'END S02-SKRIV'      TO PGM-POS                                 
193100     .                                                                    
193200     EJECT                                                                
193300 S03C-NOLLA-LISTAN-EJ-HUV      SECTION.                                   
193400     MOVE 'START S03C-NOLLA'     TO PGM-POS                               
193500                                                                          
193600     MOVE 1                               TO LIST-ACC-AVV                 
193700     MOVE 1                               TO IX                           
193800     PERFORM UNTIL IX = 6                                                 
193900       MOVE ZERO                    TO LIST3-KVEMBTYP      (IX)           
194000                                       SUM-EMB-KVEMBTYP    (IX)           
194100                                       LIST3-VLORDBTO-EMBT (IX)           
194200                                       LIST3-VKORDBTO-EMBT (IX)           
194300                                       SUM-EMB-VKORDBTO-EMBT (IX)         
194400                                       SUM-EMB-VLORDBTO-EMBT (IX)         
194500                                       EMB-PEKA-PA-SRAD    (IX)           
194600       ADD 1 TO IX                                                        
194700     END-PERFORM                                                          
194800                                                                          
194900     MOVE 1                               TO IX                           
195000     PERFORM UNTIL IX = 6                                                 
195100       MOVE ZERO                          TO LIST3-KVPALL     (IX)        
195200                                             SUM-REMB-KVPALL  (IX)        
195300                                             LIST3-KVRAM      (IX)        
195400                                             SUM-REMB-KVRAM   (IX)        
195500                                             LIST3-KVLOCK     (IX)        
195600                                             SUM-REMB-KVLOCK  (IX)        
195700                                             REMB-PEKA-PA-SRAD(IX)        
195800       ADD 1 TO IX                                                        
195900     END-PERFORM                                                          
196000                                                                          
196100     MOVE +1                           TO IX                              
196200                                                                          
196300     PERFORM UNTIL IX = 8                                                 
196400       MOVE 0                          TO WSFRA-KVKOLLI  (IX)             
196500                                          WSFRA-VKORDBTO (IX)             
196600                                          WSFRA-VLORDBTO (IX)             
196700                                          LIST3-IDORDNR     (IX)          
196800                                          LIST3-KOLLI1      (IX)          
196900                                          LIST3-KOLLI2      (IX)          
197000                                          LIST3-KOLLI3      (IX)          
197100                                          LIST3-KOLLI4      (IX)          
197200                                          LIST3-KOLLI5      (IX)          
197300                                          LIST3-KOLLI6      (IX)          
197400                                          LIST3-KOLLI7      (IX)          
197500                                          LIST3-KOLLI8      (IX)          
197600                                          LIST3-KVPALL      (IX)          
197700                                          LIST3-KVRAM       (IX)          
197800                                          LIST3-KVLOCK      (IX)          
197900       MOVE SPACE                      TO LIST3-KDKOLLI-POS1(IX)          
198000       ADD +1                          TO IX                              
198100     END-PERFORM                                                          
198200     MOVE 'END S03C-NOLLA'     TO PGM-POS                                 
198300     .                                                                    
198400     EJECT                                                                
198500 S04C-INITIERA-LISTAN          SECTION.                                   
198600     MOVE 'START S04C-INIT'     TO PGM-POS                                
198700                                                                          
198800     MOVE SPACE                          TO LIST3-TEXT-RADER              
198900                                                                          
199000     MOVE 'XXXXXXXXXXXXX'                TO LIST3-RAD-38-2                
199100     MOVE 'XXXXXXXXXXXXXXXXXXXXXX'       TO LIST3-RAD-38-4                
199200                                                                          
199300     MOVE 'XXXX '                        TO LIST3-RAD-39-2                
199400     MOVE 'KOLLINR'                      TO LIST3-RAD-39-3                
199500     MOVE 'XXXXXXXX'                     TO LIST3-RAD-39-5                
199600     MOVE 'XXXXX '                       TO LIST3-RAD-39-7                
199700     MOVE 'KOLLINR'                      TO LIST3-RAD-39-8                
199800     MOVE 'XXXXXXX'                      TO LIST3-RAD-39-10               
199900                                                                          
200000     MOVE MID1-RAD-IDDISTR (RAD-IX)      TO TEST-IDDISTR                  
200100     MOVE MID1-RAD-IDKUNDNR (RAD-IX)     TO SPAR-IDKUNDNR                 
200300                                                                          
200500     MOVE WS-PVNAMN-RAD1                 TO LIST3-GODSAVS-1               
200600     MOVE WS-PVNAMN-RAD2                 TO LIST3-GODSAVS-2               
201100     MOVE WS-ADRESS-RAD3                 TO LIST3-GODSAVSADR-3            
201200                                                                          
201300     IF DIST80-FRAKTS-NORDEN                                              
201400       MOVE 'X'                          TO LIST3-KRYSS-DAGORD            
201500       MOVE '                 '          TO LIST3-GODSADNR-0              
201600       MOVE 'BILSPED. 3223781 '          TO LIST3-GODSADNR-1              
201700       MOVE '                 '          TO LIST3-GODSADNR-2              
201800       MOVE '                 '          TO LIST3-GODSADNR-3              
201900     ELSE                                                                 
202000       IF TEST-IDDISTR = +1 OR +15                                        
202100         MOVE '                 '          TO LIST3-GODSADNR-0            
202200         MOVE '                 '          TO LIST3-GODSADNR-1            
202300         MOVE '                 '          TO LIST3-GODSADNR-2            
202400         MOVE '                 '          TO LIST3-GODSADNR-3            
202500       ELSE                                                               
202600         IF (TEST-IDDISTR = +19 AND SPAR-IDKUNDNR = +152) OR              
202700            (TEST-IDDISTR = +69 AND SPAR-IDKUNDNR = +152)                 
202800           MOVE '                 '          TO LIST3-GODSADNR-0          
202900           MOVE '                 '          TO LIST3-GODSADNR-1          
203000           MOVE '                 '          TO LIST3-GODSADNR-2          
203100           MOVE '                 '          TO LIST3-GODSADNR-3          
203200         ELSE                                                             
203300           IF TEST-IDDISTR = +69 AND SPAR-IDKUNDNR = +8890                
203400             MOVE '                 '          TO LIST3-GODSADNR-0        
203500             MOVE 'BILSPED. 22252290'          TO LIST3-GODSADNR-1        
203600             MOVE '                 '          TO LIST3-GODSADNR-2        
203700             MOVE '                 '          TO LIST3-GODSADNR-3        
203800           ELSE                                                           
203900            IF (TEST-IDDISTR = +19 AND SPAR-IDKUNDNR = +32912) OR         
204000               (TEST-IDDISTR = +69 AND SPAR-IDKUNDNR = +32912)            
204100               MOVE '                 '        TO LIST3-GODSADNR-0        
204200               MOVE '                 '        TO LIST3-GODSADNR-1        
204300               MOVE '                 '        TO LIST3-GODSADNR-2        
204400               MOVE '                 '        TO LIST3-GODSADNR-3        
204500            ELSE                                                          
204600             IF TEST-IDDISTR = +19 AND SPAR-IDKUNDNR = +38025             
204700               MOVE '                 '        TO LIST3-GODSADNR-0        
204800               MOVE 'BILSPED. 21601661'        TO LIST3-GODSADNR-1        
204900               MOVE '                 '        TO LIST3-GODSADNR-2        
205000               MOVE '                 '        TO LIST3-GODSADNR-3        
205100             ELSE                                                         
205200               MOVE '                 '        TO LIST3-GODSADNR-0        
205300               MOVE '                 '        TO LIST3-GODSADNR-1        
205400               MOVE '                 '        TO LIST3-GODSADNR-2        
205500               MOVE '                 '        TO LIST3-GODSADNR-3        
205600             END-IF                                                       
205700            END-IF                                                        
205800           END-IF                                                         
205900         END-IF                                                           
206000       END-IF                                                             
206100     END-IF                                                               
206200                                                                          
206300     MOVE FUNCTION CURRENT-DATE (1:8) TO WS-LIST3-TIUTSKR-SEK             
206400     ACCEPT LIST3-TIUTSKR       FROM DATE                                 
206500     ACCEPT WS-TIME            FROM TIME                                  
206600     MOVE WS-TIME-POS1-6               TO LIST3-TIUTSTID                  
206700     COMPUTE LIST3-TIUTSTID = WS-TIME-POS1-6 / 10000                      
206800          MOVE MID1-RAD-IDDISTR  (RAD-IX) TO LIST3-IDDISTR-EMB(1)         
206900                                             LIST3-IDDISTR-GDM            
207000          MOVE MID1-RAD-IDKUNDNR (RAD-IX) TO LIST3-IDKUNDNR-EMB(1)        
207100                                                                          
207200     IF WS-SAMMA-BILD                                                     
207300       EVALUATE TRUE                                                      
207400         WHEN MID2-FLMOTBET = JA                                          
207500           MOVE 'X'                    TO LIST3-KRYSS-MOTBET              
207600           MOVE SPACE                  TO LIST3-KRYSS-IDAVD               
207700           MOVE SPACE                  TO LIST3-KRYSS-DIST                
207800           MOVE SPACE                  TO LIST3-KRYSS-AVSBET              
207900           MOVE SPACE                  TO LIST3-IDAVD                     
208000           MOVE SPACE                  TO LIST3-IDDISTR-BET               
208100         WHEN MID2-IDAVD > ZERO                                           
208200           MOVE SPACE                  TO LIST3-KRYSS-MOTBET              
208300           MOVE 'X'                    TO LIST3-KRYSS-IDAVD               
208400           MOVE SPACE                  TO LIST3-KRYSS-DIST                
208500           MOVE SPACE                  TO LIST3-KRYSS-AVSBET              
208600           MOVE MID2-IDAVD             TO LIST3-IDAVD                     
208700           MOVE SPACE                  TO LIST3-IDDISTR-BET               
208800         WHEN MID2-IDDISTR-BET > 0                                        
208900           MOVE SPACE                  TO LIST3-KRYSS-MOTBET              
209000           MOVE SPACE                  TO LIST3-KRYSS-IDAVD               
209100           MOVE 'X'                    TO LIST3-KRYSS-DIST                
209200           MOVE SPACE                  TO LIST3-KRYSS-AVSBET              
209300           MOVE SPACE                  TO LIST3-IDAVD                     
209400           MOVE MID2-IDDISTR-BET       TO LIST3-IDDISTR-BET               
209500         WHEN OTHER                                                       
209600           MOVE SPACE                  TO LIST3-KRYSS-MOTBET              
209700           MOVE SPACE                  TO LIST3-KRYSS-IDAVD               
209800           MOVE SPACE                  TO LIST3-KRYSS-DIST                
209900           MOVE 'X'                    TO LIST3-KRYSS-AVSBET              
210000           MOVE SPACE                  TO LIST3-IDAVD                     
210100           MOVE SPACE                  TO LIST3-IDDISTR-BET               
210200       END-EVALUATE                                                       
210300     ELSE                                                                 
210400       MOVE SPACE                      TO LIST3-KRYSS-MOTBET              
210500                                          LIST3-KRYSS-IDAVD               
210600                                          LIST3-KRYSS-DIST                
210700                                          LIST3-IDAVD                     
210800       MOVE 'X'                        TO LIST3-KRYSS-AVSBET              
210900     END-IF                                                               
211000                                                                          
211100     INSPECT LIST3-IDDISTR-BET REPLACING LEADING ZERO BY SPACE            
211200     INSPECT LIST3-IDAVD       REPLACING LEADING ZERO BY SPACE            
211300     PERFORM S03C-NOLLA-LISTAN-EJ-HUV                                     
211400     MOVE 'END S04C-INIT'     TO PGM-POS                                  
211500     .                                                                    
211600     EJECT                                                                
211700 S05C-FRAKTSATT-TILL-LISTAN    SECTION.                                   
211800     MOVE 'START S05C-FRAKT'    TO PGM-POS                                
211900                                                                          
212000     MOVE VORD-KDFRAKT         TO 4732-KDFRAKT                            
212100                                  WSFRA-KDFRAKT                           
212200     IF VORD-KDFRAKT = +21                                                
212300       MOVE WS-SPECIALTEXT     TO LIST3-GODSADNR-0                        
212400     END-IF                                                               
212500                                                                          
212600     PERFORM IMS-GU-473211                                                
212700                                                                          
212800     IF SEGMENT-FINNS                                                     
212900       MOVE FRAKT-BEFRAKT (1)  TO LIST3-BEFRAKT                           
213000     ELSE                                                                 
213100       MOVE SPACE              TO LIST3-BEFRAKT                           
213200     END-IF                                                               
213300                                                                          
213400     MOVE 'FK '              TO LIST3-LEDTEXT                             
213500     MOVE VORD-KDFRAKT       TO LIST3-KDFRAKT                             
213600     MOVE 'END S05C-FRAKT'    TO PGM-POS                                  
213700     .                                                                    
213800     EJECT                                                                
214900                                                                          
215000* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
215100*                                                                         
215200*       I M S - S E K T I O N E R                                         
215300*                                                                         
215400*                                                                         
215500     SKIP3                                                                
215600 IMS-GET-MSG SECTION.                                                     
215700     MOVE '  QC' TO GODK-STATUSKODER                                      
215800     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
215900     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
216000     PERFORM IMS-STATUSKONTROLL                                           
216100     .                                                                    
216200     SKIP3                                                                
216300 IMS-INSERT-MSG SECTION.                                                  
216400     IF ENGLISH-TEXT                                                      
216500       MOVE 'N' TO MFS-KDHUVOMR                                           
216600     END-IF                                                               
216700     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
216800     MOVE SPACE TO GODK-STATUSKODER                                       
216900     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
217000     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
217100     PERFORM IMS-STATUSKONTROLL                                           
217200     .                                                                    
217300     EJECT                                                                
217400 IMS-INSERT-ALTMSG SECTION.                                               
217500     IF ENGLISH-TEXT                                                      
217600       MOVE 'N' TO ALT1-KDMFSFOR                                          
217700     END-IF                                                               
217800     MOVE LOW-VALUE TO ALT1-Z1 ALT1-Z2                                    
217900     MOVE SPACE TO GODK-STATUSKODER                                       
218000     CALL CBLTDLI USING ISRT ALT1-PCB ALT1-IO-AREA                        
218100     MOVE ALT1-STATUS-CODE TO STATUS-WS                                   
218200     PERFORM IMS-STATUSKONTROLL                                           
218300     .                                                                    
218400     EJECT                                                                
219600 IMS-GHU-WDE601  SECTION.                                                 
219700     STRING 'WDE601  (IDPRODNR =' W-IDPRODNR-X ')'                        
219800            DELIMITED BY SIZE INTO SSA1                                   
219900     MOVE '  '        TO GODK-STATUSKODER                                 
220000     CALL CBLTDLI USING GHU  WDE6-PCB DLI-IO-E601 SSA1                    
220100     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
220200     PERFORM IMS-STATUSKONTROLL                                           
220300     .                                                                    
220400     EJECT                                                                
220500 IMS-REPL-WDE601 SECTION.                                                 
220600     MOVE '  '        TO GODK-STATUSKODER                                 
220700     CALL CBLTDLI USING REPL WDE6-PCB DLI-IO-E601                         
220800     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
220900     PERFORM IMS-STATUSKONTROLL                                           
221000     .                                                                    
221100     EJECT                                                                
221200 IMS-GHNP-WDE611  SECTION.                                                
221300     MOVE 'WDE611 ' TO SSA1                                               
221400     MOVE '  GE'      TO GODK-STATUSKODER                                 
221500     CALL CBLTDLI USING GHNP  WDE6-PCB DLI-IO-E611 SSA1                   
221600     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
221700     PERFORM IMS-STATUSKONTROLL                                           
221800     .                                                                    
221900     EJECT                                                                
222000 IMS-GHNP-WDE611-KVAL  SECTION.                                           
222100     STRING 'WDE611  (IDKOLLI  >' W-IDKOLLI-X ')'                         
222200            DELIMITED BY SIZE INTO SSA1                                   
222300     MOVE '  GE'      TO GODK-STATUSKODER                                 
222400     CALL CBLTDLI USING GHNP  WDE6-PCB DLI-IO-E611 SSA1                   
222500     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
222600     PERFORM IMS-STATUSKONTROLL                                           
222700     .                                                                    
222800     EJECT                                                                
222900 IMS-REPL-WDE611 SECTION.                                                 
223000     MOVE '  '        TO GODK-STATUSKODER                                 
223100     CALL CBLTDLI USING REPL WDE6-PCB DLI-IO-E611                         
223200     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
223300     PERFORM IMS-STATUSKONTROLL                                           
223400     .                                                                    
223500     EJECT                                                                
223600 IMS-GN-WDE4D1   SECTION.                                                 
223700     STRING 'WDE4D1  (WDE4D1KY>=' W-WDE4D1KY-MIN-X                        
223800                    '&WDE4D1KY<=' W-WDE4D1KY-MAX-X ')'                    
223900            DELIMITED BY SIZE INTO SSA1                                   
224000     MOVE '  GE'      TO GODK-STATUSKODER                                 
224100     CALL CBLTDLI USING GN   WDE4D-PCB DLI-IO-E4D1 SSA1                   
224200     MOVE WDE4D-STATUS-CODE TO STATUS-WS                                  
224300     PERFORM IMS-STATUSKONTROLL                                           
224400     .                                                                    
224500     EJECT                                                                
224600 IMS-GU-473211   SECTION.                                                 
224700     STRING 'WL473201(WDGXKEY  =' 4732-WDGXKEY-X ')'                      
224800            DELIMITED BY SIZE INTO SSA1                                   
224900     STRING 'WL473211(KDSEGKEY =' 4732-KDSEGKEY-X ')'                     
225000            DELIMITED BY SIZE INTO SSA2                                   
225100     MOVE '  GE'      TO GODK-STATUSKODER                                 
225200     CALL CBLTDLI USING GU   4732-PCB DLI-IO-AREA-2 SSA1 SSA2             
225300     MOVE 4732-STATUS-CODE TO STATUS-WS                                   
225400     PERFORM IMS-STATUSKONTROLL                                           
225500     .                                                                    
225600     EJECT                                                                
225700 IMS-GU-WDQ2-CSEQ      SECTION.                                           
225800     STRING 'WDQ201  (WDQ2CSEQ =' W-WDQ2CSEQ-X ')'                        
225900            DELIMITED BY SIZE INTO SSA1                                   
226000     MOVE '    ' TO GODK-STATUSKODER                                      
226100     CALL CBLTDLI USING GU WDQ2-PCB DLI-IO-AREA-OHUV SSA1                 
226200     MOVE WDQ2-STATUS-CODE TO STATUS-WS                                   
226300     PERFORM IMS-STATUSKONTROLL                                           
226400     .                                                                    
226500     EJECT                                                                
226600 IMS-GNP-WDQ212 SECTION.                                                  
226700                                                                          
226800     STRING 'WDQ212  (IDDC     =' W-IDDC-X ')'                            
226900          DELIMITED BY SIZE     INTO SSA1                                 
227000     MOVE '    '               TO GODK-STATUSKODER                        
227100     CALL CBLTDLI USING GNP  WDQ2-PCB DLI-IO-AREA-ARB SSA1                
227200     MOVE WDQ2-STATUS-CODE     TO STATUS-WS                               
227300     PERFORM IMS-STATUSKONTROLL                                           
227400     .                                                                    
227500     EJECT                                                                
228400 IMS-STATUSKONTROLL SECTION.                                              
228500     SET STATUS-IX TO 1                                                   
228600     SEARCH GODK-STATUS                                                   
228700       AT END CALL FELLOG                                                 
228800       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
228900     END-SEARCH                                                           
229000     .                                                                    
229100     EJECT                                                                
