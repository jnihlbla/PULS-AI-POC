000100******************************************************************        
000200*     THIS PROGRAM ALSO HAS A WEB-LDC VERSION CALLED WL0128      *        
000300******************************************************************        
000400*                                                                         
000500 ID DIVISION.                                                             
000600 PROGRAM-ID.     W4033300.                                                
000700 AUTHOR.         ANDERS RYDEN.                                            
000800 DATE-WRITTEN.   FEB.  86.                                                
000900 DATE-COMPILED.                                                           
001000*                                                                         
001100*        VAL 'UU' VID UTSKRIFT AV KOLLIFLAGGA SKALL INTE GE               
001200*        NÅGON UTSKRIFT, MEN ÄR ETT GODKÄNT VAL.                          
001300*        EN TEST MED KDFRAKT FINNS HÅRDKODAD, SÖK PÅ "*KDFRAKT"           
001400*                                                                         
001500*    FUNKTION.                                                            
001600*        PROGRAMMET ÄR ETT FRÅGA/SKRIVA-LISTA PGM                         
001700*        LISTNING KOLLIFLAGGA                                             
001800*                                                                         
001900*        DET FINNS 4 INGÅNGAR TILL PROGRAMMET VILKA STYR                  
002000*        OUTPUT-MEDIA:                                                    
002100*        1)  ENTER FRÅN EGET PROGRAM                                      
002200*            PGM:ET SVARAR SKÄRMEN                                        
002300*        2)  PF4 FRÅN EGET PROGRAM                                        
002400*            PGM:ET SVARAR SKÄRMEN OCH SKRIVER EN SÅ KALLAD               
002500*            KOLLI-FLAGGA (DVS ADRESSFLAGGA) (LISTA)                      
002600*        3)  PROGRAM-TO-PROGRAM-SWITCH FRÅN 4331-, 4332- ELLER            
002700*            4335-BILDEN                                                  
002800*            PGM:ET SKRIVER ENDAST KOLLI-FLAGGA (LISTA) DVS.              
002900*            SVARAR EJ SKÄRMEN                                            
003000*        4)  GENOM TRANSKOD FRÅN ANNAN BILD                               
003100*            SVARAR SKÄRMEN MED EGET FORMAT                               
003200*                                                                         
003300*    INDATA.                                                              
003400*        TRANSAKTION: W4T333                                              
003500*                     W4T331                                              
003600*                     W4T332                                              
003700*        MID:         W4I33301                                            
003800*                                                                         
003900*    UTDATA.                                                              
004000*        MOD:         W4O33301                                            
004100*                                                                         
004200* CHANGE LOG:                                                             
004300*                                                                         
004400* LINDA NILSSON 041004                                                    
004500* ETRACKER 967014                                                         
004600*                                                                         
004700* LINDA NILSSON 050113                                                    
004800* ETRACKER 1675178                                                        
004900*                                                                         
005000* BERT ANDERSSON 051020                                                   
005100* ETRACKER 2616800                                                        
005200*                                                                         
005300* SUSANNE OLSSON 071127                                                   
005400* ETRACKER 4823800                                                        
005500*                                                                         
005600* STINA MOGREN 071212                                                     
005700* ETRACKER 5923648                                                        
005800*                                                                         
005900* GÖRAN KJELLSON 071211                                                   
006000* ETRACKER 6044087  (BORTTAG AV CALL PÅ W400SWIF)                         
006100*                                                                         
006200* CAMELIA OLGRENER 110308                                                 
006300* ETRACKER 3785720  (EDI TNT)                                             
006400*                                                                         
006500* CAMELIA OLGRENER 120105                                                 
006600* ETRACKER 9877113  (HIT-FIL ÄNDRINGAR) +                                 
006700* ETRACKER 10163433 (TRP TILL HIT)                                        
006800*                                                                         
006900* CAMELIA OLGRENER 130328                                                 
007000* ETRACKER 10189934 (EDI TILL HIT-DK)                                     
007100*                                                                         
007200* CAMELIA OLGRENER 151215                                                 
007300* ETRACKER 10271301 (EDI TILL HIT-NO)                                     
007400*                                                                         
007500* CAMELIA OLGRENER 171002                                                 
007600* JIRA 888 (DHL TAR ÖVER TNT DISTRIKTEN)                                  
007700*                                                                         
007800* CAMELIA OLGRENER 191128                                                 
007900* PBI 1569628 (EJ BARCODE /FIL TILL POSTNORD->HIT-DK)                     
008000*                                                                         
008100* CAMELIA OLGRENER 210930                                                 
008200* HIT=POSTNORD                                                            
008300* POSTNORD SKALL INTE HA NORGE LÄNGRE MEN KODEN ÄR INTE RENSAD            
008400* UTAN WWDIST83->DIST83-HIT-NO=9999 SÅ ATT DET INTE INTRÄFFAR             
008500* LÄNGRE.                                                                 
008600* OBS --> ATT RENSA VID TILLFÄLLE.                                        
008700*                                                                         
009100 ENVIRONMENT DIVISION.                                                    
009200 DATA DIVISION.                                                           
009300                                                                          
009400     EJECT                                                                
009500 WORKING-STORAGE           SECTION.                                       
009600*    -- CHECKED BY WY2000                                                 
009700                                                                          
009800 77  IDPGM                       PIC X(08)   VALUE 'W4033300'.            
009900 77  PGM-POS                     PIC X(16)   VALUE SPACE.                 
010000 77  FILLER                      PIC X(08)   VALUE 'FELTEXT:'.            
010100 77  FELTEXT                     PIC X(16)   VALUE SPACE.                 
010200 77  FILLER                      PIC X(08)   VALUE 'IMS-POS:'.            
010300 77  IMS-POS                     PIC X(16)   VALUE SPACE.                 
010400 77  JA                          PIC X       VALUE 'J'.                   
010500 77  NEJ                         PIC X       VALUE 'N'.                   
010600 77  RAETT                       PIC X       VALUE 'R'.                   
010700 77  7INCH                       PIC X       VALUE '7'.                   
010800 77  SPRAK-IX                    PIC S9(9)   VALUE +0   COMP SYNC.        
010900 77  INDX                        PIC S9(9)   VALUE +0   COMP SYNC.        
011000 77  SHIP-INDX                   PIC S9(9)   VALUE +0   COMP SYNC.        
011100 77  FK-INDX                     PIC S9(9)   VALUE +0   COMP SYNC.        
011200 77  MAX-FK-INDX                 PIC S9(9)   VALUE +6   COMP SYNC.        
011300 77  KLIID-IX                    PIC  9(9)   VALUE 0.                     
011400 77  WS-MFS-KDMFSFOR             PIC 9(1).                                
011500 77  WS-SPAR-KVORDRAD-LEVPL      PIC S9(5)   VALUE ZERO COMP-3.           
011600 77  WS-KOLLI-KVORDRAD           PIC S9(5)   VALUE ZERO COMP-3.           
011700 77  DUMMY-AREA                  PIC X(50)   VALUE SPACE.                 
011800 77  FILLER                      PIC X(8)    VALUE 'AAAAAAAA'.            
011900 77  WS-IDZON                    PIC X(2)    VALUE SPACE.                 
012000 77  WS-IDROUTE                  PIC X(1)    VALUE SPACE.                 
012100 77  WS-IDDEPOT                  PIC X(2)    VALUE SPACE.                 
012200 77  WS-KDEMBTYP                 PIC 9(1).                                
012300 77  WS-KDKOLLIT                 PIC X(1).                                
012400 77  WS-KDORDKL                  PIC X(1).                                
012500 77  WS-IDPSN                    PIC 9(3).                                
012600 77  FILLER                      PIC X(8)    VALUE 'BBBBBBBB'.            
012700 77  WS-IDTRPTNR                 PIC S9(3)   VALUE ZERO  COMP-3.          
012800 77  WS-KDFRAKT                  PIC S9(3)   VALUE ZERO  COMP-3.          
012900 77  WS-ORAD-BERADREF            PIC X(10)   VALUE SPACE.                 
013000 77  WS-ORAD-KVLEVART            PIC S9(7)   VALUE ZERO  COMP-3.          
013100 77  WS-KOLLI-KDKOLLI            PIC X(8)    VALUE SPACE.                 
013200*                                                                         
013300 77  WS-LISTA-ADRESS-5           PIC X(35) VALUE SPACE.                   
013400 77  WS-GMT-BETEXT-INFO         PIC X(35) VALUE SPACE.                    
013500 77  WS-OHUV-BEKUNDRF            PIC X(15) VALUE SPACE.                   
013600 77  WS-KOLLI-ADFLGEO            PIC X(3).                                
013700 77  WS-KOLLI-ADFLOMR            PIC 9(3).                                
013800 77  WS-TIME-DELAY               PIC S9(9)   VALUE ZERO  BINARY.          
013900 77  WS-DAGENS-DATUM             PIC 9(8)   VALUE ZERO.                   
014000 77  WS-DAGENS-DATUM-HIT         PIC 9(8)   VALUE ZERO.                   
014100 77  WS-TIDPUNKT                 PIC 9(8)   VALUE ZERO.                   
014200 77  WS-TID                      PIC 9(6)   VALUE ZERO.                   
014300*                                                                         
014400 77  WS-FLYG-TRP                 PIC S9(3)  VALUE +99.                    
014500 77  WS-FLYG-FRAKT               PIC S9(3)  VALUE +17.                    
014600 77  WS-AIR                      PIC X(3)   VALUE 'AAA'.                  
014700 77  WS-LHS                      PIC X(3)   VALUE 'LHS'.                  
014800 77  WS-TAKF-BEGMT-RAD1          PIC X(35)  VALUE SPACE.                  
014900 77  WS-TAKF-BEGMT-RAD2          PIC X(35)  VALUE SPACE.                  
015000                                                                          
015100*-- FIX                                                                   
015200 01  W009WTOP                    PIC X(8) VALUE 'W009WTOP'.               
015300 01  WMSG-JUSTNU.                                                         
015400    03 FILLER                    PIC S9(4) BINARY VALUE +30.              
015500    03 FILLER                    PIC X(22) VALUE                          
015600       'PGM W40333 KLOCKSLAG: '.                                          
015700    03 WMSG-KLOCKSLAG            PIC 9(8).                                
015800    03 FILLER                    PIC X(40) VALUE SPACE.                   
015900*                                                                         
016000 77  FILLER                      PIC X(8)    VALUE 'CCCCCCCC'.            
016100                                                                          
016200 77    WS-DC-TEST                PIC X(01).                               
016300   88  DC-WRONG                              VALUE 'N'.                   
016400   88  DC-OK                                 VALUE 'J'.                   
016500*                                                                         
016600 77    WS-KDFRAKT-TEST           PIC X(01).                               
016700   88  KDFRAKT-FINNS                         VALUE 'J'.                   
016800   88  KDFRAKT-SAKNAS                        VALUE 'N'.                   
016900*                                                                         
017000 77    IDPRODNR-IFYLLT-SW        PIC X(01).                               
017100   88  IDPRODNR-IFYLLT                       VALUE 'J'.                   
017200*                                                                         
017300 77    ONLY-ONE-CASE-SW          PIC X(01).                               
017400   88  ONLY-ONE-CASE                         VALUE 'J'.                   
017500*                                                                         
017600 77    BREAK-SW                  PIC X(01).                               
017700   88  BREAK                                 VALUE 'J'.                   
017800*                                                                         
017900 77    TAKF-FINNS-SW             PIC X(01).                               
018000*                                                                         
018100 77    SW-ADRESS-HAMTAD          PIC X(01).                               
018200   88  ADRESS-INTE-HAMTAD                    VALUE 'N'.                   
018300*                                                                         
018400 77  LISTVAL                     PIC X(8)    VALUE SPACE.                 
018500 77  WS-SLINGA-KLAR              PIC X(1).                                
018600     88  SLINGA-KLAR             VALUE 'J'.                               
018700 77  WS-IDTRANS                  PIC X(4).                                
018800     88  WS-GODKAEND-BILD        VALUE  '431C' '4331' '4332'              
018900                                 '4333' '4334' '4335' '4336'              
019000                                 '4338' '433A' '433B' '433E'              
019100                                 '433F' '431E' '431D' '433Z'              
019110                                 'L199' '4327' 'L197' .                   
019200     88  FROM-4333-BILD          VALUE  '4333'.                           
019300*OBS!GODKÄNDA TRANSAR ÄR ÄVEN HÅRD-KODADE I HUVUD-SLINGAN.                
019400                                                                          
019500*SOME KDFRAKT GIVES PRINTING OF TEXT ZONA + AREA.                         
019600 77  WS-WRITE-ZONA               PIC X(2).                                
019700     88 KDFRAKT-OK-WRITE-ZONA    VALUE '13' '17'.                         
019800*                                                                         
019900*REGARDING WS-KDPRTVAL:                                                   
020000*                                                                         
020100*THERE ARE 5 PRINTER FORMATS                                              
020200*ITS IMPORTANT TO USE THE CORRECT 88 LEVEL WHEN ADDING                    
020300*A NEW PRINTER ALIAS TO THE 88 LEVELS.                                    
020400*                                                                         
020500 77  WS-KDPRTVAL                 PIC XX.                                  
020600     88 SKRIV-KOLLIFLAGGA-A6-FORMAT   VALUE                               
020700          '00' '2 ' '3 ' '4 ' '5 ' '6 ' '8 '                              
020800          '! ' '. ' '/ ' ', ' '¤ ' ') ' '= '                              
020900          'A ' 'FG' 'KO' 'LL' 'MM' 'M2' 'OO' 'II' 'JJ' 'N '               
021000          'Q ' 'SX' 'TT' 'E ' 'A6' 'XX' 'YY' 'QW' 'Q2' 'W2'.              
021100*                                                                         
021200*    -- BELOW BELOW PRINTING TO NOVA PRINTER                              
021300*    -- NOTE!                                                             
021400*    -- WHEN CHANGING THE CONTENT IN WRITE-PA NOVA PRINTER                
021500*    -- ALSO CHANGE IN PRINTER-PGM W4034P00                               
021600*                                                                         
021700     88 SKRIV-PA-NOVA-SKRIVARE        VALUE '" ' '* ' '- '                
021800                                            '? '                          
021900                                            '0 ' '1 ' '7 ' '10'           
022000                                            '12' '13'                     
022100                                            '20' '21' '22' '23'           
022200                                            '25' '26' '27' '50'           
022210                                            '31' '32' '35' '36'           
022300                                            'AB' 'AS' 'AZ'                
022400                                            'BA'                          
022500                                            'CC'                          
022600                                            'D ' 'DD'                     
022700                                            'E3' 'E4'                     
022710                                            'F ' 'FV'                     
022800                                            'G ' 'GG' 'H '                
022900                                            'J ' 'JK'                     
022910                                            'LA' 'LP'                     
023100                                            'NN' 'NJ' 'NL' 'O '           
023200                                            'P ' 'PV' 'PS'                
023300                                            'Q1' 'Q3'                     
023400                                            'R ' 'R7'                     
023410                                            'T '                          
023500                                            'UY' 'UI' 'UJ'                
023600                                            'VU' 'VV' 'WW'                
023700                                            'W3'                          
023800                                            'XA' 'X-' 'X '                
023900                                            'Y ' 'YH'                     
024000                                            'ZZ'.                         
024100*                                                                         
024200     88 PRINT-TWO-CASE-LABELS         VALUE 'D '.                         
024300     88 DONT-PRINT-CASE-LABEL         VALUE 'UU' 'NO'.                    
024400     88 SKRIV-LANG-BARCODE            VALUE 'LL'.                         
024500*                                                                         
024600 01  WS-DARFS                    PIC  9(12).                              
024700 01  FILLER REDEFINES  WS-DARFS.                                          
024800     03  FILLER                  PIC  9(2).                               
024900     03  WS-DARFS-YYMMDD         PIC  9(6).                               
025000     03  WS-DARFS-HHMM           PIC  9(4).                               
025100 SKIP2                                                                    
025200 01  WS-TIRFS-MMDDYY             PIC  9(6).                               
025300 01  FILLER REDEFINES  WS-TIRFS-MMDDYY.                                   
025400     03  WS-TIRFS-MM             PIC  9(2).                               
025500     03  WS-TIRFS-DD             PIC  9(2).                               
025600     03  WS-TIRFS-YY             PIC  9(2).                               
025700 SKIP2                                                                    
025800 01  FILLER                      PIC X(16)  VALUE 'ARBETSFAELT'.          
025900                                                                          
026000 01  WS-BEGMT-RAD1               PIC X(35) VALUE SPACE.                   
026100 01  WS-BEGMT-RAD2               PIC X(35) VALUE SPACE.                   
026200 01  WS-ADGMT-GATA               PIC X(35) VALUE SPACE.                   
026300 01  WS-ADGMT-PADR               PIC X(35) VALUE SPACE.                   
026400 01  WS-ADGMT-LAND               PIC X(35) VALUE SPACE.                   
026500 01  WS-BEBETRAD-1               PIC X(35) VALUE SPACE.                   
026600 01  WS-BEBETRAD-2               PIC X(35) VALUE SPACE.                   
026700 01  WS-ADBETRAD-1               PIC X(35) VALUE SPACE.                   
026800 01  WS-ADBETRAD-2               PIC X(35) VALUE SPACE.                   
026900                                                                          
027000 01  WS-BEGMRK                   PIC X(94) VALUE SPACE.                   
027100 01  WS-BEGMRK-LAYOUT REDEFINES WS-BEGMRK.                                
027200   03 WS-BEGMRK-RAD1             PIC X(30).                               
027300   03 WS-BEGMRK-RAD2             PIC X(30).                               
027400   03 WS-BEGMRK-RAD3             PIC X(30).                               
027500   03 WS-BEGMRK-RAD4             PIC X(4).                                
027600                                                                          
027700 01  WS-OHUV-IDDEPT              PIC 9(2)  VALUE ZERO.                    
027800 01  WS-OHUV-TIREPDAT            PIC 9(6)  VALUE ZERO.                    
027900 01  WS-OHUV-IDSYSTEM            PIC X(4)  VALUE ZERO.                    
028000                                                                          
028100 77  WS-RESTORDER                PIC X.                                   
028200   88  RO-JA                               VALUE 'J'.                     
028300   88  RO-NEJ                              VALUE 'N'.                     
028400*                                                                         
028500 77  FILLER                      PIC X(8)  VALUE 'ARBFAELT'.              
028600*                                                                         
028700 01  ARBETSFAELT.                                                         
028800   03  WS-IDDISTR                           PIC 9(4).                     
028900   03  WS-IDKUNDNR                          PIC X(6).                     
029000   03  WS-IDKUNDNR-7                        PIC 9(7).                     
029100   03  WS-IDKUNDNR-IDGROSS                  PIC 9(7).                     
029200   03  WS-IDORDNR                           PIC X(5).                     
029300   03  WS-IDPRODNR                          PIC X(7).                     
029400   03  WS-IDPRODNR-7                        PIC 9(7).                     
029500   03  WS-IDKOLLI                           PIC X(5).                     
029600   03  WS-IDKOLLI-PRT                       PIC 9(5).                     
029700   03  WS-IDKOLLI-TOM                       PIC 9(5).                     
029800   03  WS-IDKOLLI-FLER                      PIC S9(5) COMP-3.             
029900   03  WS-KOLLI-DIKOLLIL                    PIC S9(5) COMP-3.             
030000   03  WS-KOLLI-DIKOLLIB                    PIC S9(3) COMP-3.             
030100   03  WS-KOLLI-DIKOLLIH                    PIC S9(3) COMP-3.             
030200   03  WS-KDORDKL-TXT                       PIC X(3).                     
030300   03  WS-IDPRCVAR-TXT                      PIC X(3).                     
030400   03  WS-VIKT                    PIC X(10) VALUE                         
030500                                         '9786423597'.                    
030600   03  WS-VIKT-SIFFRA             PIC 9(1)  VALUE ZERO.                   
030700   03  WS-SIFFRA                  PIC 9(1)  VALUE ZERO.                   
030800   03  WS-SUMMA                   PIC 9(4)  VALUE ZERO COMP-3.            
030900   03  WS-REST                    PIC 9(3)  VALUE ZERO COMP-3.            
031000   03  WS-HELTAL                  PIC 9(3)  VALUE ZERO COMP-3.            
031100   03  WS-CD                      PIC 9(3)  VALUE ZERO COMP-3.            
031200                                                                          
031300 01  WS-VKORDBTO                 PIC 9(6)V9 VALUE ZERO.                   
031400 01  FILLER REDEFINES WS-VKORDBTO.                                        
031500     03 WS-KILO                  PIC 9(6).                                
031600     03 WS-HEKTO                 PIC 9.                                   
031700                                                                          
031800 01  WS-KOLLI-VLORDBTO           PIC 9(4)V9(3) VALUE ZERO.                
031900 01  FILLER REDEFINES WS-KOLLI-VLORDBTO.                                  
032000     03 WS-VLM3                  PIC 9(4).                                
032100     03 WS-VLCM3                 PIC 9(3).                                
032200                                                                          
032300 01  WS-KDMATT                   PIC X.                                   
032400     88 US-MEASUREMENT           VALUE 'U'.                               
032500     88 SIS-MEASUREMENT          VALUE 'S'.                               
032600*                                                                         
032700 01  WS-IDKLIID.                                                          
032800     03 WS-IDKLIID-1-11.                                                  
032900       05 WS-IDKLIID-1-10        PIC 9(10).                               
033000       05 WS-IDKLIID-11          PIC 9(1).                                
033100     03 WS-IDKLIID-12            PIC X(2) VALUE 'SE'.                     
033200*                                                                         
033300 01  WS-IDKLIID-X.                                                        
033400     03 WS-IDKLIID-1-10-X        PIC X(10).                               
033500     03 WS-IDKLIID-11-X          PIC X(1).                                
033600     03 WS-IDKLIID-12-X          PIC X(2) VALUE 'SE'.                     
033700*                                                                         
033800*                                                                         
033900 01  WS-ADFLGEO-HIT.                                                      
034000     03 WS-IDDEPOT-HIT           PIC X(2).                                
034100     03 WS-IDROUTE-HIT           PIC X(1).                                
034200                                                                          
034300*      --- VALID IDDD CODES                                               
034400 01  FILLER                      PIC X(8)    VALUE 'WWDC99  '.            
034500*01    -COPY WWDC99                                                       
034600       EJECT                                                              
034700 01  WS-IDPRTLST.                                                         
034800     03 WS-SYSTDEL               PIC X(1).                                
034900     03 WS-LISTTYP               PIC X(2).                                
035000     03 WS-DC                    PIC X(2).                                
035100     03 WS-KDPRT                 PIC X(3).                                
035200 01  WS-IDPRTJAP REDEFINES WS-IDPRTLST.                                   
035300     03 WS-LASER-BLANKETT        PIC X(6).                                
035400     03 WS-NDC-JAP-KDPRT         PIC X(3).                                
035500                                                                          
035600     EJECT                                                                
035700 01  GENERELLA-SUBPROGRAM.                                                
035800   03 W006PRS1                 PIC X(8)   VALUE 'W006PRS1'.               
035900   03 W006PRT                  PIC X(8)   VALUE 'W006PRT '.               
036000   03 CBLTDLI                  PIC X(8)   VALUE 'CBLTDLI '.               
036100   03 FELLOG                   PIC X(8)   VALUE 'FELLOG  '.               
036200   03 W005INIT                 PIC X(8)   VALUE 'W005INIT'.               
036300   03 WWOMVAND                 PIC X(8)   VALUE 'WWOMVAND'.               
036400   03 W009WAIT                 PIC X(8)   VALUE 'W009WAIT'.               
036500     EJECT                                                                
036600                                                                          
036700*    --- AREOR TILL GEMENSAMMA SUBPROGRAM                                 
036800*                                                                         
036900 01  FILLER                      PIC X(16)  VALUE 'LÄNKAREOR'.            
037000*                                                                         
037100 01  FILLER                      PIC X(16)  VALUE 'W006PRT  '.            
037200*   -COPY W006PRT                                                         
037300*                                                                         
037400 01  FILLER                      PIC X(16)  VALUE 'WMSGINIT '.            
037500*   -COPY WMSGINIT                                                        
037600*                                                                         
037700 01  FILLER                      PIC X(16)  VALUE 'WWOMVAND '.            
037800*   -COPY WWOMVAND                                                        
037900*                                                                         
038000 01  FILLER                      PIC X(16)  VALUE 'WNDCADRE '.            
038100*   -COPY WNDCADRE                                                        
038200     EJECT                                                                
038300*****************************************************************         
038400*         AREA MED STYRTECKEN FÖR ERICSSON XXXX RAD-SKRIVARE.   *         
038500*         ANV. FÖR ATT SKRIVA KOLLIFLAGGA I A4 FORMAT I CDC.    *         
038600*****************************************************************         
038700 01  FILLER                      PIC X(16)  VALUE 'LISTA-RAD'.            
038800 01  RADREDIGERING.                                                       
038900       05  STOR-RAD1.                                                     
039000           07  LISTA-IDDISTR              PIC Z(4)  VALUE ZERO.           
039100           07  LISTA-IDKUNDNR             PIC Z(5)9 VALUE ZERO.           
039200           07  LISTA-IDORDNR              PIC Z(4)9 VALUE SPACE.          
039300       05  STOR-RAD2.                                                     
039400           07  LISTA-KDFRAKT              PIC Z9    VALUE ZERO.           
039500           07  LISTA-VKORDBTO             PIC Z(5)  VALUE ZERO.           
039600           07  LISTA-IDKOLLI              PIC Z(5)  VALUE ZERO.           
039700       05  STOR-RAD3.                                                     
039800           07  LISTA-ADRESS-1             PIC X(30) VALUE SPACE.          
039900           07  LISTA-TIRFS                PIC 9(6)  VALUE ZERO.           
040000       05  STOR-RAD4.                                                     
040100           07  LISTA-ADRESS-2             PIC X(30) VALUE SPACE.          
040200       05  STOR-RAD5.                                                     
040300           07  LISTA-ADRESS-3             PIC X(35) VALUE SPACE.          
040400       05  STOR-RAD6.                                                     
040500           07  LISTA-ADRESS-4             PIC X(35) VALUE SPACE.          
040600       05  STOR-RAD7.                                                     
040700           07  LISTA-BEGMRKTXT-RAD1       PIC X(30) VALUE SPACE.          
040800           07  LISTA-BEGMRKBC-RAD1        PIC X(30) VALUE SPACE.          
040900           07  LISTA-IDBILREG             PIC X(10) VALUE SPACE.          
041000       05  LITEN-RAD.                                                     
041100           07  LISTA-ADFLGEO              PIC X(3)  VALUE ZERO.           
041200           07  LISTA-ADFLOMR              PIC Z(2)9 VALUE ZERO.           
041300           07  LISTA-ADRUTNIV             PIC Z(2)9 VALUE ZERO.           
041400           07  LISTA-ADVMODUL             PIC Z(3)  VALUE ZERO.           
041500           07  LISTA-ADHMODUL             PIC Z(3)  VALUE ZERO.           
041600     EJECT                                                                
041700*****************************************************************         
041800*A6  AREA MED STYRTECKEN FÖR MARKPOINT TERMO SKRIVARE.          *         
041900*    ANV. FÖR ATT SKRIVA KOLLIFLAGGA I A6 FORMAT I SDC-21,      *         
042000*    SDC-23 OCH SDC-26.                                         *         
042100*****************************************************************         
042200 01  FILLER           PIC X(24)  VALUE 'KOLLI-FL-A6TERMO'.                
042300*    STYRTECKEN ENLIGT MANUAL: MARKPOINT THERMAL PRINTER                  
042400*                              LABELPOINT                                 
042500 01  CASE-LABEL-THERMO-A6.                                                
042600   03  A6-RAD        PIC X(132)  VALUE SPACE.                             
042700                                                                          
042800   03  A6-STYR-01.                                                        
042900     05  FILLER      PIC X(3)  VALUE '!CÅ'.                               
043000                                                                          
043100   03  A6-STYR-42.                                                        
043200     05  FILLER      PIC X(6)  VALUE '!Y42 0'.                            
043300                                                                          
043400   03  A6-STYR-COBRA.                                                     
043500     05  FILLER      PIC X(16) VALUE '&&??%%P%P=207,30'.                  
043600     05  FILLER      PIC X(19) VALUE '=1,0=5,8=24,0=31,96'.               
043700     05  FILLER      PIC X(21) VALUE '=32,8=33,0=34,1=45,87'.             
043800     05  FILLER      PIC X(12) VALUE '=63,13=136,0'.                      
043900     05  FILLER      PIC X(22) VALUE '=207,12=207,10%&&??000'.            
044000                                                                          
044100   03  A6-STYR-91.                                                        
044200     05  FILLER      PIC X(3)  VALUE '!PÅ'.                               
044300                                                                          
044400   03  A6-RUB-DISTRICT.                                                   
044500     05  FILLER      PIC X(25) VALUE '!F T N   20 230 L 1 1 3 '.          
044600     05  FILLER      PIC X(11) VALUE '"DISTRICT"Å'.                       
044700                                                                          
044800   03  A6-RUB-CUSTOMER.                                                   
044900     05  FILLER      PIC X(25) VALUE '!F T N   50  700 L 1 1 3 '.         
045000     05  FILLER      PIC X(11) VALUE '"CUSTOMER"Å'.                       
045100                                                                          
045200   03  A6-RUB-DEALER.                                                     
045300     05  FILLER      PIC X(25) VALUE '!F T N   50  700 L 1 1 3 '.         
045400     05  FILLER      PIC X(09) VALUE '"DEALER"Å'.                         
045500                                                                          
045600   03  A6-RUB-RETAILER.                                                   
045700     05  FILLER      PIC X(25) VALUE '!F T N   50  700 L 1 1 3 '.         
045800     05  FILLER      PIC X(11) VALUE '"RETAILER"Å'.                       
045900                                                                          
046000   03  A6-RUB-ORDERNUMBER.                                                
046100     05  FILLER      PIC X(25) VALUE '!F T N   50 1040 L 1 1 3 '.         
046200     05  FILLER      PIC X(15) VALUE '"ORDER NUMBER"Å'.                   
046300                                                                          
046400   03  A6-RUB-ADDRESS.                                                    
046500     05  FILLER      PIC X(25) VALUE '!F T N  280   80 L 1 1 3 '.         
046600     05  FILLER      PIC X(10) VALUE '"ADDRESS"Å'.                        
046700                                                                          
046800   03  A6-RUB-CASE.                                                       
046900     05  FILLER      PIC X(25) VALUE '!F T N   50 1530 L 1 1 3 '.         
047000     05  FILLER      PIC X(7)  VALUE '"CASE"Å'.                           
047100*WEIGHT-KG                                                                
047200   03  A6-RUB-WEIGHT-KG-SE.                                               
047300     05  FILLER      PIC X(25) VALUE '!F T N  470 1430 L 1 1 3 '.         
047400     05  FILLER      PIC X(12)  VALUE '"WEIGHT KG"Å'.                     
047500                                                                          
047600   03  A6-RUB-WEIGHT-KG.                                                  
047700     05  FILLER      PIC X(25) VALUE '!F T N  480 1530 L 1 1 3 '.         
047800     05  FILLER      PIC X(12)  VALUE '"WEIGHT KG"Å'.                     
047900                                                                          
048000   03  A6-RUB-KDORDKL.                                                    
048100     05  FILLER      PIC X(25) VALUE '!F T N  370 1120 L 1 1 3 '.         
048200     05  FILLER      PIC X(14) VALUE '"ORDER CLASS"Å'.                    
048300                                                                          
048400   03  A6-RUB-IDDEPT-S03.                                                 
048500     05  FILLER      PIC X(25) VALUE '!F T N  750 1530 L 1 1 3 '.         
048600     05  FILLER      PIC X(7)  VALUE '"DEPT"Å'.                           
048700                                                                          
048800   03  A6-RUB-IDDEPT.                                                     
048900     05  FILLER      PIC X(25) VALUE '!F T N  720 1530 L 1 1 3 '.         
049000     05  FILLER      PIC X(7)  VALUE '"DEPT"Å'.                           
049100                                                                          
049200   03  A6-RUB-RFS.                                                        
049300     05  FILLER      PIC X(25) VALUE '!F T N  930  80  L 1 1 3 '.         
049400     05  FILLER      PIC X(6)  VALUE '"RFS"Å'.                            
049500                                                                          
049600   03  A6-RUB-RFSDATE.                                                    
049700     05  FILLER      PIC X(25) VALUE '!F T N  930  80 L 1 1 3 '.          
049800     05  FILLER      PIC X(11) VALUE '"RFS DATE"Å'.                       
049900                                                                          
050000   03  A6-RUB-TRANSPORTINFO.                                              
050100     05  FILLER      PIC X(25) VALUE '!F T N  660  80 L 1 1 3 '.          
050200     05  FILLER      PIC X(17) VALUE '"TRANSPORT INFO"Å'.                 
050300                                                                          
050400   03  A6-RUB-DANGEROUSGOODS.                                             
050500     05  FILLER      PIC X(25) VALUE '!F T N  930 1300 L 1 1 3 '.         
050600     05  FILLER      PIC X(18)  VALUE '"DANGEROUS GOODS"Å'.               
050700                                                                          
050800   03  A6-RUB-PARTNUMBER.                                                 
050900     05  FILLER      PIC X(25) VALUE '!F T N  380  500 L 1 1 3 '.         
051000     05  FILLER      PIC X(14) VALUE '"PART NUMBER"Å'.                    
051100                                                                          
051200   03  A6-RUB-DC-WH-ADDRESS.                                              
051300     05  FILLER      PIC X(25) VALUE '!F T N  560  500 L 1 1 3 '.         
051400     05  FILLER      PIC X(16) VALUE '"DC WH ADDRESS"Å'.                  
051500                                                                          
051600   03  A6-RUB-ST-ADDRESS.                                                 
051700     05  FILLER      PIC X(25) VALUE '!F T N  930  650 L 1 1 3 '.         
051800     05  FILLER      PIC X(13) VALUE '"ST ADDRESS"Å'.                     
051900                                                                          
052000   03  A6-RUB-FREIGHTCODE.                                                
052100     05  FILLER      PIC X(25) VALUE '!F T N  250 1380 L 1 1 3 '.         
052200     05  FILLER      PIC X(15) VALUE '"FREIGHT CODE"Å'.                   
052300                                                                          
052400   03  A6-RUB-WIP.                                                        
052500     05  FILLER      PIC X(25) VALUE '!F T N  980 1200 L 1 1 3 '.         
052600     05  FILLER      PIC X(9)  VALUE '"W.I.P."Å'.                         
052700                                                                          
052800   03  A6-RUB-WIP-IT.                                                     
052900     05  FILLER      PIC X(25) VALUE '!F T N  620  700 L 1 1 3 '.         
053000     05  FILLER      PIC X(9)  VALUE '"W.I.P."Å'.                         
053100                                                                          
053200   03  A6-RUB-IDPRODNR.                                                   
053300     05  FILLER      PIC X(25) VALUE '!F T N  930 1430 L 1 1 3 '.         
053400     05  FILLER      PIC X(20) VALUE '"PRODUCTION NUMBER"Å'.              
053500*                                            Y650 X900                    
053600   03  A6-RUB-REPDAT-S03.                                                 
053700     05  FILLER      PIC X(25) VALUE '!F T N  800  650 L 1 1 3 '.         
053800     05  FILLER      PIC X(14) VALUE '"REPAIR DATE"Å'.                    
053900                                                                          
054000   03  A6-RUB-REPDAT-S09.                                                 
054100     05  FILLER      PIC X(25) VALUE '!F T N  650 900  L 1 1 3 '.         
054200     05  FILLER      PIC X(14) VALUE '"REPAIR DATE"Å'.                    
054300*                                             550 900                     
054400   03  A6-RUB-CAR-REG-S03.                                                
054500***  05  FILLER      PIC X(25) VALUE '!F T N  680 800  L 1 1 3 '.         
054600     05  FILLER      PIC X(25) VALUE '!F T N  800 1050 L 1 1 3 '.         
054700     05  FILLER      PIC X(20) VALUE '"CAR REGISTRATION"Å'.               
054800                                                                          
054900   03  A6-RUB-CAR-REG-S09.                                                
055000*    05  FILLER      PIC X(25) VALUE '!F T N  540 900  L 1 1 3 '.         
055100     05  FILLER      PIC X(25) VALUE '!F T N  710 950  L 1 1 3 '.         
055200     05  FILLER      PIC X(20) VALUE '"CAR REGISTRATION"Å'.               
055300*IDDISTR       S02-                                                       
055400   03  A6-DATA-IDDISTR-CDC.                                               
055500     05  FILLER      PIC X(25) VALUE '!F T N  150  380 R 3 3 6 '.         
055600     05  FILLER      PIC X(1)  VALUE '"'.                                 
055700     05  A6-IDDISTR-CDC        PIC Z(4)  VALUE ZERO.                      
055800     05  FILLER      PIC X(2)  VALUE '"Å'.                                
055900                                                                          
056000   03  A6-DATA-IDDISTR.                                                   
056100     05  FILLER      PIC X(25) VALUE '!F T N  170  380 R 2 2 6 '.         
056200     05  FILLER      PIC X(1)  VALUE '"'.                                 
056300     05  A6-IDDISTR  PIC Z(4)  VALUE ZERO.                                
056400     05  FILLER      PIC X(2)  VALUE '"Å'.                                
056500                                                                          
056600   03  A6-DATA-IDKUNDNR.                                                  
056700     05  FILLER      PIC X(25) VALUE '!F T N  170  810 R 2 2 6 '.         
056800     05  FILLER      PIC X(1)  VALUE '"'.                                 
056900     05  A6-IDKUNDNR PIC Z(5)9 VALUE ZERO.                                
057000     05  FILLER      PIC X(2)  VALUE '"Å'.                                
057100                                                                          
057200   03  A6-DATA-IDORDNR.                                                   
057300     05  FILLER      PIC X(25) VALUE '!F T N  170 1260 R 2 2 6 '.         
057400     05  FILLER      PIC X(1)  VALUE '"'.                                 
057500     05  A6-IDORDNR PIC Z(4)9  VALUE SPACE.                               
057600     05  FILLER      PIC X(2)  VALUE '"Å'.                                
057700                                                                          
057800   03  A6-DATA-IDKOLLI.                                                   
057900     05  FILLER      PIC X(25) VALUE '!F T N  170 1600 R 2 2 6 '.         
058000     05  FILLER      PIC X(1)  VALUE '"'.                                 
058100     05  A6-IDKOLLI  PIC Z(5)  VALUE ZERO.                                
058200     05  FILLER      PIC X(2)  VALUE '"Å'.                                
058300*KDFRAKT    I SEC S02-                                                    
058400   03  A6-DATA-KDFRAKT-SE.                                                
058500     05  FILLER      PIC X(25) VALUE '!F T N  420 1600 R 3 3 6 '.         
058600     05  FILLER      PIC X(1)  VALUE '"'.                                 
058700     05  A6-KDFRAKT-SE         PIC Z9    VALUE ZERO.                      
058800     05  FILLER      PIC X(2)  VALUE '"Å'.                                
058900                                                                          
059000   03  A6-DATA-KDFRAKT.                                                   
059100     05  FILLER      PIC X(25) VALUE '!F T N  380 1600 R 2 2 6 '.         
059200     05  FILLER      PIC X(1)  VALUE '"'.                                 
059300     05  A6-KDFRAKT  PIC Z9    VALUE ZERO.                                
059400     05  FILLER      PIC X(2)  VALUE '"Å'.                                
059500                                                                          
059600   03  A6-DATA-ADRESS1.                                                   
059700     05  FILLER      PIC X(25) VALUE '!F T N  320  80  L 4 3 1 '.         
059800     05  FILLER      PIC X(1)  VALUE '"'.                                 
059900     05  A6-ADRESS-1 PIC X(30) VALUE SPACE.                               
060000     05  FILLER      PIC X(2)  VALUE '"Å'.                                
060100                                                                          
060200   03  A6-DATA-ADRESS2.                                                   
060300     05  FILLER      PIC X(25) VALUE '!F T N  380  80  L 4 3 1 '.         
060400     05  FILLER      PIC X(1)  VALUE '"'.                                 
060500     05  A6-ADRESS-2 PIC X(30) VALUE SPACE.                               
060600     05  FILLER      PIC X(2)  VALUE '"Å'.                                
060700                                                                          
060800   03  A6-DATA-ADRESS3.                                                   
060900     05  FILLER      PIC X(25) VALUE '!F T N  440  80  L 4 3 1 '.         
061000     05  FILLER      PIC X(1)  VALUE '"'.                                 
061100     05  A6-ADRESS-3 PIC X(30) VALUE SPACE.                               
061200     05  FILLER      PIC X(2)  VALUE '"Å'.                                
061300                                                                          
061400   03  A6-DATA-ADRESS4.                                                   
061500     05  FILLER      PIC X(25) VALUE '!F T N  500  80  L 4 3 1 '.         
061600     05  FILLER      PIC X(1)  VALUE '"'.                                 
061700     05  A6-ADRESS-4 PIC X(30) VALUE SPACE.                               
061800     05  FILLER      PIC X(2)  VALUE '"Å'.                                
061900                                                                          
062000   03  A6-DATA-ADRESS5.                                                   
062100     05  FILLER      PIC X(25) VALUE '!F T N  560  80  L 4 3 1 '.         
062200     05  FILLER      PIC X(1)  VALUE '"'.                                 
062300     05  A6-ADRESS-5 PIC X(30) VALUE SPACE.                               
062400     05  FILLER      PIC X(2)  VALUE '"Å'.                                
062500                                                                          
062600   03  A6-DATA-BETEXT-S09.                                                
062700     05  FILLER      PIC X(25) VALUE '!F T N  560  80  L 4 3 1 '.         
062800     05  FILLER      PIC X(1)  VALUE '"'.                                 
062900     05  A6-BETEXT-INFO-S09    PIC X(35) VALUE SPACE.                     
063000     05  FILLER      PIC X(2)  VALUE '"Å'.                                
063100                                                                          
063200   03  A6-DATA-BETEXT-S03.                                                
063300     05  FILLER      PIC X(25) VALUE '!F T N  730  80  L 4 3 1 '.         
063400     05  FILLER      PIC X(1)  VALUE '"'.                                 
063500     05  A6-BETEXT-INFO-S03    PIC X(35) VALUE SPACE.                     
063600     05  FILLER      PIC X(2)  VALUE '"Å'.                                
063700                                                                          
063800   03  A6-DATA-WIP.                                                       
063900     05  FILLER      PIC X(25) VALUE '!F T N 1050 1200 L 1 1 6 '.         
064000     05  FILLER      PIC X(1)  VALUE '"'.                                 
064100     05  A6-WIP      PIC X(10).                                           
064200     05  FILLER      PIC X(2)  VALUE '"Å'.                                
064300                                                                          
064400   03  A6-DATA-ST-ADDRESS.                                                
064500     05  FILLER      PIC X(25) VALUE '!F T N 1050 650  L 2 2 6 '.         
064600     05  FILLER      PIC X(1)  VALUE '"'.                                 
064700     05  A6-ADFLGEO  PIC X(3)  VALUE ZERO.                                
064800     05  FILLER                PIC X(1)  VALUE SPACE.                     
064900     05  A6-ADFLOMR  PIC Z(3)  VALUE ZERO.                                
065000     05  FILLER      PIC X(2)  VALUE '"Å'.                                
065100                                                                          
065200   03  A6-DATA-TIRFS.                                                     
065300     05  FILLER      PIC X(25) VALUE '!F T N 1050   80 L 2 2 6 '.         
065400     05  FILLER      PIC X(1)  VALUE '"'.                                 
065500     05  A6-TIRFS    PIC 9(6)  VALUE ZERO.                                
065600     05  FILLER      PIC X(2)  VALUE '"Å'.                                
065700*                                                                         
065800   03  A6-DATA-TIREPDAT-S03.                                              
065900     05  FILLER      PIC X(25) VALUE '!F T N  880  650 L 4 3 3 '.         
066000     05  FILLER      PIC X(1)  VALUE '"'.                                 
066100     05  A6-TIREPDAT PIC 9(6)  VALUE ZERO.                                
066200     05  FILLER      PIC X(2)  VALUE '"Å'.                                
066300                                                                          
066400   03  A6-DATA-TIREPDAT-S09.                                              
066500     05  FILLER      PIC X(25) VALUE '!F T N  730  900 L 4 3 3 '.         
066600     05  FILLER      PIC X(1)  VALUE '"'.                                 
066700     05  A6-TIREPDAT-S09 PIC 9(6)  VALUE ZERO.                            
066800     05  FILLER      PIC X(2)  VALUE '"Å'.                                
066900                                                                          
067000   03  A6-DATA-RFS.                                                       
067100     05  FILLER      PIC X(25) VALUE '!F T N  880  200 L 2 2 6 '.         
067200     05  FILLER      PIC X(1)  VALUE '"'.                                 
067300     05  A6-RFS      PIC 9(6)  VALUE ZERO.                                
067400     05  FILLER      PIC X(2)  VALUE '"Å'.                                
067500                                                                          
067600   03  A6-DATA-ADFLGEO.                                                   
067700     05  FILLER      PIC X(25) VALUE '!F T N 1020 1200 L 2 2 6 '.         
067800     05  FILLER      PIC X(1)  VALUE '"'.                                 
067900     05  A6-FLGEO    PIC X(3)  VALUE ZERO.                                
068000     05  FILLER      PIC X(2)  VALUE '"Å'.                                
068100                                                                          
068200   03  A6-DATA-ADFLOMR.                                                   
068300     05  FILLER      PIC X(25) VALUE '!F T N 1020 1400 L 2 2 6 '.         
068400     05  FILLER      PIC X(1)  VALUE '"'.                                 
068500     05  A6-FLOMR    PIC Z(3)  VALUE ZERO.                                
068600     05  FILLER      PIC X(2)  VALUE '"Å'.                                
068700                                                                          
068800   03  A6-DATA-ADRUTNIV.                                                  
068900     05  FILLER      PIC X(25) VALUE '!F T N  880 1350 L 2 2 6 '.         
069000     05  FILLER      PIC X(1)  VALUE '"'.                                 
069100     05  A6-RUTNIV   PIC Z(3)  VALUE ZERO.                                
069200     05  FILLER      PIC X(2)  VALUE '"Å'.                                
069300                                                                          
069400   03  A6-DATA-WEIGHT.                                                    
069500     05  FILLER      PIC X(25) VALUE '!F T N  590 1600 R 2 2 6 '.         
069600     05  FILLER      PIC X(1)  VALUE '"'.                                 
069700     05  A6-VKORDBTO PIC Z(5)  VALUE ZERO.                                
069800     05  FILLER      PIC X(2)  VALUE '"Å'.                                
069900                                                                          
070000   03  A6-DATA-WEIGHT-KILO-HEKTO.                                         
070100     05  FILLER      PIC X(25) VALUE '!F T N  590 1600 R 2 2 6 '.         
070200     05  FILLER      PIC X(1)  VALUE '"'.                                 
070300     05  A6-KILO     PIC Z(5)  VALUE ZERO.                                
070400     05  A6-PUNKT    PIC X     VALUE '.'.                                 
070500     05  A6-HEKTO    PIC 9     VALUE ZERO.                                
070600     05  FILLER      PIC X(2)  VALUE '"Å'.                                
070700                                                                          
070800   03  A6-TEXT-SDC23-SHIPPER-CDC.                                         
070900     05  FILLER      PIC X(30) VALUE '!F T N 890 230 L 1 1 3 '.           
071000     05  FILLER                  PIC X(1)  VALUE '"'.                     
071100     05  FILLER      PIC X(20) VALUE 'SHIPPER: VOLVO CARS '.              
071200     05  FILLER      PIC X(18) VALUE 'CUSTOMER SERVICE, '.                
071300     05  FILLER      PIC X(21) VALUE 'SE-405 31 GOTHENBURG,'.             
071400     05  FILLER      PIC X(7)  VALUE ' SWEDEN'.                           
071500     05  FILLER                  PIC X(2)  VALUE '"Å'.                    
071600*RUB-DISTRICT                                                             
071700   03  A6-RUB-DISTRICT-NL.                                                
071800     05  FILLER      PIC X(25) VALUE '!F T N   20  250 R 1 1 3 '.         
071900     05  FILLER      PIC X(11) VALUE '"DISTRICT"Å'.                       
072000                                                                          
072100   03  A6-RUB-CUSTOMER-NL.                                                
072200     05  FILLER      PIC X(25) VALUE '!F T N   30 1100 R 1 1 3 '.         
072300     05  FILLER      PIC X(11) VALUE '"CUSTOMER"Å'.                       
072400                                                                          
072500   03  A6-RUB-ORDERNUMBER-NL.                                             
072600     05  FILLER      PIC X(25) VALUE '!F T N   30 1380 L 1 1 3 '.         
072700     05  FILLER      PIC X(15) VALUE '"ORDER NUMBER"Å'.                   
072800                                                                          
072900*RUB-ADDRESS                                                              
073000   03  A6-RUB-ADDRESS-NL.                                                 
073100     05  FILLER      PIC X(25) VALUE '!F T N  240   80 L 1 1 3 '.         
073200     05  FILLER      PIC X(10) VALUE '"ADDRESS"Å'.                        
073300                                                                          
073400   03  A6-RUB-CASE-NL.                                                    
073500     05  FILLER      PIC X(25) VALUE '!F T N  210 1530 L 1 1 3 '.         
073600     05  FILLER      PIC X(7)  VALUE '"CASE"Å'.                           
073700                                                                          
073800   03  A6-RUB-FREIGHTCODE-NL.                                             
073900     05  FILLER      PIC X(25) VALUE '!F T N  370 1380 L 1 1 3 '.         
074000     05  FILLER      PIC X(15) VALUE '"FREIGHT CODE"Å'.                   
074100                                                                          
074200*RUB-WEIGHT-KG                                                            
074300   03  A6-RUB-WEIGHT-KG-NL.                                               
074400     05  FILLER      PIC X(25) VALUE '!F T N  590 1420 L 1 1 3 '.         
074500     05  FILLER      PIC X(12)  VALUE '"WEIGHT KG"Å'.                     
074600                                                                          
074700   03  A6-RUB-IDPRODNR-NL.                                                
074800     05  FILLER      PIC X(25) VALUE '!F T N  750 1270 L 1 1 3 '.         
074900     05  FILLER      PIC X(20) VALUE '"PRODUCTION NUMBER"Å'.              
075000                                                                          
075100   03  A6-RUB-RFS-GBG.                                                    
075200     05  FILLER      PIC X(25) VALUE '!F T N  760 200  L 1 1 3 '.         
075300     05  FILLER      PIC X(6)  VALUE '"RFS"Å'.                            
075400*DATA-IDDISTR                                                             
075500   03  A6-DATA-IDDISTR-SE.                                                
075600     05  FILLER      PIC X(25) VALUE '!F T N  200  600 R 3 3 6 '.         
075700     05  FILLER      PIC X(1)  VALUE '"'.                                 
075800     05  A6-IDDISTR-SE PIC Z(4) VALUE ZERO.                               
075900     05  FILLER      PIC X(2)  VALUE '"Å'.                                
076000*                                                                         
076100   03  A6-DATA-IDDISTR-NL.                                                
076200     05  FILLER      PIC X(25) VALUE '!F T N  200  600 R 2 2 6 '.         
076300     05  FILLER      PIC X(1)  VALUE '"'.                                 
076400     05  A6-IDDISTR-NL PIC Z(4) VALUE ZERO.                               
076500     05  FILLER      PIC X(2)  VALUE '"Å'.                                
076600                                                                          
076700   03  A6-DATA-IDKUNDNR-NL.                                               
076800     05  FILLER      PIC X(25) VALUE '!F T N  160 1100 R 2 2 6 '.         
076900     05  FILLER      PIC X(1)  VALUE '"'.                                 
077000     05  A6-IDKUNDNR-NL  PIC Z(5)9 VALUE ZERO.                            
077100     05  FILLER      PIC X(2)  VALUE '"Å'.                                
077200                                                                          
077300   03  A6-DATA-IDORDNR-NL.                                                
077400     05  FILLER      PIC X(25) VALUE '!F T N  160 1600 R 2 2 6 '.         
077500     05  FILLER      PIC X(1)  VALUE '"'.                                 
077600     05  A6-IDORDNR-NL  PIC Z(4)9 VALUE SPACE.                            
077700     05  FILLER      PIC X(2)  VALUE '"Å'.                                
077800                                                                          
077900*FOR S09-SECTION                                                          
078000   03  A6-DATA-ADRESS1-NL-S09.                                            
078100     05  FILLER      PIC X(25) VALUE '!F T N  300  80  L 6 3 1 '.         
078200     05  FILLER      PIC X(1)  VALUE '"'.                                 
078300     05  A6-ADRESS-1-NL-S09  PIC X(30) VALUE SPACE.                       
078400     05  FILLER      PIC X(2)  VALUE '"Å'.                                
078500                                                                          
078600   03  A6-DATA-ADRESS2-NL-S09.                                            
078700     05  FILLER      PIC X(25) VALUE '!F T N  380  80  L 6 3 1 '.         
078800     05  FILLER      PIC X(1)  VALUE '"'.                                 
078900     05  A6-ADRESS-2-NL-S09 PIC X(30) VALUE SPACE.                        
079000     05  FILLER      PIC X(2)  VALUE '"Å'.                                
079100                                                                          
079200   03  A6-DATA-ADRESS3-NL-S09.                                            
079300     05  FILLER      PIC X(25) VALUE '!F T N  450  80  L 6 3 1 '.         
079400     05  FILLER      PIC X(1)  VALUE '"'.                                 
079500     05  A6-ADRESS-3-NL-S09 PIC X(30) VALUE SPACE.                        
079600     05  FILLER      PIC X(2)  VALUE '"Å'.                                
079700                                                                          
079800   03  A6-DATA-ADRESS4-NL-S09.                                            
079900     05  FILLER      PIC X(25) VALUE '!F T N  520  80  L 6 3 1 '.         
080000     05  FILLER      PIC X(1)  VALUE '"'.                                 
080100     05  A6-ADRESS-4-NL-S09 PIC X(30) VALUE SPACE.                        
080200     05  FILLER      PIC X(2)  VALUE '"Å'.                                
080300                                                                          
080400   03  A6-DATA-ADRESS5-N-S09.                                             
080500     05  FILLER      PIC X(25) VALUE '!F T N  600  80  L 6 3 1 '.         
080600     05  FILLER      PIC X(1)  VALUE '"'.                                 
080700     05  A6-ADRESS-5-N-S09  PIC X(35) VALUE SPACE.                        
080800     05  FILLER      PIC X(2)  VALUE '"Å'.                                
080900                                                                          
081000   03  A6-DATA-ADRESS5-NL-S09.                                            
081100     05  FILLER      PIC X(25) VALUE '!F T N  600  80  L 2 2 6 '.         
081200     05  FILLER      PIC X(1)  VALUE '"'.                                 
081300     05  A6-ADRESS-5-NL-S09 PIC X(30) VALUE SPACE.                        
081400     05  FILLER      PIC X(2)  VALUE '"Å'.                                
081500***                                                                       
081600***                                                                       
081700   03  A6-DATA-ADRESS1-NL.                                                
081800     05  FILLER      PIC X(25) VALUE '!F T N  300  80  L 6 3 1 '.         
081900     05  FILLER      PIC X(1)  VALUE '"'.                                 
082000     05  A6-ADRESS-1-NL PIC X(30) VALUE SPACE.                            
082100     05  FILLER      PIC X(2)  VALUE '"Å'.                                
082200                                                                          
082300   03  A6-DATA-ADRESS2-NL.                                                
082400     05  FILLER      PIC X(25) VALUE '!F T N  380  80  L 6 3 1 '.         
082500     05  FILLER      PIC X(1)  VALUE '"'.                                 
082600     05  A6-ADRESS-2-NL PIC X(30) VALUE SPACE.                            
082700     05  FILLER      PIC X(2)  VALUE '"Å'.                                
082800                                                                          
082900   03  A6-DATA-ADRESS3-NL.                                                
083000     05  FILLER      PIC X(25) VALUE '!F T N  460  80  L 6 3 1 '.         
083100     05  FILLER      PIC X(1)  VALUE '"'.                                 
083200     05  A6-ADRESS-3-NL PIC X(30) VALUE SPACE.                            
083300     05  FILLER      PIC X(2)  VALUE '"Å'.                                
083400                                                                          
083500   03  A6-DATA-ADRESS4-NL.                                                
083600     05  FILLER      PIC X(25) VALUE '!F T N  540  80  L 6 3 1 '.         
083700     05  FILLER      PIC X(1)  VALUE '"'.                                 
083800     05  A6-ADRESS-4-NL PIC X(30) VALUE SPACE.                            
083900     05  FILLER      PIC X(2)  VALUE '"Å'.                                
084000                                                                          
084100   03  A6-DATA-ADRESS5-N.                                                 
084200     05  FILLER      PIC X(25) VALUE '!F T N  680  80  L 6 3 1 '.         
084300     05  FILLER      PIC X(1)  VALUE '"'.                                 
084400     05  A6-ADRESS-5-N  PIC X(35) VALUE SPACE.                            
084500     05  FILLER      PIC X(2)  VALUE '"Å'.                                
084600                                                                          
084700   03  A6-DATA-ADRESS5-NL.                                                
084800     05  FILLER      PIC X(25) VALUE '!F T N  680  80  L 2 2 6 '.         
084900     05  FILLER      PIC X(1)  VALUE '"'.                                 
085000     05  A6-ADRESS-5-NL PIC X(30) VALUE SPACE.                            
085100     05  FILLER      PIC X(2)  VALUE '"Å'.                                
085200*                                                                         
085300*BERADREF                                                                 
085400   03  A6-DATA-BERADREF.                                                  
085500**N  05  FILLER      PIC X(25) VALUE '!F T N  750  80  L 6 3 1 '.         
085600     05  FILLER      PIC X(25) VALUE '!F T N  620  80  L 6 3 1 '.         
085700     05  FILLER      PIC X(1)  VALUE '"'.                                 
085800     05  A6-BERADREF PIC X(10) VALUE SPACE.                               
085900     05  FILLER      PIC X(2)  VALUE '"Å'.                                
086000                                                                          
086100*BEKUNDRF                                                                 
086200   03  A6-DATA-BEKUNDRF.                                                  
086300*    05  FILLER      PIC X(25) VALUE '!F T N  750 750  L 6 3 1 '.         
086400     05  FILLER      PIC X(25) VALUE '!F T N  620 750  L 6 3 1 '.         
086500     05  FILLER      PIC X(1)  VALUE '"'.                                 
086600     05  A6-BEKUNDRF PIC X(15) VALUE SPACE.                               
086700     05  FILLER      PIC X(2)  VALUE '"Å'.                                
086800*                                            X   Y                        
086900   03  A6-DATA-IDBILREG-S03.                                              
087000***  05  FILLER      PIC X(25) VALUE '!F T N 760 800   L 4 3 3 '.         
087100     05  FILLER      PIC X(25) VALUE '!F T N 880 1050  L 4 3 3 '.         
087200     05  FILLER      PIC X(1)  VALUE '"'.                                 
087300     05  A6-IDBILREG-S03       PIC X(10) VALUE SPACE.                     
087400     05  FILLER      PIC X(2)  VALUE '"Å'.                                
087500                                                                          
087600   03  A6-DATA-IDBILREG-S09.                                              
087700     05  FILLER      PIC X(25) VALUE '!F T N  680 800  L 4 3 3 '.         
087800     05  FILLER      PIC X(1)  VALUE '"'.                                 
087900     05  A6-IDBILREG-S09       PIC X(10) VALUE SPACE.                     
088000     05  FILLER      PIC X(2)  VALUE '"Å'.                                
088100                                                                          
088200*POSTEN - WIP-KOD                                                         
088300   03  A6-DATA-ADRESS5-POSTEN.                                            
088400     05  FILLER      PIC X(25) VALUE '!F T N  850 600  L 2 2 6 '.         
088500     05  FILLER      PIC X(1)  VALUE '"'.                                 
088600     05  A6-ADRESS-5-POSTEN    PIC X(30) VALUE SPACE.                     
088700     05  FILLER      PIC X(2)  VALUE '"Å'.                                
088800*                                                 1150                    
088900   03  A6-DATA-KDORDKL.                                                   
089000     05  FILLER      PIC X(25) VALUE '!F T N  500 1200 L 2 2 6 '.         
089100     05  FILLER      PIC X(1)  VALUE '"'.                                 
089200     05  A6-KDORDKL  PIC X     VALUE SPACE.                               
089300     05  FILLER      PIC X     VALUE '/'.                                 
089400     05  FILLER      PIC X(2)  VALUE '"Å'.                                
089500                                                                          
089600   03  A6-DATA-RESTORDER.                                                 
089700     05  FILLER      PIC X(25) VALUE '!F T N  880 1430 L 2 2 6 '.         
089800     05  FILLER      PIC X(1)  VALUE '"'.                                 
089900     05  A6-RESTORDER PIC X(2)  VALUE '  '.                               
090000     05  FILLER      PIC X(2)  VALUE '"Å'.                                
090100                                                                          
090200   03  A6-DATA-IDDEPT-S03.                                                
090300     05  FILLER      PIC X(25) VALUE '!F T N  880 1600 R 2 2 6 '.         
090400     05  FILLER      PIC X(1)  VALUE '"'.                                 
090500     05  A6-IDDEPT-S03   PIC ZZ  VALUE ZERO.                              
090600     05  FILLER      PIC X(2)  VALUE '"Å'.                                
090700                                                                          
090800   03  A6-DATA-IDDEPT.                                                    
090900     05  FILLER      PIC X(25) VALUE '!F T N  850 1600 R 2 2 6 '.         
091000     05  FILLER      PIC X(1)  VALUE '"'.                                 
091100     05  A6-IDDEPT PIC ZZ  VALUE ZERO.                                    
091200     05  FILLER      PIC X(2)  VALUE '"Å'.                                
091300                                                                          
091400*****START* ADDRESS FIELDS FOR SWEDISH DISTRICTS******************        
091500   03  A6-DATA-ADRESS1-SWE.                                               
091600     05  FILLER      PIC X(25) VALUE '!F T N  280  80  L 6 3 1 '.         
091700     05  FILLER      PIC X(1)  VALUE '"'.                                 
091800     05  A6-ADRESS-1-SWE PIC X(30) VALUE SPACE.                           
091900     05  FILLER      PIC X(2)  VALUE '"Å'.                                
092000                                                                          
092100   03  A6-DATA-ADRESS2-SWE.                                               
092200     05  FILLER      PIC X(25) VALUE '!F T N  350  80  L 6 3 1 '.         
092300     05  FILLER      PIC X(1)  VALUE '"'.                                 
092400     05  A6-ADRESS-2-SWE PIC X(30) VALUE SPACE.                           
092500     05  FILLER      PIC X(2)  VALUE '"Å'.                                
092600                                                                          
092700   03  A6-DATA-ADRESS3-SWE.                                               
092800     05  FILLER      PIC X(25) VALUE '!F T N  420  80  L 6 3 1 '.         
092900     05  FILLER      PIC X(1)  VALUE '"'.                                 
093000     05  A6-ADRESS-3-SWE PIC X(30) VALUE SPACE.                           
093100     05  FILLER      PIC X(2)  VALUE '"Å'.                                
093200                                                                          
093300   03  A6-DATA-ADRESS4-SWE.                                               
093400     05  FILLER      PIC X(25) VALUE '!F T N  550  80  L 2 2 6 '.         
093500     05  FILLER      PIC X(1)  VALUE '"'.                                 
093600     05  A6-ADRESS-4-SWE PIC X(30) VALUE SPACE.                           
093700     05  FILLER      PIC X(2)  VALUE '"Å'.                                
093800                                                                          
093900   03  A6-DATA-ADRESS5-SWE.                                               
094000     05  FILLER      PIC X(25) VALUE '!F T N  630  80  L 6 3 1 '.         
094100     05  FILLER      PIC X(1)  VALUE '"'.                                 
094200     05  A6-ADRESS-5-SWE PIC X(30) VALUE SPACE.                           
094300     05  FILLER      PIC X(2)  VALUE '"Å'.                                
094400                                                                          
094500*S09- SECTION                                                             
094600   03  A6-DATA-ADRESS1-SWE-S09.                                           
094700     05  FILLER      PIC X(25) VALUE '!F T N  280  80  L 6 3 1 '.         
094800     05  FILLER      PIC X(1)  VALUE '"'.                                 
094900     05  A6-ADRESS-1-SWE-S09 PIC X(30) VALUE SPACE.                       
095000     05  FILLER      PIC X(2)  VALUE '"Å'.                                
095100                                                                          
095200   03  A6-DATA-ADRESS2-SWE-S09.                                           
095300     05  FILLER      PIC X(25) VALUE '!F T N  350  80  L 6 3 1 '.         
095400     05  FILLER      PIC X(1)  VALUE '"'.                                 
095500     05  A6-ADRESS-2-SWE-S09 PIC X(30) VALUE SPACE.                       
095600     05  FILLER      PIC X(2)  VALUE '"Å'.                                
095700                                                                          
095800   03  A6-DATA-ADRESS3-SWE-S09.                                           
095900     05  FILLER      PIC X(25) VALUE '!F T N  420  80  L 6 3 1 '.         
096000     05  FILLER      PIC X(1)  VALUE '"'.                                 
096100     05  A6-ADRESS-3-SWE-S09 PIC X(30) VALUE SPACE.                       
096200     05  FILLER      PIC X(2)  VALUE '"Å'.                                
096300                                                                          
096400   03  A6-DATA-ADRESS4-SWE-S09.                                           
096500     05  FILLER      PIC X(25) VALUE '!F T N  550  80  L 2 2 6 '.         
096600     05  FILLER      PIC X(1)  VALUE '"'.                                 
096700     05  A6-ADRESS-4-SWE-S09 PIC X(30) VALUE SPACE.                       
096800     05  FILLER      PIC X(2)  VALUE '"Å'.                                
096900                                                                          
097000   03  A6-DATA-ADRESS5-SWE-S09.                                           
097100     05  FILLER      PIC X(25) VALUE '!F T N  630  80  L 6 3 1 '.         
097200     05  FILLER      PIC X(1)  VALUE '"'.                                 
097300     05  A6-ADRESS-5-SWE-S09 PIC X(30) VALUE SPACE.                       
097400     05  FILLER      PIC X(2)  VALUE '"Å'.                                
097500                                                                          
097600   03  A6-DATA-RFS-SE.                                                    
097700     05  FILLER      PIC X(25) VALUE '!F T N  880   80 L 2 2 6 '.         
097800     05  FILLER      PIC X(1)  VALUE '"'.                                 
097900     05  A6-RFS-SE   PIC 9(6)  VALUE ZERO.                                
098000     05  FILLER      PIC X(2)  VALUE '"Å'.                                
098100                                                                          
098200   03  A6-DATA-ADFLGEO-SE.                                                
098300     05  FILLER      PIC X(25) VALUE '!F T N 1040 1620 R 2 2 6 '.         
098400     05  FILLER      PIC X(1)  VALUE '"'.                                 
098500     05  A6-ADFLGEO-SE PIC X(3)  VALUE ZERO.                              
098600     05  FILLER      PIC X(2)  VALUE '"Å'.                                
098700                                                                          
098800   03  A6-DATA-ADFLGEO-FI.                                                
098900     05  FILLER      PIC X(25) VALUE '!F T N 1040 1620 R 2 2 6 '.         
099000     05  FILLER      PIC X(1)  VALUE '"'.                                 
099100     05  A6-ADGEO-FI    PIC X(2)  VALUE 'FI'.                             
099200     05  A6-ADFLGEO-FI  PIC X(3)  VALUE ZERO.                             
099300     05  FILLER      PIC X(2)  VALUE '"Å'.                                
099400                                                                          
099500   03  A6-DATA-ADFLGEO-NO.                                                
099600     05  FILLER      PIC X(25) VALUE '!F T N 1040 1620 R 2 2 6 '.         
099700     05  FILLER      PIC X(1)  VALUE '"'.                                 
099800     05  A6-ADGEO-NO    PIC X(2)  VALUE 'NO'.                             
099900     05  A6-ADFLGEO-NO  PIC X(3)  VALUE ZERO.                             
100000     05  FILLER      PIC X(2)  VALUE '"Å'.                                
100100*                                                                         
100200   03  A6-DATA-ADFLGEO-SE2.                                               
100300     05  FILLER      PIC X(25) VALUE '!F T N  880  820 L 2 2 6 '.         
100400     05  FILLER      PIC X(1)  VALUE '"'.                                 
100500     05  A6-ADFLGEO-SE2 PIC X(3) VALUE ZERO.                              
100600     05  FILLER      PIC X(2)  VALUE '"Å'.                                
100700                                                                          
100800   03  A6-DATA-ADFLOMR-SE.                                                
100900     05  FILLER      PIC X(25) VALUE '!F T N  880 1000 L 2 2 6 '.         
101000     05  FILLER      PIC X(1)  VALUE '"'.                                 
101100     05  A6-ADFLOMR-SE PIC Z(3)  VALUE ZERO.                              
101200     05  FILLER      PIC X(2)  VALUE '"Å'.                                
101300                                                                          
101400   03  A6-DATA-ADRUTNIV-SE.                                               
101500     05  FILLER      PIC X(25) VALUE '!F T N  880 1250 L 2 2 6 '.         
101600     05  FILLER      PIC X(1)  VALUE '"'.                                 
101700     05  A6-ADRUTNIV-SE PIC Z(3)  VALUE ZERO.                             
101800     05  FILLER      PIC X(2)  VALUE '"Å'.                                
101900******************************************************************        
102000                                                                          
102100   03  A6-DATA-IDKOLLI-NL.                                                
102200     05  FILLER      PIC X(25) VALUE '!F T N  330 1600 R 2 2 6 '.         
102300     05  FILLER      PIC X(1)  VALUE '"'.                                 
102400     05  A6-IDKOLLI-NL   PIC Z(5) VALUE ZERO.                             
102500     05  FILLER      PIC X(2)  VALUE '"Å'.                                
102600                                                                          
102700*KDFRAKT-CDC                                                              
102800   03  A6-DATA-KDFRAKT-CDC.                                               
102900     05  FILLER      PIC X(25) VALUE '!F T N  550 1600 R 3 3 6 '.         
103000     05  FILLER      PIC X(1)  VALUE '"'.                                 
103100     05  A6-KDFRAKT-CDC  PIC Z9 VALUE ZERO.                               
103200     05  FILLER      PIC X(2)  VALUE '"Å'.                                
103300                                                                          
103400   03  A6-DATA-KDFRAKT-NL.                                                
103500     05  FILLER      PIC X(25) VALUE '!F T N  550 1600 R 3 3 6 '.         
103600     05  FILLER      PIC X(1)  VALUE '"'.                                 
103700     05  A6-KDFRAKT-NL   PIC Z9 VALUE ZERO.                               
103800     05  FILLER      PIC X(2)  VALUE '"Å'.                                
103900                                                                          
104000   03  A6-DATA-KDFRAKT-ES.                                                
104100     05  FILLER      PIC X(25) VALUE '!F T N  500 1850 R 2 2 6 '.         
104200     05  FILLER      PIC X(1)  VALUE '"'.                                 
104300     05  A6-KDFRAKT-ES   PIC Z9 VALUE ZERO.                               
104400     05  FILLER      PIC X(2)  VALUE '"Å'.                                
104500                                                                          
104600*WEIGHT-NL                                                                
104700   03  A6-DATA-WEIGHT-NL.                                                 
104800     05  FILLER      PIC X(25) VALUE '!F T N  720 1590 R 2 2 6 '.         
104900     05  FILLER      PIC X(1)  VALUE '"'.                                 
105000     05  A6-VKORDBTO-NL  PIC Z(5) VALUE ZERO.                             
105100     05  FILLER      PIC X(2)  VALUE '"Å'.                                
105200                                                                          
105300   03  A6-DATA-WEIGHT-ES.                                                 
105400     05  FILLER      PIC X(25) VALUE '!F T N  680 1840 R 2 2 6 '.         
105500     05  FILLER      PIC X(1)  VALUE '"'.                                 
105600     05  A6-VKORDBTO-ES  PIC Z(5) VALUE ZERO.                             
105700     05  FILLER      PIC X(2)  VALUE '"Å'.                                
105800                                                                          
105900   03  A6-DATA-WEIGHT-KILO-HEKTO-NL.                                      
106000     05  FILLER      PIC X(25) VALUE '!F T N  720 1590 R 2 2 6 '.         
106100     05  FILLER      PIC X(1)  VALUE '"'.                                 
106200     05  A6-KILO-NL     PIC Z(4)9  VALUE ZERO.                            
106300     05  A6-PUNKT-NL    PIC X      VALUE '.'.                             
106400     05  A6-HEKTO-NL    PIC 9      VALUE ZERO.                            
106500     05  FILLER      PIC X(2)  VALUE '"Å'.                                
106600                                                                          
106700   03  A6-DATA-WEIGHT-KILO-HEKTO-ES.                                      
106800     05  FILLER      PIC X(25) VALUE '!F T N  680 1840 R 2 2 6 '.         
106900     05  FILLER      PIC X(1)  VALUE '"'.                                 
107000     05  A6-KILO-ES     PIC Z(4)9  VALUE ZERO.                            
107100     05  A6-PUNKT-ES    PIC X      VALUE '.'.                             
107200     05  A6-HEKTO-ES    PIC 9      VALUE ZERO.                            
107300     05  FILLER      PIC X(2)  VALUE '"Å'.                                
107400                                                                          
107500   03  A6-DATA-IDPRODNR-ES.                                               
107600     05  FILLER      PIC X(25) VALUE '!F T N  880 1840 R 2 2 6 '.         
107700     05  FILLER      PIC X(1)  VALUE '"'.                                 
107800     05  A6-IDPRODNR-ES  PIC Z(7)  VALUE ZERO.                            
107900     05  FILLER      PIC X(2)  VALUE '"Å'.                                
108000                                                                          
108100   03  A6-TEXT-KUND-RAD-REF.                                              
108200     05  FILLER      PIC X(25) VALUE '!F T N  880 1450 R 2 2 6 '.         
108300     05  FILLER      PIC X(1)  VALUE '"'.                                 
108400     05  A6-TXT-KUND-RAD-REF   PIC X(15).                                 
108500     05  FILLER      PIC X(2)  VALUE '"Å'.                                
108600                                                                          
108700   03  A6-TEXT-RETURN.                                                    
108800     05  FILLER      PIC X(25) VALUE '!F T N  880  750 R 2 2 6 '.         
108900     05  FILLER      PIC X(1)  VALUE '"'.                                 
109000     05  FILLER      PIC X(6)  VALUE 'RETURN'.                            
109100     05  FILLER      PIC X(2)  VALUE '"Å'.                                
109200                                                                          
109300   03  A6-BARCODE-ORDNR-LONG.                                             
109400     05  FILLER    PIC X(30) VALUE '!F C N 1040 200 L 140 2 12 '.         
109500     05  FILLER                  PIC X(1)  VALUE '"'.                     
109600     05  A6-DISTR-L              PIC 9(4).                                
109700     05  A6-KUNDNR-L             PIC 9(6).                                
109800     05  A6-ORDNR-L              PIC 9(7).                                
109900     05  A6-KOLLI-L              PIC 9(5).                                
110000     05  FILLER                  PIC X(2)  VALUE '"Å'.                    
110100                                                                          
110200   03  A6-BARCODE-TXT-ORDNR-LONG.                                         
110300     05  FILLER    PIC X(30) VALUE '!F T N 1080 200 L 2 1 13 '.           
110400     05  FILLER                  PIC X(1)  VALUE '"'.                     
110500     05  A6-DISTR-TXT-L          PIC 9(4).                                
110600     05  A6-KUNDNR-TXT-L         PIC 9(6).                                
110700     05  A6-ORDNR-TXT-L          PIC 9(7).                                
110800     05  A6-KOLLI-TXT-L          PIC 9(5).                                
110900     05  FILLER                  PIC X(2)  VALUE '"Å'.                    
111000                                                                          
111100   03  A6-BARCODE-ORDNR-SHORT.                                            
111200     05  FILLER    PIC X(30) VALUE '!F C N 1040 200 L 140 2 12 '.         
111300     05  FILLER                  PIC X(1)  VALUE '"'.                     
111400     05  A6-DISTR-S              PIC 9(4).                                
111500     05  A6-KUNDNR-S             PIC 9(6).                                
111600     05  A6-ORDNR-S              PIC 9(5).                                
111700     05  A6-KOLLI-S              PIC 9(5).                                
111800     05  FILLER                  PIC X(2)  VALUE '"Å'.                    
111900                                                                          
112000   03  A6-BARCODE-TXT-ORDNR-SHORT.                                        
112100     05  FILLER    PIC X(30) VALUE '!F T N 1080 200 L 2 1 13 '.           
112200     05  FILLER                  PIC X(1)  VALUE '"'.                     
112300     05  A6-DISTR-TXT-S          PIC 9(4).                                
112400     05  A6-KUNDNR-TXT-S         PIC 9(6).                                
112500     05  A6-ORDNR-TXT-S          PIC 9(5).                                
112600     05  A6-KOLLI-TXT-S          PIC 9(5).                                
112700     05  FILLER                  PIC X(2)  VALUE '"Å'.                    
112800                                                                          
112900   03  A6-BARCODE-ORDNR-ES.                                               
113000     05  FILLER    PIC X(30) VALUE '!F C N 1040 550 L 140 2 12 '.         
113100     05  FILLER                  PIC X(1)  VALUE '"'.                     
113200     05  A6-DISTR-ES             PIC 9(4).                                
113300     05  A6-KUNDNR-ES            PIC 9(6).                                
113400     05  A6-ORDNR-ES             PIC 9(5).                                
113500     05  A6-KOLLI-ES             PIC 9(5).                                
113600     05  A6-VKORDBTO-ESP         PIC 9(6)V9.                              
113700     05  FILLER                  PIC X(2)  VALUE '"Å'.                    
113800                                                                          
113900                                                                          
114000   03  A6-BARCODE-TXT-ORDNR-ES.                                           
114100     05  FILLER    PIC X(30) VALUE '!F T N 1080 550 L 2 1 13 '.           
114200     05  FILLER                  PIC X(1)  VALUE '"'.                     
114300     05  A6-DISTR-TXT-ES         PIC 9(4).                                
114400     05  A6-KUNDNR-TXT-ES        PIC 9(6).                                
114500     05  A6-ORDNR-TXT-ES         PIC 9(5).                                
114600     05  A6-KOLLI-TXT-ES         PIC 9(5).                                
114700     05  A6-VKORDBTO-TXT-ES      PIC 9(6)V9.                              
114800     05  FILLER                  PIC X(2)  VALUE '"Å'.                    
114900                                                                          
115000   03  A6-BARCODE-ORDNR-SHORT-ENG.                                        
115100     05  FILLER    PIC X(30) VALUE '!F C N 1280 200 L 200 2 12 '.         
115200     05  FILLER                  PIC X(1)  VALUE '"'.                     
115300     05  A6-DISTR-S-ENG          PIC 9(4).                                
115400     05  A6-KUNDNR-S-ENG         PIC 9(6).                                
115500     05  A6-ORDNR-S-ENG          PIC 9(5).                                
115600     05  A6-KOLLI-S-ENG          PIC 9(5).                                
115700     05  FILLER                  PIC X(2)  VALUE '"Å'.                    
115800                                                                          
115900   03  A6-BARCODE-TXT-ORDNR-SHORT-ENG.                                    
116000     05  FILLER    PIC X(30) VALUE '!F T N 1310 200 L 2 1 13 '.           
116100     05  FILLER                  PIC X(1)  VALUE '"'.                     
116200     05  A6-DISTR-TXT-S-ENG      PIC 9(4).                                
116300     05  A6-KUNDNR-TXT-S-ENG     PIC 9(6).                                
116400     05  A6-ORDNR-TXT-S-ENG      PIC 9(5).                                
116500     05  A6-KOLLI-TXT-S-ENG      PIC 9(5).                                
116600     05  FILLER                  PIC X(2)  VALUE '"Å'.                    
116700                                                                          
116800   03  A6-TEXT-SHIPPER-CDC.                                               
116900     05  FILLER      PIC X(30) VALUE '!F T N 1080 480 L 1 1 3 '.          
117000     05  FILLER                  PIC X(1)  VALUE '"'.                     
117100     05  FILLER      PIC X(20) VALUE 'SHIPPER: VOLVO CARS '.              
117200     05  FILLER      PIC X(18) VALUE 'CUSTOMER SERVICE, '.                
117300     05  FILLER      PIC X(21) VALUE 'SE-405 31 GOTHENBURG,'.             
117400     05  FILLER      PIC X(7)  VALUE ' SWEDEN'.                           
117500     05  FILLER                  PIC X(2)  VALUE '"Å'.                    
117600*POSTEN BARCODE--------------------------------Y---X---LEFT-FONT          
117700   03  A6-TEXT-PRODUKT.                                                   
117800     05  FILLER      PIC X(25) VALUE '!F T N  600 560  L 1 1 3 '.         
117900     05  FILLER      PIC X(10)  VALUE '"PRODUKT"Å'.                       
118000                                                                          
118100   03  A6-TEXT-PRODUKT-1090.                                              
118200     05  FILLER      PIC X(25) VALUE '!F T N  540 700  L 1 1 3 '.         
118300     05  FILLER      PIC X(10)  VALUE '"PRODUKT"Å'.                       
118400                                                                          
118500   03  A6-REFILL-Q-TEXT-PRODUKT.                                          
118600     05  FILLER      PIC X(25) VALUE '!F T N  420 430  L 1 1 3 '.         
118700     05  FILLER      PIC X(10)  VALUE '"PRODUKT"Å'.                       
118800                                                                          
118900   03  A6-TEXT-HIT.                                                       
119000     05  FILLER      PIC X(25) VALUE '!F T N  710 590  L 1 1 3 '.         
119100     05  FILLER      PIC X(6)  VALUE '"HIT"Å'.                            
119200                                                                          
119300   03  A6-TEXT-HIT-1090.                                                  
119400     05  FILLER      PIC X(25) VALUE '!F T N 630  750  L 1 1 3 '.         
119500     05  FILLER      PIC X(6)  VALUE '"HIT"Å'.                            
119600                                                                          
119700   03  A6-TEXT-PAK-1090.                                                  
119800     05  FILLER      PIC X(25) VALUE '!F T N 710  700  L 1 1 3 '.         
119900     05  FILLER      PIC X(6)  VALUE '"HIT"Å'.                            
120000                                                                          
120100   03  A6-REFILL-Q-TEXT-HIT.                                              
120200     05  FILLER      PIC X(25) VALUE '!F T N  510 470  L 1 1 3 '.         
120300     05  FILLER      PIC X(6)  VALUE '"HIT"Å'.                            
120400                                                                          
120500   03  A6-TEXT-PAK.                                                       
120600     05  FILLER      PIC X(25) VALUE '!F T N  710 590  L 1 1 3 '.         
120700     05  FILLER      PIC X(6)  VALUE '"PAK"Å'.                            
120800                                                                          
120900   03  A6-REFILL-Q-TEXT-PAK.                                              
121000     05  FILLER      PIC X(25) VALUE '!F T N  510 470  L 1 1 3 '.         
121100     05  FILLER      PIC X(6)  VALUE '"PAK"Å'.                            
121200                                                                          
121300   03  A6-TEXT-48.                                                        
121400     05  FILLER      PIC X(25) VALUE '!F T N  660 600  L 1 1 3 '.         
121500     05  FILLER      PIC X(5)  VALUE '"48"Å'.                             
121600                                                                          
121700   03  A6-REFILL-Q-TEXT-48.                                               
121800     05  FILLER      PIC X(25) VALUE '!F T N  460 470  L 1 1 3 '.         
121900     05  FILLER      PIC X(5)  VALUE '"48"Å'.                             
122000                                                                          
122100   03  A6-TEXT-69.                                                        
122200     05  FILLER      PIC X(25) VALUE '!F T N 660  600  L 1 1 3 '.         
122300     05  FILLER      PIC X(5)  VALUE '"69"Å'.                             
122400                                                                          
122500   03  A6-TEXT-54-1090.                                                   
122600     05  FILLER      PIC X(25) VALUE '!F T N 660  700  L 1 1 3 '.         
122700     05  FILLER      PIC X(5)  VALUE '"54"Å'.                             
122800                                                                          
122900   03  A6-TEXT-48-1090.                                                   
123000     05  FILLER      PIC X(25) VALUE '!F T N 580  750  L 1 1 3 '.         
123100     05  FILLER      PIC X(5)  VALUE '"48"Å'.                             
123200                                                                          
123300   03  A6-REFILL-Q-TEXT-54.                                               
123400     05  FILLER      PIC X(25) VALUE '!F T N  460 470  L 1 1 3 '.         
123500     05  FILLER      PIC X(5)  VALUE '"54"Å'.                             
123600*POSTEN----------------------------------------Y----X--RIGHT---           
123700                                                                          
123800   03  A6-SORTERINGSKOD.                                                  
123900     05  FILLER      PIC X(25) VALUE '!F T N  330 1400 R 2 2 6 '.         
124000     05  FILLER      PIC X(1)  VALUE '"'.                                 
124100     05  A6-ADFLGEO-SORT-SE                                               
124200                     PIC X(2) VALUE 'SE'.                                 
124300     05  A6-ADFLGEO-SORT                                                  
124400                     PIC X(3) VALUE ZERO.                                 
124500     05  FILLER      PIC X(2)  VALUE '"Å'.                                
124600                                                                          
124700   03  A6-TEXT-SORTCODE.                                                  
124800     05  FILLER      PIC X(25) VALUE '!F T N  210 1400 R 1 1 3 '.         
124900     05  FILLER      PIC X(12)  VALUE '"SORT CODE"Å'.                     
125000                                                                          
125100   03  A6-8700-SORTERINGSKOD.                                             
125200     05  FILLER      PIC X(25) VALUE '!F T N 1050 1600 R 2 2 6 '.         
125300     05  FILLER      PIC X(1)  VALUE '"'.                                 
125400     05  A6-8700-ADFLGEO-SORT PIC X(3) VALUE ZERO.                        
125500     05  FILLER      PIC X(2)  VALUE '"Å'.                                
125600                                                                          
125700   03  A6-TEXT-SORTCODE-1090.                                             
125800     05  FILLER      PIC X(25) VALUE '!F T N  910 1600 R 1 1 3 '.         
125900     05  FILLER      PIC X(12)  VALUE '"SORT CODE"Å'.                     
126000                                                                          
126100   03  A6-8700-TEXT-SORTCODE.                                             
126200     05  FILLER      PIC X(25) VALUE '!F T N  910 1600 R 1 1 3 '.         
126300     05  FILLER      PIC X(12)  VALUE '"SORT CODE"Å'.                     
126400                                                                          
126500   03  A6-REFILL-Q-SORTERINGSKOD.                                         
126600     05  FILLER      PIC X(25) VALUE '!F T N  700 1600 R 2 2 6 '.         
126700     05  FILLER      PIC X(1)  VALUE '"'.                                 
126800     05  A6-REFILL-Q-ADFLGEO-SORT PIC X(3) VALUE ZERO.                    
126900     05  FILLER      PIC X(2)  VALUE '"Å'.                                
127000                                                                          
127100   03  A6-REFILL-Q-TEXT-SORTCODE.                                         
127200     05  FILLER      PIC X(25) VALUE '!F T N  580 1430 L 1 1 3 '.         
127300     05  FILLER      PIC X(12)  VALUE '"SORT CODE"Å'.                     
127400*POSTEN                                      Y    X                       
127500   03  A6-BARCODE-POSTEN.                                                 
127600     05  FILLER    PIC X(30) VALUE '!F C N  670  750 L 100 3 41'.         
127700     05  FILLER                  PIC X(1)  VALUE '"'.                     
127800     05  A6-POSTEN1              PIC X(13).                               
127900     05  FILLER                  PIC X(2)  VALUE '"Å'.                    
128000                                                                          
128100*HIT 1090                                                                 
128200   03  A6-BARCODE-POSTEN-1090.                                            
128300     05  FILLER    PIC X(30) VALUE '!F C N  620  850 L 100 3 41'.         
128400     05  FILLER                  PIC X(1)  VALUE '"'.                     
128500     05  A6-POSTEN1-1090         PIC X(13).                               
128600     05  FILLER                  PIC X(2)  VALUE '"Å'.                    
128700                                                                          
128800   03  A6-TEXT-POSTEN.                                                    
128900     05  FILLER    PIC X(30) VALUE '!F T N  710 750  L 1 1 3'.            
129000     05  FILLER                  PIC X(1)  VALUE '"'.                     
129100     05  A6-POSTEN1-TEXT         PIC X(13).                               
129200     05  FILLER                  PIC X(2)  VALUE '"Å'.                    
129300                                                                          
129400   03  A6-TEXT-POSTEN-1090.                                               
129500     05  FILLER    PIC X(30) VALUE '!F T N  650 850  L 1 1 3'.            
129600     05  FILLER                  PIC X(1)  VALUE '"'.                     
129700     05  A6-POSTEN1-TEXT-1090    PIC X(13).                               
129800     05  FILLER                  PIC X(2)  VALUE '"Å'.                    
129900                                                                          
130000   03  A6-REFILL-Q-BARCODE-POSTEN.                                        
130100     05  FILLER    PIC X(30) VALUE '!F C N  510  600 L 100 3 41'.         
130200     05  FILLER                  PIC X(1)  VALUE '"'.                     
130300     05  A6-REFILL-Q-POSTEN1     PIC X(13).                               
130400     05  FILLER                  PIC X(2)  VALUE '"Å'.                    
130500*                                            Y   X                        
130600   03  A6-REFILL-Q-TEXT-POSTEN.                                           
130700     05  FILLER    PIC X(30) VALUE '!F T N  550 600  L 1 1 3'.            
130800     05  FILLER                  PIC X(1)  VALUE '"'.                     
130900     05  A6-REFILL-Q-POSTEN1-TEXT PIC X(13).                              
131000     05  FILLER                  PIC X(2)  VALUE '"Å'.                    
131100                                                                          
131200     EJECT                                                                
131300*****************************************************************         
131400*         AREA MED STYRTECKEN FÖR MARKPOINT TERMO SKRIVARE.     *         
131500*         ANV. FÖR ATT SKRIVA KOLLIFLAGGA I A6-FORMAT I CDC     *         
131600*         A6REFIL = CASE LABEL SIZE A6 REFILL                   *         
131700*****************************************************************         
131800 01  FILLER           PIC X(24)  VALUE 'KOLLI-FLA6 TERMO'.                
131900*    STYRTECKEN ENLIGT MANUAL: MARKPOINT THERMAL PRINTER                  
132000*                              LABELPOINT                                 
132100 01  CASE-LABEL-THERMO-A6REFIL.                                           
132200   03  A6REFIL-RAD   PIC X(132)  VALUE SPACE.                             
132300                                                                          
132400   03  A6REFIL-STYR-01.                                                   
132500     05  FILLER      PIC X(3)  VALUE '!CÅ'.                               
132600                                                                          
132700   03  A6REFIL-STYR-91.                                                   
132800     05  FILLER      PIC X(3)  VALUE '!PÅ'.                               
132900*DISTRICT                                                                 
133000   03  A6REFIL-RUB-DISTRICT.                                              
133100     05  FILLER      PIC X(25) VALUE '!F T N   40 450  R 1 1 3 '.         
133200     05  FILLER      PIC X(11) VALUE '"DISTRICT"Å'.                       
133300                                                                          
133400   03  A6REFIL-RUB-CUSTOMER.                                              
133500     05  FILLER      PIC X(25) VALUE '!F T N   50 1100 R 1 1 3 '.         
133600     05  FILLER      PIC X(11) VALUE '"CUSTOMER"Å'.                       
133700                                                                          
133800   03  A6REFIL-RUB-DEALER.                                                
133900     05  FILLER      PIC X(25) VALUE '!F T N   50 1100 R 1 1 3 '.         
134000     05  FILLER      PIC X(09) VALUE '"DEALER"Å'.                         
134100                                                                          
134200   03  A6REFIL-RUB-RETAILER.                                              
134300     05  FILLER      PIC X(25) VALUE '!F T N   50 1100 R 1 1 3 '.         
134400     05  FILLER      PIC X(11) VALUE '"RETAILER"Å'.                       
134500                                                                          
134600   03  A6REFIL-RUB-ORDER-NUMBER.                                          
134700     05  FILLER      PIC X(25) VALUE '!F T N   50 1600 R 1 1 3 '.         
134800     05  FILLER      PIC X(15) VALUE '"ORDER NUMBER"Å'.                   
134900*ADDRESS                                                                  
135000   03  A6REFIL-RUB-ADDRESS.                                               
135100     05  FILLER      PIC X(25) VALUE '!F T N  230 180  L 1 1 3 '.         
135200     05  FILLER      PIC X(10) VALUE '"ADDRESS"Å'.                        
135300                                                                          
135400   03  A6REFIL-RUB-CASE.                                                  
135500     05  FILLER      PIC X(25) VALUE '!F T N  220 1600 R 1 1 3 '.         
135600     05  FILLER      PIC X(7)  VALUE '"CASE"Å'.                           
135700                                                                          
135800   03  A6REFIL-RUB-FREIGHT-CODE.                                          
135900     05  FILLER      PIC X(25) VALUE '!F T N  380 1600 R 1 1 3 '.         
136000     05  FILLER      PIC X(15) VALUE '"FREIGHT CODE"Å'.                   
136100                                                                          
136200   03  A6REFIL-RUB-FREIGHT-CODE-NDC.                                      
136300     05  FILLER      PIC X(25) VALUE '!F T N  710 1600 R 1 1 3 '.         
136400     05  FILLER      PIC X(15) VALUE '"FREIGHT CODE"Å'.                   
136500                                                                          
136600   03  A6REFIL-RUB-WEIGHT-REFILL.                                         
136700     05  FILLER      PIC X(25) VALUE '!F T N  980 1600 R 1 1 3 '.         
136800     05  FILLER      PIC X(12)  VALUE '"WEIGHT KG"Å'.                     
136900*RFS                                           Y   X                      
137000   03  A6REFIL-RUB-RFS.                                                   
137100     05  FILLER      PIC X(25) VALUE '!F T N  790 180  L 1 1 3 '.         
137200     05  FILLER      PIC X(6)  VALUE '"RFS"Å'.                            
137300                                                                          
137400   03  A6REFIL-RUB-DC-WH-ADDRESS.                                         
137500     05  FILLER      PIC X(25) VALUE '!F T N  650  570 L 1 1 3 '.         
137600     05  FILLER      PIC X(16) VALUE '"DC WH ADDRESS"Å'.                  
137700*                                              Y    X                     
137800   03  A6REFIL-RUB-DC-WH-ADRS-POST.                                       
137900     05  FILLER      PIC X(25) VALUE '!F T N  600  600 L 1 1 3 '.         
138000     05  FILLER      PIC X(16) VALUE '"DC WH ADDRESS"Å'.                  
138100                                                                          
138200   03  A6REFIL-RUB-4-2-DC-WH-ADRS-NDC.                                    
138300     05  FILLER      PIC X(25) VALUE '!F T N  630  750 L 1 1 3 '.         
138400     05  FILLER      PIC X(16) VALUE '"DC WH ADDRESS"Å'.                  
138500                                                                          
138600   03  A6REFIL-RUB-ST-ADDRESS.                                            
138700     05  FILLER      PIC X(25) VALUE '!F T N  790  750 L 1 1 3 '.         
138800     05  FILLER      PIC X(13) VALUE '"ST ADDRESS"Å'.                     
138900                                                                          
139000   03  A6REFIL-RUB-4-3-ST-ADRES-POST.                                     
139100     05  FILLER      PIC X(25) VALUE '!F T N  770 1000 L 1 1 3 '.         
139200     05  FILLER      PIC X(13) VALUE '"ST ADDRESS"Å'.                     
139300                                                                          
139400   03  A6REFIL-RUB-4-3-CARRIER.                                           
139500     05  FILLER      PIC X(25) VALUE '!F T N  770  750 L 1 1 3 '.         
139600     05  FILLER      PIC X(10) VALUE '"CARRIER"Å'.                        
139700*IDDISTR                                      170  600                    
139800   03  A6REFIL-DATA-IDDISTR.                                              
139900     05  FILLER      PIC X(25) VALUE '!F T N  180  670 R 3 3 6 '.         
140000     05  FILLER      PIC X(1)  VALUE '"'.                                 
140100     05  A6REFIL-IDDISTR PIC Z(4) VALUE ZERO.                             
140200     05  FILLER      PIC X(2)  VALUE '"Å'.                                
140300                                                                          
140400   03  A6REFIL-DATA-IDKUNDNR.                                             
140500     05  FILLER      PIC X(25) VALUE '!F T N  170 1100 R 2 2 6 '.         
140600     05  FILLER      PIC X(1)  VALUE '"'.                                 
140700     05  A6REFIL-IDKUNDNR PIC Z(5)9 VALUE ZERO.                           
140800     05  FILLER      PIC X(2)  VALUE '"Å'.                                
140900                                                                          
141000   03  A6REFIL-DATA-IDORDNR.                                              
141100     05  FILLER      PIC X(25) VALUE '!F T N  170 1600 R 2 2 6 '.         
141200     05  FILLER      PIC X(1)  VALUE '"'.                                 
141300     05  A6REFIL-IDORDNR PIC Z(4)9 VALUE SPACE.                           
141400     05  FILLER      PIC X(2)  VALUE '"Å'.                                
141500                                                                          
141600   03  A6REFIL-DATA-ADRESS1.                                              
141700     05  FILLER      PIC X(25) VALUE '!F T N  290 180  L 3 2 3 '.         
141800     05  FILLER      PIC X(1)  VALUE '"'.                                 
141900     05  A6REFIL-ADRESS1 PIC X(30) VALUE SPACE.                           
142000     05  FILLER      PIC X(2)  VALUE '"Å'.                                
142100                                                                          
142200   03  A6REFIL-DATA-ADRESS2.                                              
142300     05  FILLER      PIC X(25) VALUE '!F T N  370 180  L 3 2 3 '.         
142400     05  FILLER      PIC X(1)  VALUE '"'.                                 
142500     05  A6REFIL-ADRESS2 PIC X(30) VALUE SPACE.                           
142600     05  FILLER      PIC X(2)  VALUE '"Å'.                                
142700                                                                          
142800   03  A6REFIL-DATA-ADRESS3.                                              
142900     05  FILLER      PIC X(25) VALUE '!F T N  450 180  L 3 2 3 '.         
143000     05  FILLER      PIC X(1)  VALUE '"'.                                 
143100     05  A6REFIL-ADRESS3 PIC X(30) VALUE SPACE.                           
143200     05  FILLER      PIC X(2)  VALUE '"Å'.                                
143300                                                                          
143400   03  A6REFIL-DATA-ADRESS4.                                              
143500     05  FILLER      PIC X(25) VALUE '!F T N  530 180  L 3 2 3 '.         
143600     05  FILLER      PIC X(1)  VALUE '"'.                                 
143700     05  A6REFIL-ADRESS4 PIC X(30) VALUE SPACE.                           
143800     05  FILLER      PIC X(2)  VALUE '"Å'.                                
143900                                                                          
144000   03  A6REFIL-DATA-IDKOLLI.                                              
144100     05  FILLER      PIC X(25) VALUE '!F T N  340 1600 R 2 2 6 '.         
144200     05  FILLER      PIC X(1)  VALUE '"'.                                 
144300     05  A6REFIL-IDKOLLI PIC Z(5) VALUE ZERO.                             
144400     05  FILLER      PIC X(2)  VALUE '"Å'.                                
144500*                                             220                         
144600   03  A6REFIL-RUB-KDORDKL.                                               
144700     05  FILLER      PIC X(25) VALUE '!F T N  580 1150 R 1 1 3 '.         
144800     05  FILLER      PIC X(14) VALUE '"ORDER CLASS"Å'.                    
144900*                                             220                         
145000   03  A6REFIL-RUB-KDORDKL-NDC.                                           
145100     05  FILLER      PIC X(25) VALUE '!F T N  380 1600 R 1 1 3 '.         
145200     05  FILLER      PIC X(14) VALUE '"ORDER CLASS"Å'.                    
145300*                                                                         
145400*KDORDKL  340                                                             
145500   03  A6REFIL-DATA-KDORDKL.                                              
145600     05  FILLER      PIC X(25) VALUE '!F T N  650 1300 R 2 2 6 '.         
145700     05  FILLER      PIC X(1)  VALUE '"'.                                 
145800     05  A6REFIL-KDORDKL   PIC X(1) VALUE ZERO.                           
145900     05  FILLER      PIC X     VALUE '/'.                                 
146000     05  FILLER      PIC X(2)  VALUE '"Å'.                                
146100*                                                                         
146200*KDORDKL  340                                                             
146300   03  A6REFIL-DATA-KDORDKL-NDC.                                          
146400     05  FILLER      PIC X(25) VALUE '!F T N  490 1600 R 2 2 6 '.         
146500     05  FILLER      PIC X(1)  VALUE '"'.                                 
146600     05  A6REFIL-KDORDKL-NDC   PIC X(1) VALUE ZERO.                       
146700     05  FILLER      PIC X(2)  VALUE '"Å'.                                
146800*KDFRAKT                                                                  
146900   03  A6REFIL-DATA-KDFRAKT.                                              
147000     05  FILLER      PIC X(25) VALUE '!F T N  600 1600 R 4 4 6 '.         
147100     05  FILLER      PIC X(1)  VALUE '"'.                                 
147200     05  A6REFIL-KDFRAKT PIC Z9 VALUE ZERO.                               
147300     05  FILLER      PIC X(2)  VALUE '"Å'.                                
147400                                                                          
147500*KDFRAKT                                                                  
147600   03  A6REFIL-DATA-KDFRAKT-NDC.                                          
147700     05  FILLER      PIC X(25) VALUE '!F T N  930 1600 R 4 4 6 '.         
147800     05  FILLER      PIC X(1)  VALUE '"'.                                 
147900     05  A6REFIL-KDFRAKT-NDC   PIC Z9 VALUE ZERO.                         
148000     05  FILLER      PIC X(2)  VALUE '"Å'.                                
148100                                                                          
148200   03  A6REFIL-DATA-WEIGHT-REFILL.                                        
148300     05  FILLER      PIC X(25) VALUE '!F T N 1090 1600 R 2 2 6 '.         
148400     05  FILLER      PIC X(1)  VALUE '"'.                                 
148500     05  A6REFIL-VKORDBTO-REFILL  PIC Z(5) VALUE ZERO.                    
148600     05  FILLER      PIC X(2)  VALUE '"Å'.                                
148700                                                                          
148800   03  A6REFIL-DATA-KILO-HEKTO-REFILL.                                    
148900     05  FILLER      PIC X(25) VALUE '!F T N 1090 1600 R 2 2 6 '.         
149000     05  FILLER      PIC X(1)  VALUE '"'.                                 
149100     05  A6REFIL-KILO-REFILL     PIC Z(5)   VALUE ZERO.                   
149200     05  A6REFIL-PUNKT-REFILL    PIC X      VALUE '.'.                    
149300     05  A6REFIL-HEKTO-REFILL    PIC 9      VALUE ZERO.                   
149400     05  FILLER      PIC X(2)  VALUE '"Å'.                                
149500*RFS                                                                      
149600   03  FILLER        PIC X(08)  VALUE 'TIRFS  '.                          
149700   03  A6REFIL-DATA-RFS.                                                  
149800     05  FILLER      PIC X(25) VALUE '!F T N  910  180 L 2 2 6 '.         
149900     05  FILLER      PIC X(1)  VALUE '"'.                                 
150000     05  A6REFIL-TIRFS PIC X(6) VALUE ZERO.                               
150100     05  FILLER      PIC X(2)  VALUE '"Å'.                                
150200*                                              Y    X                     
150300   03  A6REFIL-ST-ADRESS.                                                 
150400     05  FILLER      PIC X(25) VALUE '!F T N  910  750 L 2 2 6 '.         
150500     05  FILLER      PIC X(1)  VALUE '"'.                                 
150600     05  A6REFIL-ADFLGEO-REFILL  PIC X(3).                                
150700     05  FILLER      PIC X(1)  VALUE SPACE.                               
150800     05  A6REFIL-ADFLOMR-REFILL  PIC Z(3)   VALUE ZERO.                   
150900     05  FILLER      PIC X(2)  VALUE '"Å'.                                
151000                                                                          
151100   03  A6REFIL-ST-ADRESS-POSTEN.                                          
151200     05  FILLER      PIC X(25) VALUE '!F T N  890 1000 L 2 2 6 '.         
151300     05  FILLER      PIC X(1)  VALUE '"'.                                 
151400     05  A6REFIL-ADFLGEO-REFILL-POST  PIC X(3).                           
151500     05  FILLER      PIC X(1)  VALUE SPACE.                               
151600     05  A6REFIL-ADFLOMR-REFILL-POST  PIC Z(3)   VALUE ZERO.              
151700     05  FILLER      PIC X(2)  VALUE '"Å'.                                
151800                                                                          
151900   03  A6REFIL-FRAKT-TEXT.                                                
152000     05  FILLER      PIC X(25) VALUE '!F T N  890  750 L 2 2 6 '.         
152100     05  FILLER      PIC X(1)  VALUE '"'.                                 
152200     05  A6REFIL-FRAKT-TXT     PIC X(12).                                 
152300     05  FILLER      PIC X(2)  VALUE '"Å'.                                
152400*                                                                         
152500   03  A6REFIL-REFILL-KVLEVART.                                           
152600     05  FILLER      PIC X(25) VALUE '!F T N  720 200 L 3 2 3 '.          
152700     05  FILLER               PIC X(1)  VALUE '"'.                        
152800     05  A6REFIL-KVLEVART     PIC Z(5).                                   
152900     05  FILLER               PIC X(3)  VALUE 'PCS'.                      
153000     05  FILLER               PIC X(2)  VALUE '"Å'.                       
153100                                                                          
153200   03  A6REFIL-REFILL-KVLEVART-NDC.                                       
153300     05  FILLER      PIC X(25) VALUE '!F T N  760 200 L 3 2 3 '.          
153400     05  FILLER               PIC X(1)  VALUE '"'.                        
153500     05  A6REFIL-KVLEVART-NDC PIC Z(5).                                   
153600     05  FILLER               PIC X(3)  VALUE 'PCS'.                      
153700     05  FILLER               PIC X(2)  VALUE '"Å'.                       
153800                                                                          
153900   03  A6REFIL-PART-NUMBER-TXT-NDC.                                       
154000     05  FILLER      PIC X(25) VALUE '!F T N  400 1000 R 1 1 3 '.         
154100     05  FILLER      PIC X(14) VALUE '"PART NUMBER"Å'.                    
154200*                                                                         
154300   03  A6REFIL-PART-NUMBER-TXT.                                           
154400     05  FILLER      PIC X(25) VALUE '!F T N  650 1600 R 1 1 3 '.         
154500     05  FILLER      PIC X(14) VALUE '"PART NUMBER"Å'.                    
154600*                                                                         
154700   03  A6REFIL-REFILL-ARTNR.                                              
154800     05  FILLER      PIC X(25) VALUE '!F T N  740 1600 R 4 3 3 '.         
154900     05  FILLER               PIC X(1)  VALUE '"'.                        
155000     05  A6REFIL-IDARTNR      PIC X(9).                                   
155100     05  FILLER               PIC X(2)  VALUE '"Å'.                       
155200                                                                          
155300   03  A6REFIL-REFILL-ARTNR-NDC.                                          
155400     05  FILLER      PIC X(25) VALUE '!F T N  620 1457 R 4 3 6 '.         
155500     05  FILLER               PIC X(1)  VALUE '"'.                        
155600     05  A6REFIL-IDARTNR-NDC  PIC X(9).                                   
155700     05  FILLER               PIC X(2)  VALUE '"Å'.                       
155800                                                                          
155900   03  A6REFIL-REFILL-Q-NDC.                                              
156000*    05  FILLER      PIC X(25) VALUE '!F T N  740 180 L 3 3 6 '.          
156100     05  FILLER      PIC X(25) VALUE '!F T N  650 180 L 5 5 6 '.          
156200     05  FILLER               PIC X(1)  VALUE '"'.                        
156300     05  FILLER               PIC X(1)  VALUE 'Q'.                        
156400     05  FILLER               PIC X(2)  VALUE '"Å'.                       
156500*                                                                         
156600   03  A6REFIL-REFILL-DC-WH-ADR.                                          
156700     05  FILLER      PIC X(25) VALUE '!F T N  760 400 L 4 3 3 '.          
156800     05  FILLER               PIC X(1)  VALUE '"'.                        
156900     05  A6REFIL-ADLAGOMR     PIC Z(3).                                   
157000     05  A6REFIL-ADGANG       PIC Z(3).                                   
157100     05  FILLER               PIC X(1)  VALUE SPACE.                      
157200     05  A6REFIL-ADPLATS      PIC Z(5).                                   
157300     05  FILLER               PIC X(2)  VALUE '"Å'.                       
157400*                                              Y   X                      
157500   03  A6REFIL-REFIL-DC-WH-ADR-POST.                                      
157600     05  FILLER      PIC X(25) VALUE '!F T N  720 550 L 2 2 6 '.          
157700     05  FILLER               PIC X(1)  VALUE '"'.                        
157800     05  A6REFIL-ADLAGOMR-POST PIC Z(3).                                  
157900     05  FILLER               PIC X(1)  VALUE SPACE.                      
158000     05  A6REFIL-ADGANG-POST  PIC Z(3).                                   
158100     05  FILLER               PIC X(1)  VALUE SPACE.                      
158200     05  A6REFIL-ADPLATS-POST PIC Z(5).                                   
158300     05  FILLER               PIC X(2)  VALUE '"Å'.                       
158400                                                                          
158500   03  A6REFIL-REFILL-DC-WH-ADR-NDC.                                      
158600     05  FILLER      PIC X(25) VALUE '!F T N  760 490 L 2 2 6 '.          
158700     05  FILLER               PIC X(1)  VALUE '"'.                        
158800     05  A6REFIL-ADLAGOMR-NDC PIC Z(3).                                   
158900     05  A6REFIL-ADGANG-NDC   PIC Z(3).                                   
159000     05  FILLER               PIC X(1)  VALUE SPACE.                      
159100     05  A6REFIL-ADPLATS-NDC  PIC Z(5).                                   
159200     05  FILLER               PIC X(2)  VALUE '"Å'.                       
159300                                                                          
159400   03  A6REFIL-BARCODE.                                                   
159500     05  FILLER    PIC X(30) VALUE '!F C N 1060 300 L 140 2 12 '.         
159600     05  FILLER                  PIC X(1)  VALUE '"'.                     
159700     05  A6REFIL-DISTR           PIC 9(4).                                
159800     05  A6REFIL-KUNDNR          PIC 9(6).                                
159900     05  A6REFIL-ORDNR           PIC 9(7).                                
160000     05  A6REFIL-KOLLI           PIC 9(5).                                
160100     05  FILLER                  PIC X(2)  VALUE '"Å'.                    
160200                                                                          
160300   03  A6REFIL-TEXT-BELOW-BARCODE.                                        
160400     05  FILLER      PIC X(30) VALUE '!F T N 1090 300 L 1 1 3 '.          
160500     05  FILLER                  PIC X(1)  VALUE '"'.                     
160600     05  A6REFIL-DIST            PIC 9(4).                                
160700     05  A6REFIL-KUNDN           PIC 9(6).                                
160800     05  A6REFIL-ORDN            PIC 9(7).                                
160900     05  A6REFIL-KOLI            PIC 9(5).                                
161000     05  FILLER                  PIC X(2)  VALUE '"Å'.                    
161100                                                                          
161200   03  A6REFIL-TEXT-SHIPPER-CDC.                                          
161300     05  FILLER      PIC X(30) VALUE '!F T N 1120 300 L 1 1 3 '.          
161400     05  FILLER                  PIC X(1)  VALUE '"'.                     
161500     05  FILLER      PIC X(20) VALUE 'SHIPPER: VOLVO CARS '.              
161600     05  FILLER      PIC X(18) VALUE 'CUSTOMER SERVICE, '.                
161700     05  FILLER      PIC X(21) VALUE 'SE-405 31 GOTHENBURG,'.             
161800     05  FILLER      PIC X(7)  VALUE ' SWEDEN'.                           
161900     05  FILLER                  PIC X(2)  VALUE '"Å'.                    
162000     EJECT                                                                
162100*****************************************************************         
162200*         AREA MED STYRTECKEN FÖR MARKPOINT TERMO SKRIVARE.     *         
162300*         ANV. FÖR ATT SKRIVA KOLLIFLAGGA I A6-FORMAT I CDC     *         
162400*         A6-VLDC = CASE LABEL SIZE A6 VIA LDC                  *         
162500*****************************************************************         
162600 01  FILLER           PIC X(24)  VALUE 'KOLLI-FLA6 TERMO VLDC'.           
162700*    STYRTECKEN ENLIGT MANUAL: MARKPOINT THERMAL PRINTER                  
162800*                              LABELPOINT                                 
162900 01  CASE-LABEL-THERMO-A6-VLDC.                                           
163000   03  A6-VLDC-RAD   PIC X(132)  VALUE SPACE.                             
163100                                                                          
163200   03  A6-VLDC-STYR-01.                                                   
163300     05  FILLER      PIC X(3)  VALUE '!CÅ'.                               
163400                                                                          
163500   03  A6-VLDC-STYR-91.                                                   
163600     05  FILLER      PIC X(3)  VALUE '!PÅ'.                               
163700                                                                          
163800   03  A6-VLDC-RUB-DISTR.                                                 
163900     05  FILLER      PIC X(25) VALUE '!F T N   50 380  R 1 1 3 '.         
164000     05  FILLER      PIC X(11) VALUE '"DISTRICT"Å'.                       
164100                                                                          
164200   03  A6-VLDC-RUB-KUNDNR.                                                
164300     05  FILLER      PIC X(25) VALUE '!F T N   50 810  R 1 1 3 '.         
164400     05  FILLER      PIC X(11) VALUE '"CUSTOMER"Å'.                       
164500                                                                          
164600   03  A6-VLDC-RUB-ORDNR.                                                 
164700     05  FILLER      PIC X(25) VALUE '!F T N   50 1260 R 1 1 3 '.         
164800     05  FILLER      PIC X(15) VALUE '"ORDER NUMBER"Å'.                   
164900                                                                          
165000   03  A6-VLDC-RUB-KOLLI.                                                 
165100     05  FILLER      PIC X(25) VALUE '!F T N   50 1600 R 1 1 3 '.         
165200     05  FILLER      PIC X(7)  VALUE '"CASE"Å'.                           
165300                                                                          
165400   03  A6-VLDC-RUB-ADRESS.                                                
165500     05  FILLER      PIC X(25) VALUE '!F T N  220  80  L 1 1 3 '.         
165600     05  FILLER      PIC X(10) VALUE '"ADDRESS"Å'.                        
165700                                                                          
165800   03  A6-VLDC-RUB-FRAKT.                                                 
165900     05  FILLER      PIC X(25) VALUE '!F T N  260 1600 R 1 1 3 '.         
166000     05  FILLER      PIC X(15) VALUE '"FREIGHT CODE"Å'.                   
166100                                                                          
166200   03  A6-VLDC-RUB-LDC-ADRESS.                                            
166300     05  FILLER      PIC X(25) VALUE '!F T N  460  330 L 1 1 3 '.         
166400     05  FILLER      PIC X(14) VALUE '"LDC ADDRESS"Å'.                    
166500*RUB WEIGHT  480                                                          
166600   03  A6-VLDC-RUB-VIKT.                                                  
166700     05  FILLER      PIC X(25) VALUE '!F T N  530 1600 R 1 1 3 '.         
166800     05  FILLER      PIC X(12)  VALUE '"WEIGHT KG"Å'.                     
166900                                                                          
167000   03  A6-VLDC-RUB-TRANSPORTINFO.                                         
167100     05  FILLER      PIC X(25) VALUE '!F T N  770  80 L 1 1 3 '.          
167200     05  FILLER      PIC X(17) VALUE '"TRANSPORT INFO"Å'.                 
167300                                                                          
167400   03  A6-VLDC-RUB-RFS.                                                   
167500     05  FILLER      PIC X(25) VALUE '!F T N  950   80 L 1 1 3 '.         
167600     05  FILLER      PIC X(6)  VALUE '"RFS"Å'.                            
167700                                                                          
167800   03  A6-VLDC-RUB-ST-ADRESS.                                             
167900     05  FILLER      PIC X(25) VALUE '!F T N  950  650 L 1 1 3 '.         
168000     05  FILLER      PIC X(13) VALUE '"ST ADDRESS"Å'.                     
168100                                                                          
168200   03  A6-VLDC-RUB-WIP.                                                   
168300     05  FILLER      PIC X(25) VALUE '!F T N 1000 1200 L 1 1 3 '.         
168400     05  FILLER      PIC X(9)  VALUE '"W.I.P."Å'.                         
168500*DISTR-SE                                                                 
168600   03  A6-VLDC-DATA-DISTR-SE.                                             
168700     05  FILLER      PIC X(25) VALUE '!F T N  190  380 R 3 3 6 '.         
168800     05  FILLER      PIC X(1)  VALUE '"'.                                 
168900     05  A6-VLDC-IDDISTR-SE    PIC Z(4) VALUE ZERO.                       
169000     05  FILLER      PIC X(2)  VALUE '"Å'.                                
169100                                                                          
169200   03  A6-VLDC-DATA-DISTR.                                                
169300     05  FILLER      PIC X(25) VALUE '!F T N  170  380 R 2 2 6 '.         
169400     05  FILLER      PIC X(1)  VALUE '"'.                                 
169500     05  A6-VLDC-IDDISTR PIC Z(4) VALUE ZERO.                             
169600     05  FILLER      PIC X(2)  VALUE '"Å'.                                
169700                                                                          
169800   03  A6-VLDC-DATA-KUNDNR.                                               
169900     05  FILLER      PIC X(25) VALUE '!F T N  170  810 R 2 2 6 '.         
170000     05  FILLER      PIC X(1)  VALUE '"'.                                 
170100     05  A6-VLDC-IDKUNDNR PIC Z(5)9 VALUE ZERO.                           
170200     05  FILLER      PIC X(2)  VALUE '"Å'.                                
170300                                                                          
170400   03  A6-VLDC-DATA-ORDNR.                                                
170500     05  FILLER      PIC X(25) VALUE '!F T N  170 1260 R 2 2 6 '.         
170600     05  FILLER      PIC X(1)  VALUE '"'.                                 
170700     05  A6-VLDC-IDORDNR PIC Z(4)9 VALUE SPACE.                           
170800     05  FILLER      PIC X(2)  VALUE '"Å'.                                
170900                                                                          
171000   03  A6-VLDC-DATA-KOLLI.                                                
171100     05  FILLER      PIC X(25) VALUE '!F T N  170 1600 R 2 2 6 '.         
171200     05  FILLER      PIC X(1)  VALUE '"'.                                 
171300     05  A6-VLDC-IDKOLLI PIC Z(5) VALUE ZERO.                             
171400     05  FILLER      PIC X(2)  VALUE '"Å'.                                
171500                                                                          
171600   03  A6-VLDC-DATA-ADRESS1.                                              
171700     05  FILLER      PIC X(25) VALUE '!F T N  270  80  L 4 3 1 '.         
171800     05  FILLER      PIC X(1)  VALUE '"'.                                 
171900     05  A6-VLDC-ADRESS1 PIC X(30) VALUE SPACE.                           
172000     05  FILLER      PIC X(2)  VALUE '"Å'.                                
172100                                                                          
172200   03  A6-VLDC-DATA-ADRESS2.                                              
172300     05  FILLER      PIC X(25) VALUE '!F T N  310  80  L 3 3 7 '.         
172400     05  FILLER      PIC X(1)  VALUE '"'.                                 
172500     05  A6-VLDC-ADRESS2 PIC X(30) VALUE SPACE.                           
172600     05  FILLER      PIC X(2)  VALUE '"Å'.                                
172700                                                                          
172800   03  A6-VLDC-DATA-ADRESS3.                                              
172900     05  FILLER      PIC X(25) VALUE '!F T N  360  80  L 4 3 1 '.         
173000     05  FILLER      PIC X(1)  VALUE '"'.                                 
173100     05  A6-VLDC-ADRESS3 PIC X(30) VALUE SPACE.                           
173200     05  FILLER      PIC X(2)  VALUE '"Å'.                                
173300                                                                          
173400   03  A6-VLDC-DATA-ADRESS4.                                              
173500     05  FILLER      PIC X(25) VALUE '!F T N  410  80  L 4 3 1 '.         
173600     05  FILLER      PIC X(1)  VALUE '"'.                                 
173700     05  A6-VLDC-ADRESS4 PIC X(30) VALUE SPACE.                           
173800     05  FILLER      PIC X(2)  VALUE '"Å'.                                
173900*KDFRAKT-SE                                                               
174000   03  A6-VLDC-DATA-FRAKT-SE.                                             
174100     05  FILLER      PIC X(25) VALUE '!F T N  440 1600 R 3 3 6 '.         
174200     05  FILLER      PIC X(1)  VALUE '"'.                                 
174300     05  A6-VLDC-KDFRAKT-SE    PIC Z9 VALUE ZERO.                         
174400     05  FILLER      PIC X(2)  VALUE '"Å'.                                
174500                                                                          
174600   03  A6-VLDC-DATA-FRAKT.                                                
174700     05  FILLER      PIC X(25) VALUE '!F T N  380 1600 R 3 3 6 '.         
174800     05  FILLER      PIC X(1)  VALUE '"'.                                 
174900     05  A6-VLDC-KDFRAKT PIC Z9 VALUE ZERO.                               
175000     05  FILLER      PIC X(2)  VALUE '"Å'.                                
175100                                                                          
175200   03  A6-VLDC-VIA.                                                       
175300     05  FILLER      PIC X(25) VALUE '!F T N  620  80 L 2 2 6 '.          
175400     05  FILLER      PIC X(1)  VALUE '"'.                                 
175500     05  FILLER      PIC X(3)  VALUE 'VIA'.                               
175600     05  FILLER      PIC X(2)  VALUE '"Å'.                                
175700                                                                          
175800   03  A6-VLDC-DATA-LDC-ADRESS1.                                          
175900     05  FILLER      PIC X(25) VALUE '!F T N  510 330  L 4 3 1 '.         
176000     05  FILLER      PIC X(1)  VALUE '"'.                                 
176100     05  A6-VLDC-LDC-ADRESS1   PIC X(30) VALUE SPACE.                     
176200     05  FILLER      PIC X(2)  VALUE '"Å'.                                
176300                                                                          
176400   03  A6-VLDC-DATA-LDC-ADRESS2.                                          
176500     05  FILLER      PIC X(25) VALUE '!F T N  550 330  L 3 3 7 '.         
176600     05  FILLER      PIC X(1)  VALUE '"'.                                 
176700     05  A6-VLDC-LDC-ADRESS2   PIC X(30) VALUE SPACE.                     
176800     05  FILLER      PIC X(2)  VALUE '"Å'.                                
176900                                                                          
177000   03  A6-VLDC-DATA-LDC-ADRESS3.                                          
177100     05  FILLER      PIC X(25) VALUE '!F T N  600 330  L 4 3 1 '.         
177200     05  FILLER      PIC X(1)  VALUE '"'.                                 
177300     05  A6-VLDC-LDC-ADRESS3   PIC X(30) VALUE SPACE.                     
177400     05  FILLER      PIC X(2)  VALUE '"Å'.                                
177500                                                                          
177600   03  A6-VLDC-DATA-LDC-ADRESS4.                                          
177700     05  FILLER      PIC X(25) VALUE '!F T N  650 330  L 4 3 1 '.         
177800     05  FILLER      PIC X(1)  VALUE '"'.                                 
177900     05  A6-VLDC-LDC-ADRESS4   PIC X(30) VALUE SPACE.                     
178000     05  FILLER      PIC X(2)  VALUE '"Å'.                                
178100                                                                          
178200   03  A6-VLDC-DATA-LDC-ADRESS5.                                          
178300     05  FILLER      PIC X(25) VALUE '!F T N  700 330  L 4 3 1 '.         
178400     05  FILLER      PIC X(1)  VALUE '"'.                                 
178500     05  A6-VLDC-LDC-ADRESS5   PIC X(30) VALUE SPACE.                     
178600     05  FILLER      PIC X(2)  VALUE '"Å'.                                
178700                                                                          
178800   03  A6-VLDC-DATA-VIKT.                                                 
178900     05  FILLER      PIC X(25) VALUE '!F T N  650 1600 R 2 2 6 '.         
179000     05  FILLER      PIC X(1)  VALUE '"'.                                 
179100     05  A6-VLDC-VKORDBTO      PIC Z(5) VALUE ZERO.                       
179200     05  FILLER      PIC X(2)  VALUE '"Å'.                                
179300                                                                          
179400   03  A6-VLDC-DATA-KILO-HEKTO.                                           
179500     05  FILLER      PIC X(25) VALUE '!F T N  650 1600 R 2 2 6 '.         
179600     05  FILLER      PIC X(1)  VALUE '"'.                                 
179700     05  A6-VLDC-KILO          PIC Z(5)   VALUE ZERO.                     
179800     05  A6-VLDC-PUNKT         PIC X      VALUE '.'.                      
179900     05  A6-VLDC-HEKTO         PIC 9      VALUE ZERO.                     
180000     05  FILLER      PIC X(2)  VALUE '"Å'.                                
180100                                                                          
180200   03  A6-VLDC-DATA-TRPINFO.                                              
180300     05  FILLER       PIC X(30) VALUE '!F T N  890  80 L 2 2 6 '.         
180400     05  FILLER               PIC X(1)  VALUE '"'.                        
180500     05  A6-VLDC-IDZON        PIC X(1).                                   
180600     05  FILLER               PIC X(1)  VALUE SPACE.                      
180700     05  A6-VLDC-IDDEPOT      PIC X(2).                                   
180800     05  FILLER               PIC X(1)  VALUE SPACE.                      
180900     05  A6-VLDC-IDROUTE      PIC X.                                      
181000     05  FILLER               PIC X(2)  VALUE '"Å'.                       
181100                                                                          
181200   03  A6-VLDC-DATA-RFS.                                                  
181300     05  FILLER      PIC X(25) VALUE '!F T N 1070   80 L 2 2 6 '.         
181400     05  FILLER      PIC X(1)  VALUE '"'.                                 
181500     05  A6-VLDC-TIRFS PIC 9(6) VALUE ZERO.                               
181600     05  FILLER      PIC X(2)  VALUE '"Å'.                                
181700                                                                          
181800   03  A6-VLDC-DATA-ST-ADRESS.                                            
181900     05  FILLER      PIC X(25) VALUE '!F T N 1070  650 L 2 2 6 '.         
182000     05  FILLER      PIC X(1)  VALUE '"'.                                 
182100     05  A6-VLDC-ADFLGEO       PIC X(3).                                  
182200     05  FILLER      PIC X(1)  VALUE SPACE.                               
182300     05  A6-VLDC-ADFLOMR       PIC Z(3)   VALUE ZERO.                     
182400     05  FILLER      PIC X(2)  VALUE '"Å'.                                
182500                                                                          
182600   03  A6-VLDC-DATA-WIP.                                                  
182700     05  FILLER      PIC X(25) VALUE '!F T N 1070 1200 L 1 1 6 '.         
182800     05  FILLER      PIC X(1)  VALUE '"'.                                 
182900     05  A6-VLDC-WIP           PIC X(10).                                 
183000     05  FILLER      PIC X(2)  VALUE '"Å'.                                
183100                                                                          
183200   03  A6-VLDC-TEXT-SHIPPER-CDC.                                          
183300     05  FILLER      PIC X(30) VALUE '!F T N 1120  200 L 1 1 3 '.         
183400     05  FILLER      PIC X(1)  VALUE '"'.                                 
183500     05  FILLER      PIC X(20) VALUE 'SHIPPER: VOLVO CARS '.              
183600     05  FILLER      PIC X(18) VALUE 'CUSTOMER SERVICE, '.                
183700     05  FILLER      PIC X(21) VALUE 'SE-405 31 GOTHENBURG,'.             
183800     05  FILLER      PIC X(7)  VALUE ' SWEDEN'.                           
183900     05  FILLER      PIC X(2)  VALUE '"Å'.                                
184000                                                                          
184100     EJECT                                                                
184200*****************************************************************         
184300*NOVA     AREA MED STYRTECKEN FÖR NOVA TERMO SKRIVARE.          *         
184400*         ANV. FÖR ATT SKRIVA KOLLIFLAGGA I A6-FORMAT I CDC     *         
184500*         NOVA-VLDC = CASE LABEL SIZE A6 VIA LDC                *         
184600*****************************************************************         
184700 01  FILLER          PIC X(24)  VALUE 'KOLLI-FLNOVA TERMO VLDC'.          
184800*    STYRTECKEN ENLIGT MANUAL: IMAJE/NOVA THERMAL PRINTER                 
184900*                              LABELPOINT                                 
185000 01  CASE-LABEL-THERMO-NOVA-VLDC.                                         
185100   03  NOVA-VLDC-RAD PIC X(132)  VALUE SPACE.                             
185200                                                                          
185300   03  NOVA-VLDC-STYR-01.                                                 
185400     05  FILLER      PIC X(3)  VALUE '!CÅ'.                               
185500                                                                          
185600   03  NOVA-VLDC-STYR-42.                                                 
185700     05  FILLER      PIC X(6)  VALUE '!Y42 0'.                            
185800                                                                          
185900   03  NOVA-VLDC-STYR-91.                                                 
186000     05  FILLER      PIC X(3)  VALUE '!PÅ'.                               
186100*BOOKMARK                                                                 
186200   03  NOVA-VLDC-RUB-DISTR.                                               
186300     05  FILLER      PIC X(25) VALUE '!F T W  50 1800  R 1 1 3 '.         
186400     05  FILLER      PIC X(11) VALUE '"DISTRICT"Å'.                       
186500*                                                                         
186600   03  NOVA-VLDC-RUB-KUNDNR.                                              
186700     05  FILLER      PIC X(25) VALUE '!F T W  50 700  R 1 1 3 '.          
186800     05  FILLER      PIC X(11) VALUE '"CUSTOMER"Å'.                       
186900                                                                          
187000   03  NOVA-VLDC-RUB-ORDNR.                                               
187100     05  FILLER      PIC X(25) VALUE '!F T W  50   50 R 1 1 3 '.          
187200     05  FILLER      PIC X(15) VALUE '"ORDER NUMBER"Å'.                   
187300                                                                          
187400   03  NOVA-VLDC-RUB-KOLLI.                                               
187500     05  FILLER      PIC X(25) VALUE '!F T W  240  50  R 1 1 3 '.         
187600     05  FILLER      PIC X(7)  VALUE '"CASE"Å'.                           
187700                                                                          
187800   03  NOVA-VLDC-RUB-ADRESS.                                              
187900     05  FILLER      PIC X(25) VALUE '!F T W 300 1950  L 1 1 3 '.         
188000     05  FILLER      PIC X(10) VALUE '"ADDRESS"Å'.                        
188100                                                                          
188200   03  NOVA-VLDC-RUB-FRAKT.                                               
188300     05  FILLER      PIC X(25) VALUE '!F T W  460  50  R 1 1 3 '.         
188400     05  FILLER      PIC X(15) VALUE '"FREIGHT CODE"Å'.                   
188500                                                                          
188600   03  NOVA-VLDC-RUB-LDC-ADRESS.                                          
188700     05  FILLER      PIC X(25) VALUE '!F T W  780 1950 L 1 1 3 '.         
188800     05  FILLER      PIC X(14) VALUE '"LDC ADDRESS"Å'.                    
188900                                                                          
189000   03  NOVA-VLDC-RUB-VIKT.                                                
189100     05  FILLER      PIC X(25) VALUE '!F T W  730  50  R 1 1 3 '.         
189200     05  FILLER      PIC X(12) VALUE '"WEIGHT KG"Å'.                      
189300*                                                                         
189400   03  NOVA-VLDC-RUB-TRANSPORTINFO.                                       
189500     05  FILLER      PIC X(25) VALUE '!F T W 1180 1950 L 1 1 3'.          
189600     05  FILLER      PIC X(17) VALUE '"TRANSPORT INFO"Å'.                 
189700                                                                          
189800   03  NOVA-VLDC-RUB-RFS.                                                 
189900     05  FILLER      PIC X(25) VALUE '!F T W 1420 1950 L 1 1 3 '.         
190000     05  FILLER      PIC X(6)  VALUE '"RFS"Å'.                            
190100                                                                          
190200   03  NOVA-VLDC-RUB-ST-ADRESS.                                           
190300     05  FILLER      PIC X(25) VALUE '!F T W 1400 1200 L 1 1 3 '.         
190400     05  FILLER      PIC X(13) VALUE '"ST ADDRESS"Å'.                     
190500                                                                          
190600   03  NOVA-VLDC-RUB-WIP.                                                 
190700     05  FILLER      PIC X(25) VALUE '!F T W 1400  300 L 1 1 3 '.         
190800     05  FILLER      PIC X(9)  VALUE '"W.I.P."Å'.                         
190900*IDDISTR-SE                                                               
191000   03  NOVA-VLDC-DATA-DISTR-SE.                                           
191100     05  FILLER      PIC X(25) VALUE '!F T W  220 1950 L 4 4 6 '.         
191200     05  FILLER      PIC X(1)  VALUE '"'.                                 
191300     05  NOVA-VLDC-IDDISTR-SE  PIC Z(4) VALUE ZERO.                       
191400     05  FILLER      PIC X(2)  VALUE '"Å'.                                
191500                                                                          
191600   03  NOVA-VLDC-DATA-DISTR.                                              
191700     05  FILLER      PIC X(25) VALUE '!F T W  220 1950 L 3 3 6 '.         
191800     05  FILLER      PIC X(1)  VALUE '"'.                                 
191900     05  NOVA-VLDC-IDDISTR PIC Z(4) VALUE ZERO.                           
192000     05  FILLER      PIC X(2)  VALUE '"Å'.                                
192100*IDKUNDNR                                                                 
192200   03  NOVA-VLDC-DATA-KUNDNR.                                             
192300     05  FILLER      PIC X(25) VALUE '!F T W  220  700 R 3 3 6 '.         
192400     05  FILLER      PIC X(1)  VALUE '"'.                                 
192500     05  NOVA-VLDC-IDKUNDNR PIC Z(5)9 VALUE ZERO.                         
192600     05  FILLER      PIC X(2)  VALUE '"Å'.                                
192700                                                                          
192800   03  NOVA-VLDC-DATA-ORDNR.                                              
192900     05  FILLER      PIC X(25) VALUE '!F T W  220   20 R 3 3 6 '.         
193000     05  FILLER      PIC X(1)  VALUE '"'.                                 
193100     05  NOVA-VLDC-IDORDNR PIC Z(4)9 VALUE SPACE.                         
193200     05  FILLER      PIC X(2)  VALUE '"Å'.                                
193300                                                                          
193400   03  NOVA-VLDC-DATA-KOLLI.                                              
193500     05  FILLER      PIC X(25) VALUE '!F T W  420   20 R 3 3 6 '.         
193600     05  FILLER      PIC X(1)  VALUE '"'.                                 
193700     05  NOVA-VLDC-IDKOLLI PIC Z(5) VALUE ZERO.                           
193800     05  FILLER      PIC X(2)  VALUE '"Å'.                                
193900                                                                          
194000   03  NOVA-VLDC-DATA-ADRESS1.                                            
194100     05  FILLER      PIC X(25) VALUE '!F T W  380 1950 L 4 3 3 '.         
194200     05  FILLER      PIC X(1)  VALUE '"'.                                 
194300     05  NOVA-VLDC-ADRESS1 PIC X(30) VALUE SPACE.                         
194400     05  FILLER      PIC X(2)  VALUE '"Å'.                                
194500                                                                          
194600   03  NOVA-VLDC-DATA-ADRESS2.                                            
194700     05  FILLER      PIC X(25) VALUE '!F T W  480 1950 L 4 3 3 '.         
194800     05  FILLER      PIC X(1)  VALUE '"'.                                 
194900     05  NOVA-VLDC-ADRESS2 PIC X(30) VALUE SPACE.                         
195000     05  FILLER      PIC X(2)  VALUE '"Å'.                                
195100                                                                          
195200   03  NOVA-VLDC-DATA-ADRESS3.                                            
195300     05  FILLER      PIC X(25) VALUE '!F T W  580 1950 L 4 3 3 '.         
195400     05  FILLER      PIC X(1)  VALUE '"'.                                 
195500     05  NOVA-VLDC-ADRESS3 PIC X(30) VALUE SPACE.                         
195600     05  FILLER      PIC X(2)  VALUE '"Å'.                                
195700                                                                          
195800   03  NOVA-VLDC-DATA-ADRESS4.                                            
195900     05  FILLER      PIC X(25) VALUE '!F T W  680 1950 L 4 3 3 '.         
196000     05  FILLER      PIC X(1)  VALUE '"'.                                 
196100     05  NOVA-VLDC-ADRESS4 PIC X(30) VALUE SPACE.                         
196200     05  FILLER      PIC X(2)  VALUE '"Å'.                                
196300                                                                          
196400*KDFRAKT-SE                                                               
196500   03  NOVA-VLDC-DATA-FRAKT-SE.                                           
196600     05  FILLER      PIC X(25) VALUE '!F T W  700  20  R 4 4 6 '.         
196700     05  FILLER      PIC X(1)  VALUE '"'.                                 
196800     05  NOVA-VLDC-KDFRAKT-SE  PIC Z9 VALUE ZERO.                         
196900     05  FILLER      PIC X(2)  VALUE '"Å'.                                
197000                                                                          
197100   03  NOVA-VLDC-DATA-FRAKT.                                              
197200     05  FILLER      PIC X(25) VALUE '!F T W  700  20  R 3 3 6 '.         
197300     05  FILLER      PIC X(1)  VALUE '"'.                                 
197400     05  NOVA-VLDC-KDFRAKT PIC Z9 VALUE ZERO.                             
197500     05  FILLER      PIC X(2)  VALUE '"Å'.                                
197600*KVAR START                                                               
197700   03  NOVA-VLDC-VIA.                                                     
197800     05  FILLER      PIC X(25) VALUE '!F T W  960 1950 L 3 3 6'.          
197900     05  FILLER      PIC X(1)  VALUE '"'.                                 
198000     05  FILLER      PIC X(3)  VALUE 'VIA'.                               
198100     05  FILLER      PIC X(2)  VALUE '"Å'.                                
198200                                                                          
198300   03  NOVA-VLDC-DATA-LDC-ADRESS1.                                        
198400     05  FILLER      PIC X(25) VALUE '!F T W  790 1500 L 4 3 3 '.         
198500     05  FILLER      PIC X(1)  VALUE '"'.                                 
198600     05  NOVA-VLDC-LDC-ADRESS1 PIC X(30) VALUE SPACE.                     
198700     05  FILLER      PIC X(2)  VALUE '"Å'.                                
198800                                                                          
198900   03  NOVA-VLDC-DATA-LDC-ADRESS2.                                        
199000     05  FILLER      PIC X(25) VALUE '!F T W  880 1500 L 4 3 3 '.         
199100     05  FILLER      PIC X(1)  VALUE '"'.                                 
199200     05  NOVA-VLDC-LDC-ADRESS2 PIC X(30) VALUE SPACE.                     
199300     05  FILLER      PIC X(2)  VALUE '"Å'.                                
199400                                                                          
199500   03  NOVA-VLDC-DATA-LDC-ADRESS3.                                        
199600     05  FILLER      PIC X(25) VALUE '!F T W  970 1500 L 4 3 3 '.         
199700     05  FILLER      PIC X(1)  VALUE '"'.                                 
199800     05  NOVA-VLDC-LDC-ADRESS3 PIC X(30) VALUE SPACE.                     
199900     05  FILLER      PIC X(2)  VALUE '"Å'.                                
200000                                                                          
200100   03  NOVA-VLDC-DATA-LDC-ADRESS4.                                        
200200     05  FILLER      PIC X(25) VALUE '!F T W 1060 1500 L 4 3 3 '.         
200300     05  FILLER      PIC X(1)  VALUE '"'.                                 
200400     05  NOVA-VLDC-LDC-ADRESS4 PIC X(30) VALUE SPACE.                     
200500     05  FILLER      PIC X(2)  VALUE '"Å'.                                
200600                                                                          
200700   03  NOVA-VLDC-DATA-LDC-ADRESS5.                                        
200800     05  FILLER      PIC X(25) VALUE '!F T W 1150 1500 L 4 3 3 '.         
200900     05  FILLER      PIC X(1)  VALUE '"'.                                 
201000     05  NOVA-VLDC-LDC-ADRESS5 PIC X(30) VALUE SPACE.                     
201100     05  FILLER      PIC X(2)  VALUE '"Å'.                                
201200                                                                          
201300   03  NOVA-VLDC-DATA-VIKT.                                               
201400     05  FILLER      PIC X(25) VALUE '!F T W  920   20 R 3 3 6 '.         
201500     05  FILLER      PIC X(1)  VALUE '"'.                                 
201600     05  NOVA-VLDC-VKORDBTO    PIC Z(5) VALUE ZERO.                       
201700     05  FILLER      PIC X(2)  VALUE '"Å'.                                
201800                                                                          
201900   03  NOVA-VLDC-DATA-KILO-HEKTO.                                         
202000     05  FILLER      PIC X(25) VALUE '!F T W   920  20 R 3 3 6 '.         
202100     05  FILLER      PIC X(1)  VALUE '"'.                                 
202200     05  NOVA-VLDC-KILO        PIC Z(5)   VALUE ZERO.                     
202300     05  NOVA-VLDC-PUNKT       PIC X      VALUE '.'.                      
202400     05  NOVA-VLDC-HEKTO       PIC 9      VALUE ZERO.                     
202500     05  FILLER      PIC X(2)  VALUE '"Å'.                                
202600                                                                          
202700   03  NOVA-VLDC-DATA-TRPINFO.                                            
202800     05  FILLER       PIC X(30) VALUE '!F T W 1360 1950 L 3 3 6'.         
202900     05  FILLER               PIC X(1)  VALUE '"'.                        
203000     05  NOVA-VLDC-IDZON      PIC X(1).                                   
203100     05  FILLER               PIC X(1)  VALUE SPACE.                      
203200     05  NOVA-VLDC-IDDEPOT    PIC X(2).                                   
203300     05  FILLER               PIC X(1)  VALUE SPACE.                      
203400     05  NOVA-VLDC-IDROUTE    PIC X.                                      
203500     05  FILLER               PIC X(2)  VALUE '"Å'.                       
203600                                                                          
203700   03  NOVA-VLDC-DATA-RFS.                                                
203800     05  FILLER      PIC X(25) VALUE '!F T W 1600 1950 L 3 3 6 '.         
203900     05  FILLER      PIC X(1)  VALUE '"'.                                 
204000     05  NOVA-VLDC-TIRFS PIC 9(6) VALUE ZERO.                             
204100     05  FILLER      PIC X(2)  VALUE '"Å'.                                
204200                                                                          
204300   03  NOVA-VLDC-DATA-ST-ADRESS.                                          
204400     05  FILLER      PIC X(25) VALUE '!F T W 1570 1200 L 3 3 6 '.         
204500     05  FILLER      PIC X(1)  VALUE '"'.                                 
204600     05  NOVA-VLDC-ADFLGEO     PIC X(3).                                  
204700     05  FILLER      PIC X(1)  VALUE SPACE.                               
204800     05  NOVA-VLDC-ADFLOMR     PIC Z(3)   VALUE ZERO.                     
204900     05  FILLER      PIC X(2)  VALUE '"Å'.                                
205000                                                                          
205100   03  NOVA-VLDC-DATA-WIP.                                                
205200     05  FILLER      PIC X(25) VALUE '!F T W 1510  40  R 1 1 6 '.         
205300     05  FILLER      PIC X(1)  VALUE '"'.                                 
205400     05  NOVA-VLDC-WIP         PIC X(10).                                 
205500     05  FILLER      PIC X(2)  VALUE '"Å'.                                
205600                                                                          
205700   03  NOVA-VLDC-DATA-BILREG.                                             
205800     05  FILLER      PIC X(25) VALUE '!F T W 1200 150  R 1 1 6 '.         
205900     05  FILLER      PIC X(1)  VALUE '"'.                                 
206000     05  NOVA-VLDC-IDBILREG    PIC X(10).                                 
206100     05  FILLER      PIC X(2)  VALUE '"Å'.                                
206200*NOT UTILIZED                                                             
206300   03  NOVA-VLDC-BARCODE.                                                 
206400     05  FILLER    PIC X(30) VALUE '!F C W 1400 200 L 170 2 12 '.         
206500     05  FILLER                  PIC X(1)  VALUE '"'.                     
206600     05  NOVA-VLDC-DISTR         PIC 9(4).                                
206700     05  NOVA-VLDC-KUNDNR        PIC 9(6).                                
206800     05  NOVA-VLDC-ORDNR         PIC 9(7).                                
206900     05  NOVA-VLDC-KOLLI         PIC 9(5).                                
207000     05  FILLER                  PIC X(2)  VALUE '"Å'.                    
207100*NOT UTILIZED                                                             
207200   03  NOVA-VLDC-TEXT-BELOW-BARCODE.                                      
207300     05  FILLER      PIC X(30) VALUE '!F T W 1430 200 L 1 1 3 '.          
207400     05  FILLER                  PIC X(1)  VALUE '"'.                     
207500     05  NOVA-VLDC-DIST          PIC 9(4).                                
207600     05  NOVA-VLDC-KUNDN         PIC 9(6).                                
207700     05  NOVA-VLDC-ORDN          PIC 9(7).                                
207800     05  NOVA-VLDC-KOLI          PIC 9(5).                                
207900     05  FILLER                  PIC X(2)  VALUE '"Å'.                    
208000                                                                          
208100   03  NOVA-VLDC-TEXT-SHIPPER-CDC.                                        
208200     05  FILLER      PIC X(30) VALUE '!F T W 1620 1350 L 1 1 3 '.         
208300     05  FILLER      PIC X(1)  VALUE '"'.                                 
208400     05  FILLER      PIC X(20) VALUE 'SHIPPER: VOLVO CARS '.              
208500     05  FILLER      PIC X(18) VALUE 'CUSTOMER SERVICE, '.                
208600     05  FILLER      PIC X(21) VALUE 'SE-405 31 GOTHENBURG,'.             
208700     05  FILLER      PIC X(7)  VALUE ' SWEDEN'.                           
208800     05  FILLER      PIC X(2)  VALUE '"Å'.                                
208900                                                                          
209000     EJECT                                                                
209100*****************************************************************         
209200*         AREA MED STYRTECKEN FÖR MARKPOINT TERMO SKRIVARE.     *         
209300*         ANV. FÖR ATT SKRIVA KOLLIFLAGGA I 7I-FORMAT I CDC     *         
209400*         7I-VLDC = CASE LABEL SIZE 7I VIA LDC                  *         
209500*****************************************************************         
209600 01  FILLER           PIC X(24)  VALUE 'KOLLI-FLA6 TERMO VLDC'.           
209700*    STYRTECKEN ENLIGT MANUAL: MARKPOINT THERMAL PRINTER                  
209800*                              LABELPOINT                                 
209900 01  CASE-LABEL-THERMO-7I-VLDC.                                           
210000   03  7I-VLDC-RAD   PIC X(132)  VALUE SPACE.                             
210100                                                                          
210200   03  7I-VLDC-STYR-01.                                                   
210300     05  FILLER      PIC X(3)  VALUE '!CÅ'.                               
210400                                                                          
210500   03  7I-VLDC-STYR-91.                                                   
210600     05  FILLER      PIC X(3)  VALUE '!PÅ'.                               
210700                                                                          
210800   03  7I-VLDC-RUB-DISTR.                                                 
210900     05  FILLER      PIC X(25) VALUE '!F T N   50 380  R 1 1 3 '.         
211000     05  FILLER      PIC X(11) VALUE '"DISTRICT"Å'.                       
211100*RUB-KUNDNR                                                               
211200   03  7I-VLDC-RUB-KUNDNR.                                                
211300     05  FILLER      PIC X(25) VALUE '!F T N   50 810  R 1 1 3 '.         
211400     05  FILLER      PIC X(11) VALUE '"CUSTOMER"Å'.                       
211500                                                                          
211600   03  7I-VLDC-RUB-ORDNR.                                                 
211700     05  FILLER      PIC X(25) VALUE '!F T N   50 1260 R 1 1 3 '.         
211800     05  FILLER      PIC X(15) VALUE '"ORDER NUMBER"Å'.                   
211900                                                                          
212000   03  7I-VLDC-RUB-KOLLI.                                                 
212100     05  FILLER      PIC X(25) VALUE '!F T N   50 1600 R 1 1 3 '.         
212200     05  FILLER      PIC X(7)  VALUE '"CASE"Å'.                           
212300                                                                          
212400   03  7I-VLDC-RUB-ADRESS.                                                
212500     05  FILLER      PIC X(25) VALUE '!F T N  220  80  L 1 1 3 '.         
212600     05  FILLER      PIC X(10) VALUE '"ADDRESS"Å'.                        
212700                                                                          
212800   03  7I-VLDC-RUB-FRAKT.                                                 
212900     05  FILLER      PIC X(25) VALUE '!F T N  260 1600 R 1 1 3 '.         
213000     05  FILLER      PIC X(15) VALUE '"FREIGHT CODE"Å'.                   
213100                                                                          
213200   03  7I-VLDC-RUB-LDC-ADRESS.                                            
213300     05  FILLER      PIC X(25) VALUE '!F T N  460  330 L 1 1 3 '.         
213400     05  FILLER      PIC X(14) VALUE '"LDC ADDRESS"Å'.                    
213500                                                                          
213600   03  7I-VLDC-RUB-VIKT.                                                  
213700     05  FILLER      PIC X(25) VALUE '!F T N  480 1600 R 1 1 3 '.         
213800     05  FILLER      PIC X(12) VALUE '"WEIGHT KG"Å'.                      
213900                                                                          
214000   03  7I-VLDC-RUB-TRANSPORTINFO.                                         
214100     05  FILLER      PIC X(25) VALUE '!F T N  770  80 L 1 1 3 '.          
214200     05  FILLER      PIC X(17) VALUE '"TRANSPORT INFO"Å'.                 
214300                                                                          
214400   03  7I-VLDC-RUB-RFS.                                                   
214500     05  FILLER      PIC X(25) VALUE '!F T N  950   80 L 1 1 3 '.         
214600     05  FILLER      PIC X(6)  VALUE '"RFS"Å'.                            
214700                                                                          
214800   03  7I-VLDC-RUB-ST-ADRESS.                                             
214900     05  FILLER      PIC X(25) VALUE '!F T N  950  650 L 1 1 3 '.         
215000     05  FILLER      PIC X(13) VALUE '"ST ADDRESS"Å'.                     
215100                                                                          
215200   03  7I-VLDC-RUB-WIP.                                                   
215300     05  FILLER      PIC X(25) VALUE '!F T N 1000 1200 L 1 1 3 '.         
215400     05  FILLER      PIC X(9)  VALUE '"W.I.P."Å'.                         
215500*IDDISTR-SE                                                               
215600   03  7I-VLDC-DATA-DISTR-SE.                                             
215700     05  FILLER      PIC X(25) VALUE '!F T N  170  380 R 3 3 6 '.         
215800     05  FILLER      PIC X(1)  VALUE '"'.                                 
215900     05  7I-VLDC-IDDISTR-SE    PIC Z(4) VALUE ZERO.                       
216000     05  FILLER      PIC X(2)  VALUE '"Å'.                                
216100                                                                          
216200   03  7I-VLDC-DATA-DISTR.                                                
216300     05  FILLER      PIC X(25) VALUE '!F T N  170  380 R 2 2 6 '.         
216400     05  FILLER      PIC X(1)  VALUE '"'.                                 
216500     05  7I-VLDC-IDDISTR PIC Z(4) VALUE ZERO.                             
216600     05  FILLER      PIC X(2)  VALUE '"Å'.                                
216700*IDKUNDNR                                                                 
216800   03  7I-VLDC-DATA-KUNDNR.                                               
216900     05  FILLER      PIC X(25) VALUE '!F T N  170  960 R 2 2 6 '.         
217000     05  FILLER      PIC X(1)  VALUE '"'.                                 
217100     05  7I-VLDC-IDKUNDNR PIC Z(5)9 VALUE ZERO.                           
217200     05  FILLER      PIC X(2)  VALUE '"Å'.                                
217300                                                                          
217400   03  7I-VLDC-DATA-ORDNR.                                                
217500     05  FILLER      PIC X(25) VALUE '!F T N  170 1260 R 2 2 6 '.         
217600     05  FILLER      PIC X(1)  VALUE '"'.                                 
217700     05  7I-VLDC-IDORDNR PIC Z(4)9 VALUE SPACE.                           
217800     05  FILLER      PIC X(2)  VALUE '"Å'.                                
217900                                                                          
218000   03  7I-VLDC-DATA-KOLLI.                                                
218100     05  FILLER      PIC X(25) VALUE '!F T N  170 1600 R 2 2 6 '.         
218200     05  FILLER      PIC X(1)  VALUE '"'.                                 
218300     05  7I-VLDC-IDKOLLI PIC Z(5) VALUE ZERO.                             
218400     05  FILLER      PIC X(2)  VALUE '"Å'.                                
218500                                                                          
218600   03  7I-VLDC-DATA-ADRESS1.                                              
218700     05  FILLER      PIC X(25) VALUE '!F T N  270  80  L 4 3 1 '.         
218800     05  FILLER      PIC X(1)  VALUE '"'.                                 
218900     05  7I-VLDC-ADRESS1 PIC X(30) VALUE SPACE.                           
219000     05  FILLER      PIC X(2)  VALUE '"Å'.                                
219100                                                                          
219200   03  7I-VLDC-DATA-ADRESS2.                                              
219300     05  FILLER      PIC X(25) VALUE '!F T N  310  80  L 3 3 7 '.         
219400     05  FILLER      PIC X(1)  VALUE '"'.                                 
219500     05  7I-VLDC-ADRESS2 PIC X(30) VALUE SPACE.                           
219600     05  FILLER      PIC X(2)  VALUE '"Å'.                                
219700                                                                          
219800   03  7I-VLDC-DATA-ADRESS3.                                              
219900     05  FILLER      PIC X(25) VALUE '!F T N  360  80  L 4 3 1 '.         
220000     05  FILLER      PIC X(1)  VALUE '"'.                                 
220100     05  7I-VLDC-ADRESS3 PIC X(30) VALUE SPACE.                           
220200     05  FILLER      PIC X(2)  VALUE '"Å'.                                
220300                                                                          
220400   03  7I-VLDC-DATA-ADRESS4.                                              
220500     05  FILLER      PIC X(25) VALUE '!F T N  410  80  L 4 3 1 '.         
220600     05  FILLER      PIC X(1)  VALUE '"'.                                 
220700     05  7I-VLDC-ADRESS4 PIC X(30) VALUE SPACE.                           
220800     05  FILLER      PIC X(2)  VALUE '"Å'.                                
220900                                                                          
221000   03  7I-VLDC-IDBILREG-LDC.                                              
221100     05  FILLER      PIC X(25) VALUE '!F T N  460  80  L 4 3 1 '.         
221200     05  FILLER      PIC X(1)  VALUE '"'.                                 
221300     05  7I-VLDC-IDBILREG PIC X(10) VALUE SPACE.                          
221400     05  FILLER      PIC X(2)  VALUE '"Å'.                                
221500                                                                          
221600*KDFRAKT-SE                                                               
221700   03  7I-VLDC-DATA-FRAKT-SE.                                             
221800     05  FILLER      PIC X(25) VALUE '!F T N  480 1600 R 4 4 6 '.         
221900     05  FILLER      PIC X(1)  VALUE '"'.                                 
222000     05  7I-VLDC-KDFRAKT-SE    PIC Z9 VALUE ZERO.                         
222100     05  FILLER      PIC X(2)  VALUE '"Å'.                                
222200                                                                          
222300   03  7I-VLDC-DATA-FRAKT.                                                
222400     05  FILLER      PIC X(25) VALUE '!F T N  380 1600 R 2 2 6 '.         
222500     05  FILLER      PIC X(1)  VALUE '"'.                                 
222600     05  7I-VLDC-KDFRAKT PIC Z9 VALUE ZERO.                               
222700     05  FILLER      PIC X(2)  VALUE '"Å'.                                
222800                                                                          
222900   03  7I-VLDC-VIA.                                                       
223000     05  FILLER      PIC X(25) VALUE '!F T N  620  80 L 2 2 6 '.          
223100     05  FILLER      PIC X(1)  VALUE '"'.                                 
223200     05  FILLER      PIC X(3)  VALUE 'VIA'.                               
223300     05  FILLER      PIC X(2)  VALUE '"Å'.                                
223400                                                                          
223500   03  7I-VLDC-DATA-LDC-ADRESS1.                                          
223600     05  FILLER      PIC X(25) VALUE '!F T N  510 330  L 4 3 1 '.         
223700     05  FILLER      PIC X(1)  VALUE '"'.                                 
223800     05  7I-VLDC-LDC-ADRESS1   PIC X(30) VALUE SPACE.                     
223900     05  FILLER      PIC X(2)  VALUE '"Å'.                                
224000                                                                          
224100   03  7I-VLDC-DATA-LDC-ADRESS2.                                          
224200     05  FILLER      PIC X(25) VALUE '!F T N  550 330  L 3 3 7 '.         
224300     05  FILLER      PIC X(1)  VALUE '"'.                                 
224400     05  7I-VLDC-LDC-ADRESS2   PIC X(30) VALUE SPACE.                     
224500     05  FILLER      PIC X(2)  VALUE '"Å'.                                
224600                                                                          
224700   03  7I-VLDC-DATA-LDC-ADRESS3.                                          
224800     05  FILLER      PIC X(25) VALUE '!F T N  600 330  L 4 3 1 '.         
224900     05  FILLER      PIC X(1)  VALUE '"'.                                 
225000     05  7I-VLDC-LDC-ADRESS3   PIC X(30) VALUE SPACE.                     
225100     05  FILLER      PIC X(2)  VALUE '"Å'.                                
225200                                                                          
225300   03  7I-VLDC-DATA-LDC-ADRESS4.                                          
225400     05  FILLER      PIC X(25) VALUE '!F T N  650 330  L 4 3 1 '.         
225500     05  FILLER      PIC X(1)  VALUE '"'.                                 
225600     05  7I-VLDC-LDC-ADRESS4   PIC X(30) VALUE SPACE.                     
225700     05  FILLER      PIC X(2)  VALUE '"Å'.                                
225800                                                                          
225900   03  7I-VLDC-DATA-LDC-ADRESS5.                                          
226000     05  FILLER      PIC X(25) VALUE '!F T N  700 330  L 4 3 1 '.         
226100     05  FILLER      PIC X(1)  VALUE '"'.                                 
226200     05  7I-VLDC-LDC-ADRESS5   PIC X(30) VALUE SPACE.                     
226300     05  FILLER      PIC X(2)  VALUE '"Å'.                                
226400                                                                          
226500   03  7I-VLDC-DATA-VIKT.                                                 
226600     05  FILLER      PIC X(25) VALUE '!F T N  600 1600 R 2 2 6 '.         
226700     05  FILLER      PIC X(1)  VALUE '"'.                                 
226800     05  7I-VLDC-VKORDBTO      PIC Z(5) VALUE ZERO.                       
226900     05  FILLER      PIC X(2)  VALUE '"Å'.                                
227000                                                                          
227100   03  7I-VLDC-DATA-KILO-HEKTO.                                           
227200     05  FILLER      PIC X(25) VALUE '!F T N  600 1600 R 2 2 6 '.         
227300     05  FILLER      PIC X(1)  VALUE '"'.                                 
227400     05  7I-VLDC-KILO          PIC Z(5)   VALUE ZERO.                     
227500     05  7I-VLDC-PUNKT         PIC X      VALUE '.'.                      
227600     05  7I-VLDC-HEKTO         PIC 9      VALUE ZERO.                     
227700     05  FILLER      PIC X(2)  VALUE '"Å'.                                
227800                                                                          
227900   03  7I-VLDC-DATA-TRPINFO.                                              
228000     05  FILLER       PIC X(30) VALUE '!F T N  890  80 L 2 2 6 '.         
228100     05  FILLER               PIC X(1)  VALUE '"'.                        
228200     05  7I-VLDC-IDZON        PIC X(1).                                   
228300     05  FILLER               PIC X(1)  VALUE SPACE.                      
228400     05  7I-VLDC-IDDEPOT      PIC X(2).                                   
228500     05  FILLER               PIC X(1)  VALUE SPACE.                      
228600     05  7I-VLDC-IDROUTE      PIC X.                                      
228700     05  FILLER               PIC X(2)  VALUE '"Å'.                       
228800                                                                          
228900   03  7I-VLDC-DATA-RFS.                                                  
229000     05  FILLER      PIC X(25) VALUE '!F T N 1070   80 L 2 2 6 '.         
229100     05  FILLER      PIC X(1)  VALUE '"'.                                 
229200     05  7I-VLDC-TIRFS PIC 9(6) VALUE ZERO.                               
229300     05  FILLER      PIC X(2)  VALUE '"Å'.                                
229400                                                                          
229500   03  7I-VLDC-DATA-ST-ADRESS.                                            
229600     05  FILLER      PIC X(25) VALUE '!F T N 1070  650 L 2 2 6 '.         
229700     05  FILLER      PIC X(1)  VALUE '"'.                                 
229800     05  7I-VLDC-ADFLGEO       PIC X(3).                                  
229900     05  FILLER      PIC X(1)  VALUE SPACE.                               
230000     05  7I-VLDC-ADFLOMR       PIC Z(3)   VALUE ZERO.                     
230100     05  FILLER      PIC X(2)  VALUE '"Å'.                                
230200                                                                          
230300   03  7I-VLDC-DATA-WIP.                                                  
230400     05  FILLER      PIC X(25) VALUE '!F T N 1070 1200 L 1 1 6 '.         
230500     05  FILLER      PIC X(1)  VALUE '"'.                                 
230600     05  7I-VLDC-WIP           PIC X(10).                                 
230700     05  FILLER      PIC X(2)  VALUE '"Å'.                                
230800                                                                          
230900   03  7I-VLDC-BARCODE.                                                   
231000     05  FILLER    PIC X(30) VALUE '!F C N 1310 200 L 170 2 12 '.         
231100     05  FILLER                  PIC X(1)  VALUE '"'.                     
231200     05  7I-VLDC-DISTR           PIC 9(4).                                
231300     05  7I-VLDC-KUNDNR          PIC 9(6).                                
231400     05  7I-VLDC-ORDNR           PIC 9(7).                                
231500     05  7I-VLDC-KOLLI           PIC 9(5).                                
231600     05  FILLER                  PIC X(2)  VALUE '"Å'.                    
231700                                                                          
231800   03  7I-VLDC-TEXT-BELOW-BARCODE.                                        
231900     05  FILLER      PIC X(30) VALUE '!F T N 1340 200 L 1 1 3 '.          
232000     05  FILLER                  PIC X(1)  VALUE '"'.                     
232100     05  7I-VLDC-DIST            PIC 9(4).                                
232200     05  7I-VLDC-KUNDN           PIC 9(6).                                
232300     05  7I-VLDC-ORDN            PIC 9(7).                                
232400     05  7I-VLDC-KOLI            PIC 9(5).                                
232500     05  FILLER                  PIC X(2)  VALUE '"Å'.                    
232600                                                                          
232700   03  7I-VLDC-TEXT-SHIPPER-CDC.                                          
232800     05  FILLER      PIC X(30) VALUE '!F T N 1380  200 L 1 1 3 '.         
232900     05  FILLER      PIC X(1)  VALUE '"'.                                 
233000     05  FILLER      PIC X(20) VALUE 'SHIPPER: VOLVO CARS '.              
233100     05  FILLER      PIC X(18) VALUE 'CUSTOMER SERVICE, '.                
233200     05  FILLER      PIC X(21) VALUE 'SE-405 31 GOTHENBURG,'.             
233300     05  FILLER      PIC X(7)  VALUE ' SWEDEN'.                           
233400     05  FILLER      PIC X(2)  VALUE '"Å'.                                
233500                                                                          
233600     EJECT                                                                
233700*****************************************************************         
233800*NOVA     AREA MED STYRTECKEN FÖR NOVA TERMO SKRIVARE.          *         
233900*         ANV. FÖR ATT SKRIVA KOLLIFLAGGA I A5 FORMAT I CDC     *         
234000*         NOVA-LBL = CASE LABEL SIZE NOVA FORMAT                *         
234100*****************************************************************         
234200 01  FILLER           PIC X(24)  VALUE 'KOLLI-FLNOVA  TERMO'.             
234300*    STYRTECKEN ENLIGT MANUAL: IMAJE/NOVA THERMAL PRINTER                 
234400*                              LABELPOINT                                 
234500 01  CASE-LABEL-THERMO-NOVA-A6.                                           
234600   03  NOVA-LBL-RAD  PIC X(132)  VALUE SPACE.                             
234700                                                                          
234800   03  NOVA-LBL-STYR-COBRA.                                               
234900     05  FILLER      PIC X(16) VALUE '&&??%%P%P=207,30'.                  
235000     05  FILLER      PIC X(19) VALUE '=1,0=5,8=24,0=31,96'.               
235100     05  FILLER      PIC X(21) VALUE '=32,8=33,0=34,1=45,87'.             
235200     05  FILLER      PIC X(12) VALUE '=63,13=136,0'.                      
235300     05  FILLER      PIC X(22) VALUE '=207,12=207,10%&&??000'.            
235400                                                                          
235500   03  NOVA-LBL-STYR-01.                                                  
235600     05  FILLER      PIC X(3)  VALUE '!CÅ'.                               
235700                                                                          
235800   03  NOVA-LBL-STYR-91.                                                  
235900     05  FILLER      PIC X(3)  VALUE '!PÅ'.                               
236000                                                                          
236100   03  NOVA-LBL-STYR-42.                                                  
236200     05  FILLER      PIC X(6)  VALUE '!Y42 0'.                            
236300                                                                          
236400*RUB-DISTRICT                                                             
236500   03  NOVA-LBL-RUB-1-1.                                                  
236600     05  FILLER      PIC X(25) VALUE '!F T W  50 1800  R 1 1 3 '.         
236700     05  FILLER      PIC X(11) VALUE '"DISTRICT"Å'.                       
236800*CUSTOMER                                                                 
236900   03  NOVA-LBL-RUB-CUSTOMER.                                             
237000     05  FILLER      PIC X(25) VALUE '!F T W  50  700  R 1 1 3 '.         
237100     05  FILLER      PIC X(11) VALUE '"CUSTOMER"Å'.                       
237200                                                                          
237300   03  NOVA-LBL-RUB-DEALER.                                               
237400     05  FILLER      PIC X(25) VALUE '!F T W  50  700  R 1 1 3 '.         
237500     05  FILLER      PIC X(09) VALUE '"DEALER"Å'.                         
237600                                                                          
237700   03  NOVA-LBL-RUB-RETAILER.                                             
237800     05  FILLER      PIC X(25) VALUE '!F T W  50  700  R 1 1 3 '.         
237900     05  FILLER      PIC X(11) VALUE '"RETAILER"Å'.                       
238000                                                                          
238100   03  NOVA-LBL-RUB-1-3.                                                  
238200     05  FILLER      PIC X(25) VALUE '!F T W   50   50 R 1 1 3 '.         
238300     05  FILLER      PIC X(15) VALUE '"ORDER NUMBER"Å'.                   
238400*RUB-ADDRESS                                                              
238500   03  NOVA-LBL-RUB-2-1.                                                  
238600     05  FILLER      PIC X(25) VALUE '!F T W 310 1950  L 1 1 3 '.         
238700     05  FILLER      PIC X(10) VALUE '"ADDRESS"Å'.                        
238800                                                                          
238900   03  NOVA-LBL-RUB-2-5.                                                  
239000     05  FILLER      PIC X(25) VALUE '!F T W  250   50 R 1 1 3 '.         
239100     05  FILLER      PIC X(7)  VALUE '"CASE"Å'.                           
239200*                                                                         
239300   03  NOVA-LBL-RUB-2-6.                                                  
239400     05  FILLER      PIC X(25) VALUE '!F T W  450   50 R 1 1 3 '.         
239500     05  FILLER      PIC X(15) VALUE '"FREIGHT CODE"Å'.                   
239600                                                                          
239700   03  NOVA-LBL-RUB-FREIGHT-REFILL.                                       
239800     05  FILLER      PIC X(25) VALUE '!F T W 1110   20 R 1 1 3 '.         
239900     05  FILLER      PIC X(15) VALUE '"FREIGHT CODE"Å'.                   
240000                                                                          
240100*RUB-WEIGHT KG                                                            
240200   03  NOVA-LBL-RUB-3-1-WEIGHT-S11.                                       
240300     05  FILLER      PIC X(25) VALUE '!F T W 1190   50 R 1 1 3 '.         
240400     05  FILLER      PIC X(12)  VALUE '"WEIGHT KG"Å'.                     
240500                                                                          
240600*RUB-WEIGHT KG                                                            
240700   03  NOVA-LBL-RUB-3-1-WEIGHT-SE.                                        
240800     05  FILLER      PIC X(25) VALUE '!F T W  770   50 R 1 1 3 '.         
240900     05  FILLER      PIC X(12)  VALUE '"WEIGHT KG"Å'.                     
241000                                                                          
241100   03  NOVA-LBL-RUB-3-1-WEIGHT.                                           
241200     05  FILLER      PIC X(25) VALUE '!F T W  770   50 R 1 1 3 '.         
241300     05  FILLER      PIC X(12)  VALUE '"WEIGHT KG"Å'.                     
241400                                                                          
241500   03  NOVA-LBL-RUB-3-1-WEIGHT-NDC.                                       
241600     05  FILLER      PIC X(25) VALUE '!F T W  770   50 R 1 1 3 '.         
241700     05  FILLER      PIC X(16)  VALUE '"WEIGHT POUNDS"Å'.                 
241800*                                                                         
241900   03  NOVA-LBL-RUB-3-1-WEIGHT-GB.                                        
242000     05  FILLER      PIC X(25) VALUE '!F T W  1410  50 R 1 1 3 '.         
242100     05  FILLER      PIC X(12)  VALUE '"WEIGHT KG"Å'.                     
242200                                                                          
242300   03  NOVA-LBL-RUB-3-1-WEIGHT-REFILL.                                    
242400     05  FILLER      PIC X(25) VALUE '!F T W 1360   20 R 1 1 3 '.         
242500     05  FILLER      PIC X(12)  VALUE '"WEIGHT KG"Å'.                     
242600                                                                          
242700   03  NOVA-LBL-RUB-KDORDKL-DEA-NDC.                                      
242800*    05  FILLER      PIC X(25) VALUE '!F T W  560  410 R 1 1 3 '.         
242900     05  FILLER      PIC X(25) VALUE '!F T W  450   20 R 1 1 3 '.         
243000     05  FILLER      PIC X(14) VALUE '"ORDER CLASS"Å'.                    
243100*                                                                         
243200   03  NOVA-LBL-RUB-KDORDKL-DEA.                                          
243300     05  FILLER      PIC X(25) VALUE '!F T W  560  410 R 1 1 3 '.         
243400     05  FILLER      PIC X(14) VALUE '"ORDER CLASS"Å'.                    
243500*                                                                         
243600   03  NOVA-LBL-RUB-KDORDKL-S11.                                          
243700     05  FILLER      PIC X(25) VALUE '!F T W  980  20  R 1 1 3 '.         
243800     05  FILLER      PIC X(14) VALUE '"ORDER CLASS"Å'.                    
243900*                                                                         
244000   03  NOVA-LBL-RUB-KDORDKL.                                              
244100     05  FILLER      PIC X(25) VALUE '!F T W  460  410 R 1 1 3 '.         
244200     05  FILLER      PIC X(14) VALUE '"ORDER CLASS"Å'.                    
244300*                                                                         
244400   03  NOVA-LBL-RUB-IDDEPT.                                               
244500     05  FILLER      PIC X(25) VALUE '!F T W 1310   50 L 1 1 3 '.         
244600     05  FILLER      PIC X(7)  VALUE '"DEPT"Å'.                           
244700                                                                          
244800   03  NOVA-LBL-RUB-3-3-DEPOT.                                            
244900     05  FILLER      PIC X(25) VALUE '!F T W 920   50 L 1 1 3 '.          
245000     05  FILLER      PIC X(08) VALUE '"DEPOT"Å'.                          
245100                                                                          
245200   03  NOVA-LBL-RUB-3-4-ROUTE.                                            
245300     05  FILLER      PIC X(25) VALUE '!F T W 920   50  L 1 1 3 '.         
245400     05  FILLER      PIC X(08) VALUE '"ROUTE"Å'.                          
245500*1378 - CDC + REFILL                         1420                         
245600   03  NOVA-LBL-RUB-4-1-RFS.                                              
245700     05  FILLER      PIC X(25) VALUE '!F T W 1220 1950 L 1 1 3 '.         
245800     05  FILLER      PIC X(6)  VALUE '"RFS"Å'.                            
245900                                                                          
246000   03  NOVA-LBL-RUB-LINEREF.                                              
246100     05  FILLER      PIC X(25) VALUE '!F T W 1050 1950 L 1 1 3 '.         
246200     05  FILLER      PIC X(11)  VALUE '"LINE REF"Å'.                      
246300                                                                          
246400   03  NOVA-LBL-RUB-CUSTREF.                                              
246500     05  FILLER      PIC X(25) VALUE '!F T W 1050  950 L 1 1 3 '.         
246600     05  FILLER      PIC X(11) VALUE '"CUST REF"Å'.                       
246700                                                                          
246800   03  NOVA-LBL-RUB-RFS-POST.                                             
246900     05  FILLER      PIC X(25) VALUE '!F T W 1220 1950 L 1 1 3 '.         
247000     05  FILLER      PIC X(6)  VALUE '"RFS"Å'.                            
247100                                                                          
247200   03  NOVA-LBL-RUB-4-1-NDC.                                              
247300     05  FILLER      PIC X(25) VALUE '!F T W 1220 1500 R 1 1 3 '.         
247400     05  FILLER      PIC X(11) VALUE '"RFS DATE"Å'.                       
247500                                                                          
247600   03  NOVA-LBL-RUB-4-3.                                                  
247700     05  FILLER      PIC X(25) VALUE '!F T W 1220  550 R 1 1 3 '.         
247800     05  FILLER      PIC X(21) VALUE '"PRODUCTION NUMBER"Å'.              
247900*                                                                         
248000   03  NOVA-LBL-RUB-4-3-TRANSPORT.                                        
248100     05  FILLER      PIC X(25) VALUE '!F T W  930 1950 L 1 1 3 '.         
248200     05  FILLER      PIC X(17) VALUE '"TRANSPORT INFO"Å'.                 
248300                                                                          
248400   03  NOVA-LBL-RUB-3-1-IDPSN.                                            
248500     05  FILLER      PIC X(25) VALUE '!F T W 1080  550 R 1 1 3 '.         
248600     05  FILLER      PIC X(18)  VALUE '"DANGEROUS GOODS"Å'.               
248700*                                                                         
248800   03  NOVA-LBL-RUB-PARTNUMBER.                                           
248900     05  FILLER      PIC X(25) VALUE '!F T W  780  800 L 1 1 3 '.         
249000     05  FILLER      PIC X(14) VALUE '"PART NUMBER"Å'.                    
249100                                                                          
249200   03  NOVA-LBL-RUB-PARTNUMBER-NDC.                                       
249300     05  FILLER      PIC X(25) VALUE '!F T W  590 1000 L 1 1 3 '.         
249400     05  FILLER      PIC X(14) VALUE '"PART NUMBER"Å'.                    
249500*                                              Y    X                     
249600   03  NOVA-LBL-RUB-DC-WH-ADDRESS.                                        
249700     05  FILLER      PIC X(25) VALUE '!F T W 1090 1000 L 1 1 3 '.         
249800     05  FILLER      PIC X(16) VALUE '"DC WH ADDRESS"Å'.                  
249900                                                                          
250000   03  NOVA-LBL-RUB-DC-WH-ADRS-NDC.                                       
250100     05  FILLER      PIC X(25) VALUE '!F T W 1090 1000 L 1 1 3 '.         
250200     05  FILLER      PIC X(16) VALUE '"DC WH ADDRESS"Å'.                  
250300                                                                          
250400   03  NOVA-LBL-RUB-4-3-ST-ADDRESS.                                       
250500     05  FILLER      PIC X(25) VALUE '!F T W 1210 1000 L 1 1 3 '.         
250600     05  FILLER      PIC X(13) VALUE '"ST ADDRESS"Å'.                     
250700*1378                                                                     
250800*                                                                         
250900   03  NOVA-LBL-RUB-GB-ST-ADDRESS.                                        
251000     05  FILLER      PIC X(25) VALUE '!F T W 1220 1100 L 1 1 3 '.         
251100     05  FILLER      PIC X(13) VALUE '"ST ADDRESS"Å'.                     
251200                                                                          
251300   03  NOVA-LBL-RUB-4-3-CARRIER.                                          
251400     05  FILLER      PIC X(25) VALUE '!F T W 1270  850 L 1 1 3 '.         
251500     05  FILLER      PIC X(10) VALUE '"CARRIER"Å'.                        
251600                                                                          
251700   03  NOVA-LBL-RUB-REPDAT.                                               
251800     05  FILLER      PIC X(25) VALUE '!F T W 1120  650 L 1 1 3 '.         
251900     05  FILLER      PIC X(14) VALUE '"REPAIR DATE"Å'.                    
252000*                                             X    Y                      
252100   03  NOVA-LBL-RUB-REPDAT-S11.                                           
252200***  05  FILLER      PIC X(25) VALUE '!F T W  840  950 L 1 1 3 '.         
252300     05  FILLER      PIC X(25) VALUE '!F T W 1100  600 L 1 1 3 '.         
252400     05  FILLER      PIC X(14) VALUE '"REPAIR DATE"Å'.                    
252500                                                                          
252600   03  NOVA-LBL-RUB-CAR-REG.                                              
252700     05  FILLER      PIC X(25) VALUE '!F T W 1000  650 L 1 1 3 '.         
252800     05  FILLER      PIC X(19) VALUE '"CAR REGISTRATION"Å'.               
252900*                                            1100  650                    
253000   03  NOVA-LBL-RUB-CAR-REG-S11.                                          
253100***  05  FILLER      PIC X(25) VALUE '!F T W 1110  950 L 1 1 3 '.         
253200     05  FILLER      PIC X(25) VALUE '!F T W 1110 1100 L 1 1 3 '.         
253300     05  FILLER      PIC X(19) VALUE '"CAR REGISTRATION"Å'.               
253400                                                                          
253500*IDDISTR-SE                                                               
253600   03  NOVA-LBL-DATA-1-1-SE.                                              
253700     05  FILLER      PIC X(25) VALUE '!F T W  270 1950 L 4 4 6 '.         
253800     05  FILLER      PIC X(1)  VALUE '"'.                                 
253900     05  NOVA-LBL-IDDISTR-SE PIC Z(4) VALUE ZERO.                         
254000     05  FILLER      PIC X(2)  VALUE '"Å'.                                
254100                                                                          
254200   03  NOVA-LBL-DATA-1-1.                                                 
254300     05  FILLER      PIC X(25) VALUE '!F T W  220 1950 L 3 3 6 '.         
254400     05  FILLER      PIC X(1)  VALUE '"'.                                 
254500     05  NOVA-LBL-IDDISTR PIC Z(4) VALUE ZERO.                            
254600     05  FILLER      PIC X(2)  VALUE '"Å'.                                
254700                                                                          
254800   03  NOVA-LBL-DATA-1-2.                                                 
254900     05  FILLER      PIC X(25) VALUE '!F T W  220  700 R 3 3 6 '.         
255000     05  FILLER      PIC X(1)  VALUE '"'.                                 
255100     05  NOVA-LBL-IDKUNDNR PIC Z(5)9 VALUE ZERO.                          
255200     05  FILLER      PIC X(2)  VALUE '"Å'.                                
255300                                                                          
255400   03  NOVA-LBL-DATA-1-3.                                                 
255500     05  FILLER      PIC X(25) VALUE '!F T W  220  50 R 3 3 6 '.          
255600     05  FILLER      PIC X(1)  VALUE '"'.                                 
255700     05  NOVA-LBL-IDORDNR PIC Z(4)9 VALUE SPACE.                          
255800     05  FILLER      PIC X(2)  VALUE '"Å'.                                
255900                                                                          
256000   03  NOVA-LBL-DATA-ADRESS1.                                             
256100     05  FILLER      PIC X(25) VALUE '!F T W  400 1950 L 4 3 3 '.         
256200     05  FILLER      PIC X(1)  VALUE '"'.                                 
256300     05  NOVA-LBL-ADRESS1 PIC X(30) VALUE SPACE.                          
256400     05  FILLER      PIC X(2)  VALUE '"Å'.                                
256500                                                                          
256600   03  NOVA-LBL-DATA-ADRESS2.                                             
256700     05  FILLER      PIC X(25) VALUE '!F T W  500 1950 L 4 3 3 '.         
256800     05  FILLER      PIC X(1)  VALUE '"'.                                 
256900     05  NOVA-LBL-ADRESS2 PIC X(30) VALUE SPACE.                          
257000     05  FILLER      PIC X(2)  VALUE '"Å'.                                
257100                                                                          
257200   03  NOVA-LBL-DATA-ADRESS3.                                             
257300     05  FILLER      PIC X(25) VALUE '!F T W  600 1950 L 4 3 3 '.         
257400     05  FILLER      PIC X(1)  VALUE '"'.                                 
257500     05  NOVA-LBL-ADRESS3 PIC X(30) VALUE SPACE.                          
257600     05  FILLER      PIC X(2)  VALUE '"Å'.                                
257700                                                                          
257800   03  NOVA-LBL-DATA-ADRESS4.                                             
257900     05  FILLER      PIC X(25) VALUE '!F T W  700 1950 L 4 3 3 '.         
258000     05  FILLER      PIC X(1)  VALUE '"'.                                 
258100     05  NOVA-LBL-ADRESS4 PIC X(30) VALUE SPACE.                          
258200     05  FILLER      PIC X(2)  VALUE '"Å'.                                
258300                                                                          
258400   03  NOVA-LBL-DATA-ADRESS5.                                             
258500     05  FILLER      PIC X(25) VALUE '!F T W  800 1950 L 4 3 3 '.         
258600     05  FILLER      PIC X(1)  VALUE '"'.                                 
258700     05  NOVA-LBL-ADRESS5 PIC X(30) VALUE SPACE.                          
258800     05  FILLER      PIC X(2)  VALUE '"Å'.                                
258900                                                                          
259000   03  NOVA-LBL-DATA-2-1.                                                 
259100     05  FILLER      PIC X(25) VALUE '!F T W  400 1950 L 4 3 3 '.         
259200     05  FILLER      PIC X(1)  VALUE '"'.                                 
259300     05  NOVA-LBL-ADRESS-1 PIC X(30) VALUE SPACE.                         
259400     05  FILLER      PIC X(2)  VALUE '"Å'.                                
259500                                                                          
259600   03  NOVA-LBL-DATA-2-2.                                                 
259700     05  FILLER      PIC X(25) VALUE '!F T W  500 1950 L 4 3 3 '.         
259800     05  FILLER      PIC X(1)  VALUE '"'.                                 
259900     05  NOVA-LBL-ADRESS-2 PIC X(30) VALUE SPACE.                         
260000     05  FILLER      PIC X(2)  VALUE '"Å'.                                
260100                                                                          
260200   03  NOVA-LBL-DATA-2-3.                                                 
260300     05  FILLER      PIC X(25) VALUE '!F T W  600 1950 L 4 3 3 '.         
260400     05  FILLER      PIC X(1)  VALUE '"'.                                 
260500     05  NOVA-LBL-ADRESS-3 PIC X(30) VALUE SPACE.                         
260600     05  FILLER      PIC X(2)  VALUE '"Å'.                                
260700                                                                          
260800   03  NOVA-LBL-DATA-2-4.                                                 
260900     05  FILLER      PIC X(25) VALUE '!F T W  700 1950 L 4 3 3 '.         
261000     05  FILLER      PIC X(1)  VALUE '"'.                                 
261100     05  NOVA-LBL-ADRESS-4 PIC X(30) VALUE SPACE.                         
261200     05  FILLER      PIC X(2)  VALUE '"Å'.                                
261300                                                                          
261400   03  NOVA-LBL-ADRESS-2-5.                                               
261500     05  FILLER      PIC X(25) VALUE '!F T W  800 1950 L 4 3 3 '.         
261600     05  FILLER      PIC X(1)  VALUE '"'.                                 
261700     05  NOVA-LBL-ADRESS-5 PIC X(30) VALUE SPACE.                         
261800     05  FILLER      PIC X(2)  VALUE '"Å'.                                
261900*                                                                         
262000   03  NOVA-LBL-ADRESS-2-5-LDC.                                           
262100     05  FILLER      PIC X(25) VALUE '!F T W  880 1950 L 3 3 6 '.         
262200     05  FILLER      PIC X(1)  VALUE '"'.                                 
262300     05  NOVA-LBL-ADRESS-5-LDC PIC X(15) VALUE SPACE.                     
262400     05  FILLER      PIC X(2)  VALUE '"Å'.                                
262500*BETEXT-S11                                                               
262600   03  NOVA-DATA-BETEXT-S11.                                              
262700     05  FILLER      PIC X(25) VALUE '!F T W  900 1950 L 4 3 3 '.         
262800     05  FILLER      PIC X(1)  VALUE '"'.                                 
262900     05  NOVA-DATA-BETEXT-NOVA-S11   PIC X(35) VALUE SPACE.               
263000     05  FILLER      PIC X(2)  VALUE '"Å'.                                
263100*                                                                         
263200*TIREPDAT                                      Y    X                     
263300   03  NOVA-LBL-TIREPDAT.                                                 
263400     05  FILLER      PIC X(25) VALUE '!F T W 1190  650 L 4 3 3 '.         
263500     05  FILLER      PIC X(1)  VALUE '"'.                                 
263600     05  NOVA-TIREPDAT        PIC X(6) VALUE ZERO.                        
263700     05  FILLER      PIC X(2)  VALUE '"Å'.                                
263800                                                                          
263900*TIREPDAT                                     Y   X 650                   
264000   03  NOVA-LBL-TIREPDAT-S11.                                             
264100     05  FILLER      PIC X(25) VALUE '!F T W 1190  600 L 4 3 3 '.         
264200     05  FILLER      PIC X(1)  VALUE '"'.                                 
264300     05  NOVA-TIREPDAT-S11    PIC X(6) VALUE ZERO.                        
264400     05  FILLER      PIC X(2)  VALUE '"Å'.                                
264500*                                           Y1180 X650                    
264600*BETEXT                                                                   
264700   03  NOVA-DATA-BETEXT.                                                  
264800     05  FILLER      PIC X(25) VALUE '!F T W 1180 1950 L 4 3 3 '.         
264900     05  FILLER      PIC X(1)  VALUE '"'.                                 
265000     05  NOVA-DATA-BETEXT-NOVA PIC X(35) VALUE SPACE.                     
265100     05  FILLER      PIC X(2)  VALUE '"Å'.                                
265200*                                                                         
265300   03  NOVA-LBL-IDBILREG-LDC-S11.                                         
265400***  05  FILLER      PIC X(25) VALUE '!F T W 1190  950 L 4 3 3 '.         
265500     05  FILLER      PIC X(25) VALUE '!F T W 1190 1100 L 4 3 3 '.         
265600     05  FILLER      PIC X(1)  VALUE '"'.                                 
265700     05  NOVA-LBL-IDBILREG-S11 PIC X(10) VALUE SPACE.                     
265800     05  FILLER      PIC X(2)  VALUE '"Å'.                                
265900                                                                          
266000   03  NOVA-LBL-IDBILREG-LDC.                                             
266100     05  FILLER      PIC X(25) VALUE '!F T W 1080  650 L 4 3 3 '.         
266200     05  FILLER      PIC X(1)  VALUE '"'.                                 
266300     05  NOVA-LBL-IDBILREG    PIC X(10) VALUE SPACE.                      
266400     05  FILLER      PIC X(2)  VALUE '"Å'.                                
266500                                                                          
266600*****START* ADDRESS FIELDS FOR SWEDISH DISTRICTS                          
266700   03  NOVA-LBL-DATA-2-1-SWE.                                             
266800     05  FILLER      PIC X(25) VALUE '!F T W  400 1950 L 4 3 3 '.         
266900     05  FILLER      PIC X(1)  VALUE '"'.                                 
267000     05  NOVA-LBL-ADRESS-1-SWE PIC X(20) VALUE SPACE.                     
267100*    05  NOVA-LBL-ADRESS-1-SWE PIC X(13) VALUE SPACE.                     
267200     05  FILLER      PIC X(2)  VALUE '"Å'.                                
267300                                                                          
267400   03  NOVA-LBL-DATA-2-2-SWE.                                             
267500     05  FILLER      PIC X(25) VALUE '!F T W  500 1950 L 4 3 3 '.         
267600     05  FILLER      PIC X(1)  VALUE '"'.                                 
267700     05  NOVA-LBL-ADRESS-2-SWE PIC X(30) VALUE SPACE.                     
267800     05  FILLER      PIC X(2)  VALUE '"Å'.                                
267900                                                                          
268000   03  NOVA-LBL-DATA-2-3-SWE.                                             
268100     05  FILLER      PIC X(25) VALUE '!F T W  600 1950 L 4 3 3 '.         
268200     05  FILLER      PIC X(1)  VALUE '"'.                                 
268300     05  NOVA-LBL-ADRESS-3-SWE PIC X(30) VALUE SPACE.                     
268400     05  FILLER      PIC X(2)  VALUE '"Å'.                                
268500*RAD4 STÖRRE TEXT                                                         
268600   03  NOVA-LBL-DATA-2-4-SWE.                                             
268700     05  FILLER      PIC X(25) VALUE '!F T W  800 1950 L 3 2 6 '.         
268800     05  FILLER      PIC X(1)  VALUE '"'.                                 
268900     05  NOVA-LBL-ADRESS-4-SWE PIC X(30) VALUE SPACE.                     
269000     05  FILLER      PIC X(2)  VALUE '"Å'.                                
269100                                                                          
269200   03  NOVA-LBL-DATA-2-5-SWE.                                             
269300     05  FILLER      PIC X(25) VALUE '!F T W  900 1950 L 4 3 3 '.         
269400     05  FILLER      PIC X(1)  VALUE '"'.                                 
269500     05  NOVA-LBL-ADRESS-5-SWE PIC X(30) VALUE SPACE.                     
269600     05  FILLER      PIC X(2)  VALUE '"Å'.                                
269700*                                                                         
269800   03  NOVA-LBL-DATA-2-5.                                                 
269900     05  FILLER      PIC X(25) VALUE '!F T W  420  20 R 3 3 6 '.          
270000     05  FILLER      PIC X(1)  VALUE '"'.                                 
270100     05  NOVA-LBL-IDKOLLI PIC Z(5) VALUE ZERO.                            
270200     05  FILLER      PIC X(2)  VALUE '"Å'.                                
270300*KDFRAKT-SE                                                               
270400   03  NOVA-LBL-DATA-2-6-SE.                                              
270500     05  FILLER      PIC X(25) VALUE '!F T W  730  20 R 5 5 6 '.          
270600     05  FILLER      PIC X(1)  VALUE '"'.                                 
270700     05  NOVA-LBL-KDFRAKT-SE   PIC Z9 VALUE ZERO.                         
270800     05  FILLER      PIC X(2)  VALUE '"Å'.                                
270900                                                                          
271000   03  NOVA-LBL-DATA-2-6.                                                 
271100     05  FILLER      PIC X(25) VALUE '!F T W  690  20 R 3 3 6 '.          
271200     05  FILLER      PIC X(1)  VALUE '"'.                                 
271300     05  NOVA-LBL-KDFRAKT PIC Z9 VALUE ZERO.                              
271400     05  FILLER      PIC X(2)  VALUE '"Å'.                                
271500                                                                          
271600   03  NOVA-LBL-DATA-FREIGHT-REFILL.                                      
271700     05  FILLER      PIC X(25) VALUE '!F T W 1280  20 R 3 3 6 '.          
271800     05  FILLER      PIC X(1)  VALUE '"'.                                 
271900     05  NOVA-LBL-KDFRAKT-REFILL PIC Z9 VALUE ZERO.                       
272000     05  FILLER      PIC X(2)  VALUE '"Å'.                                
272100                                                                          
272200   03  NOVA-LBL-DATA-WEIGHT.                                              
272300     05  FILLER      PIC X(25) VALUE '!F T W  940  20 R 3 3 6 '.          
272400     05  FILLER      PIC X(1)  VALUE '"'.                                 
272500     05  NOVA-LBL-VKORDBTO PIC Z(5) VALUE ZERO.                           
272600     05  FILLER      PIC X(2)  VALUE '"Å'.                                
272700*                                                                         
272800   03  NOVA-LBL-DATA-WEIGHT-S11.                                          
272900     05  FILLER      PIC X(25) VALUE '!F T W 1370  20 R 3 3 6 '.          
273000     05  FILLER      PIC X(1)  VALUE '"'.                                 
273100     05  NOVA-LBL-VKORDBTO-S11 PIC Z(5) VALUE ZERO.                       
273200     05  FILLER      PIC X(2)  VALUE '"Å'.                                
273300*                                                                         
273400   03  NOVA-LBL-DATA-WEIGHT-GB.                                           
273500     05  FILLER      PIC X(25) VALUE '!F T W  1580 20 R 3 3 6 '.          
273600     05  FILLER      PIC X(1)  VALUE '"'.                                 
273700     05  NOVA-LBL-VKORDBTO-GB  PIC Z(5) VALUE ZERO.                       
273800     05  FILLER      PIC X(2)  VALUE '"Å'.                                
273900*                                                                         
274000*REFILL                                                                   
274100   03  NOVA-LBL-DATA-WEIGHT-REFILL.                                       
274200     05  FILLER      PIC X(25) VALUE '!F T W 1530  20 R 3 3 6 '.          
274300     05  FILLER      PIC X(1)  VALUE '"'.                                 
274400     05  NOVA-LBL-VKORDBTO-REFILL PIC Z(5) VALUE ZERO.                    
274500     05  FILLER      PIC X(2)  VALUE '"Å'.                                
274600                                                                          
274700   03  NOVA-LBL-DATA-WEIGHT-NDC.                                          
274800     05  FILLER      PIC X(25) VALUE '!F T W  940  20 R 3 3 6 '.          
274900     05  FILLER      PIC X(1)  VALUE '"'.                                 
275000     05  NOVA-LBL-VKORDBTO-NDC PIC Z(5) VALUE ZERO.                       
275100     05  FILLER      PIC X(2)  VALUE '"Å'.                                
275200                                                                          
275300   03  NOVA-LBL-DATA-KILO-HEKTO-S11.                                      
275400     05  FILLER      PIC X(25) VALUE '!F T W 1370  20 R 3 3 6 '.          
275500     05  FILLER      PIC X(1)  VALUE '"'.                                 
275600     05  NOVA-LBL-KILO-S11    PIC Z(4)9  VALUE ZERO.                      
275700     05  NOVA-LBL-PUNKT-S11   PIC X      VALUE '.'.                       
275800     05  NOVA-LBL-HEKTO-S11   PIC 9      VALUE ZERO.                      
275900     05  FILLER      PIC X(2)  VALUE '"Å'.                                
276000                                                                          
276100   03  NOVA-LBL-DATA-KILO-HEKTO.                                          
276200     05  FILLER      PIC X(25) VALUE '!F T W  940  20 R 3 3 6 '.          
276300     05  FILLER      PIC X(1)  VALUE '"'.                                 
276400     05  NOVA-LBL-KILO    PIC Z(4)9  VALUE ZERO.                          
276500     05  NOVA-LBL-PUNKT   PIC X      VALUE '.'.                           
276600     05  NOVA-LBL-HEKTO   PIC 9      VALUE ZERO.                          
276700     05  FILLER      PIC X(2)  VALUE '"Å'.                                
276800                                                                          
276900*REFILL                                                                   
277000   03  NOVA-LBL-DATA-KILO-HEKTO-REF.                                      
277100     05  FILLER      PIC X(25) VALUE '!F T W 1530  20 R 3 3 6 '.          
277200     05  FILLER      PIC X(1)  VALUE '"'.                                 
277300     05  NOVA-LBL-KILO-REFILL    PIC Z(5)   VALUE ZERO.                   
277400     05  NOVA-LBL-PUNKT-REFILL   PIC X      VALUE '.'.                    
277500     05  NOVA-LBL-HEKTO-REFILL   PIC 9      VALUE ZERO.                   
277600     05  FILLER      PIC X(2)  VALUE '"Å'.                                
277700*                                                                         
277800   03  NOVA-LBL-DATA-KILO-HEKTO-GB.                                       
277900     05  FILLER      PIC X(25) VALUE '!F T W  1580 20 R 3 3 6 '.          
278000     05  FILLER      PIC X(1)  VALUE '"'.                                 
278100     05  NOVA-LBL-KILO-GB    PIC Z(5)   VALUE ZERO.                       
278200     05  NOVA-LBL-PUNKT-GB   PIC X      VALUE '.'.                        
278300     05  NOVA-LBL-HEKTO-GB   PIC 9      VALUE ZERO.                       
278400     05  FILLER      PIC X(2)  VALUE '"Å'.                                
278500                                                                          
278600   03  NOVA-LBL-DATA-IDDEPT.                                              
278700     05  FILLER      PIC X(25) VALUE '!F T W 1150   20 R 3 3 6 '.         
278800     05  FILLER      PIC X(1)  VALUE '"'.                                 
278900     05  NOVA-LBL-IDDEPT PIC ZZ VALUE ZERO.                               
279000     05  FILLER      PIC X(2)  VALUE '"Å'.                                
279100*                                                                         
279200   03  NOVA-LBL-DATA-RESTORDER.                                           
279300     05  FILLER      PIC X(25) VALUE '!F T W 1200  350 R 3 3 6 '.         
279400     05  FILLER      PIC X(1)  VALUE '"'.                                 
279500     05  NOVA-LBL-RESTORDER PIC X(2) VALUE '  '.                          
279600     05  FILLER      PIC X(2)  VALUE '"Å'.                                
279700                                                                          
279800   03  NOVA-LBL-DATA-KDORDKL-DEA-NDC.                                     
279900*    05  FILLER      PIC X(25) VALUE '!F T W  730   20 R 3 3 6 '.         
280000     05  FILLER      PIC X(25) VALUE '!F T W  610   20 R 3 3 6 '.         
280100     05  FILLER      PIC X(1)  VALUE '"'.                                 
280200     05  NOVA-LBL-KDORDKL-DEA-NDC PIC 9 VALUE ZERO.                       
280300     05  FILLER      PIC X(2)  VALUE '"Å'.                                
280400*                                             680  700                    
280500   03  NOVA-LBL-DATA-KDORDKL-DEA.                                         
280600     05  FILLER      PIC X(25) VALUE '!F T W  730   20 R 3 3 6 '.         
280700     05  FILLER      PIC X(1)  VALUE '"'.                                 
280800     05  NOVA-LBL-KDORDKL-DEA  PIC 9 VALUE ZERO.                          
280900     05  FILLER      PIC X     VALUE '/'.                                 
281000     05  FILLER      PIC X(2)  VALUE '"Å'.                                
281100*                                             680  700                    
281200   03  NOVA-LBL-DATA-KDORDKL.                                             
281300     05  FILLER      PIC X(25) VALUE '!F T W  650  420 R 3 3 6 '.         
281400     05  FILLER      PIC X(1)  VALUE '"'.                                 
281500     05  NOVA-LBL-KDORDKL PIC 9 VALUE ZERO.                               
281600     05  FILLER      PIC X     VALUE '/'.                                 
281700     05  FILLER      PIC X(2)  VALUE '"Å'.                                
281800*                                             680  700                    
281900   03  NOVA-LBL-DATA-KDORDKL-S11.                                         
282000     05  FILLER      PIC X(25) VALUE '!F T W 1150   20 R 3 3 6 '.         
282100     05  FILLER      PIC X(1)  VALUE '"'.                                 
282200     05  NOVA-LBL-KDORDKL-S11  PIC 9 VALUE ZERO.                          
282300     05  FILLER      PIC X(2)  VALUE '"Å'.                                
282400*                                             680  700                    
282500*1378 - CDC                                                               
282600   03  NOVA-LBL-DATA-4-1.                                                 
282700     05  FILLER      PIC X(25) VALUE '!F T W 1400 1950 L 3 3 6 '.         
282800     05  FILLER      PIC X(1)  VALUE '"'.                                 
282900     05  NOVA-LBL-TIRFS PIC 9(6) VALUE ZERO.                              
283000     05  FILLER      PIC X(2)  VALUE '"Å'.                                
283100*1378 1600                                                                
283200   03  NOVA-LBL-DATA-POST.                                                
283300     05  FILLER      PIC X(25) VALUE '!F T W 1400 1950 L 3 3 6 '.         
283400     05  FILLER      PIC X(1)  VALUE '"'.                                 
283500     05  NOVA-LBL-TIRFS-POST   PIC 9(6) VALUE ZERO.                       
283600     05  FILLER      PIC X(2)  VALUE '"Å'.                                
283700*                                                                         
283800   03  NOVA-LBL-RUB-WIP.                                                  
283900     05  FILLER      PIC X(25) VALUE '!F T W 1220  300 L 1 1 3 '.         
284000     05  FILLER      PIC X(9)  VALUE '"W.I.P."Å'.                         
284100*WIP                                                                      
284200   03  NOVA-LBL-DATA-WIP.                                                 
284300     05  FILLER      PIC X(25) VALUE '!F T W 1300  300 L 1 1 6 '.         
284400     05  FILLER      PIC X(1)  VALUE '"'.                                 
284500     05  NOVA-LBL-WIP PIC X(10).                                          
284600     05  FILLER      PIC X(2)  VALUE '"Å'.                                
284700*1378                                                                     
284800   03  NOVA-LBL-ADFLGEO-GB-GRP.                                           
284900     05  FILLER      PIC X(25) VALUE '!F T W 1400 1100 L 3 3 6 '.         
285000     05  FILLER      PIC X(1)  VALUE '"'.                                 
285100     05  NOVA-LBL-ADFLGEO-GB PIC X(3) VALUE ZERO.                         
285200     05  FILLER      PIC X(2)  VALUE '"Å'.                                
285300                                                                          
285400   03  NOVA-LBL-ADFLGEO-STD-GRP.                                          
285500     05  FILLER      PIC X(25) VALUE '!F T W 1390 1150 L 3 3 6 '.         
285600     05  FILLER      PIC X(1)  VALUE '"'.                                 
285700     05  NOVA-LBL-ADFLGEO-STD PIC X(3) VALUE ZERO.                        
285800     05  FILLER      PIC X(2)  VALUE '"Å'.                                
285900*POSTEN                                        Y   X                      
286000   03  NOVA-LBL-ADFLGEO-POSTEN.                                           
286100     05  FILLER      PIC X(25) VALUE '!F T W 1390 1150 L 3 3 6 '.         
286200     05  FILLER      PIC X(1)  VALUE '"'.                                 
286300     05  NOVA-LBL-ADFLGEO-POST   PIC X(3) VALUE ZERO.                     
286400     05  FILLER      PIC X(2)  VALUE '"Å'.                                
286500                                                                          
286600   03  NOVA-LBL-ADFLOMR-STD-GRP.                                          
286700     05  FILLER      PIC X(25) VALUE '!F T W 1390  800 L 3 3 6 '.         
286800     05  FILLER      PIC X(1)  VALUE '"'.                                 
286900     05  NOVA-LBL-ADFLOMR-STD PIC Z(3) VALUE ZERO.                        
287000     05  FILLER      PIC X(2)  VALUE '"Å'.                                
287100*1378                                                                     
287200   03  NOVA-LBL-ADFLOMR-GB-GRP.                                           
287300     05  FILLER      PIC X(25) VALUE '!F T W 1400  700 L 3 3 6 '.         
287400     05  FILLER      PIC X(1)  VALUE '"'.                                 
287500     05  NOVA-LBL-ADFLOMR-GB PIC Z(3) VALUE ZERO.                         
287600     05  FILLER      PIC X(2)  VALUE '"Å'.                                
287700*POSTEN                                                                   
287800   03  NOVA-LBL-ADFLOMR-POSTEN.                                           
287900     05  FILLER      PIC X(25) VALUE '!F T W 1390  700 L 3 3 6 '.         
288000     05  FILLER      PIC X(1)  VALUE '"'.                                 
288100     05  NOVA-LBL-ADFLOMR-POST PIC Z(2) VALUE ZERO.                       
288200     05  FILLER      PIC X(2)  VALUE '"Å'.                                
288300                                                                          
288400   03  NOVA-LBL-ADRUTNIV-STD-GRP.                                         
288500     05  FILLER      PIC X(25) VALUE '!F T W 1390  500 L 3 3 6 '.         
288600     05  FILLER      PIC X(1)  VALUE '"'.                                 
288700     05  NOVA-LBL-ADRUTNIV-STD PIC Z(3) VALUE ZERO.                       
288800     05  FILLER      PIC X(2)  VALUE '"Å'.                                
288900                                                                          
289000   03  NOVA-LBL-ADRUTNIV-GB-GRP.                                          
289100     05  FILLER      PIC X(25) VALUE '!F T W 1600  450 L 3 3 6 '.         
289200     05  FILLER      PIC X(1)  VALUE '"'.                                 
289300     05  NOVA-LBL-ADRUTNIV-GB PIC Z(3) VALUE ZERO.                        
289400     05  FILLER      PIC X(2)  VALUE '"Å'.                                
289500*POSTEN                                        Y    X                     
289600   03  NOVA-LBL-ADRUTNIV-POSTEN.                                          
289700     05  FILLER      PIC X(25) VALUE '!F T W 1390  300 L 3 3 6 '.         
289800     05  FILLER      PIC X(1)  VALUE '"'.                                 
289900     05  NOVA-LBL-ADRUTNIV-POST PIC Z(2) VALUE ZERO.                      
290000     05  FILLER      PIC X(2)  VALUE '"Å'.                                
290100                                                                          
290200   03  NOVA-LBL-DATA-RUTNIV-SPA.                                          
290300     05  FILLER      PIC X(25) VALUE '!F T W 1390  300 L 3 3 6 '.         
290400     05  FILLER      PIC X(1)  VALUE '"'.                                 
290500     05  NOVA-LBL-ADRUTNIV-SPA PIC Z(3) VALUE ZERO.                       
290600     05  FILLER      PIC X(2)  VALUE '"Å'.                                
290700                                                                          
290800   03  NOVA-LBL-ST-ADRESS.                                                
290900     05  FILLER      PIC X(25) VALUE '!F T W 1390 1150 L 3 3 6 '.         
291000     05  FILLER      PIC X(1)  VALUE '"'.                                 
291100     05  NOVA-LBL-ADFLGEO-REFILL PIC X(3).                                
291200     05  FILLER      PIC X(1)  VALUE SPACE.                               
291300     05  NOVA-LBL-ADFLOMR-REFILL PIC Z(3)   VALUE ZERO.                   
291400     05  FILLER      PIC X(2)  VALUE '"Å'.                                
291500                                                                          
291600   03  NOVA-LBL-FRAKT-TEXT.                                               
291700     05  FILLER      PIC X(25) VALUE '!F T W 1390 850 L 3 3 6 '.          
291800     05  FILLER      PIC X(1)  VALUE '"'.                                 
291900     05  NOVA-LBL-FRAKT-TXT    PIC X(12).                                 
292000     05  FILLER      PIC X(2)  VALUE '"Å'.                                
292100                                                                          
292200   03  NOVA-LBL-DATA-4-7.                                                 
292300     05  FILLER      PIC X(25) VALUE '!F T W 1550  700 R 3 3 6 '.         
292400     05  FILLER      PIC X(1)  VALUE '"'.                                 
292500     05  NOVA-LBL-IDPRODNR PIC Z(7) VALUE ZERO.                           
292600     05  FILLER      PIC X(2)  VALUE '"Å'.                                
292700   03  NOVA-LBL-TEXT-VOR-INT.                                             
292800     05  FILLER      PIC X(25) VALUE '!F T W 1260  600 L 3 3 6 '.         
292900     05  FILLER      PIC X(1)  VALUE '"'.                                 
293000     05  FILLER      PIC X(11) VALUE 'VOR INT'.                           
293100     05  FILLER      PIC X(2)  VALUE '"Å'.                                
293200*                                              Y    X                     
293300   03  NOVA-LBL-1478-BERADREF.                                            
293400     05  FILLER      PIC X(25) VALUE '!F T W 1150 1950 L 4 3 3 '.         
293500     05  FILLER      PIC X(1)  VALUE '"'.                                 
293600     05  NOVA-1478-BERADREF     PIC X(10).                                
293700     05  FILLER      PIC X(2)  VALUE '"Å'.                                
293800                                                                          
293900   03  NOVA-LBL-REFILL-BERADREF.                                          
294000     05  FILLER      PIC X(25) VALUE '!F T W 1150 1100 L 3 3 6 '.         
294100     05  FILLER      PIC X(1)  VALUE '"'.                                 
294200     05  NOVA-LBL-BERADREF     PIC X(10).                                 
294300     05  FILLER      PIC X(2)  VALUE '"Å'.                                
294400*BEKUNDRF                                                                 
294500   03  NOVA-LBL-1478-BEKUNDRF.                                            
294600     05  FILLER      PIC X(25) VALUE '!F T W 1150 1260 L 4 3 3 '.         
294700     05  FILLER      PIC X(1)  VALUE '"'.                                 
294800     05  NOVA-1478-BEKUNDRF     PIC X(15).                                
294900     05  FILLER      PIC X(2)  VALUE '"Å'.                                
295000                                                                          
295100   03  NOVA-LBL-TEXT-RETURN.                                              
295200     05  FILLER      PIC X(25) VALUE '!F T W 1020 150 L 3 3 6 '.          
295300     05  FILLER      PIC X(1)  VALUE '"'.                                 
295400     05  FILLER      PIC X(6)  VALUE 'RETURN'.                            
295500     05  FILLER      PIC X(2)  VALUE '"Å'.                                
295600*                                                                         
295700   03  NOVA-LBL-REFILL-KVLEVART.                                          
295800     05  FILLER      PIC X(25) VALUE '!F T W 1210 1950 L 8 4 3'.          
295900     05  FILLER               PIC X(1)  VALUE '"'.                        
296000     05  NOVA-LBL-KVLEVART    PIC Z(6).                                   
296100     05  FILLER               PIC X(1)  VALUE SPACE.                      
296200     05  FILLER               PIC X(3)  VALUE 'PCS'.                      
296300     05  FILLER               PIC X(2)  VALUE '"Å'.                       
296400                                                                          
296500   03  NOVA-LBL-REFILL-KVLEVART-NDC.                                      
296600     05  FILLER      PIC X(25) VALUE '!F T W 1180 1750 L 8 4 3'.          
296700     05  FILLER               PIC X(1)  VALUE '"'.                        
296800     05  NOVA-LBL-KVLEVART-NDC PIC Z(6).                                  
296900     05  FILLER               PIC X(3)  VALUE 'PCS'.                      
297000     05  FILLER               PIC X(2)  VALUE '"Å'.                       
297100* 450 380                                                                 
297200   03  NOVA-LBL-REFILL-ARTNR.                                             
297300     05  FILLER      PIC X(25) VALUE '!F T W 1000   20 R 4 4 6 '.         
297400     05  FILLER               PIC X(1)  VALUE '"'.                        
297500     05  NOVA-LBL-IDARTNR     PIC X(9).                                   
297600     05  FILLER               PIC X(2)  VALUE '"Å'.                       
297700*                                                                         
297800   03  NOVA-LBL-REFILL-ARTNR-NDC.                                         
297900     05  FILLER      PIC X(25) VALUE '!F T W  1049  20 R 8 5 6 '.         
298000     05  FILLER               PIC X(1)  VALUE '"'.                        
298100     05  NOVA-LBL-IDARTNR-NDC PIC X(9).                                   
298200     05  FILLER               PIC X(2)  VALUE '"Å'.                       
298300                                                                          
298400   03  NOVA-LBL-REFILL-Q-NDC.                                             
298500*    05  FILLER      PIC X(25) VALUE '!F T W 1150 1950 L 5 5 6'.          
298600     05  FILLER      PIC X(25) VALUE '!F T W 1000 1950 L 8 8 6'.          
298700     05  FILLER               PIC X(1)  VALUE '"'.                        
298800     05  FILLER               PIC X(1)  VALUE 'Q'.                        
298900     05  FILLER               PIC X(2)  VALUE '"Å'.                       
299000*                                                                         
299100   03  NOVA-LBL-REFILL-DC-WH-ADR.                                         
299200     05  FILLER      PIC X(25) VALUE '!F T W 1180  330 R 4 3 3'.          
299300     05  FILLER               PIC X(1)  VALUE '"'.                        
299400     05  NOVA-LBL-ADLAGOMR    PIC Z(3).                                   
299500     05  NOVA-LBL-ADGANG      PIC Z(3).                                   
299600     05  FILLER               PIC X(1)  VALUE SPACE.                      
299700     05  NOVA-LBL-ADPLATS     PIC Z(5).                                   
299800     05  FILLER               PIC X(2)  VALUE '"Å'.                       
299900*OLD                                                                      
300000   03  NOVA-LBL-REFILL-DC-WH-ADR-NDC.                                     
300100     05  FILLER      PIC X(25) VALUE '!F T W 1180  330 R 4 3 3'.          
300200     05  FILLER               PIC X(1)  VALUE '"'.                        
300300     05  NOVA-LBL-ADLAGOMR-NDC PIC Z(3).                                  
300400     05  FILLER               PIC X(1)  VALUE SPACE.                      
300500     05  NOVA-LBL-ADGANG-NDC  PIC Z(3).                                   
300600     05  FILLER               PIC X(1)  VALUE SPACE.                      
300700     05  NOVA-LBL-ADPLATS-NDC PIC Z(5).                                   
300800     05  FILLER               PIC X(2)  VALUE '"Å'.                       
300900*                                                                         
301000   03  NOVA-LBL-TRPINFO-SDC23-GB.                                         
301100     05  FILLER       PIC X(30) VALUE '!F T W 1100 1950 L 3 3 6'.         
301200     05  FILLER               PIC X(1)  VALUE '"'.                        
301300     05  NOVA-LBL-IDZON       PIC X(1).                                   
301400     05  FILLER               PIC X(1)  VALUE SPACE.                      
301500     05  NOVA-LBL-IDDEPOT     PIC X(2).                                   
301600     05  FILLER               PIC X(2)  VALUE SPACE.                      
301700     05  NOVA-LBL-IDROUTE     PIC X.                                      
301800     05  FILLER               PIC X(2)  VALUE '"Å'.                       
301900*                                                                         
302000   03  NOVA-LBL-TEXT-SDC23-SHIP-CDC.                                      
302100     05  FILLER      PIC X(30) VALUE '!F T W 1600 1800 L 1 1 3'.          
302200     05  FILLER                  PIC X(1)  VALUE '"'.                     
302300     05  FILLER      PIC X(20) VALUE 'SHIPPER: VOLVO CARS '.              
302400     05  FILLER      PIC X(18) VALUE 'CUSTOMER SERVICE, '.                
302500     05  FILLER      PIC X(21) VALUE 'SE-405 31 GOTHENBURG,'.             
302600     05  FILLER      PIC X(7)  VALUE ' SWEDEN'.                           
302700     05  FILLER                  PIC X(2)  VALUE '"Å'.                    
302800*                                                                         
302900   03  NOVA-LBL-TEXT-BEGMRK-ES.                                           
303000     05  FILLER    PIC X(30) VALUE '!F T W  950 1950 L 5 3 3 '.           
303100     05  FILLER                  PIC X(1)  VALUE '"'.                     
303200     05  NOVA-LBL-BEGMRKTXT-RAD1 PIC X(30).                               
303300     05  FILLER                  PIC X(2)  VALUE '"Å'.                    
303400                                                                          
303500   03  NOVA-LBL-BARCODE-BEGMRK-ES.                                        
303600     05  FILLER    PIC X(30) VALUE '!F C W 1130 1950 L 140 3 12 '.        
303700     05  FILLER                  PIC X(1)  VALUE '"'.                     
303800     05  NOVA-LBL-BEGMRKBC-RAD1   PIC X(30).                              
303900     05  FILLER                  PIC X(2)  VALUE '"Å'.                    
304000                                                                          
304100   03  NOVA-LBL-BARCODE.                                                  
304200     05  FILLER    PIC X(30) VALUE '!F C W 1550 1800 L 140 3 12'.         
304300     05  FILLER                  PIC X(1)  VALUE '"'.                     
304400     05  NOVA-LBL-DISTR          PIC 9(4).                                
304500     05  NOVA-LBL-KUNDNR         PIC 9(6).                                
304600     05  NOVA-LBL-ORDNR          PIC 9(7).                                
304700     05  NOVA-LBL-KOLLI          PIC 9(5).                                
304800     05  FILLER                  PIC X(2)  VALUE '"Å'.                    
304900*                                                                         
305000   03  NOVA-LBL-BARCODE-POST.                                             
305100     05  FILLER    PIC X(30) VALUE '!F C W 1550 1850 L 140 3 12'.         
305200     05  FILLER                  PIC X(1)  VALUE '"'.                     
305300     05  NOVA-LBL-DISTR-POST     PIC 9(4).                                
305400     05  NOVA-LBL-KUNDNR-POST    PIC 9(6).                                
305500     05  NOVA-LBL-ORDNR-POST     PIC 9(7).                                
305600     05  NOVA-LBL-KOLLI-POST     PIC 9(5).                                
305700     05  FILLER                  PIC X(2)  VALUE '"Å'.                    
305800                                                                          
305900   03  NOVA-LBL-BARCODE-LONG.                                             
306000     05  FILLER    PIC X(30) VALUE '!F C W 1550 1450 L 140 3 12'.         
306100     05  FILLER                  PIC X(1)  VALUE '"'.                     
306200     05  NOVA-LBL-DISTR-LONG     PIC 9(4).                                
306300     05  NOVA-LBL-KUNDNR-LONG    PIC 9(6).                                
306400     05  NOVA-LBL-ORDNR-LONG     PIC 9(7).                                
306500     05  NOVA-LBL-KOLLI-LONG     PIC 9(5).                                
306600     05  FILLER                  PIC X(2)  VALUE '"Å'.                    
306700*                                            1570                         
306800   03  NOVA-LBL-TEXT-BELOW-BARCODE.                                       
306900     05  FILLER      PIC X(30) VALUE '!F T W 1620 1800 L 1 1 3'.          
307000     05  FILLER                  PIC X(1)  VALUE '"'.                     
307100     05  NOVA-LBL-DIST           PIC 9(4).                                
307200     05  NOVA-LBL-KUNDN          PIC 9(6).                                
307300     05  NOVA-LBL-ORDN           PIC 9(7).                                
307400     05  NOVA-LBL-KOLI           PIC 9(5).                                
307500     05  FILLER                  PIC X(2)  VALUE '"Å'.                    
307600*POSTEN                                                                   
307700   03  NOVA-LBL-TXT-BLW-BARCODE-POST.                                     
307800     05  FILLER      PIC X(30) VALUE '!F T W 1620 1850 L 1 1 3'.          
307900     05  FILLER                  PIC X(1)  VALUE '"'.                     
308000     05  NOVA-LBL-DIST-POST      PIC 9(4).                                
308100     05  NOVA-LBL-KUNDN-POST     PIC 9(6).                                
308200     05  NOVA-LBL-ORDN-POST      PIC 9(7).                                
308300     05  NOVA-LBL-KOLI-POST      PIC 9(5).                                
308400     05  FILLER                  PIC X(2)  VALUE '"Å'.                    
308500                                                                          
308600*NOT UTILIZED                                                             
308700   03  NOVA-LBL-BARCODE-IT.                                               
308800     05  FILLER    PIC X(30) VALUE '!F C W 1550 350 L 140 3 12 '.         
308900     05  FILLER                  PIC X(1)  VALUE '"'.                     
309000     05  NOVA-LBL-DISTR-IT       PIC 9(4).                                
309100     05  NOVA-LBL-KUNDNR-IT      PIC 9(6).                                
309200     05  NOVA-LBL-ORDNR-IT       PIC 9(5).                                
309300     05  NOVA-LBL-KOLLI-IT       PIC 9(5).                                
309400     05  FILLER                  PIC X(2)  VALUE '"Å'.                    
309500*NOT UTILIZED                                                             
309600   03  NOVA-LBL-TEXT-BARCODE-IT.                                          
309700     05  FILLER      PIC X(30) VALUE '!F T W 1600 350 L 1 1 3 '.          
309800     05  FILLER                  PIC X(1)  VALUE '"'.                     
309900     05  NOVA-LBL-DIST-IT        PIC 9(4).                                
310000     05  NOVA-LBL-KUNDN-IT       PIC 9(6).                                
310100     05  NOVA-LBL-ORDN-IT        PIC 9(5).                                
310200     05  NOVA-LBL-KOLI-IT        PIC 9(5).                                
310300     05  FILLER                  PIC X(2)  VALUE '"Å'.                    
310400                                                                          
310500   03  NOVA-LBL-TEXT-SHIPPER-CDC.                                         
310600     05  FILLER      PIC X(30) VALUE '!F T W 1620 1300 L 1 1 3'.          
310700     05  FILLER                  PIC X(1)  VALUE '"'.                     
310800     05  FILLER      PIC X(20) VALUE 'SHIPPER: VOLVO CARS '.              
310900     05  FILLER      PIC X(18) VALUE 'CUSTOMER SERVICE, '.                
311000     05  FILLER      PIC X(21) VALUE 'SE-405 31 GOTHENBURG,'.             
311100     05  FILLER      PIC X(7)  VALUE ' SWEDEN'.                           
311200     05  FILLER                  PIC X(2)  VALUE '"Å'.                    
311300                                                                          
311400   03  NOVA-LBL-TEXT-SHIPPER-CDC-POST.                                    
311500     05  FILLER      PIC X(30) VALUE '!F T W 1620 1300 L 1 1 3 '.         
311600     05  FILLER                  PIC X(1)  VALUE '"'.                     
311700     05  FILLER      PIC X(20) VALUE 'SHIPPER: VOLVO CARS '.              
311800     05  FILLER      PIC X(18) VALUE 'CUSTOMER SERVICE, '.                
311900     05  FILLER      PIC X(21) VALUE 'SE-405 31 GOTHENBURG,'.             
312000     05  FILLER      PIC X(7)  VALUE ' SWEDEN'.                           
312100     05  FILLER                  PIC X(2)  VALUE '"Å'.                    
312200*POSTEN                                                                   
312300   03  NOVA-LBL-TEXT-PRODUKT.                                             
312400     05  FILLER      PIC X(25) VALUE '!F T W  950 990  R 1 1 3 '.         
312500     05  FILLER      PIC X(10)  VALUE '"PRODUKT"Å'.                       
312600                                                                          
312700   03  NOVA-LBL-TEXT-PRODUKT-1090.                                        
312800     05  FILLER      PIC X(25) VALUE '!F T W  950 990  R 1 1 3 '.         
312900     05  FILLER      PIC X(10)  VALUE '"PRODUKT"Å'.                       
313000                                                                          
313100   03  NOVA-LBL-TEXT-PRODUKT-NO.                                          
313200     05  FILLER      PIC X(25) VALUE '!F T W  950 990  R 1 1 3 '.         
313300     05  FILLER      PIC X(10)  VALUE '"PRODUKT"Å'.                       
313400                                                                          
313500   03  NOVA-LBL-REFILL-TEXT-PRODUKT.                                      
313600     05  FILLER      PIC X(25) VALUE '!F T W  950 990  R 1 1 3 '.         
313700     05  FILLER      PIC X(10)  VALUE '"PRODUKT"Å'.                       
313800                                                                          
313900   03  NOVA-LBL-8700-TEXT-PRODUKT.                                        
314000     05  FILLER      PIC X(25) VALUE '!F T W  950 999  R 1 1 3 '.         
314100     05  FILLER      PIC X(10)  VALUE '"PRODUKT"Å'.                       
314200                                                                          
314300   03  NOVA-LBL-TEXT-HIT.                                                 
314400     05  FILLER      PIC X(25) VALUE '!F T W 1050 990  R 1 1 3 '.         
314500     05  FILLER      PIC X(6)  VALUE '"HIT"Å'.                            
314600                                                                          
314700   03  NOVA-LBL-TEXT-HIT-1090.                                            
314800     05  FILLER      PIC X(25) VALUE '!F T W 1050 990  R 1 1 3 '.         
314900     05  FILLER      PIC X(6)  VALUE '"HIT"Å'.                            
315000                                                                          
315100   03  NOVA-LBL-REFILL-TEXT-HIT.                                          
315200     05  FILLER      PIC X(25) VALUE '!F T W 1050 990  R 1 1 3 '.         
315300     05  FILLER      PIC X(6)  VALUE '"HIT"Å'.                            
315400                                                                          
315500   03  NOVA-LBL-8700-TEXT-HIT.                                            
315600     05  FILLER      PIC X(25) VALUE '!F T W 1050 990  R 1 1 3 '.         
315700     05  FILLER      PIC X(6)  VALUE '"HIT"Å'.                            
315800                                                                          
315900   03  NOVA-LBL-TEXT-PAK.                                                 
316000     05  FILLER      PIC X(25) VALUE '!F T W 1050 990  R 1 1 3 '.         
316100     05  FILLER      PIC X(6)  VALUE '"PAK"Å'.                            
316200                                                                          
316300   03  NOVA-LBL-TEXT-PAK-1090.                                            
316400     05  FILLER      PIC X(25) VALUE '!F T W 1050 990  R 1 1 3 '.         
316500     05  FILLER      PIC X(6)  VALUE '"PAK"Å'.                            
316600                                                                          
316700   03  NOVA-LBL-REFILL-TEXT-PAK.                                          
316800     05  FILLER      PIC X(25) VALUE '!F T W 1050 990  R 1 1 3 '.         
316900     05  FILLER      PIC X(6)  VALUE '"PAK"Å'.                            
317000                                                                          
317100   03  NOVA-LBL-8700-TEXT-PAK.                                            
317200     05  FILLER      PIC X(25) VALUE '!F T W 1050 990  R 1 1 3 '.         
317300     05  FILLER      PIC X(6)  VALUE '"PAK"Å'.                            
317400                                                                          
317500   03  NOVA-LBL-TEXT-48.                                                  
317600     05  FILLER      PIC X(25) VALUE '!F T W 1000 990  R 1 1 3 '.         
317700     05  FILLER      PIC X(5)  VALUE '"48"Å'.                             
317800                                                                          
317900   03  NOVA-LBL-TEXT-49-1090.                                             
318000     05  FILLER      PIC X(25) VALUE '!F T W 1000 990  R 1 1 3 '.         
318100     05  FILLER      PIC X(5)  VALUE '"49"Å'.                             
318200                                                                          
318300   03  NOVA-LBL-8700-TEXT-48.                                             
318400     05  FILLER      PIC X(25) VALUE '!F T W 1000 990  R 1 1 3 '.         
318500     05  FILLER      PIC X(5)  VALUE '"48"Å'.                             
318600                                                                          
318700   03  NOVA-LBL-TEXT-69.                                                  
318800     05  FILLER      PIC X(25) VALUE '!F T W 1000 990  R 1 1 3 '.         
318900     05  FILLER      PIC X(5)  VALUE '"69"Å'.                             
319000                                                                          
319100   03  NOVA-LBL-TEXT-54-1090.                                             
319200     05  FILLER      PIC X(25) VALUE '!F T W 1000 990  R 1 1 3 '.         
319300     05  FILLER      PIC X(5)  VALUE '"54"Å'.                             
319400                                                                          
319500   03  NOVA-LBL-REFILL-TEXT-54.                                           
319600     05  FILLER      PIC X(25) VALUE '!F T W 1000 990  R 1 1 3 '.         
319700     05  FILLER      PIC X(5)  VALUE '"54"Å'.                             
319800                                                                          
319900   03  NOVA-LBL-8700-TEXT-54.                                             
320000     05  FILLER      PIC X(25) VALUE '!F T W 1000 990  R 1 1 3 '.         
320100     05  FILLER      PIC X(5)  VALUE '"54"Å'.                             
320200*POSTEN                                                                   
320300   03  NOVA-LBL-SORTERINGSKOD-1090.                                       
320400     05  FILLER      PIC X(25) VALUE '!F T W 1550 320  L 4 3 3 '.         
320500     05  FILLER      PIC X(1)  VALUE '"'.                                 
320600     05  NOVA-LBL-ADFLGEO-1090-FI  PIC X(2)  VALUE 'FI'.                  
320700     05  NOVA-LBL-ADFLGEO-SORT-1090 PIC X(3) VALUE ZERO.                  
320800     05  FILLER      PIC X(2)  VALUE '"Å'.                                
320900                                                                          
321000   03  NOVA-LBL-SORTERINGSKOD-NO.                                         
321100     05  FILLER      PIC X(25) VALUE '!F T W 1550 320  L 4 3 3 '.         
321200     05  FILLER      PIC X(1)  VALUE '"'.                                 
321300     05  NOVA-LBL-ADFLGEO-NO       PIC X(2)  VALUE 'NO'.                  
321400     05  NOVA-LBL-ADFLGEO-SORT-NO  PIC X(3)  VALUE ZERO.                  
321500     05  FILLER      PIC X(2)  VALUE '"Å'.                                
321600                                                                          
321700   03  NOVA-LBL-SORTERINGSKOD.                                            
321800     05  FILLER      PIC X(25) VALUE '!F T W 1550 320  L 4 3 3 '.         
321900     05  FILLER      PIC X(1)  VALUE '"'.                                 
322000     05  NOVA-LBL-ADFLGEO-SE   PIC X(2) VALUE 'SE'.                       
322100     05  NOVA-LBL-ADFLGEO-SORT PIC X(3) VALUE ZERO.                       
322200     05  FILLER      PIC X(2)  VALUE '"Å'.                                
322300                                                                          
322400   03  NOVA-LBL-REFILL-SORTERINGSKOD.                                     
322500     05  FILLER      PIC X(25) VALUE '!F T W  750 850  L 4 3 3 '.         
322600     05  FILLER      PIC X(1)  VALUE '"'.                                 
322700     05  NOVA-LBL-REFILL-ADFLGEO-SORT PIC X(3) VALUE ZERO.                
322800     05  FILLER      PIC X(2)  VALUE '"Å'.                                
322900                                                                          
323000   03  NOVA-LBL-8700-SORTERINGSKOD.                                       
323100     05  FILLER      PIC X(25) VALUE '!F T W  930 550  L 4 3 3 '.         
323200     05  FILLER      PIC X(1)  VALUE '"'.                                 
323300     05  NOVA-LBL-8700-ADFLGEO-SORT PIC X(3) VALUE ZERO.                  
323400     05  FILLER      PIC X(2)  VALUE '"Å'.                                
323500*POSTEN                                                                   
323600   03  NOVA-LBL-BARCODE-POSTEN.                                           
323700     05  FILLER    PIC X(30) VALUE '!F C W 1050  950 L 100 3 41'.         
323800     05  FILLER                  PIC X(1)  VALUE '"'.                     
323900     05  NOVA-LBL-POSTEN1        PIC X(13).                               
324000     05  FILLER                  PIC X(2)  VALUE '"Å'.                    
324100                                                                          
324200   03  NOVA-LBL-BARCODE-POSTEN-1090.                                      
324300     05  FILLER    PIC X(30) VALUE '!F C W 1050  950 L 100 3 41'.         
324400     05  FILLER                  PIC X(1)  VALUE '"'.                     
324500     05  NOVA-LBL-POSTEN1-1090   PIC X(13).                               
324600     05  FILLER                  PIC X(2)  VALUE '"Å'.                    
324700                                                                          
324800   03  NOVA-LBL-REFILL-BARCODE-POSTEN.                                    
324900     05  FILLER    PIC X(30) VALUE '!F C W 1050  950 L 100 3 41'.         
325000     05  FILLER                  PIC X(1)  VALUE '"'.                     
325100     05  NOVA-LBL-REFILL-POSTEN1 PIC X(13).                               
325200     05  FILLER                  PIC X(2)  VALUE '"Å'.                    
325300                                                                          
325400   03  NOVA-LBL-8700-BARCODE-POSTEN.                                      
325500     05  FILLER    PIC X(30) VALUE '!F C W 1050  950 L 100 3 41'.         
325600     05  FILLER                  PIC X(1)  VALUE '"'.                     
325700     05  NOVA-LBL-8700-POSTEN1   PIC X(13).                               
325800     05  FILLER                  PIC X(2)  VALUE '"Å'.                    
325900                                                                          
326000   03  NOVA-LBL-TEXT-POSTEN.                                              
326100     05  FILLER    PIC X(30) VALUE '!F T W 1120  950 L 1 1 3'.            
326200     05  FILLER                  PIC X(1)  VALUE '"'.                     
326300     05  NOVA-LBL-POSTEN1-TEXT   PIC X(13).                               
326400     05  FILLER                  PIC X(2)  VALUE '"Å'.                    
326500                                                                          
326600   03  NOVA-LBL-TEXT-POSTEN-1090.                                         
326700     05  FILLER    PIC X(30) VALUE '!F T W 1080  950 L 1 1 3'.            
326800     05  FILLER                  PIC X(1)  VALUE '"'.                     
326900     05  NOVA-LBL-POSTEN1-TEXT-1090 PIC X(13).                            
327000     05  FILLER                  PIC X(2)  VALUE '"Å'.                    
327100                                                                          
327200   03  NOVA-LBL-REFILL-TEXT-POSTEN.                                       
327300     05  FILLER    PIC X(30) VALUE '!F T W 1120  950 L 1 1 3'.            
327400     05  FILLER                      PIC X(1)  VALUE '"'.                 
327500     05  NOVA-LBL-REFILL-POSTEN1-TEXT PIC X(13).                          
327600     05  FILLER                      PIC X(2)  VALUE '"Å'.                
327700                                                                          
327800   03  NOVA-LBL-8700-TEXT-POSTEN.                                         
327900     05  FILLER    PIC X(30) VALUE '!F T W  1120 950 L 1 1 3'.            
328000     05  FILLER                    PIC X(1)  VALUE '"'.                   
328100     05  NOVA-LBL-8700-POSTEN1-TEXT PIC X(13).                            
328200     05  FILLER                    PIC X(2)  VALUE '"Å'.                  
328300                                                                          
328400     EJECT                                                                
328500*****************************************************************         
328600*         AREA MED STYRTECKEN FÖR MARKPOINT TERMO SKRIVARE.     *         
328700*         ANV. FÖR ATT SKRIVA KOLLIFLAGGA I 7INCH FORMAT I CDC  *         
328800*         CL7INCH = CASE LABEL SIZE 7INCH                                 
328900*****************************************************************         
329000 01  FILLER           PIC X(24)  VALUE 'KOLLI-FL7INCH TERMO'.             
329100*    STYRTECKEN ENLIGT MANUAL: MARKPOINT THERMAL PRINTER                  
329200*                              LABELPOINT                                 
329300 01  CASE-LABEL-THERMO-7INCH.                                             
329400   03  CL7INCH-RAD   PIC X(132)  VALUE SPACE.                             
329500                                                                          
329600   03  CL7INCH-STYR-COBRA.                                                
329700     05  FILLER      PIC X(16) VALUE '&&??%%P%P=207,30'.                  
329800     05  FILLER      PIC X(19) VALUE '=1,0=5,8=24,0=31,96'.               
329900     05  FILLER      PIC X(21) VALUE '=32,8=33,0=34,1=45,87'.             
330000     05  FILLER      PIC X(12) VALUE '=63,13=136,0'.                      
330100     05  FILLER      PIC X(22) VALUE '=207,12=207,10%&&??000'.            
330200                                                                          
330300   03  CL7INCH-STYR-01.                                                   
330400     05  FILLER      PIC X(3)  VALUE '!CÅ'.                               
330500                                                                          
330600   03  CL7INCH-STYR-91.                                                   
330700     05  FILLER      PIC X(3)  VALUE '!PÅ'.                               
330800                                                                          
330900   03  CL7INCH-STYR-42.                                                   
331000     05  FILLER      PIC X(6)  VALUE '!Y42 0'.                            
331100                                                                          
331200*RUB-DISTRICT                                                             
331300   03  CL7INCH-RUB-1-1.                                                   
331400     05  FILLER      PIC X(25) VALUE '!F T N   20 790  R 2 1 3 '.         
331500     05  FILLER      PIC X(11) VALUE '"DISTRICT"Å'.                       
331600*                                                                         
331700   03  TACDIS-RUB-DISTRICT.                                               
331800     05  FILLER      PIC X(25) VALUE '!F T N  50  850  L 1 1 3 '.         
331900     05  FILLER      PIC X(11) VALUE '"DISTRICT"Å'.                       
332000*CUSTOMER                                                                 
332100   03  CL7INCH-RUB-CUSTOMER.                                              
332200     05  FILLER      PIC X(25) VALUE '!F T N   50 1500 R 2 1 3 '.         
332300     05  FILLER      PIC X(11) VALUE '"CUSTOMER"Å'.                       
332400                                                                          
332500   03  TACDIS-RUB-CUSTOMER.                                               
332600     05  FILLER      PIC X(25) VALUE '!F T N   50  200 L 2 1 3 '.         
332700     05  FILLER      PIC X(11) VALUE '"CUSTOMER"Å'.                       
332800                                                                          
332900   03  CL7INCH-RUB-DEALER.                                                
333000     05  FILLER      PIC X(25) VALUE '!F T N   50 1500 R 2 1 3 '.         
333100     05  FILLER      PIC X(09) VALUE '"DEALER"Å'.                         
333200                                                                          
333300   03  CL7INCH-RUB-RETAILER.                                              
333400     05  FILLER      PIC X(25) VALUE '!F T N   50 1500 R 2 1 3 '.         
333500     05  FILLER      PIC X(11) VALUE '"RETAILER"Å'.                       
333600                                                                          
333700   03  CL7INCH-RUB-1-3.                                                   
333800     05  FILLER      PIC X(25) VALUE '!F T N   50 2150 R 2 1 3 '.         
333900     05  FILLER      PIC X(15) VALUE '"ORDER NUMBER"Å'.                   
334000*ORDER-NUMBER                                                             
334100   03  TACDIS-RUB-ORDER-NUMBER.                                           
334200     05  FILLER      PIC X(25) VALUE '!F T N   60 2130 R 1 1 3 '.         
334300     05  FILLER      PIC X(15) VALUE '"ORDER NUMBER"Å'.                   
334400*ORDER-NUMMER                                                             
334500   03  TACDIS-RUB-ORDER-NUMMER.                                           
334600     05  FILLER      PIC X(25) VALUE '!F T N 1500 200  L 1 1 3 '.         
334700     05  FILLER      PIC X(15) VALUE '"ORDER NUMMER"Å'.                   
334800*RUB-ADDRESS                                                              
334900   03  CL7INCH-RUB-2-1.                                                   
335000     05  FILLER      PIC X(25) VALUE '!F T N  310 180  L 1 1 3 '.         
335100     05  FILLER      PIC X(10) VALUE '"ADDRESS"Å'.                        
335200                                                                          
335300   03  TACDIS-RUB-ADDRESS.                                                
335400     05  FILLER      PIC X(25) VALUE '!F T N  310 750  L 1 1 3 '.         
335500     05  FILLER      PIC X(10) VALUE '"ADDRESS"Å'.                        
335600                                                                          
335700   03  CL7INCH-RUB-2-5.                                                   
335800     05  FILLER      PIC X(25) VALUE '!F T N  310 2150 R 1 1 3 '.         
335900     05  FILLER      PIC X(14) VALUE '"CASE NUMBER"Å'.                    
336000                                                                          
336100   03  TACDIS-RUB-CASE-NUMBER.                                            
336200     05  FILLER      PIC X(25) VALUE '!F T N  290 2140 R 1 1 3 '.         
336300     05  FILLER      PIC X(14) VALUE '"CASE NUMBER"Å'.                    
336400                                                                          
336500   03  CL7INCH-RUB-2-6.                                                   
336600     05  FILLER      PIC X(25) VALUE '!F T N  580 2150 R 2 1 3 '.         
336700     05  FILLER      PIC X(15) VALUE '"FREIGHT CODE"Å'.                   
336800                                                                          
336900   03  CL7INCH-RUB-FREIGHT-REFILL.                                        
337000     05  FILLER      PIC X(25) VALUE '!F T N 1310 2150 R 1 1 3 '.         
337100     05  FILLER      PIC X(15) VALUE '"FREIGHT CODE"Å'.                   
337200                                                                          
337300   03  TACDIS-RUB-FREIGHT-CODE.                                           
337400     05  FILLER      PIC X(25) VALUE '!F T N  50 1300  L 1 1 3 '.         
337500     05  FILLER      PIC X(15) VALUE '"FREIGHT CODE"Å'.                   
337600                                                                          
337700*RUB-WEIGHT KG                                                            
337800   03  CL7INCH-RUB-3-1-WEIGHT-SE.                                         
337900     05  FILLER      PIC X(25) VALUE '!F T N  940 2150 R 2 1 3 '.         
338000     05  FILLER      PIC X(12)  VALUE '"WEIGHT KG"Å'.                     
338100                                                                          
338200   03  CL7INCH-RUB-3-1-WEIGHT.                                            
338300     05  FILLER      PIC X(25) VALUE '!F T N  840 2150 R 2 1 3 '.         
338400     05  FILLER      PIC X(12)  VALUE '"WEIGHT KG"Å'.                     
338500                                                                          
338600   03  CL7INCH-RUB-3-1-WEIGHT-NDC.                                        
338700     05  FILLER      PIC X(25) VALUE '!F T N  920 1500 R 2 1 3 '.         
338800     05  FILLER      PIC X(16)  VALUE '"WEIGHT POUNDS"Å'.                 
338900                                                                          
339000   03  CL7INCH-RUB-3-1-WEIGHT-GB.                                         
339100     05  FILLER      PIC X(25) VALUE '!F T N 1430 2050 R 2 1 3 '.         
339200     05  FILLER      PIC X(12)  VALUE '"WEIGHT KG"Å'.                     
339300*                                                                         
339400   03  CL7INCH-RUB-WEIGHT-REFILL.                                         
339500     05  FILLER      PIC X(25) VALUE '!F T N 1520 2150 R 1 1 3 '.         
339600     05  FILLER      PIC X(12)  VALUE '"WEIGHT KG"Å'.                     
339700                                                                          
339800*KDORDKL                                      1850                        
339900   03  CL7INCH-RUB-KDORDKL-S01.                                           
340000     05  FILLER      PIC X(25) VALUE '!F T N 1180 2150 R 2 1 3 '.         
340100     05  FILLER      PIC X(14) VALUE '"ORDER CLASS"Å'.                    
340200                                                                          
340300   03  CL7INCH-RUB-KDORDKL-S08.                                           
340400     05  FILLER      PIC X(25) VALUE '!F T N  580 1850 R 2 1 3 '.         
340500     05  FILLER      PIC X(14) VALUE '"ORDER CLASS"Å'.                    
340600                                                                          
340700   03  TACDIS-RUB-KDORDKL.                                                
340800     05  FILLER      PIC X(25) VALUE '!F T N  530 2140 R 1 1 3 '.         
340900     05  FILLER      PIC X(14) VALUE '"ORDER CLASS"Å'.                    
341000                                                                          
341100   03  TACDIS-RUB-STRECK.                                                 
341200     05  FILLER      PIC X(25) VALUE '!F T N  810  200 L 1 1 3 '.         
341300     05  FILLER      PIC X(55)                                            
341400     VALUE '"------------------------------------------------"Å'.         
341500                                                                          
341600   03  TACDIS-RUB-STRECK2.                                                
341700     05  FILLER      PIC X(25) VALUE '!F T N  810 1100 L 1 1 3 '.         
341800     05  FILLER      PIC X(55)                                            
341900     VALUE '"------------------------------------------------"Å'.         
342000                                                                          
342100   03  TACDIS-RUB-STRECK3.                                                
342200     05  FILLER      PIC X(25) VALUE '!F T N 1690  200 L 1 1 3 '.         
342300     05  FILLER      PIC X(55)                                            
342400     VALUE '"------------------------------------------------"Å'.         
342500                                                                          
342600   03  TACDIS-RUB-STRECK4.                                                
342700     05  FILLER      PIC X(25) VALUE '!F T N 1690 1100 L 1 1 3 '.         
342800     05  FILLER      PIC X(55)                                            
342900     VALUE '"------------------------------------------------"Å'.         
343000                                                                          
343100   03  CL7INCH-RUB-IDDEPT.                                                
343200     05  FILLER      PIC X(25) VALUE '!F T N 1100 2150 R 2 1 3 '.         
343300     05  FILLER      PIC X(7)  VALUE '"DEPT"Å'.                           
343400                                                                          
343500   03  TACDIS-RUB-IDDEPT.                                                 
343600     05  FILLER      PIC X(25) VALUE '!F T N  560 200  L 1 1 3 '.         
343700     05  FILLER      PIC X(7)  VALUE '"DEPT"Å'.                           
343800                                                                          
343900   03  TACDIS-RUB-MEKANIKERPLATS.                                         
344000     05  FILLER      PIC X(25) VALUE '!F T N  910 200  L 1 1 3 '.         
344100     05  FILLER      PIC X(17) VALUE '"MEKANIKERPLATS"Å'.                 
344200                                                                          
344300   03  TACDIS-RUB-REG-NUMMER.                                             
344400     05  FILLER      PIC X(25) VALUE '!F T N 1070 200  L 1 1 3 '.         
344500     05  FILLER      PIC X(13) VALUE '"REG NUMMER"Å'.                     
344600                                                                          
344700   03  TACDIS-RUB-FORPLOCK.                                               
344800     05  FILLER      PIC X(25) VALUE '!F T N 1470 2130 R 1 1 3 '.         
344900     05  FILLER      PIC X(11) VALUE '"FÖRPLOCK"Å'.                       
345000                                                                          
345100   03  TACDIS-RUB-KUNDINFO.                                               
345200     05  FILLER      PIC X(25) VALUE '!F T N 1060  750 L 1 1 3 '.         
345300     05  FILLER      PIC X(11) VALUE '"KUNDINFO"Å'.                       
345400                                                                          
345500   03  CL7INCH-RUB-3-3-DEPOT.                                             
345600**N  05  FILLER      PIC X(25) VALUE '!F T N 920  180  L 2 1 3 '.         
345700     05  FILLER      PIC X(25) VALUE '!F T N 1000 180  L 2 1 3 '.         
345800     05  FILLER      PIC X(08) VALUE '"DEPOT"Å'.                          
345900                                                                          
346000   03  CL7INCH-RUB-3-4-ROUTE.                                             
346100**N  05  FILLER      PIC X(25) VALUE '!F T N 920  530  L 2 1 3 '.         
346200     05  FILLER      PIC X(25) VALUE '!F T N 1000 630  L 2 1 3 '.         
346300     05  FILLER      PIC X(08) VALUE '"ROUTE"Å'.                          
346400*RFS                                                                      
346500   03  CL7INCH-RUB-REFILL-RFS.                                            
346600     05  FILLER      PIC X(25) VALUE '!F T N 1220 240  R 1 1 3 '.         
346700     05  FILLER      PIC X(6)  VALUE '"RFS"Å'.                            
346800                                                                          
346900   03  CL7INCH-RUB-4-1-RFS.                                               
347000     05  FILLER      PIC X(25) VALUE '!F T N 1180 240  R 2 1 3 '.         
347100     05  FILLER      PIC X(6)  VALUE '"RFS"Å'.                            
347200*                                              Y   X                      
347300   03  CL7INCH-RUB-LINEREF.                                               
347400     05  FILLER      PIC X(25) VALUE '!F T N 1010 310  R 2 1 3 '.         
347500     05  FILLER      PIC X(11) VALUE '"LINE REF"Å'.                       
347600*                                              Y   X                      
347700   03  CL7INCH-RUB-CUSTREF.                                               
347800     05  FILLER      PIC X(25) VALUE '!F T N 1010 1150 R 2 1 3 '.         
347900     05  FILLER      PIC X(11) VALUE '"CUST REF"Å'.                       
348000*                                              Y   X                      
348100   03  TACDIS-RUB-REP-DATUM.                                              
348200     05  FILLER      PIC X(25) VALUE '!F T N 1470  750 L 1 1 3 '.         
348300     05  FILLER      PIC X(12) VALUE '"REP DATUM"Å'.                      
348400                                                                          
348500   03  TACDIS-RUB-RFS-DATUM.                                              
348600     05  FILLER      PIC X(25) VALUE '!F T N  390 200  L 1 1 3 '.         
348700     05  FILLER      PIC X(6)  VALUE '"RFS"Å'.                            
348800*                                              Y   X                      
348900   03  CL7INCH-RUB-RFS-POST.                                              
349000     05  FILLER      PIC X(25) VALUE '!F T N 1230 240  R 2 1 3 '.         
349100     05  FILLER      PIC X(6)  VALUE '"RFS"Å'.                            
349200                                                                          
349300   03  CL7INCH-RUB-4-1-NDC.                                               
349400     05  FILLER      PIC X(25) VALUE '!F T N 1080 180  L 2 1 3 '.         
349500     05  FILLER      PIC X(11) VALUE '"RFS DATE"Å'.                       
349600                                                                          
349700   03  CL7INCH-RUB-4-3.                                                   
349800     05  FILLER      PIC X(25) VALUE '!F T N 1080 2150 R 2 1 3 '.         
349900     05  FILLER      PIC X(21) VALUE '"PRODUCTION NUMBER"Å'.              
350000                                                                          
350100   03  CL7INCH-RUB-4-3-TRANSPORT.                                         
350200     05  FILLER      PIC X(25) VALUE '!F T N 1080 1300 R 2 1 3 '.         
350300     05  FILLER      PIC X(12) VALUE '"TRANSPORT"Å'.                      
350400                                                                          
350500   03  CL7INCH-RUB-3-1-IDPSN.                                             
350600     05  FILLER      PIC X(25) VALUE '!F T N 1080 1700 R 2 1 3 '.         
350700     05  FILLER      PIC X(18)  VALUE '"DANGEROUS GOODS"Å'.               
350800                                                                          
350900   03  CL7INCH-RUB-4-1-PARTNUMBER.                                        
351000     05  FILLER      PIC X(25) VALUE '!F T N  860 2150 R 1 1 3 '.         
351100     05  FILLER      PIC X(14) VALUE '"PART NUMBER"Å'.                    
351200                                                                          
351300   03  CL7INCH-RUB-4-1-PARTNUMBER-NDC.                                    
351400     05  FILLER      PIC X(25) VALUE '!F T N  580 2150 R 1 1 3 '.         
351500     05  FILLER      PIC X(14) VALUE '"PART NUMBER"Å'.                    
351600*                                              Y    X                     
351700   03  CL7INCH-RUB-4-2-DC-WH-ADDRESS.                                     
351800     05  FILLER      PIC X(25) VALUE '!F T N 1080 1100 L 1 1 3 '.         
351900     05  FILLER      PIC X(16) VALUE '"DC WH ADDRESS"Å'.                  
352000                                                                          
352100   03  CL7INCH-RUB-4-2-DC-WH-ADRS-NDC.                                    
352200     05  FILLER      PIC X(25) VALUE '!F T N 1090 1300 L 2 1 3 '.         
352300     05  FILLER      PIC X(16) VALUE '"DC WH ADDRESS"Å'.                  
352400                                                                          
352500   03  CL7INCH-RUB-4-3-ST-ADDRESS.                                        
352600     05  FILLER      PIC X(25) VALUE '!F T N 1230 1100 L 1 1 3 '.         
352700     05  FILLER      PIC X(13) VALUE '"ST ADDRESS"Å'.                     
352800                                                                          
352900   03  CL7INCH-RUB-4-3-CARRIER.                                           
353000     05  FILLER      PIC X(25) VALUE '!F T N 1180  900 L 2 1 3 '.         
353100     05  FILLER      PIC X(10) VALUE '"CARRIER"Å'.                        
353200*IDDISTR-SE                                                               
353300   03  CL7INCH-DATA-1-1-SE.                                               
353400     05  FILLER      PIC X(25) VALUE '!F T N  230  800 R 4 4 6 '.         
353500     05  FILLER      PIC X(1)  VALUE '"'.                                 
353600     05  CL7INCH-IDDISTR-SE  PIC Z(4) VALUE ZERO.                         
353700     05  FILLER      PIC X(2)  VALUE '"Å'.                                
353800                                                                          
353900   03  CL7INCH-DATA-1-1.                                                  
354000     05  FILLER      PIC X(25) VALUE '!F T N  230  600 R 3 3 6 '.         
354100     05  FILLER      PIC X(1)  VALUE '"'.                                 
354200     05  CL7INCH-IDDISTR PIC Z(4) VALUE ZERO.                             
354300     05  FILLER      PIC X(2)  VALUE '"Å'.                                
354400                                                                          
354500   03  TACDIS-DATA-IDDISTR.                                               
354600     05  FILLER      PIC X(25) VALUE '!F T N  230  750 L 3 3 6 '.         
354700     05  FILLER      PIC X(1)  VALUE '"'.                                 
354800     05  TACDIS-IDDISTR PIC Z(4) VALUE ZERO.                              
354900     05  FILLER      PIC X(2)  VALUE '"Å'.                                
355000                                                                          
355100   03  TACDIS-DATA-IDKUNDNR.                                              
355200     05  FILLER      PIC X(25) VALUE '!F T N  230 180  L 3 3 6 '.         
355300     05  FILLER      PIC X(1)  VALUE '"'.                                 
355400     05  TACDIS-IDKUNDNR PIC Z(5)9 VALUE ZERO.                            
355500     05  FILLER      PIC X(2)  VALUE '"Å'.                                
355600*                                                 1500                    
355700   03  CL7INCH-DATA-1-2.                                                  
355800     05  FILLER      PIC X(25) VALUE '!F T N  230 1200 R 3 3 6 '.         
355900     05  FILLER      PIC X(1)  VALUE '"'.                                 
356000     05  CL7INCH-IDKUNDNR PIC Z(5)9 VALUE ZERO.                           
356100     05  FILLER      PIC X(2)  VALUE '"Å'.                                
356200                                                                          
356300   03  CL7INCH-IDKUNDNR-GRP.                                              
356400     05  FILLER      PIC X(25) VALUE '!F T N  230 1500 R 3 3 6 '.         
356500     05  FILLER      PIC X(1)  VALUE '"'.                                 
356600     05  CL7INCH-IDKUNDNR-B PIC Z(5)9 VALUE ZERO.                         
356700     05  FILLER      PIC X(2)  VALUE '"Å'.                                
356800                                                                          
356900   03  TACDIS-DATA-IDORDNR.                                               
357000     05  FILLER      PIC X(25) VALUE '!F T N  240 2140 R 3 3 6 '.         
357100     05  FILLER      PIC X(1)  VALUE '"'.                                 
357200     05  TACDIS-IDORDNR PIC Z(4)9 VALUE SPACE.                            
357300     05  FILLER      PIC X(2)  VALUE '"Å'.                                
357400                                                                          
357500   03  CL7INCH-IDORDNR-GRP.                                               
357600     05  FILLER      PIC X(25) VALUE '!F T N  230 2150 R 3 3 6 '.         
357700     05  FILLER      PIC X(1)  VALUE '"'.                                 
357800     05  CL7INCH-IDORDNR PIC Z(4)9 VALUE SPACE.                           
357900     05  FILLER      PIC X(2)  VALUE '"Å'.                                
358000                                                                          
358100   03  CL7INCH-DATA-ADRESS1.                                              
358200     05  FILLER      PIC X(25) VALUE '!F T N  410 190  L 4 3 3 '.         
358300     05  FILLER      PIC X(1)  VALUE '"'.                                 
358400     05  CL7INCH-ADRESS1 PIC X(30) VALUE SPACE.                           
358500     05  FILLER      PIC X(2)  VALUE '"Å'.                                
358600                                                                          
358700   03  CL7INCH-DATA-ADRESS2.                                              
358800     05  FILLER      PIC X(25) VALUE '!F T N  520 190  L 4 3 3 '.         
358900     05  FILLER      PIC X(1)  VALUE '"'.                                 
359000     05  CL7INCH-ADRESS2 PIC X(30) VALUE SPACE.                           
359100     05  FILLER      PIC X(2)  VALUE '"Å'.                                
359200                                                                          
359300   03  CL7INCH-DATA-ADRESS3.                                              
359400     05  FILLER      PIC X(25) VALUE '!F T N 630  190  L 4 3 3 '.         
359500     05  FILLER      PIC X(1)  VALUE '"'.                                 
359600     05  CL7INCH-ADRESS3 PIC X(30) VALUE SPACE.                           
359700     05  FILLER      PIC X(2)  VALUE '"Å'.                                
359800                                                                          
359900   03  CL7INCH-DATA-ADRESS4.                                              
360000     05  FILLER      PIC X(25) VALUE '!F T N 740  190  L 4 3 3 '.         
360100     05  FILLER      PIC X(1)  VALUE '"'.                                 
360200     05  CL7INCH-ADRESS4 PIC X(30) VALUE SPACE.                           
360300     05  FILLER      PIC X(2)  VALUE '"Å'.                                
360400                                                                          
360500   03  CL7INCH-DATA-ADRESS5.                                              
360600     05  FILLER      PIC X(25) VALUE '!F T N 830  190  L 4 3 3 '.         
360700     05  FILLER      PIC X(1)  VALUE '"'.                                 
360800     05  CL7INCH-ADRESS5 PIC X(30) VALUE SPACE.                           
360900     05  FILLER      PIC X(2)  VALUE '"Å'.                                
361000*                                                                         
361100*                                                                         
361200   03  TACDIS-BEGMT-RAD1.                                                 
361300     05  FILLER      PIC X(25) VALUE '!F T N  370 750  L 2 1 3 '.         
361400     05  FILLER      PIC X(1)  VALUE '"'.                                 
361500     05  TACD-BEGMT-RAD1 PIC X(30) VALUE SPACE.                           
361600     05  FILLER      PIC X(2)  VALUE '"Å'.                                
361700                                                                          
361800   03  TACDIS-BEGMT-RAD2.                                                 
361900     05  FILLER      PIC X(25) VALUE '!F T N  450 750  L 2 1 3 '.         
362000     05  FILLER      PIC X(1)  VALUE '"'.                                 
362100     05  TACD-BEGMT-RAD2 PIC X(30) VALUE SPACE.                           
362200     05  FILLER      PIC X(2)  VALUE '"Å'.                                
362300                                                                          
362400   03  TACDIS-ADGMT-GATA.                                                 
362500     05  FILLER      PIC X(25) VALUE '!F T N 520  750  L 2 1 3 '.         
362600     05  FILLER      PIC X(1)  VALUE '"'.                                 
362700     05  TACD-ADGMT-GATA PIC X(30) VALUE SPACE.                           
362800     05  FILLER      PIC X(2)  VALUE '"Å'.                                
362900                                                                          
363000   03  TACDIS-ADGMT-PADR.                                                 
363100     05  FILLER      PIC X(25) VALUE '!F T N 590  750  L 2 1 3 '.         
363200     05  FILLER      PIC X(1)  VALUE '"'.                                 
363300     05  TACD-ADGMT-PADR PIC X(30) VALUE SPACE.                           
363400     05  FILLER      PIC X(2)  VALUE '"Å'.                                
363500                                                                          
363600*BETEXT                                                                   
363700   03  CL7INCH-DATA-BETEXT-DC.                                            
363800     05  FILLER      PIC X(25) VALUE '!F T N 660  750  L 2 1 3 '.         
363900     05  FILLER      PIC X(1)  VALUE '"'.                                 
364000     05  CL7INCH-DATA-BETEXT-INFO-DC  PIC X(35) VALUE SPACE.              
364100     05  FILLER      PIC X(2)  VALUE '"Å'.                                
364200*                                                                         
364300   03  TACDIS-DATA-KUNDINFO1.                                             
364400     05  FILLER      PIC X(25) VALUE '!F T N 1120 750  L 2 1 3 '.         
364500     05  FILLER      PIC X(1)  VALUE '"'.                                 
364600     05  TACDIS-KUNDINFO1 PIC X(35) VALUE SPACE.                          
364700     05  FILLER      PIC X(2)  VALUE '"Å'.                                
364800                                                                          
364900   03  TACDIS-DATA-KUNDINFO2.                                             
365000     05  FILLER      PIC X(25) VALUE '!F T N 1200 750  L 2 1 3 '.         
365100     05  FILLER      PIC X(1)  VALUE '"'.                                 
365200     05  TACDIS-KUNDINFO2 PIC X(35) VALUE SPACE.                          
365300     05  FILLER      PIC X(2)  VALUE '"Å'.                                
365400                                                                          
365500   03  TACDIS-DATA-BETELNR-TACD.                                          
365600     05  FILLER      PIC X(25) VALUE '!F T N 1280 750  L 2 1 3 '.         
365700     05  FILLER      PIC X(1)  VALUE '"'.                                 
365800     05  TACDIS-BETELNR-TACD   PIC X(20) VALUE SPACE.                     
365900     05  FILLER      PIC X(2)  VALUE '"Å'.                                
366000                                                                          
366100   03  TACDIS-DATA-TETACDBO.                                              
366200     05  FILLER      PIC X(25) VALUE '!F T N 1390 750  L 2 1 3 '.         
366300     05  FILLER      PIC X(1)  VALUE '"'.                                 
366400     05  TACDIS-TETACDBO  PIC X(35) VALUE SPACE.                          
366500     05  FILLER      PIC X(2)  VALUE '"Å'.                                
366600                                                                          
366700   03  CL7INCH-DATA-2-1.                                                  
366800     05  FILLER      PIC X(25) VALUE '!F T N  410 190  L 4 3 3 '.         
366900     05  FILLER      PIC X(1)  VALUE '"'.                                 
367000     05  CL7INCH-ADRESS-1 PIC X(30) VALUE SPACE.                          
367100     05  FILLER      PIC X(2)  VALUE '"Å'.                                
367200                                                                          
367300   03  CL7INCH-DATA-2-2.                                                  
367400     05  FILLER      PIC X(25) VALUE '!F T N  500 190  L 4 3 3 '.         
367500     05  FILLER      PIC X(1)  VALUE '"'.                                 
367600     05  CL7INCH-ADRESS-2 PIC X(30) VALUE SPACE.                          
367700     05  FILLER      PIC X(2)  VALUE '"Å'.                                
367800                                                                          
367900   03  CL7INCH-DATA-2-3.                                                  
368000     05  FILLER      PIC X(25) VALUE '!F T N 590  190  L 4 3 3 '.         
368100     05  FILLER      PIC X(1)  VALUE '"'.                                 
368200     05  CL7INCH-ADRESS-3 PIC X(30) VALUE SPACE.                          
368300     05  FILLER      PIC X(2)  VALUE '"Å'.                                
368400                                                                          
368500   03  CL7INCH-DATA-2-4.                                                  
368600     05  FILLER      PIC X(25) VALUE '!F T N 740  190  L 4 3 3 '.         
368700     05  FILLER      PIC X(1)  VALUE '"'.                                 
368800     05  CL7INCH-ADRESS-4 PIC X(30) VALUE SPACE.                          
368900     05  FILLER      PIC X(2)  VALUE '"Å'.                                
369000                                                                          
369100   03  CL7INCH-ADRESS-2-5.                                                
369200     05  FILLER      PIC X(25) VALUE '!F T N 830  190  L 4 3 3 '.         
369300     05  FILLER      PIC X(1)  VALUE '"'.                                 
369400     05  CL7INCH-ADRESS-5 PIC X(30) VALUE SPACE.                          
369500     05  FILLER      PIC X(2)  VALUE '"Å'.                                
369600                                                                          
369700   03  CL7INCH-ADRESS-2-5-S08.                                            
369800     05  FILLER      PIC X(25) VALUE '!F T N 750  190  L 4 3 3 '.         
369900     05  FILLER      PIC X(1)  VALUE '"'.                                 
370000     05  CL7INCH-ADRESS-5-LDC PIC X(15) VALUE SPACE.                      
370100     05  FILLER      PIC X(2)  VALUE '"Å'.                                
370200*                                                                         
370300*BETEXT                                                                   
370400   03  CL7INCH-DATA-BETEXT-S08.                                           
370500     05  FILLER      PIC X(25) VALUE '!F T N 990  190  L 4 3 3 '.         
370600     05  FILLER      PIC X(1)  VALUE '"'.                                 
370700     05  CL7INCH-DATA-BETEXT-INF-S08   PIC X(35) VALUE SPACE.             
370800     05  FILLER      PIC X(2)  VALUE '"Å'.                                
370900*                                                                         
371000                                                                          
371100*****START* ADDRESS FIELDS FOR S08-MARKPOINT SECTION                      
371200   03  CL7INCH-ADRESS-1-GRP-S08.                                          
371300     05  FILLER      PIC X(25) VALUE '!F T N  410 190  L 4 3 3 '.         
371400     05  FILLER      PIC X(1)  VALUE '"'.                                 
371500     05  CL7INCH-ADRESS-1-SWE-S08 PIC X(20) VALUE SPACE.                  
371600*    05  CL7INCH-ADRESS-1-SWE-S08 PIC X(13) VALUE SPACE.                  
371700     05  FILLER      PIC X(2)  VALUE '"Å'.                                
371800                                                                          
371900   03  CL7INCH-ADRESS-2-GRP-S08.                                          
372000     05  FILLER      PIC X(25) VALUE '!F T N  500 190  L 4 3 3 '.         
372100     05  FILLER      PIC X(1)  VALUE '"'.                                 
372200     05  CL7INCH-ADRESS-2-SWE-S08 PIC X(30) VALUE SPACE.                  
372300     05  FILLER      PIC X(2)  VALUE '"Å'.                                
372400                                                                          
372500   03  CL7INCH-ADRESS-3-GRP-S08.                                          
372600     05  FILLER      PIC X(25) VALUE '!F T N 590  190  L 4 3 3 '.         
372700     05  FILLER      PIC X(1)  VALUE '"'.                                 
372800     05  CL7INCH-ADRESS-3-SWE-S08 PIC X(30) VALUE SPACE.                  
372900     05  FILLER      PIC X(2)  VALUE '"Å'.                                
373000                                                                          
373100   03  CL7INCH-ADRESS-4-GRP-S08.                                          
373200*    05  FILLER      PIC X(25) VALUE '!F T N 690  190  L 3 2 6 '.         
373300     05  FILLER      PIC X(25) VALUE '!F T N 770  190  L 3 2 6 '.         
373400     05  FILLER      PIC X(1)  VALUE '"'.                                 
373500     05  CL7INCH-ADRESS-4-SWE-S08 PIC X(30) VALUE SPACE.                  
373600     05  FILLER      PIC X(2)  VALUE '"Å'.                                
373700*                                                                         
373800   03  CL7INCH-ADRESS-5-GRP-S08.                                          
373900     05  FILLER      PIC X(25) VALUE '!F T N 780  190  L 4 3 3 '.         
374000     05  FILLER      PIC X(1)  VALUE '"'.                                 
374100     05  CL7INCH-ADRESS-5-SWE-S08 PIC X(30) VALUE SPACE.                  
374200     05  FILLER      PIC X(2)  VALUE '"Å'.                                
374300                                                                          
374400*  03  CL7INCH-ADRESS-5-S01-GRP.                                          
374500*    05  FILLER      PIC X(25) VALUE '!F T N 720  190  L 4 3 3 '.         
374600*    05  FILLER      PIC X(1)  VALUE '"'.                                 
374700*    05  CL7INCH-ADRESS-5-S01 PIC X(30) VALUE SPACE.                      
374800*    05  FILLER      PIC X(2)  VALUE '"Å'.                                
374900*                                                                         
375000*****START* ADDRESS FIELDS FOR SWEDISH DISTRICTS                          
375100   03  CL7INCH-ADRESS-1-GRP.                                              
375200     05  FILLER      PIC X(25) VALUE '!F T N  450 190  L 4 3 3 '.         
375300     05  FILLER      PIC X(1)  VALUE '"'.                                 
375400     05  CL7INCH-ADRESS-1-SWE PIC X(20) VALUE SPACE.                      
375500*    05  CL7INCH-ADRESS-1-SWE PIC X(13) VALUE SPACE.                      
375600     05  FILLER      PIC X(2)  VALUE '"Å'.                                
375700                                                                          
375800   03  CL7INCH-ADRESS-2-GRP.                                              
375900     05  FILLER      PIC X(25) VALUE '!F T N  550 190  L 4 3 3 '.         
376000     05  FILLER      PIC X(1)  VALUE '"'.                                 
376100     05  CL7INCH-ADRESS-2-SWE PIC X(30) VALUE SPACE.                      
376200     05  FILLER      PIC X(2)  VALUE '"Å'.                                
376300                                                                          
376400   03  CL7INCH-ADRESS-3-GRP.                                              
376500     05  FILLER      PIC X(25) VALUE '!F T N 630  190  L 4 3 3 '.         
376600     05  FILLER      PIC X(1)  VALUE '"'.                                 
376700     05  CL7INCH-ADRESS-3-SWE PIC X(30) VALUE SPACE.                      
376800     05  FILLER      PIC X(2)  VALUE '"Å'.                                
376900                                                                          
377000   03  CL7INCH-ADRESS-4-GRP.                                              
377100     05  FILLER      PIC X(25) VALUE '!F T N 830  190  L 3 2 6 '.         
377200     05  FILLER      PIC X(1)  VALUE '"'.                                 
377300     05  CL7INCH-ADRESS-4-SWE PIC X(30) VALUE SPACE.                      
377400     05  FILLER      PIC X(2)  VALUE '"Å'.                                
377500*                                                                         
377600   03  CL7INCH-ADRESS-5-GRP.                                              
377700     05  FILLER      PIC X(25) VALUE '!F T N 930  190  L 4 3 3 '.         
377800     05  FILLER      PIC X(1)  VALUE '"'.                                 
377900     05  CL7INCH-ADRESS-5-SWE PIC X(30) VALUE SPACE.                      
378000     05  FILLER      PIC X(2)  VALUE '"Å'.                                
378100                                                                          
378200   03  CL7INCH-ADRESS-5-S01-GRP.                                          
378300     05  FILLER      PIC X(25) VALUE '!F T N 720  190  L 4 3 3 '.         
378400     05  FILLER      PIC X(1)  VALUE '"'.                                 
378500     05  CL7INCH-ADRESS-5-S01 PIC X(30) VALUE SPACE.                      
378600     05  FILLER      PIC X(2)  VALUE '"Å'.                                
378700*                                                                         
378800*BETEXT                                                                   
378900   03  CL7INCH-INFO-BETEXT-S082.                                          
379000     05  FILLER      PIC X(25) VALUE '!F T N  990 190  L 4 3 3 '.         
379100     05  FILLER      PIC X(1)  VALUE '"'.                                 
379200     05  CL7INCH-DATA-BETEXT-S082    PIC X(35) VALUE SPACE.               
379300     05  FILLER      PIC X(2)  VALUE '"Å'.                                
379400*                                                                         
379500*                                                                         
379600*BETEXT                                                                   
379700   03  CL7INCH-DATA-BETEXT-GRP-S01.                                       
379800**N  05  FILLER      PIC X(25) VALUE '!F T N  820 190  L 4 3 3 '.         
379900     05  FILLER      PIC X(25) VALUE '!F T N  950 190  L 4 3 3 '.         
380000     05  FILLER      PIC X(1)  VALUE '"'.                                 
380100     05  CL7INCH-DATA-BETEXT-SWE-S01 PIC X(35) VALUE SPACE.               
380200     05  FILLER      PIC X(2)  VALUE '"Å'.                                
380300*                                                                         
380400   03  CL7INCH-IDBILREG-LDC.                                              
380500     05  FILLER      PIC X(25) VALUE '!F T N 950 1100  L 4 3 3 '.         
380600     05  FILLER      PIC X(1)  VALUE '"'.                                 
380700     05  CL7INCH-IDBILREG     PIC X(10) VALUE SPACE.                      
380800     05  FILLER      PIC X(2)  VALUE '"Å'.                                
380900                                                                          
381000*TIREPDAT-S01                                 Y    X                      
381100   03  CL7INCH-TIREPDAT-LDC-S01.                                          
381200     05  FILLER      PIC X(25) VALUE '!F T N 970  880  L 4 3 3 '.         
381300     05  FILLER      PIC X(1)  VALUE '"'.                                 
381400     05  CL7INCH-TIREPDAT-S01 PIC X(06) VALUE SPACE.                      
381500     05  FILLER      PIC X(2)  VALUE '"Å'.                                
381600                                                                          
381700*TIREPDAT-RUB                                 Y    X                      
381800   03  CL7INCH-TIREPDAT-RUB-S01.                                          
381900     05  FILLER      PIC X(25) VALUE '!F T N 880  880  L 1 1 3 '.         
382000     05  FILLER      PIC X(14) VALUE '"REPAIR DATE"Å'.                    
382100                                                                          
382200*IDBILREG-S01                                 Y    X                      
382300   03  CL7INCH-IDBILREG-LDC-S01.                                          
382400     05  FILLER      PIC X(25) VALUE '!F T N 970 1200  L 4 3 3 '.         
382500     05  FILLER      PIC X(1)  VALUE '"'.                                 
382600     05  CL7INCH-IDBILREG-S01 PIC X(10) VALUE SPACE.                      
382700     05  FILLER      PIC X(2)  VALUE '"Å'.                                
382800*                                             Y    X                      
382900   03  CL7INCH-IDBILREG-RUB-S01.                                          
383000     05  FILLER      PIC X(25) VALUE '!F T N 880 1200  L 1 1 3 '.         
383100     05  FILLER      PIC X(20) VALUE '"CAR REGISTRATION"Å'.               
383200*                                                                         
383300*BERADREF-S01                                 Y    X                      
383400   03  CL7INCH-BERADREF-GRP.                                              
383500     05  FILLER      PIC X(25) VALUE '!F T N 970  190  L 4 3 3 '.         
383600     05  FILLER      PIC X(1)  VALUE '"'.                                 
383700     05  CL7INCH-BERADREF-S01 PIC X(10) VALUE SPACE.                      
383800     05  FILLER      PIC X(2)  VALUE '"Å'.                                
383900*                                             Y    X                      
384000*BERADREF-S08                                 Y    X                      
384100   03  CL7INCH-BERADREF-GRP-S08.                                          
384200     05  FILLER      PIC X(25) VALUE '!F T N 900  190  L 4 3 3 '.         
384300     05  FILLER      PIC X(1)  VALUE '"'.                                 
384400     05  CL7INCH-BERADREF-S08 PIC X(10) VALUE SPACE.                      
384500     05  FILLER      PIC X(2)  VALUE '"Å'.                                
384600*                                             Y    X                      
384700   03  CL7INCH-BERADREF-RUB-S08.                                          
384800     05  FILLER      PIC X(25) VALUE '!F T N 810  190  L 1 1 3 '.         
384900     05  FILLER      PIC X(20) VALUE '"LINE REFERENCE"Å'.                 
385000*                                             Y    X                      
385100   03  CL7INCH-BERADREF-RUB-S01.                                          
385200     05  FILLER      PIC X(25) VALUE '!F T N 880  190  L 1 1 3 '.         
385300     05  FILLER      PIC X(20) VALUE '"LINE REFERENCE"Å'.                 
385400                                                                          
385500   03  TACDIS-DATA-IDBILREG.                                              
385600     05  FILLER      PIC X(25) VALUE '!F T N 1170 200  L 4 3 3 '.         
385700     05  FILLER      PIC X(1)  VALUE '"'.                                 
385800     05  TACDIS-IDBILREG     PIC X(10) VALUE SPACE.                       
385900     05  FILLER      PIC X(2)  VALUE '"Å'.                                
386000                                                                          
386100   03  TACDIS-DATA-IDKOLLI.                                               
386200     05  FILLER      PIC X(25) VALUE '!F T N  480 2140 R 3 3 6 '.         
386300     05  FILLER      PIC X(1)  VALUE '"'.                                 
386400     05  TACDIS-IDKOLLI PIC Z(5) VALUE ZERO.                              
386500     05  FILLER      PIC X(2)  VALUE '"Å'.                                
386600                                                                          
386700   03  CL7INCH-DATA-IDKOLLI.                                              
386800     05  FILLER      PIC X(25) VALUE '!F T N  490 2150 R 3 3 6 '.         
386900     05  FILLER      PIC X(1)  VALUE '"'.                                 
387000     05  CL7INCH-IDKOLLI PIC Z(5) VALUE ZERO.                             
387100     05  FILLER      PIC X(2)  VALUE '"Å'.                                
387200*KDFRAKT-SE                                                               
387300   03  CL7INCH-DATA-2-6-SE.                                               
387400     05  FILLER      PIC X(25) VALUE '!F T N  860 2150 R 5 5 6 '.         
387500     05  FILLER      PIC X(1)  VALUE '"'.                                 
387600     05  CL7INCH-KDFRAKT-SE    PIC Z9 VALUE ZERO.                         
387700     05  FILLER      PIC X(2)  VALUE '"Å'.                                
387800*                                                        2 1 3            
387900   03  TACDIS-DATA-KDFRAKT.                                               
388000     05  FILLER      PIC X(25) VALUE '!F T N  230 1300 L 3 3 6 '.         
388100     05  FILLER      PIC X(1)  VALUE '"'.                                 
388200     05  TACDIS-KDFRAKT PIC Z9 VALUE ZERO.                                
388300     05  FILLER      PIC X(2)  VALUE '"Å'.                                
388400                                                                          
388500   03  CL7INCH-DATA-2-6.                                                  
388600     05  FILLER      PIC X(25) VALUE '!F T N  760 2150 R 3 3 6 '.         
388700     05  FILLER      PIC X(1)  VALUE '"'.                                 
388800     05  CL7INCH-KDFRAKT PIC Z9 VALUE ZERO.                               
388900     05  FILLER      PIC X(2)  VALUE '"Å'.                                
389000*                                                                         
389100   03  CL7INCH-DATA-KDFRAKT-REFILL.                                       
389200     05  FILLER      PIC X(25) VALUE '!F T N 1490 2150 R 3 3 6 '.         
389300     05  FILLER      PIC X(1)  VALUE '"'.                                 
389400     05  CL7INCH-KDFRAKT-REFILL PIC Z9 VALUE ZERO.                        
389500     05  FILLER      PIC X(2)  VALUE '"Å'.                                
389600                                                                          
389700   03  CL7INCH-DATA-WEIGHT.                                               
389800     05  FILLER      PIC X(25) VALUE '!F T N 1120 2150 R 3 3 6 '.         
389900     05  FILLER      PIC X(1)  VALUE '"'.                                 
390000     05  CL7INCH-VKORDBTO PIC Z(5) VALUE ZERO.                            
390100     05  FILLER      PIC X(2)  VALUE '"Å'.                                
390200                                                                          
390300   03  CL7INCH-DATA-WEIGHT-GB.                                            
390400     05  FILLER      PIC X(25) VALUE '!F T N 1600 2100 R 3 3 6 '.         
390500     05  FILLER      PIC X(1)  VALUE '"'.                                 
390600     05  CL7INCH-VKORDBTO-GB   PIC Z(5) VALUE ZERO.                       
390700     05  FILLER      PIC X(2)  VALUE '"Å'.                                
390800*                                                                         
390900   03  CL7INCH-DATA-WEIGHT-REFILL.                                        
391000     05  FILLER      PIC X(25) VALUE '!F T N 1700 2150 R 3 3 6 '.         
391100     05  FILLER      PIC X(1)  VALUE '"'.                                 
391200     05  CL7INCH-VKORDBTO-REFILL  PIC Z(5) VALUE ZERO.                    
391300     05  FILLER      PIC X(2)  VALUE '"Å'.                                
391400                                                                          
391500   03  CL7INCH-DATA-WEIGHT-NDC.                                           
391600     05  FILLER      PIC X(25) VALUE '!F T N 1000 1500 R 3 3 6 '.         
391700     05  FILLER      PIC X(1)  VALUE '"'.                                 
391800     05  CL7INCH-VKORDBTO-NDC  PIC Z(5) VALUE ZERO.                       
391900     05  FILLER      PIC X(2)  VALUE '"Å'.                                
392000                                                                          
392100   03  CL7INCH-DATA-KILO-HEKTO.                                           
392200     05  FILLER      PIC X(25) VALUE '!F T N 1120 2150 R 3 3 6 '.         
392300     05  FILLER      PIC X(1)  VALUE '"'.                                 
392400     05  CL7INCH-KILO     PIC Z(4)9  VALUE ZERO.                          
392500     05  CL7INCH-PUNKT    PIC X      VALUE '.'.                           
392600     05  CL7INCH-HEKTO    PIC 9      VALUE ZERO.                          
392700     05  FILLER      PIC X(2)  VALUE '"Å'.                                
392800                                                                          
392900   03  CL7INCH-DATA-KILO-HEKTO-REFILL.                                    
393000     05  FILLER      PIC X(25) VALUE '!F T N 1700 2150 R 3 3 6 '.         
393100     05  FILLER      PIC X(1)  VALUE '"'.                                 
393200     05  CL7INCH-KILO-REFILL     PIC Z(5)   VALUE ZERO.                   
393300     05  CL7INCH-PUNKT-REFILL    PIC X      VALUE '.'.                    
393400     05  CL7INCH-HEKTO-REFILL    PIC 9      VALUE ZERO.                   
393500     05  FILLER      PIC X(2)  VALUE '"Å'.                                
393600                                                                          
393700   03  CL7INCH-DATA-KILO-HEKTO-GB.                                        
393800     05  FILLER      PIC X(25) VALUE '!F T N 1600 2150 R 3 3 6 '.         
393900     05  FILLER      PIC X(1)  VALUE '"'.                                 
394000     05  CL7INCH-KILO-GB     PIC Z(5)   VALUE ZERO.                       
394100     05  CL7INCH-PUNKT-GB    PIC X      VALUE '.'.                        
394200     05  CL7INCH-HEKTO-GB    PIC 9      VALUE ZERO.                       
394300     05  FILLER      PIC X(2)  VALUE '"Å'.                                
394400                                                                          
394500   03  CL7INCH-DATA-IDDEPT.                                               
394600     05  FILLER      PIC X(25) VALUE '!F T N 1280 2150 R 3 3 6 '.         
394700     05  FILLER      PIC X(1)  VALUE '"'.                                 
394800     05  CL7INCH-IDDEPT PIC ZZ VALUE ZERO.                                
394900     05  FILLER      PIC X(2)  VALUE '"Å'.                                
395000                                                                          
395100   03  TACDIS-DATA-IDDEPT.                                                
395200     05  FILLER      PIC X(25) VALUE '!F T N  750 200  L 3 3 6 '.         
395300     05  FILLER      PIC X(1)  VALUE '"'.                                 
395400     05  TACDIS-IDDEPT PIC ZZ VALUE ZERO.                                 
395500     05  FILLER      PIC X(2)  VALUE '"Å'.                                
395600                                                                          
395700   03  CL7INCH-DATA-RESTORDER.                                            
395800     05  FILLER      PIC X(25) VALUE '!F T N 1360 2150 R 3 3 6 '.         
395900     05  FILLER      PIC X(1)  VALUE '"'.                                 
396000     05  CL7INCH-RESTORDER PIC X(2) VALUE '  '.                           
396100     05  FILLER      PIC X(2)  VALUE '"Å'.                                
396200                                                                          
396300*                                                                         
396400   03  CL7INCH-RUB-KDORDKL2-NDC.                                          
396500     05  FILLER      PIC X(25) VALUE '!F T N 1080 2140 R 1 1 3 '.         
396600     05  FILLER      PIC X(14) VALUE '"ORDER CLASS"Å'.                    
396700*                                                                         
396800   03  CL7INCH-DATA-KDORDKL2-NDC.                                         
396900     05  FILLER      PIC X(25) VALUE '!F T N 1280 2140 R 3 3 6 '.         
397000     05  FILLER      PIC X(1)  VALUE '"'.                                 
397100     05  CL7INCH-KDORDKL2-NDC  PIC 9 VALUE ZERO.                          
397200     05  FILLER      PIC X(2)  VALUE '"Å'.                                
397300*                                                                         
397400   03  CL7INCH-RUB-KDORDKL2.                                              
397500     05  FILLER      PIC X(25) VALUE '!F T N  550 2140 R 1 1 3 '.         
397600     05  FILLER      PIC X(14) VALUE '"ORDER CLASS"Å'.                    
397700*                                                                         
397800   03  CL7INCH-DATA-KDORDKL2.                                             
397900     05  FILLER      PIC X(25) VALUE '!F T N  730 2140 R 3 3 6 '.         
398000     05  FILLER      PIC X(1)  VALUE '"'.                                 
398100     05  CL7INCH-KDORDKL2      PIC 9 VALUE ZERO.                          
398200     05  FILLER      PIC X     VALUE '/'.                                 
398300     05  FILLER      PIC X(2)  VALUE '"Å'.                                
398400                                                                          
398500   03  CL7INCH-DATA-KDORDKL-S01.                                          
398600     05  FILLER      PIC X(25) VALUE '!F T N 1360 2150 R 3 3 6 '.         
398700     05  FILLER      PIC X(1)  VALUE '"'.                                 
398800     05  CL7INCH-KDORDKL-S01   PIC 9 VALUE ZERO.                          
398900     05  FILLER      PIC X(2)  VALUE '"Å'.                                
399000                                                                          
399100   03  CL7INCH-DATA-KDORDKL-S08.                                          
399200     05  FILLER      PIC X(25) VALUE '!F T N  760 1750 R 3 3 6 '.         
399300     05  FILLER      PIC X(1)  VALUE '"'.                                 
399400     05  CL7INCH-KDORDKL-S08   PIC 9 VALUE ZERO.                          
399500     05  FILLER      PIC X     VALUE '/'.                                 
399600     05  FILLER      PIC X(2)  VALUE '"Å'.                                
399700                                                                          
399800   03  TACDIS-DATA-KDORDKL.                                               
399900     05  FILLER      PIC X(25) VALUE '!F T N  700 2150 R 3 3 6 '.         
400000     05  FILLER      PIC X(1)  VALUE '"'.                                 
400100     05  TACDIS-KDORDKL PIC 9 VALUE ZERO.                                 
400200     05  FILLER      PIC X(2)  VALUE '"Å'.                                
400300*TIRFS                                                                    
400400   03  CL7INCH-DATA-TIRFS.                                                
400500     05  FILLER      PIC X(25) VALUE '!F T N 1400  180 L 3 3 6 '.         
400600     05  FILLER      PIC X(1)  VALUE '"'.                                 
400700     05  CL7INCH-TIRFS PIC 9(6) VALUE ZERO.                               
400800     05  FILLER      PIC X(2)  VALUE '"Å'.                                
400900                                                                          
401000   03  CL7INCH-DATA-POST.                                                 
401100     05  FILLER      PIC X(25) VALUE '!F T N 1210  180 L 3 3 6 '.         
401200     05  FILLER      PIC X(1)  VALUE '"'.                                 
401300     05  CL7INCH-TIRFS-POST    PIC 9(6) VALUE ZERO.                       
401400     05  FILLER      PIC X(2)  VALUE '"Å'.                                
401500                                                                          
401600   03  TACDIS-DATA-TIRFS.                                                 
401700     05  FILLER      PIC X(25) VALUE '!F T N  500  200 L 4 3 3 '.         
401800     05  FILLER      PIC X(1)  VALUE '"'.                                 
401900     05  TACDIS-TIRFS         PIC 9(6) VALUE ZERO.                        
402000     05  FILLER      PIC X(2)  VALUE '"Å'.                                
402100*                                              Y    X                     
402200   03  TACDIS-DATA-TIREPDAT.                                              
402300     05  FILLER      PIC X(25) VALUE '!F T N 1650  750 L 3 3 6 '.         
402400     05  FILLER      PIC X(1)  VALUE '"'.                                 
402500     05  TACDIS-TIREPDAT      PIC 9(6) VALUE ZERO.                        
402600     05  FILLER      PIC X(2)  VALUE '"Å'.                                
402700*                                              Y    X                     
402800   03  TACDIS-DATA-TIHHMM.                                                
402900     05  FILLER      PIC X(25) VALUE '!F T N 1650 1500 L 3 3 6 '.         
403000     05  FILLER      PIC X(1)  VALUE '"'.                                 
403100     05  TACDIS-TIHH          PIC X(2) VALUE ZERO.                        
403200     05  FILLER               PIC X(1) VALUE ':'.                         
403300     05  TACDIS-TIMM          PIC X(2) VALUE ZERO.                        
403400     05  FILLER      PIC X(2)  VALUE '"Å'.                                
403500                                                                          
403600   03  CL7INCH-DATA-4-2.                                                  
403700     05  FILLER      PIC X(25) VALUE '!F T N 1360 900  L 3 3 6 '.         
403800     05  FILLER      PIC X(1)  VALUE '"'.                                 
403900     05  CL7INCH-ADFLGEO PIC X(3) VALUE ZERO.                             
404000     05  FILLER      PIC X(2)  VALUE '"Å'.                                
404100*POSTEN                                        Y   X                      
404200   03  CL7INCH-ADFLGEO-POSTEN.                                            
404300     05  FILLER      PIC X(25) VALUE '!F T N 1400 1000 L 3 3 6 '.         
404400     05  FILLER      PIC X(1)  VALUE '"'.                                 
404500     05  CL7INCH-ADFLGEO-POST    PIC X(3) VALUE ZERO.                     
404600     05  FILLER      PIC X(2)  VALUE '"Å'.                                
404700                                                                          
404800   03  CL7INCH-DATA-4-3.                                                  
404900     05  FILLER      PIC X(25) VALUE '!F T N 1360 1400 L 3 3 6 '.         
405000     05  FILLER      PIC X(1)  VALUE '"'.                                 
405100     05  CL7INCH-ADFLOMR PIC Z(3) VALUE ZERO.                             
405200     05  FILLER      PIC X(2)  VALUE '"Å'.                                
405300*POSTEN                                                                   
405400   03  CL7INCH-ADFLOMR-POSTEN.                                            
405500     05  FILLER      PIC X(25) VALUE '!F T N 1400 1400 L 3 3 6 '.         
405600     05  FILLER      PIC X(1)  VALUE '"'.                                 
405700     05  CL7INCH-ADFLOMR-POST  PIC Z(2) VALUE ZERO.                       
405800     05  FILLER      PIC X(2)  VALUE '"Å'.                                
405900                                                                          
406000   03  CL7INCH-DATA-4-4.                                                  
406100     05  FILLER      PIC X(25) VALUE '!F T N 1360 1700 L 3 3 6 '.         
406200     05  FILLER      PIC X(1)  VALUE '"'.                                 
406300     05  CL7INCH-ADRUTNIV PIC Z(3) VALUE ZERO.                            
406400     05  FILLER      PIC X(2)  VALUE '"Å'.                                
406500*POSTEN                                        Y    X                     
406600   03  CL7INCH-ADRUTNIV-POSTEN.                                           
406700     05  FILLER      PIC X(25) VALUE '!F T N 1400 1700 L 3 3 6 '.         
406800     05  FILLER      PIC X(1)  VALUE '"'.                                 
406900     05  CL7INCH-ADRUTNIV-POST PIC Z(2) VALUE ZERO.                       
407000     05  FILLER      PIC X(2)  VALUE '"Å'.                                
407100                                                                          
407200   03  CL7INCH-DATA-4-4-RUTNIV-SPA.                                       
407300     05  FILLER      PIC X(25) VALUE '!F T N 1360  200 L 3 3 6 '.         
407400     05  FILLER      PIC X(1)  VALUE '"'.                                 
407500     05  CL7INCH-ADRUTNIV-SPA  PIC Z(3) VALUE ZERO.                       
407600     05  FILLER      PIC X(2)  VALUE '"Å'.                                
407700                                                                          
407800   03  CL7INCH-ST-ADRESS.                                                 
407900     05  FILLER      PIC X(25) VALUE '!F T N 1400 1100 L 3 3 6 '.         
408000     05  FILLER      PIC X(1)  VALUE '"'.                                 
408100     05  CL7INCH-ADFLGEO-REFILL  PIC X(3).                                
408200     05  FILLER      PIC X(1)  VALUE SPACE.                               
408300     05  CL7INCH-ADFLOMR-REFILL  PIC Z(3)   VALUE ZERO.                   
408400     05  FILLER      PIC X(2)  VALUE '"Å'.                                
408500                                                                          
408600   03  CL7INCH-FRAKT-TEXT.                                                
408700     05  FILLER      PIC X(25) VALUE '!F T N 1360  900 L 3 3 6 '.         
408800     05  FILLER      PIC X(1)  VALUE '"'.                                 
408900     05  CL7INCH-FRAKT-TXT     PIC X(12).                                 
409000     05  FILLER      PIC X(2)  VALUE '"Å'.                                
409100                                                                          
409200   03  CL7INCH-DATA-4-7.                                                  
409300     05  FILLER      PIC X(25) VALUE '!F T N 1260 2150 R 3 3 6 '.         
409400     05  FILLER      PIC X(1)  VALUE '"'.                                 
409500     05  CL7INCH-IDPRODNR PIC Z(7) VALUE ZERO.                            
409600     05  FILLER      PIC X(2)  VALUE '"Å'.                                
409700                                                                          
409800   03  CL7INCH-TEXT-KUND-RAD-REF.                                         
409900     05  FILLER      PIC X(25) VALUE '!F T N 1260 1300 C 3 3 6 '.         
410000     05  FILLER      PIC X(1)  VALUE '"'.                                 
410100     05  CL7INCH-TXT-KUND-RAD-REF    PIC X(15).                           
410200     05  FILLER      PIC X(2)  VALUE '"Å'.                                
410300                                                                          
410400   03  CL7INCH-TEXT-VOR-INT.                                              
410500     05  FILLER      PIC X(25) VALUE '!F T N 1260  600 L 3 3 6 '.         
410600     05  FILLER      PIC X(1)  VALUE '"'.                                 
410700     05  FILLER      PIC X(11) VALUE 'VOR INT'.                           
410800     05  FILLER      PIC X(2)  VALUE '"Å'.                                
410900                                                                          
411000   03  CL7INCH-REFILL-BERADREF.                                           
411100     05  FILLER      PIC X(25) VALUE '!F T N 1260 1100 L 3 3 6 '.         
411200     05  FILLER      PIC X(1)  VALUE '"'.                                 
411300     05  CL7INCH-BERADREF      PIC X(10).                                 
411400     05  FILLER      PIC X(2)  VALUE '"Å'.                                
411500                                                                          
411600   03  CL7INCH-DATA-BERADREF.                                             
411700     05  FILLER      PIC X(25) VALUE '!F T N 1100  200 L 4 3 3 '.         
411800     05  FILLER      PIC X(1)  VALUE '"'.                                 
411900     05  CL7INCH-1478-BERADREF      PIC X(10).                            
412000     05  FILLER      PIC X(2)  VALUE '"Å'.                                
412100                                                                          
412200   03  CL7INCH-DATA-BEKUNDRF.                                             
412300     05  FILLER      PIC X(25) VALUE '!F T N 1100 1000 L 4 3 3 '.         
412400     05  FILLER      PIC X(1)  VALUE '"'.                                 
412500     05  CL7INCH-1478-BEKUNDRF     PIC X(15).                             
412600     05  FILLER      PIC X(2)  VALUE '"Å'.                                
412700                                                                          
412800   03  TACDIS-DATA-BERADREF.                                              
412900     05  FILLER      PIC X(25) VALUE '!F T N 1600  200 L 4 3 3 '.         
413000     05  FILLER      PIC X(1)  VALUE '"'.                                 
413100     05  TACDIS-BERADREF      PIC X(10).                                  
413200     05  FILLER      PIC X(2)  VALUE '"Å'.                                
413300                                                                          
413400   03  TACDIS-DATA-MEKANIKERPLATS.                                        
413500     05  FILLER      PIC X(25) VALUE '!F T N  1010 200 L 4 3 3 '.         
413600     05  FILLER      PIC X(1)  VALUE '"'.                                 
413700     05  TACDIS-BEMEKAN PIC X(18).                                        
413800     05  FILLER      PIC X(2)  VALUE '"Å'.                                
413900                                                                          
414000   03  TACDIS-DATA-FORPLOCK.                                              
414100     05  FILLER      PIC X(25) VALUE '!F T N 1650 2140 R 3 3 6 '.         
414200     05  FILLER      PIC X(1)  VALUE '"'.                                 
414300     05  TACDIS-FLFPLOCK       PIC X  VALUE 'Y'.                          
414400     05  FILLER      PIC X(2)  VALUE '"Å'.                                
414500                                                                          
414600   03  CL7INCH-TEXT-ZONA01.                                               
414700     05  FILLER      PIC X(25) VALUE '!F T N 1020 150 L 3 3 6 '.          
414800     05  FILLER      PIC X(1)  VALUE '"'.                                 
414900     05  FILLER      PIC X(4)  VALUE 'ZONA'.                              
415000     05  CL7INCH-IDZON PIC X(2).                                          
415100     05  FILLER      PIC X(2)  VALUE '"Å'.                                
415200                                                                          
415300   03  CL7INCH-TEXT-RETURN.                                               
415400     05  FILLER      PIC X(25) VALUE '!F T N 1020 150 L 3 3 6 '.          
415500     05  FILLER      PIC X(1)  VALUE '"'.                                 
415600     05  FILLER      PIC X(6)  VALUE 'RETURN'.                            
415700     05  FILLER      PIC X(2)  VALUE '"Å'.                                
415800                                                                          
415900   03  CL7INCH-REFILL-Q-NDC.                                              
416000*    05  FILLER      PIC X(25) VALUE '!F T N 1160 180 L 5 5 6 '.          
416100     05  FILLER      PIC X(25) VALUE '!F T N 1000 180 L 8 8 6 '.          
416200     05  FILLER               PIC X(1)  VALUE '"'.                        
416300     05  FILLER               PIC X(1)  VALUE 'Q'.                        
416400     05  FILLER               PIC X(2)  VALUE '"Å'.                       
416500*PCS                                                                      
416600   03  CL7INCH-REFILL-KVLEVART.                                           
416700     05  FILLER      PIC X(25) VALUE '!F T N 1200 250 L 8 4 3 '.          
416800     05  FILLER               PIC X(1)  VALUE '"'.                        
416900     05  CL7INCH-KVLEVART     PIC Z(6).                                   
417000     05  FILLER               PIC X(3)  VALUE 'PCS'.                      
417100     05  FILLER               PIC X(2)  VALUE '"Å'.                       
417200                                                                          
417300   03  CL7INCH-REFILL-KVLEVART-NDC.                                       
417400     05  FILLER      PIC X(25) VALUE '!F T N 1160 420 L 2 3 6 '.          
417500     05  FILLER               PIC X(1)  VALUE '"'.                        
417600     05  CL7INCH-KVLEVART-NDC PIC Z(6).                                   
417700     05  FILLER               PIC X(3)  VALUE 'PCS'.                      
417800     05  FILLER               PIC X(2)  VALUE '"Å'.                       
417900*                                                                         
418000   03  CL7INCH-REFILL-ARTNR.                                              
418100     05  FILLER      PIC X(25) VALUE '!F T N 1040 2150 R 3 3 6 '.         
418200     05  FILLER               PIC X(1)  VALUE '"'.                        
418300     05  CL7INCH-IDARTNR      PIC X(9).                                   
418400     05  FILLER               PIC X(2)  VALUE '"Å'.                       
418500                                                                          
418600   03  CL7INCH-REFILL-ARTNR-NDC.                                          
418700*    05  FILLER      PIC X(25) VALUE '!F T N 1030 2150 R 4 4 6 '.         
418800     05  FILLER      PIC X(25) VALUE '!F T N 1030 2150 R 8 5 6 '.         
418900     05  FILLER               PIC X(1)  VALUE '"'.                        
419000     05  CL7INCH-IDARTNR-NDC  PIC X(9).                                   
419100     05  FILLER               PIC X(2)  VALUE '"Å'.                       
419200                                                                          
419300   03  CL7INCH-REFILL-DC-WH-ADR.                                          
419400     05  FILLER      PIC X(25) VALUE '!F T N 1180 1050 L 4 3 3 '.         
419500     05  FILLER               PIC X(1)  VALUE '"'.                        
419600     05  CL7INCH-ADLAGOMR     PIC Z(3).                                   
419700     05  FILLER               PIC X(1)  VALUE SPACE.                      
419800     05  CL7INCH-ADGANG       PIC Z(3).                                   
419900     05  FILLER               PIC X(2)  VALUE SPACE.                      
420000     05  CL7INCH-ADPLATS      PIC Z(5).                                   
420100     05  FILLER               PIC X(2)  VALUE '"Å'.                       
420200                                                                          
420300   03  CL7INCH-REFILL-DC-WH-ADR-NDC.                                      
420400     05  FILLER      PIC X(25) VALUE '!F T N 1180 1250 L 4 3 3 '.         
420500     05  FILLER               PIC X(1)  VALUE '"'.                        
420600     05  CL7INCH-ADLAGOMR-NDC PIC Z(3).                                   
420700     05  FILLER               PIC X(1)  VALUE SPACE.                      
420800     05  CL7INCH-ADGANG-NDC   PIC Z(3).                                   
420900     05  FILLER               PIC X(2)  VALUE SPACE.                      
421000     05  CL7INCH-ADPLATS-NDC  PIC Z(5).                                   
421100     05  FILLER               PIC X(2)  VALUE '"Å'.                       
421200                                                                          
421300   03  TACDIS-DATA-TRPINFO.                                               
421400     05  FILLER       PIC X(30) VALUE '!F T N 1120 180 L 3 3 6 '.         
421500     05  FILLER               PIC X(1)  VALUE '"'.                        
421600     05  TACDIS-IDDEPOT      PIC X(2).                                    
421700     05  FILLER               PIC X(2)  VALUE '"Å'.                       
421800                                                                          
421900   03  CL7INCH-TRPINFO-SDC23-GB.                                          
422000     05  FILLER       PIC X(30) VALUE '!F T N 1120 300 L 3 3 6 '.         
422100     05  FILLER               PIC X(1)  VALUE '"'.                        
422200     05  CL7INCH-IDDEPOT      PIC X(2).                                   
422300     05  FILLER               PIC X(2)  VALUE SPACE.                      
422400     05  CL7INCH-IDROUTE      PIC X.                                      
422500     05  FILLER               PIC X(2)  VALUE '"Å'.                       
422600                                                                          
422700   03  CL7INCH-TEXT-SDC23-SHIPPER-CDC.                                    
422800     05  FILLER      PIC X(30) VALUE '!F T N 1700 350 L 2 1 3 '.          
422900     05  FILLER                  PIC X(1)  VALUE '"'.                     
423000     05  FILLER      PIC X(20) VALUE 'SHIPPER: VOLVO CARS '.              
423100     05  FILLER      PIC X(18) VALUE 'CUSTOMER SERVICE, '.                
423200     05  FILLER      PIC X(21) VALUE 'SE-405 31 GOTHENBURG,'.             
423300     05  FILLER      PIC X(7)  VALUE ' SWEDEN'.                           
423400     05  FILLER                  PIC X(2)  VALUE '"Å'.                    
423500                                                                          
423600   03  CL7INCH-TEXT-BEGMRK-ES.                                            
423700     05  FILLER      PIC X(30) VALUE '!F T N  950 190 L 5 3 3 '.          
423800     05  FILLER                  PIC X(1)  VALUE '"'.                     
423900     05  CL7INCH-BEGMRKTXT-RAD1  PIC X(30).                               
424000     05  FILLER                  PIC X(2)  VALUE '"Å'.                    
424100                                                                          
424200   03  CL7INCH-BARCODE.                                                   
424300     05  FILLER    PIC X(30) VALUE '!F C N 1570 350 L 140 3 12 '.         
424400     05  FILLER                  PIC X(1)  VALUE '"'.                     
424500     05  CL7INCH-DISTR           PIC 9(4).                                
424600     05  CL7INCH-KUNDNR          PIC 9(6).                                
424700     05  CL7INCH-ORDNR           PIC 9(7).                                
424800     05  CL7INCH-KOLLI           PIC 9(5).                                
424900     05  FILLER                  PIC X(2)  VALUE '"Å'.                    
425000                                                                          
425100   03  TACDIS-BARCODE.                                                    
425200     05  FILLER    PIC X(30) VALUE '!F C N 920  660 L 100 3 12 '.         
425300     05  FILLER                  PIC X(1)  VALUE '"'.                     
425400     05  TACDIS-DISTR            PIC 9(4).                                
425500     05  TACDIS-KUNDNR           PIC 9(6).                                
425600     05  TACDIS-ORDNR            PIC 9(7).                                
425700     05  TACDIS-KOLLI            PIC 9(5).                                
425800     05  FILLER                  PIC X(2)  VALUE '"Å'.                    
425900                                                                          
426000   03  CL7INCH-BARCODE-POST.                                              
426100     05  FILLER    PIC X(30) VALUE '!F C N 1420 350 L 140 3 12 '.         
426200     05  FILLER                  PIC X(1)  VALUE '"'.                     
426300     05  CL7INCH-DISTR-POST      PIC 9(4).                                
426400     05  CL7INCH-KUNDNR-POST     PIC 9(6).                                
426500     05  CL7INCH-ORDNR-POST      PIC 9(7).                                
426600     05  CL7INCH-KOLLI-POST      PIC 9(5).                                
426700     05  FILLER                  PIC X(2)  VALUE '"Å'.                    
426800                                                                          
426900   03  CL7INCH-BARCODE-LONG.                                              
427000     05  FILLER    PIC X(30) VALUE '!F C N 1570 350 L 140 3 13 '.         
427100     05  FILLER                  PIC X(1)  VALUE '"'.                     
427200     05  CL7INCH-DISTR-LONG      PIC 9(4).                                
427300     05  CL7INCH-KUNDNR-LONG     PIC 9(6).                                
427400     05  CL7INCH-ORDNR-LONG      PIC 9(7).                                
427500     05  CL7INCH-KOLLI-LONG      PIC 9(5).                                
427600     05  FILLER                  PIC X(2)  VALUE '"Å'.                    
427700                                                                          
427800   03  CL7INCH-TEXT-BELOW-BARCODE.                                        
427900     05  FILLER      PIC X(30) VALUE '!F T N 1630 350 L 2 1 3 '.          
428000     05  FILLER                  PIC X(1)  VALUE '"'.                     
428100     05  CL7INCH-DIST            PIC 9(4).                                
428200     05  CL7INCH-KUNDN           PIC 9(6).                                
428300     05  CL7INCH-ORDN            PIC 9(7).                                
428400     05  CL7INCH-KOLI            PIC 9(5).                                
428500     05  FILLER                  PIC X(2)  VALUE '"Å'.                    
428600                                                                          
428700   03  TACDIS-TEXT-BELOW-BARCODE.                                         
428800     05  FILLER      PIC X(30) VALUE '!F T N 960 2090 R 1 1 3 '.          
428900     05  FILLER                  PIC X(1)  VALUE '"'.                     
429000     05  TACDIS-DIST             PIC 9(4).                                
429100     05  TACDIS-KUNDN            PIC 9(6).                                
429200     05  TACDIS-ORDN             PIC 9(7).                                
429300     05  TACDIS-KOLI             PIC 9(5).                                
429400     05  FILLER                  PIC X(2)  VALUE '"Å'.                    
429500*POSTEN                                                                   
429600   03  CL7INCH-TXT-BLW-BARCODE-POST.                                      
429700     05  FILLER      PIC X(30) VALUE '!F T N 1470 350 L 2 1 3 '.          
429800     05  FILLER                  PIC X(1)  VALUE '"'.                     
429900     05  CL7INCH-DIST-POST       PIC 9(4).                                
430000     05  CL7INCH-KUNDN-POST      PIC 9(6).                                
430100     05  CL7INCH-ORDN-POST       PIC 9(7).                                
430200     05  CL7INCH-KOLI-POST       PIC 9(5).                                
430300     05  FILLER                  PIC X(2)  VALUE '"Å'.                    
430400                                                                          
430500   03  CL7INCH-LITEN-TEXT-SHIPPER-CDC.                                    
430600     05  FILLER      PIC X(30) VALUE '!F T N 1430 850 L 2 1 3 '.          
430700     05  FILLER                  PIC X(1)  VALUE '"'.                     
430800     05  FILLER      PIC X(20) VALUE 'SHIPPER: VOLVO CARS '.              
430900     05  FILLER      PIC X(18) VALUE 'CUSTOMER SERVICE, '.                
431000     05  FILLER      PIC X(21) VALUE 'SE-405 31 GOTHENBURG,'.             
431100     05  FILLER      PIC X(7)  VALUE ' SWEDEN'.                           
431200     05  FILLER                  PIC X(2)  VALUE '"Å'.                    
431300                                                                          
431400   03  CL7INCH-BARCODE-IT.                                                
431500     05  FILLER    PIC X(30) VALUE '!F C N 1570 350 L 140 3 12 '.         
431600     05  FILLER                  PIC X(1)  VALUE '"'.                     
431700     05  CL7INCH-DISTR-IT        PIC 9(4).                                
431800     05  CL7INCH-KUNDNR-IT       PIC 9(6).                                
431900     05  CL7INCH-ORDNR-IT        PIC 9(5).                                
432000     05  CL7INCH-KOLLI-IT        PIC 9(5).                                
432100     05  FILLER                  PIC X(2)  VALUE '"Å'.                    
432200                                                                          
432300   03  CL7INCH-TEXT-BARCODE-IT.                                           
432400     05  FILLER      PIC X(30) VALUE '!F T N 1630 350 L 2 1 3 '.          
432500     05  FILLER                  PIC X(1)  VALUE '"'.                     
432600     05  CL7INCH-DIST-IT         PIC 9(4).                                
432700     05  CL7INCH-KUNDN-IT        PIC 9(6).                                
432800     05  CL7INCH-ORDN-IT         PIC 9(5).                                
432900     05  CL7INCH-KOLI-IT         PIC 9(5).                                
433000     05  FILLER                  PIC X(2)  VALUE '"Å'.                    
433100                                                                          
433200   03  CL7INCH-BARCODE-ES.                                                
433300     05  FILLER    PIC X(30) VALUE '!F C N 1570 350 L 140 3 12 '.         
433400     05  FILLER                  PIC X(1)  VALUE '"'.                     
433500     05  CL7INCH-DISTR-ES        PIC 9(4).                                
433600     05  CL7INCH-KUNDNR-ES       PIC 9(6).                                
433700     05  CL7INCH-ORDNR-ES        PIC 9(5).                                
433800     05  CL7INCH-KOLLI-ES        PIC 9(5).                                
433900     05  CL7INCH-VKORDBTO-ES     PIC 9(6)V9.                              
434000     05  FILLER                  PIC X(2)  VALUE '"Å'.                    
434100                                                                          
434200   03  CL7INCH-TEXT-BARCODE-ES.                                           
434300     05  FILLER      PIC X(30) VALUE '!F T N 1630 350 L 2 1 3 '.          
434400     05  FILLER                  PIC X(1)  VALUE '"'.                     
434500     05  CL7INCH-DIST-ES         PIC 9(4).                                
434600     05  CL7INCH-KUNDN-ES        PIC 9(6).                                
434700     05  CL7INCH-ORDN-ES         PIC 9(5).                                
434800     05  CL7INCH-KOLI-ES         PIC 9(5).                                
434900     05  CL7INCH-VKORDBT-ES      PIC 9(6)V9.                              
435000     05  FILLER                  PIC X(2)  VALUE '"Å'.                    
435100                                                                          
435200   03  CL7INCH-TEXT-SHIPPER-CDC.                                          
435300     05  FILLER      PIC X(30) VALUE '!F T N 1690 350 L 2 1 3 '.          
435400     05  FILLER                  PIC X(1)  VALUE '"'.                     
435500     05  FILLER      PIC X(20) VALUE 'SHIPPER: VOLVO CARS '.              
435600     05  FILLER      PIC X(18) VALUE 'CUSTOMER SERVICE, '.                
435700     05  FILLER      PIC X(21) VALUE 'SE-405 31 GOTHENBURG,'.             
435800     05  FILLER      PIC X(7)  VALUE ' SWEDEN'.                           
435900     05  FILLER                  PIC X(2)  VALUE '"Å'.                    
436000                                                                          
436100   03  CL7INCH-TEXT-SHIPPER-CDC-POST.                                     
436200     05  FILLER      PIC X(30) VALUE '!F T N 1470 800 L 2 1 3 '.          
436300     05  FILLER                  PIC X(1)  VALUE '"'.                     
436400     05  FILLER      PIC X(20) VALUE 'SHIPPER: VOLVO CARS '.              
436500     05  FILLER      PIC X(18) VALUE 'CUSTOMER SERVICE, '.                
436600     05  FILLER      PIC X(21) VALUE 'SE-405 31 GOTHENBURG,'.             
436700     05  FILLER      PIC X(7)  VALUE ' SWEDEN'.                           
436800     05  FILLER                  PIC X(2)  VALUE '"Å'.                    
436900*POSTEN                                                                   
437000   03  CL7INCH-TEXT-PRODUKT.                                              
437100     05  FILLER      PIC X(25) VALUE '!F T N 1070 490  R 1 1 3 '.         
437200     05  FILLER      PIC X(10)  VALUE '"PRODUKT"Å'.                       
437300                                                                          
437400   03  CL7INCH-TEXT-PRODUKT-1090.                                         
437500     05  FILLER      PIC X(25) VALUE '!F T N 1030 390  R 2 1 3 '.         
437600     05  FILLER      PIC X(10)  VALUE '"PRODUKT"Å'.                       
437700                                                                          
437800   03  CL7INCH-REFILL-TEXT-PRODUKT.                                       
437900     05  FILLER      PIC X(25) VALUE '!F T N  640 800  R 2 1 3 '.         
438000     05  FILLER      PIC X(10)  VALUE '"PRODUKT"Å'.                       
438100                                                                          
438200   03  CL7INCH-8700-TEXT-PRODUKT.                                         
438300     05  FILLER      PIC X(25) VALUE '!F T N  890 400  R 2 1 3 '.         
438400     05  FILLER      PIC X(10)  VALUE '"PRODUKT"Å'.                       
438500                                                                          
438600   03  CL7INCH-TEXT-HIT.                                                  
438700     05  FILLER      PIC X(25) VALUE '!F T N 1180 450  R 2 1 3 '.         
438800     05  FILLER      PIC X(6)  VALUE '"HIT"Å'.                            
438900                                                                          
439000   03  CL7INCH-TEXT-HIT-1090.                                             
439100     05  FILLER      PIC X(25) VALUE '!F T N 1130 350  R 2 1 3 '.         
439200     05  FILLER      PIC X(6)  VALUE '"HIT"Å'.                            
439300                                                                          
439400   03  CL7INCH-REFILL-TEXT-HIT.                                           
439500     05  FILLER      PIC X(25) VALUE '!F T N  750 750  R 2 1 3 '.         
439600     05  FILLER      PIC X(6)  VALUE '"HIT"Å'.                            
439700                                                                          
439800   03  CL7INCH-8700-TEXT-HIT.                                             
439900     05  FILLER      PIC X(25) VALUE '!F T N  990 380  R 2 1 3 '.         
440000     05  FILLER      PIC X(6)  VALUE '"HIT"Å'.                            
440100                                                                          
440200   03  CL7INCH-TEXT-PAK.                                                  
440300     05  FILLER      PIC X(25) VALUE '!F T N 1180 450  R 2 1 3 '.         
440400     05  FILLER      PIC X(6)  VALUE '"PAK"Å'.                            
440500                                                                          
440600   03  CL7INCH-TEXT-PAK-1090.                                             
440700     05  FILLER      PIC X(25) VALUE '!F T N 1130 350  R 2 1 3 '.         
440800     05  FILLER      PIC X(6)  VALUE '"PAK"Å'.                            
440900                                                                          
441000   03  CL7INCH-REFILL-TEXT-PAK.                                           
441100     05  FILLER      PIC X(25) VALUE '!F T N  750 750  R 2 1 3 '.         
441200     05  FILLER      PIC X(6)  VALUE '"PAK"Å'.                            
441300                                                                          
441400   03  CL7INCH-8700-TEXT-PAK.                                             
441500     05  FILLER      PIC X(25) VALUE '!F T N  990 380  R 2 1 3 '.         
441600     05  FILLER      PIC X(6)  VALUE '"PAK"Å'.                            
441700                                                                          
441800   03  CL7INCH-TEXT-48.                                                   
441900     05  FILLER      PIC X(25) VALUE '!F T N 1120 440  R 2 1 3 '.         
442000     05  FILLER      PIC X(5)  VALUE '"48"Å'.                             
442100                                                                          
442200   03  CL7INCH-TEXT-48-EJ-SE.                                             
442300     05  FILLER      PIC X(25) VALUE '!F T N 1080 340  R 2 1 3 '.         
442400     05  FILLER      PIC X(5)  VALUE '"48"Å'.                             
442500                                                                          
442600   03  CL7INCH-TEXT-49-1090.                                              
442700     05  FILLER      PIC X(25) VALUE '!F T N 1080 340  R 2 1 3 '.         
442800     05  FILLER      PIC X(5)  VALUE '"49"Å'.                             
442900                                                                          
443000   03  CL7INCH-REFILL-TEXT-48.                                            
443100     05  FILLER      PIC X(25) VALUE '!F T N  700 750  R 2 1 3 '.         
443200     05  FILLER      PIC X(5)  VALUE '"48"Å'.                             
443300                                                                          
443400   03  CL7INCH-8700-TEXT-48.                                              
443500     05  FILLER      PIC X(25) VALUE '!F T N  940 380  R 2 1 3 '.         
443600     05  FILLER      PIC X(5)  VALUE '"48"Å'.                             
443700                                                                          
443800   03  CL7INCH-TEXT-69.                                                   
443900     05  FILLER      PIC X(25) VALUE '!F T N 1120 440  R 2 1 3 '.         
444000     05  FILLER      PIC X(5)  VALUE '"69"Å'.                             
444100                                                                          
444200   03  CL7INCH-TEXT-54-1090.                                              
444300     05  FILLER      PIC X(25) VALUE '!F T N 1180 340  R 2 1 3 '.         
444400     05  FILLER      PIC X(5)  VALUE '"54"Å'.                             
444500                                                                          
444600   03  CL7INCH-REFILL-TEXT-54.                                            
444700     05  FILLER      PIC X(25) VALUE '!F T N  700 750  R 2 1 3 '.         
444800     05  FILLER      PIC X(5)  VALUE '"54"Å'.                             
444900                                                                          
445000   03  CL7INCH-8700-TEXT-54.                                              
445100     05  FILLER      PIC X(25) VALUE '!F T N  940 380  R 2 1 3 '.         
445200     05  FILLER      PIC X(5)  VALUE '"54"Å'.                             
445300*POSTEN                                                                   
445400   03  CL7INCH-SORTERINGSKOD-1090.                                        
445500     05  FILLER      PIC X(25) VALUE '!F T N 1100 600  L 4 3 3 '.         
445600     05  FILLER      PIC X(1)  VALUE '"'.                                 
445700     05  CL7INCH-ADFLGEO-1090-FI   PIC X(2)  VALUE 'FI'.                  
445800     05  CL7INCH-ADFLGEO-SORT-1090 PIC X(3)  VALUE ZERO.                  
445900     05  FILLER      PIC X(2)  VALUE '"Å'.                                
446000                                                                          
446100   03  CL7INCH-SORTERINGSKOD-NO.                                          
446200     05  FILLER      PIC X(25) VALUE '!F T N 1100 600  L 4 3 3 '.         
446300     05  FILLER      PIC X(1)  VALUE '"'.                                 
446400     05  CL7INCH-ADFLGEO-NO        PIC X(2)  VALUE 'NO'.                  
446500     05  CL7INCH-ADFLGEO-SORT-NO   PIC X(3)  VALUE ZERO.                  
446600     05  FILLER      PIC X(2)  VALUE '"Å'.                                
446700                                                                          
446800   03  CL7INCH-SORTERINGSKOD.                                             
446900     05  FILLER      PIC X(25) VALUE '!F T N 1120 600  L 4 3 3 '.         
447000     05  FILLER      PIC X(1)  VALUE '"'.                                 
447100     05  CL7INCH-ADFLGEO-SE    PIC X(2) VALUE 'SE'.                       
447200     05  CL7INCH-ADFLGEO-SORT  PIC X(3) VALUE ZERO.                       
447300     05  FILLER      PIC X(2)  VALUE '"Å'.                                
447400                                                                          
447500   03  CL7INCH-REFILL-SORTERINGSKOD.                                      
447600     05  FILLER      PIC X(25) VALUE '!F T N  750 850  L 4 3 3 '.         
447700     05  FILLER      PIC X(1)  VALUE '"'.                                 
447800     05  CL7INCH-REFILL-ADFLGEO-SORT PIC X(3) VALUE ZERO.                 
447900     05  FILLER      PIC X(2)  VALUE '"Å'.                                
448000                                                                          
448100   03  CL7INCH-8700-SORTERINGSKOD.                                        
448200     05  FILLER      PIC X(25) VALUE '!F T N  930 550  L 4 3 3 '.         
448300     05  FILLER      PIC X(1)  VALUE '"'.                                 
448400     05  CL7INCH-8700-ADFLGEO-SORT PIC X(3) VALUE ZERO.                   
448500     05  FILLER      PIC X(2)  VALUE '"Å'.                                
448600*POSTEN                                                                   
448700   03  CL7INCH-BARCODE-POSTEN.                                            
448800     05  FILLER    PIC X(30) VALUE '!F C N 1120 1050 L 100 3 41'.         
448900     05  FILLER                  PIC X(1)  VALUE '"'.                     
449000     05  CL7INCH-POSTEN1         PIC X(13).                               
449100     05  FILLER                  PIC X(2)  VALUE '"Å'.                    
449200                                                                          
449300   03  CL7INCH-BARCODE-POSTEN-1090.                                       
449400     05  FILLER    PIC X(30) VALUE '!F C N 1090 1050 L 100 3 41'.         
449500     05  FILLER                  PIC X(1)  VALUE '"'.                     
449600     05  CL7INCH-POSTEN1-1090    PIC X(13).                               
449700     05  FILLER                  PIC X(2)  VALUE '"Å'.                    
449800                                                                          
449900   03  CL7INCH-REFILL-BARCODE-POSTEN.                                     
450000     05  FILLER    PIC X(30) VALUE '!F C N  750 1090 L 100 3 41'.         
450100     05  FILLER                  PIC X(1)  VALUE '"'.                     
450200     05  CL7INCH-REFILL-POSTEN1  PIC X(13).                               
450300     05  FILLER                  PIC X(2)  VALUE '"Å'.                    
450400                                                                          
450500   03  CL7INCH-8700-BARCODE-POSTEN.                                       
450600     05  FILLER    PIC X(30) VALUE '!F C N  940  800 L 100 3 41'.         
450700     05  FILLER                  PIC X(1)  VALUE '"'.                     
450800     05  CL7INCH-8700-POSTEN1    PIC X(13).                               
450900     05  FILLER                  PIC X(2)  VALUE '"Å'.                    
451000                                                                          
451100   03  CL7INCH-TEXT-POSTEN.                                               
451200     05  FILLER    PIC X(30) VALUE '!F T N 1170 1050 L 2 1 3'.            
451300     05  FILLER                  PIC X(1)  VALUE '"'.                     
451400     05  CL7INCH-POSTEN1-TEXT    PIC X(13).                               
451500     05  FILLER                  PIC X(2)  VALUE '"Å'.                    
451600                                                                          
451700   03  CL7INCH-TEXT-POSTEN-1090.                                          
451800     05  FILLER    PIC X(30) VALUE '!F T N 1140 1050 L 2 1 3'.            
451900     05  FILLER                  PIC X(1)  VALUE '"'.                     
452000     05  CL7INCH-POSTEN1-TEXT-1090 PIC X(13).                             
452100     05  FILLER                  PIC X(2)  VALUE '"Å'.                    
452200                                                                          
452300   03  CL7INCH-REFILL-TEXT-POSTEN.                                        
452400     05  FILLER    PIC X(30) VALUE '!F T N  810 1090 L 2 1 3'.            
452500     05  FILLER                      PIC X(1)  VALUE '"'.                 
452600     05  CL7INCH-REFILL-POSTEN1-TEXT PIC X(13).                           
452700     05  FILLER                      PIC X(2)  VALUE '"Å'.                
452800                                                                          
452900   03  CL7INCH-8700-TEXT-POSTEN.                                          
453000     05  FILLER    PIC X(30) VALUE '!F T N  990  800 L 2 1 3'.            
453100     05  FILLER                    PIC X(1)  VALUE '"'.                   
453200     05  CL7INCH-8700-POSTEN1-TEXT PIC X(13).                             
453300     05  FILLER                    PIC X(2)  VALUE '"Å'.                  
453400                                                                          
453500     EJECT                                                                
453600*****************************************************************         
453700*         AREA MED STYRTECKEN FÖR ZEBRA TERMO SKRIVARE.         *         
453800*        ANV. FÖR ATT SKRIVA KOLLIFLAGGA I 7INCH FORMAT I BORN. *         
453900*****************************************************************         
454000 01  FILLER                  PIC X(16)  VALUE 'ZEBRA PRINT BORN'.         
454100*    --- CASE LABEL 7INCH FORMAT FOR ZEBRA PRINTER BORN                   
454200*                                                                         
454300 01      KOLLI-FLAGGA-ZEBRA.                                              
454400   03    KF-ZEBRA-RAD            PIC X(132)   VALUE  SPACE.               
454500*                                                                         
454600*    --- STEERING LINES FOR ZEBRA                                         
454700*                                                                         
454800   03    KF-ZEBRA-STYR-01        PIC X(80)   VALUE                        
454900         '^XA^CF0^FWR^FS                                      '.          
455000   03    KF-ZEBRA-STYR-02        PIC X(80)   VALUE                        
455100         '^BY5,,                                              '.          
455200   03    KF-ZEBRA-STYR-03        PIC X(80)   VALUE                        
455300         '^XZ                                                 '.          
455400*                                                                         
455500*    --- TEXT LINES                                                       
455600*                                                                         
455700   03    KF-ZEBRA-RUB-1-1        PIC X(80)   VALUE                        
455800         '^FO1675,325^A0R,50,50^FDDISTRICT^FS                 '.          
455900   03    KF-ZEBRA-RUB-1-2        PIC X(80)   VALUE                        
456000         '^FO1675,1125^A0R,50,50^FDCUSTOMER^FS                '.          
456100   03    KF-ZEBRA-RUB-1-3        PIC X(80)   VALUE                        
456200         '^FO1675,2075^A0R,50,50^FDORDER NUMBER^FS           '.           
456300   03    KF-ZEBRA-RUB-2-1        PIC X(80)   VALUE                        
456400         '^FO1375,0075^A0R,50,50^FDADDRESS^FS                 '.          
456500   03    KF-ZEBRA-RUB-2-2        PIC X(80)   VALUE                        
456600         '^FO1375,2300^A0R,50,50^FDCASE^FS                    '.          
456700   03    KF-ZEBRA-RUB-3-1        PIC X(80)   VALUE                        
456800         '^FO1075,2100^A0R,50,50^FDFREIGHT CODE^FS            '.          
456900   03    KF-ZEBRA-RUB-4-1        PIC X(80)   VALUE                        
457000         '^FO0775,2175^A0R,50,50^FDWEIGHT KG^FS               '.          
457100   03    KF-ZEBRA-RUB-5-1        PIC X(80)   VALUE                        
457200         '^FO0475,1950^A0R,50,50^FDPRODUCTION NUMBER^FS       '.          
457300*                                                                         
457400*    --- DATA FOR LABEL                                                   
457500*                                                                         
457600   03    KF-ZEBRA-DATA-1-1.                                               
457700     05    FILLER                  PIC X(27)   VALUE                      
457800         '^FO1425,0050^A0R,250,250^FD'.                                   
457900     05    ZEBRA-IDDISTR           PIC 9(4).                              
458000     05    FILLER                  PIC X(49)   VALUE                      
458100         '^FS                       '.                                    
458200   03    KF-ZEBRA-DATA-1-2.                                               
458300     05    FILLER                  PIC X(27)   VALUE                      
458400         '^FO1425,0650^A0R,250,250^FD'.                                   
458500     05    ZEBRA-IDKUNDNR          PIC 9(6).                              
458600     05    FILLER                  PIC X(47)   VALUE                      
458700         '^FS                       '.                                    
458800   03    KF-ZEBRA-DATA-1-3.                                               
458900     05    FILLER                  PIC X(27)   VALUE                      
459000         '^FO1425,1825^A0R,250,250^FD'.                                   
459100     05    ZEBRA-IDORDNR           PIC 9(05).                             
459200     05    FILLER                  PIC X(48)   VALUE                      
459300         '^FS                       '.                                    
459400   03    KF-ZEBRA-DATA-2-1.                                               
459500     05    FILLER                  PIC X(27)   VALUE                      
459600         '^FO1240,0050^A0R,125,125^FD'.                                   
459700     05    ZEBRA-ADRESS-1          PIC X(27).                             
459800     05    FILLER                  PIC X(26)   VALUE                      
459900         '^FS                    '.                                       
460000   03    KF-ZEBRA-DATA-3-1.                                               
460100     05    FILLER                  PIC X(27)   VALUE                      
460200         '^FO1120,0050^A0R,125,125^FD'.                                   
460300     05    ZEBRA-ADRESS-2          PIC X(27).                             
460400     05    FILLER                  PIC X(26)   VALUE                      
460500         '^FS                    '.                                       
460600   03    KF-ZEBRA-DATA-4-1.                                               
460700     05    FILLER                  PIC X(27)   VALUE                      
460800         '^FO1000,0050^A0R,125,125^FD'.                                   
460900     05    ZEBRA-ADRESS-3          PIC X(27).                             
461000     05    FILLER                  PIC X(26)   VALUE                      
461100         '^FS                    '.                                       
461200   03    KF-ZEBRA-DATA-5-1.                                               
461300     05    FILLER                  PIC X(27)   VALUE                      
461400         '^FO0880,0050^A0R,125,125^FD'.                                   
461500     05    ZEBRA-ADRESS-4          PIC X(32).                             
461600     05    FILLER                  PIC X(21)   VALUE                      
461700         '^FS                 '.                                          
461800   03    KF-ZEBRA-DATA-ADRESS-5.                                          
461900     05    FILLER                  PIC X(27)   VALUE                      
462000         '^FO0760,0050^A0R,125,125^FD'.                                   
462100     05    ZEBRA-ADRESS-5          PIC X(27).                             
462200     05    FILLER                  PIC X(26)   VALUE                      
462300         '^FS                    '.                                       
462400   03    KF-ZEBRA-DATA-6-1.                                               
462500     05    FILLER                  PIC X(27)   VALUE                      
462600         '^FO1125,1950^A0R,250,250^FD'.                                   
462700     05    ZEBRA-IDKOLLI           PIC 9(04).                             
462800     05    FILLER                  PIC X(49)   VALUE                      
462900         '^FS                       '.                                    
463000   03    KF-ZEBRA-DATA-7-1.                                               
463100     05    FILLER                  PIC X(27)   VALUE                      
463200         '^FO0825,2200^A0R,250,250^FD'.                                   
463300     05    ZEBRA-KDFRAKT           PIC X(02).                             
463400     05    FILLER                  PIC X(51)   VALUE                      
463500         '^FS                       '.                                    
463600   03    KF-ZEBRA-DATA-8-1.                                               
463700     05    FILLER                  PIC X(27)   VALUE                      
463800         '^FO0525,1950^A0R,250,250^FD'.                                   
463900     05    ZEBRA-VKORDBTO          PIC 9(04).                             
464000     05    FILLER                  PIC X(49)   VALUE                      
464100         '^FS                       '.                                    
464200   03    KF-ZEBRA-DATA-9-1.                                               
464300     05    FILLER                  PIC X(27)   VALUE                      
464400         '^FO0225,0050^A0R,250,250^FD'.                                   
464500     05    ZEBRA-KDORDKL-TXT       PIC X(03).                             
464600     05    FILLER                  PIC X(50)   VALUE                      
464700         '^FS                       '.                                    
464800   03    KF-ZEBRA-DATA-9-2.                                               
464900     05    FILLER                  PIC X(27)   VALUE                      
465000         '^FO0225,0675^A0R,250,250^FD'.                                   
465100     05    ZEBRA-IDPRCVAR-TXT      PIC X(03).                             
465200     05    FILLER                  PIC X(50)   VALUE                      
465300         '^FS                       '.                                    
465400   03    KF-ZEBRA-DATA-9-3.                                               
465500     05    FILLER                  PIC X(27)   VALUE                      
465600         '^FO0225,1600^A0R,250,250^FD'.                                   
465700     05    ZEBRA-IDPRODNR          PIC 9(07).                             
465800     05    FILLER                  PIC X(46)   VALUE                      
465900         '^FS                       '.                                    
466000*                                                                         
466100*    --- DATA FOR BARCODE ON LABEL                                        
466200*                                                                         
466300   03    KF-ZEBRA-CODE-1-1.                                               
466400     05    FILLER                  PIC X(30)   VALUE                      
466500         '^FO050,82^B3N,N,200,Y,N^FWR^FD'.                                
466600     05    FILLER                  PIC X(1)    VALUE                      
466700         'D'.                                                             
466800     05    ZEBRA-CODE-IDDISTR      PIC 9(04).                             
466900     05    FILLER                  PIC X(1)    VALUE                      
467000         'K'.                                                             
467100     05    ZEBRA-CODE-IDKUNDNR     PIC 9(06).                             
467200     05    FILLER                  PIC X(1)    VALUE                      
467300         'O'.                                                             
467400     05    ZEBRA-CODE-IDORDNR      PIC 9(05).                             
467500     05    FILLER                  PIC X(1)    VALUE                      
467600         'C'.                                                             
467700     05    ZEBRA-CODE-IDKOLLI      PIC 9(04).                             
467800     05    FILLER                  PIC X(27)   VALUE                      
467900         '                          '.                                    
468000*A6ZEBRA NDC USA CANADA OCH AUSTRALIEN **************************         
468100*        AREA MED STYRTECKEN FÖR ZEBRA TERMO SKRIVARE.          *         
468200*        ANV. FÖR ATT SKRIVA KOLLIFLAGGA I A6 FORMAT I NDC    . *         
468300*****************************************************************         
468400 01  FILLER                  PIC X(16)  VALUE 'ZEBRA PRINT NDC '.         
468500*    --- CASE LABEL A6 FORMAT FOR ZEBRA PRINTER NDC 41, 42, 43,           
468600*    --- 44, 51                                                           
468700*                                                                         
468800*                                                                         
468900*    STYRTECKEN ENLIGT MANUAL: ZEBRA 170XI  USER GUIDE                    
469000*                                                                         
469100 01      KOLLI-FLAGGA-ZEBRA.                                              
469200   03    A6-ZEBRA-RAD            PIC X(132)   VALUE  SPACE.               
469300*                                                                         
469400*    --- STEERING LINES FOR ZEBRA                                         
469500*                                                                         
469600   03    A6-ZEBRA-STYR-01        PIC X(80)   VALUE                        
469700         '^XA^CF0^FWR^FS                                      '.          
469800   03    A6-ZEBRA-STYR-02        PIC X(80)   VALUE                        
469900         '^BY2,,                                              '.          
470000   03    A6-ZEBRA-STYR-03        PIC X(80)   VALUE                        
470100         '^XZ                                                 '.          
470200*                                                                         
470300   03    A6-ZEBRA-RUB-DISTRICT      PIC X(80)   VALUE                     
470400         '^FO0810,0070^A0R,0025,0030^FDDISTRICT^FS             '.         
470500   03    A6-ZEBRA-RUB-RETAILER      PIC X(80)   VALUE                     
470600         '^FO0810,0400^A0R,0025,0030^FDRETAILER^FS             '.         
470700   03    A6-ZEBRA-RUB-DEALER        PIC X(80)   VALUE                     
470800         '^FO0810,0400^A0R,0025,0030^FDDEALER^FS               '.         
470900   03    A6-ZEBRA-RUB-ORDER-NUMBER  PIC X(80)   VALUE                     
471000         '^FO0810,0700^A0R,0025,0030^FDORDER NUMBER^FS         '.         
471100   03    A6-ZEBRA-RUB-CASE-NUMBER   PIC X(80)   VALUE                     
471200         '^FO0810,1000^A0R,0025,0030^FDCASE NUMBER^FS          '.         
471300   03    A6-ZEBRA-RUB-CONSIGNEE     PIC X(80)   VALUE                     
471400         '^FO0690,0070^A0R,0025,0030^FDCONSIGNEE^FS            '.         
471500   03    A6-ZEBRA-RUB-RFS           PIC X(80)   VALUE                     
471600         '^FO0690,1000^A0R,0025,0030^FDRFS^FS                  '.         
471700   03    A6-ZEBRA-RUB-FREIGHT-CODE  PIC X(80)   VALUE                     
471800         '^FO0570,1000^A0R,0025,0030^FDFREIGHT CODE^FS         '.         
471900   03    A6-ZEBRA-RUB-TRANSPORT     PIC X(80)   VALUE                     
472000         '^FO0450,1000^A0R,0025,0030^FDTRANSPORT^FS            '.         
472100   03    A6-ZEBRA-RUB-SHIPPER       PIC X(80)   VALUE                     
472200         '^FO0380,0070^A0R,0025,0030^FDSHIPPER^FS              '.         
472300   03    A6-ZEBRA-RUB-ORDER-TYPE    PIC X(80)   VALUE                     
472400         '^FO0340,1000^A0R,0025,0030^FDORDER TYPE^FS           '.         
472500   03    A6-ZEBRA-RUB-WEIGHT        PIC X(80)   VALUE                     
472600         '^FO0300,0600^A0R,0025,0030^FDWEIGHT^FS               '.         
472700   03    A6-ZEBRA-RUB-PICKER        PIC X(80)   VALUE                     
472800         '^FO0210,0600^A0R,0025,0030^FDPICKER^FS               '.         
472900   03    A6-ZEBRA-RUB-DANGEROUS     PIC X(80)   VALUE                     
473000         '^FO0190,0900^A0R,0025,0030^FDDANGEROUS GOODS^FS      '.         
473100*                                                                         
473200*    --- DATA FOR LABEL                                                   
473300*                                                                         
473400   03    A6-ZEBRA-DATA-IDDISTR.                                           
473500     05    FILLER                  PIC X(29)   VALUE                      
473600         '^FO0730,0070^A0R,0070,0060^FD'.                                 
473700     05    A6-ZEBRA-IDDISTR        PIC 9(4).                              
473800     05    FILLER                  PIC X(47)   VALUE                      
473900         '^FS                       '.                                    
474000   03    A6-ZEBRA-DATA-IDKUNDNR.                                          
474100     05    FILLER                  PIC X(29)   VALUE                      
474200         '^FO0730,0400^A0R,0070,0060^FD'.                                 
474300     05    A6-ZEBRA-IDKUNDNR       PIC Z(6)9.                             
474400     05    FILLER                  PIC X(47)   VALUE                      
474500         '^FS                       '.                                    
474600   03    A6-ZEBRA-DATA-IDORDNR.                                           
474700     05    FILLER                  PIC X(29)   VALUE                      
474800         '^FO0730,0700^A0R,0070,0060^FD'.                                 
474900     05    A6-ZEBRA-IDORDNR        PIC Z(05)9.                            
475000     05    FILLER                  PIC X(46)   VALUE                      
475100         '^FS                       '.                                    
475200*                                                                         
475300   03    A6-ZEBRA-DATA-IDKOLLI.                                           
475400     05    FILLER                  PIC X(29)   VALUE                      
475500         '^FO0730,1000^A0R,0070,0060^FD'.                                 
475600     05    A6-ZEBRA-IDKOLLI        PIC Z(05)9.                            
475700     05    FILLER                  PIC X(46)   VALUE                      
475800         '^FS                       '.                                    
475900*                                                                         
476000   03    A6-ZEBRA-DATA-TIRFS.                                             
476100     05    FILLER                  PIC X(29)   VALUE                      
476200         '^FO0610,1000^A0R,0070,0060^FD'.                                 
476300     05    A6-ZEBRA-TIRFS          PIC 9(06).                             
476400     05    FILLER                  PIC X(46)   VALUE                      
476500         '^FS                       '.                                    
476600*                                                                         
476700   03    A6-ZEBRA-DATA-KDFRAKT.                                           
476800     05    FILLER                  PIC X(29)   VALUE                      
476900         '^FO0490,1000^AOR,0070,0060^FD'.                                 
477000     05    A6-ZEBRA-KDFRAKT        PIC Z(05)9.                            
477100     05    FILLER                  PIC X(46)   VALUE                      
477200         '^FS                       '.                                    
477300*                                                                         
477400   03    A6-ZEBRA-DATA-ADRESS-1.                                          
477500     05    FILLER                  PIC X(29)   VALUE                      
477600         '^FO0640,0070^A0R,0040,0050^FD'.                                 
477700     05    A6-ZEBRA-ADRESS-1       PIC X(27).                             
477800     05    FILLER                  PIC X(46)   VALUE                      
477900         '^FS                    '.                                       
478000*                                                                         
478100   03    A6-ZEBRA-DATA-ADRESS-2.                                          
478200     05    FILLER                  PIC X(29)   VALUE                      
478300         '^FO0590,0070^A0R,0040,0050^FD'.                                 
478400     05    A6-ZEBRA-ADRESS-2       PIC X(27).                             
478500     05    FILLER                  PIC X(24)   VALUE                      
478600         '^FS                    '.                                       
478700*                                                                         
478800   03    A6-ZEBRA-DATA-ADRESS-3.                                          
478900     05    FILLER                  PIC X(29)   VALUE                      
479000         '^FO0540,0070^A0R,0040,0050^FD'.                                 
479100     05    A6-ZEBRA-ADRESS-3       PIC X(27).                             
479200     05    FILLER                  PIC X(24)   VALUE                      
479300         '^FS                    '.                                       
479400*                                                                         
479500   03    A6-ZEBRA-DATA-ADRESS-4.                                          
479600     05    FILLER                  PIC X(29)   VALUE                      
479700         '^FO0490,0070^A0R,0040,0050^FD'.                                 
479800     05    A6-ZEBRA-ADRESS-4       PIC X(32).                             
479900     05    FILLER                  PIC X(19)   VALUE                      
480000         '^FS               '.                                            
480100*                                                                         
480200   03    A6-ZEBRA-DATA-ADRESS-5.                                          
480300     05    FILLER                  PIC X(29)   VALUE                      
480400         '^FO0440,0070^A0R,0040,0050^FD'.                                 
480500     05    A6-ZEBRA-ADRESS-5       PIC X(27).                             
480600     05    FILLER                  PIC X(24)   VALUE                      
480700         '^FS                    '.                                       
480800*                                                                         
480900   03    A6-ZEBRA-DATA-ORDER-TYPE.                                        
481000     05    FILLER                  PIC X(29)   VALUE                      
481100         '^FO0220,0900^A0R,0100,0080^FD'.                                 
481200     05    A6-ZEBRA-ORDER-TYPE     PIC X(08).                             
481300     05    FILLER                  PIC X(43)   VALUE                      
481400         '^FS                       '.                                    
481500*                                                                         
481600   03    A6-ZEBRA-DATA-ORDER-TYPE-JP.                                     
481700     05    FILLER                  PIC X(29)   VALUE                      
481800         '^FO0220,1100^A0R,0100,0080^FD'.                                 
481900     05    A6-ZEBRA-ORDER-TYPE-JP  PIC X(08).                             
482000     05    FILLER                  PIC X(43)   VALUE                      
482100         '^FS                       '.                                    
482200*                                                                         
482300   03    A6-ZEBRA-DATA-VKORDBTO.                                          
482400     05    FILLER                  PIC X(29)   VALUE                      
482500         '^FO0260,0700^A0R,0070,0060^FD'.                                 
482600     05    A6-ZEBRA-VKORDBTO       PIC Z(05).                             
482700     05    FILLER                  PIC X(47)   VALUE                      
482800         '^FS                       '.                                    
482900*                                                                         
483000   03  A6-ZEBRA-DATA-WEIGHT.                                              
483100     05    FILLER                  PIC X(29)   VALUE                      
483200         '^FO0260,0700^A0R,0070,0060^FD'.                                 
483300     05    A6-ZEBRA-KILO PIC Z(5).                                        
483400     05    A6-ZEBRA-PUNKT PIC X   VALUE '.'.                              
483500     05    A6-ZEBRA-HEKTO PIC Z   VALUE ZERO.                             
483600     05    FILLER                  PIC X(44)   VALUE                      
483700         '^FS                       '.                                    
483800*                                                                         
483900   03  A6-ZEBRA-DATA-IDTRPTNR.                                            
484000     05    FILLER                  PIC X(29)   VALUE                      
484100         '^FO0380,1000^A0R,0070,0060^FD'.                                 
484200     05    A6-ZEBRA-IDTRPTNR       PIC Z(05).                             
484300     05    FILLER                  PIC X(46)   VALUE                      
484400         '^FS                       '.                                    
484500*                                                                         
484600   03  A6-ZEBRA-DATA-IDPSN.                                               
484700     05    FILLER                  PIC X(29)   VALUE                      
484800         '^FO0090,1050^A0R,0140,0160^FD'.                                 
484900     05    A6-ZEBRA-IDPSN          PIC 9(05).                             
485000     05    FILLER                  PIC X(46)   VALUE                      
485100         '^FS                       '.                                    
485200*                                                                         
485300   03  A6-ZEBRA-DATA-H.                                                   
485400     05    FILLER                  PIC X(29)   VALUE                      
485500         '^FO0030,1000^A0R,0140,0160^FD'.                                 
485600     05    A6-ZEBRA-H              PIC X.                                 
485700     05    FILLER                  PIC X(50)   VALUE                      
485800         '^FS                       '.                                    
485900*SHIPPER                                                                  
486000   03  A6-ZEBRA-DATA-SHIPPER.                                             
486100     05    FILLER                  PIC X(29)   VALUE                      
486200         '^FO0340,0070^A0R,0030,0030^FD'.                                 
486300     05  A6-ZEBRA-SHIPPER          PIC X(32)   VALUE ZERO.                
486400     05    FILLER                  PIC X(19)   VALUE                      
486500         '^FS'.                                                           
486600*                                                                         
486700   03  A6-ZEBRA-DATA-SHIPPER-COMPANY.                                     
486800     05    FILLER                  PIC X(29)   VALUE                      
486900         '^FO0340,0070^A0R,0030,0030^FD'.                                 
487000     05  A6-ZEBRA-SHIPPER-COMPANY  PIC X(32)   VALUE ZERO.                
487100     05    FILLER                  PIC X(19)   VALUE                      
487200         '^FS'.                                                           
487300*                                                                         
487400   03  A6-ZEBRA-DATA-SHIPPER-NAME.                                        
487500     05    FILLER                  PIC X(29)   VALUE                      
487600         '^FO0300,0070^A0R,0030,0030^FD'.                                 
487700     05  A6-ZEBRA-SHIPPER-NAME     PIC X(32)   VALUE ZERO.                
487800     05    FILLER                  PIC X(19)   VALUE                      
487900         '^FS'.                                                           
488000*                                                                         
488100   03  A6-ZEBRA-DATA-SHIPPER-STREET.                                      
488200     05    FILLER                  PIC X(29)   VALUE                      
488300         '^FO0260,0070^A0R,0030,0030^FD'.                                 
488400     05  A6-ZEBRA-SHIPPER-STREET   PIC X(32)   VALUE ZERO.                
488500     05    FILLER                  PIC X(19)   VALUE                      
488600         '^FS'.                                                           
488700*                                                                         
488800   03  A6-ZEBRA-DATA-SHIPPER-CITY.                                        
488900     05    FILLER                  PIC X(29)   VALUE                      
489000         '^FO0220,0070^A0R,0030,0030^FD'.                                 
489100     05  A6-ZEBRA-SHIPPER-CITY     PIC X(32)   VALUE ZERO.                
489200     05    FILLER                  PIC X(19)   VALUE                      
489300         '^FS'.                                                           
489400*                                                                         
489500   03  A6-ZEBRA-DATA-SHIPPER-COUNTRY.                                     
489600     05    FILLER                  PIC X(29)   VALUE                      
489700         '^FO0180,0070^A0R,0030,0030^FD'.                                 
489800     05  A6-ZEBRA-SHIPPER-COUNTRY  PIC X(32)   VALUE ZERO.                
489900     05    FILLER                  PIC X(19)   VALUE                      
490000         '^FS'.                                                           
490100*                                                                         
490200   03  A6-ZEBRA-DATA-IDPLOCK.                                             
490300     05    FILLER                  PIC X(29)   VALUE                      
490400         '^FO0170,0700^A0R,0070,0060^FD'.                                 
490500     05    A6-ZEBRA-IDPLOCK        PIC Z(07).                             
490600     05    FILLER                  PIC X(34)   VALUE                      
490700         '^FS                       '.                                    
490800*                                                                         
490900*                                                                         
491000*    --- DATA FOR BARCODE ON LABEL                                        
491100*                                                                         
491200   03    A6-ZEBRA-BARCODE.                                                
491300     05    FILLER                  PIC X(33)   VALUE                      
491400         '^FO0050,0200^B3R,N,100,Y,N^FWR^FD'.                             
491500     05    A6-ZEBRA-CODE-IDDISTR      PIC 9(04).                          
491600     05    A6-ZEBRA-CODE-IDKUNDNR     PIC 9(06).                          
491700     05    A6-ZEBRA-CODE-IDORDNR      PIC 9(07).                          
491800     05    A6-ZEBRA-CODE-IDKOLLI      PIC 9(05).                          
491900     05    FILLER                     PIC X(25)   VALUE                   
492000         '^FS'.                                                           
492100*                                                                         
492200   03    A6-ZEBRA-BARCODE-JAPAN.                                          
492300     05    FILLER                  PIC X(33)   VALUE                      
492400         '^FO0050,0150^B3R,N,100,Y,N^FWR^FD'.                             
492500     05    A6-ZEBRA-JAPAN-IDKUNDNR    PIC 9(06).                          
492600     05    A6-ZEBRA-JAPAN-KDORDKL     PIC 9(01).                          
492700     05    A6-ZEBRA-JAPAN-VKORDBTO    PIC 9(5)V9.                         
492800     05    A6-ZEBRA-JAPAN-VLORDBTO    PIC 9(1)V9(3).                      
492900     05    A6-ZEBRA-JAPAN-IDORDNR     PIC 9(05).                          
493000     05    A6-ZEBRA-JAPAN-IDKOLLI     PIC 9(05).                          
493100     05    A6-ZEBRA-JAPAN-IDTRPTNR    PIC 9(03).                          
493200     05    FILLER                     PIC X(27)   VALUE                   
493300         '^FS'.                                                           
493400*                                                                         
493410*                                                                         
493500*A6 ZEBRA NDC  END OF US CASELABEL ******************************         
493600*                                                                         
493700*                                                                         
493800*A6LASER NDC-JAPAN **********************************************         
493900*    AREA MED STYRTECKEN FÖR LASER SKRIVARE.                    *         
494000*    ANV. FÖR ATT SKRIVA KOLLIFLAGGA I A6 FORMAT I NDC JAPAN.   *         
494100*****************************************************************         
494200 01  FILLER                  PIC X(16)  VALUE 'LASERPRT JAP NDC'.         
494300*    --- CASE LABEL A6 FORMAT FOR LASER PRINTER NDC 61, 62.               
494400*                                                                         
494500*    STYRTECKEN FÖR FONT OCH STORLEK ENL. BLANKETTDEFINITION              
494600*  W40301 PÅ W.XXXX.ZZZZZ                                                 
494700*                                                                         
494800 01      KOLLI-FLAGGA-LASER.                                              
494900   03    A6-LASER-RAD            PIC X(132)   VALUE  SPACE.               
495000*                                                                         
495100*    --- STEERING LINES FOR LASER                                         
495200*                                                                         
495300   03    A6-LASER-RUB-DISTRICT      PIC X(80)   VALUE                     
495400         'DISTRICT                                              '.        
495500   03    A6-LASER-RUB-RETAILER      PIC X(80)   VALUE                     
495600         'RETAILER                                              '.        
495700   03    A6-LASER-RUB-ORDER-NUMBER  PIC X(80)   VALUE                     
495800         'ORDER NUMBER                                          '.        
495900   03    A6-LASER-RUB-CASE-NUMBER   PIC X(80)   VALUE                     
496000         'CASE NUMBER                                           '.        
496100   03    A6-LASER-RUB-CONSIGNEE     PIC X(80)   VALUE                     
496200         'CONSIGNEE                                             '.        
496300   03    A6-LASER-RUB-RFS           PIC X(80)   VALUE                     
496400         'RFS                                                   '.        
496500   03    A6-LASER-RUB-FREIGHT-CODE  PIC X(80)   VALUE                     
496600         'FREIGHT CODE                                          '.        
496700   03    A6-LASER-RUB-TRANSPORT     PIC X(80)   VALUE                     
496800         'TRANSPORT                                             '.        
496900   03    A6-LASER-RUB-SHIPPER       PIC X(80)   VALUE                     
497000         'SHIPPER                                               '.        
497100   03    A6-LASER-RUB-ORDER-TYPE    PIC X(80)   VALUE                     
497200         'ORDER TYPE                                            '.        
497300   03    A6-LASER-RUB-WEIGHT        PIC X(80)   VALUE                     
497400         'WEIGHT                                                '.        
497500   03    A6-LASER-RUB-DANGEROUS     PIC X(80)   VALUE                     
497600         'DANGEROUS                                             '.        
497700*                                                                         
497800*    --- DATA FOR LABEL                                                   
497900*                                                                         
498000   03    A6-LASER-DATA-IDDISTR.                                           
498100     05    A6-LASER-IDDISTR        PIC 9(4).                              
498200     05    FILLER                  PIC X(76)   VALUE SPACE.               
498300*                                                                         
498400   03    A6-LASER-DATA-IDKUNDNR.                                          
498500     05    A6-LASER-IDKUNDNR       PIC Z(6)9.                             
498600     05    FILLER                  PIC X(73)   VALUE SPACE.               
498700*                                                                         
498800   03    A6-LASER-DATA-IDORDNR.                                           
498900     05    A6-LASER-IDORDNR        PIC Z(05)9.                            
499000     05    FILLER                  PIC X(74)   VALUE SPACE.               
499100*                                                                         
499200   03    A6-LASER-DATA-IDKOLLI.                                           
499300     05    A6-LASER-IDKOLLI        PIC Z(05)9.                            
499400     05    FILLER                  PIC X(74)   VALUE SPACE.               
499500*                                                                         
499600   03    A6-LASER-DATA-TIRFS.                                             
499700     05    A6-LASER-TIRFS          PIC 9(06).                             
499800     05    FILLER                  PIC X(74)   VALUE SPACE.               
499900*                                                                         
500000   03    A6-LASER-DATA-KDFRAKT.                                           
500100     05    A6-LASER-KDFRAKT        PIC Z(05)9.                            
500200     05    FILLER                  PIC X(74)   VALUE SPACE.               
500300*                                                                         
500400   03    A6-LASER-DATA-ADRESS-1.                                          
500500     05    A6-LASER-ADRESS-1       PIC X(35).                             
500600     05    FILLER                  PIC X(45)   VALUE SPACE.               
500700*                                                                         
500800   03    A6-LASER-DATA-ADRESS-2.                                          
500900     05    A6-LASER-ADRESS-2       PIC X(35).                             
501000     05    FILLER                  PIC X(45)   VALUE SPACE.               
501100*                                                                         
501200   03    A6-LASER-DATA-ADRESS-3.                                          
501300     05    A6-LASER-ADRESS-3       PIC X(35).                             
501400     05    FILLER                  PIC X(45)   VALUE SPACE.               
501500*                                                                         
501600   03    A6-LASER-DATA-ADRESS-4.                                          
501700     05    A6-LASER-ADRESS-4       PIC X(35).                             
501800     05    FILLER                  PIC X(45)   VALUE SPACE.               
501900*                                                                         
502000   03    A6-LASER-DATA-ADRESS-5.                                          
502100     05    A6-LASER-ADRESS-5       PIC X(35).                             
502200     05    FILLER                  PIC X(45)   VALUE SPACE.               
502300*                                                                         
502400   03    A6-LASER-DATA-ORDER-TYPE.                                        
502500     05    A6-LASER-ORDER-TYPE     PIC X(03).                             
502600     05    FILLER                  PIC X(77)   VALUE SPACE.               
502700*                                                                         
502800   03    A6-LASER-DATA-VKORDBTO.                                          
502900     05    A6-LASER-VKORDBTO       PIC Z(05).                             
503000     05    FILLER                  PIC X(75)   VALUE SPACE.               
503100*                                                                         
503200   03  A6-LASER-DATA-WEIGHT.                                              
503300     05    A6-LASER-KILO PIC Z(4)9.                                       
503400     05    A6-LASER-PUNKT PIC X   VALUE '.'.                              
503500     05    A6-LASER-HEKTO PIC 9   VALUE ZERO.                             
503600     05    FILLER                  PIC X(73)   VALUE SPACE.               
503700*                                                                         
503800   03  A6-LASER-DATA-IDTRPTNR.                                            
503900     05    A6-LASER-IDTRPTNR       PIC Z(05).                             
504000     05    FILLER                  PIC X(75)   VALUE SPACE.               
504100*SHIPPER                                                                  
504200   03  A6-LASER-DATA-SHIPPER.                                             
504300     05  A6-LASER-SHIPPER          PIC X(32)   VALUE ZERO.                
504400     05    FILLER                  PIC X(48)   VALUE SPACE.               
504500*                                                                         
504600   03  A6-LASER-DATA-SHIPPER-COMPANY.                                     
504700     05  A6-LASER-SHIPPER-COMPANY  PIC X(32)   VALUE ZERO.                
504800     05    FILLER                  PIC X(48)   VALUE SPACE.               
504900*                                                                         
505000   03  A6-LASER-DATA-SHIPPER-NAME.                                        
505100     05  A6-LASER-SHIPPER-NAME     PIC X(32)   VALUE ZERO.                
505200     05    FILLER                  PIC X(18)   VALUE SPACE.               
505300*                                                                         
505400   03  A6-LASER-DATA-SHIPPER-STREET.                                      
505500     05  A6-LASER-SHIPPER-STREET   PIC X(32)   VALUE ZERO.                
505600     05    FILLER                  PIC X(18)   VALUE SPACE.               
505700*                                                                         
505800   03  A6-LASER-DATA-SHIPPER-CITY.                                        
505900     05  A6-LASER-SHIPPER-CITY     PIC X(32)   VALUE ZERO.                
506000     05    FILLER                  PIC X(18)   VALUE SPACE.               
506100*                                                                         
506200   03  A6-LASER-DATA-SHIPPER-COUNTRY.                                     
506300     05  A6-LASER-SHIPPER-COUNTRY  PIC X(32)   VALUE ZERO.                
506400     05    FILLER                  PIC X(18)   VALUE SPACE.               
506500*                                                                         
506600*                                                                         
506700*    --- DATA FOR BARCODE ON LABEL                                        
506800*                                                                         
506900   03  A6-LASER-BARCODE.                                                  
507000     05  A6-LASER-CODE-IDDISTR       PIC X(04).                           
507100     05  A6-LASER-CODE-IDKUNDNR      PIC X(06).                           
507200     05  A6-LASER-CODE-IDORDNR       PIC X(05).                           
507300     05  A6-LASER-CODE-IDKOLLI       PIC X(05).                           
507400     05  FILLER                      PIC X(60)   VALUE SPACE.             
507500*                                                                         
507600   03  A6-LASER-BARCODE-TXT.                                              
507700     05  A6-LASER-CODE-IDDISTR-TXT   PIC X(04).                           
507800     05  A6-LASER-CODE-IDKUNDNR-TXT  PIC X(06).                           
507900     05  A6-LASER-CODE-IDORDNR-TXT   PIC X(05).                           
508000     05  A6-LASER-CODE-IDKOLLI-TXT   PIC X(05).                           
508100     05  FILLER                      PIC X(60)   VALUE SPACE.             
508200                                                                          
508300*A6 LASER NDC  END **********************************************         
508400******************************************************************        
508500 01  FILLER                     PIC X(16) VALUE 'DISTR-COPY-TEXT'.        
508600 01     TEST-IDDISTR            PIC 9(5)              COMP-3.             
508700 01     FILLER REDEFINES TEST-IDDISTR.                                    
508800*  03   -COPY WWDIST03.                                                   
508900     SKIP2                                                                
509000 01     FILLER REDEFINES TEST-IDDISTR.                                    
509100*  03   -COPY WWDIST05.                                                   
509200     SKIP2                                                                
509300 01     FILLER REDEFINES TEST-IDDISTR.                                    
509400*  03   -COPY WWDIST13.                                                   
509500     SKIP2                                                                
509600 01     FILLER REDEFINES TEST-IDDISTR.                                    
509700*  03   -COPY WWDIST14.                                                   
509800     SKIP2                                                                
509900 01     FILLER REDEFINES TEST-IDDISTR.                                    
510000*  03   -COPY WWDIST18.                                                   
510100     SKIP2                                                                
510200 01     FILLER REDEFINES TEST-IDDISTR.                                    
510300*  03   -COPY WWDIST30                                                    
510400     SKIP2                                                                
510500 01     FILLER REDEFINES TEST-IDDISTR.                                    
510600*  03   -COPY WWDIST34                                                    
510700     SKIP2                                                                
510800 01     FILLER REDEFINES TEST-IDDISTR.                                    
510900*  03   -COPY WWDIST35                                                    
511000     SKIP2                                                                
511100 01     FILLER REDEFINES TEST-IDDISTR.                                    
511200*  03   -COPY WWDIST83                                                    
511300     SKIP2                                                                
511400 01  FILLER                      PIC X(16)  VALUE 'REFILLTABDC'.          
511500*   -COPY WWDIST57                                                        
511600                                                                          
511700     SKIP2                                                                
511800 01  FILLER                      PIC X(16)  VALUE 'Q-MARKING'.            
511900*   -COPY WWDIST75                                                        
512000     SKIP2                                                                
512100                                                                          
512200 01  FILLER                      PIC X(16) VALUE 'TRPINFOPOSTEN'.         
512300*   -COPY WWTRP01                                                         
512400     SKIP2                                                                
512500                                                                          
512600 01  FILLER                      PIC X(16)  VALUE '2-FLAGGOR'.            
512700*   -COPY WWDIST29                                                        
512800     EJECT                                                                
512900 01  FILLER                      PIC X(08)  VALUE 'LDC-AKUT'.             
513000*   -COPY WWKUND12                                                        
513100     EJECT                                                                
513200                                                                          
513300*01    -COPY WWDC99              -PRE DIST-KUND-                          
513400                                                                          
513500******************************************************************        
513600 01  FILLER                      PIC X(16)  VALUE 'NYCKLAR-DLI'.          
513700*                                                                         
513800 01  NYCKLAR-TILL-DLI.                                                    
513900   03  W-IDGMT-X.                                                         
514000     05  W-IDDISTR               PIC S9(5)   VALUE ZERO  COMP-3.          
514100     05  W-IDKUNDNR              PIC S9(7)   VALUE ZERO  COMP-3.          
514200*                                                                         
514300     03  W-IDGMTREF-X.                                                    
514400         05  W-IDGMTREF          PIC X(17)   VALUE SPACE.                 
514500*                                                                         
514600   03  W-WDB101KY-X.                                                      
514700     05  W-WDB1-IDPARTNR         PIC X(9)    VALUE SPACE.                 
514800     05  W-WDB1-IDFTG            PIC 9(2)    VALUE ZERO.                  
514900*                                                                         
515000   03    W-WDE4E1KY-MAX-X.                                                
515100     05    W-IDPRODNR-WDE4E-MAX  PIC S9(7)   VALUE ZERO  COMP-3.          
515200     05    W-WDE4E1-MAX          PIC X(19)   VALUE HIGH-VALUE.            
515300                                                                          
515400   03    W-WDE4E1KY-MIN-X.                                                
515500     05    W-IDPRODNR-WDE4E-MIN  PIC S9(7)   VALUE ZERO  COMP-3.          
515600     05    W-WDE4E1-MIN          PIC X(19)   VALUE LOW-VALUE.             
515700                                                                          
515800   03 W-IDPRODNR-WDE611-X.                                                
515900     05  W-IDPRODNR-WDE611     PIC S9(7)   VALUE ZERO  COMP-3.            
516000   03 W-IDKOLLI-WDE611-X.                                                 
516100     05  W-IDKOLLI-WDE611      PIC S9(5)   VALUE ZERO  COMP-3.            
516200                                                                          
516300   03 W-WDE421KY-X.                                                       
516400     05  W-IDPRODNR-WDE421     PIC S9(7)   VALUE ZERO  COMP-3.            
516500     05  W-IDKOLLI-WDE421      PIC S9(5)   VALUE ZERO  COMP-3.            
516600                                                                          
516700                                                                          
516800     EJECT                                                                
516900   03  W-WDB501KY-X.                                                      
517000     05  W-501-IDDC              PIC X(2).                                
517100     05  W-501-KDFRAKT           PIC S9(3)   VALUE ZERO  COMP-3.          
517200     05  W-501-IDDISTR           PIC S9(5)   VALUE ZERO  COMP-3.          
517300     05  W-501-IDKUNDNR          PIC S9(7)   VALUE ZERO  COMP-3.          
517400*                                                                         
517500   03  W-WDB501KY-DEFAULT-X.                                              
517600     05  W-501-IDDC-DEFAULT      PIC X(2).                                
517700     05  W-501-KDFRAKT-DEFAULT   PIC S9(3)   VALUE ZERO    COMP-3.        
517800     05  W-501-IDDISTR-DEFAULT   PIC S9(5)   VALUE ZERO    COMP-3.        
517900     05  W-501-IDKUNDNR-DEFAULT  PIC S9(7)   VALUE 9999999 COMP-3.        
518000*                                                                         
518100   03  W-KUNDORDER-SEK-X.                                                 
518200     05  W-4A1-IDDISTR           PIC S9(5)   VALUE ZERO  COMP-3.          
518300     05  W-4A1-IDKUNDNR          PIC S9(7)   VALUE ZERO  COMP-3.          
518400     05  W-4A1-IDKUNDRF.                                                  
518500       07  W-4A1-IDORDNR         PIC X(5).                                
518600       07  FILLER                PIC X(5).                                
518700*                                                                         
518800   03  W-KUNDORDER-X.                                                     
518900     05  W-401-IDDISTR           PIC S9(5)   VALUE ZERO  COMP-3.          
519000     05  W-401-IDKUNDNR          PIC S9(7)   VALUE ZERO  COMP-3.          
519100     05  W-401-IDKUNDRF.                                                  
519200       07  W-401-IDORDNR         PIC X(5).                                
519300       07  FILLER                PIC X(5).                                
519400     05  W-401-IDPRODNR          PIC S9(7)   VALUE ZERO  COMP-3.          
519500     05  W-401-IDPLKLST          PIC S9(3)   VALUE ZERO  COMP-3.          
519600*                                                                         
519700   03  W-IDPURAD-X.                                                       
519800     05  W-IDPURAD               PIC S9(5)   VALUE 00001 COMP-3.          
519900*                                                                         
520000   03  W-WDQ301KY-X.                                                      
520100     05  W-301-IDORDER           PIC S9(7)   VALUE ZERO  COMP-3.          
520200     05  W-301-IDDC              PIC X(2)    VALUE SPACE.                 
520300     05  W-301-IDPRODNR          PIC S9(7)   VALUE ZERO  COMP-3.          
520400     05  W-301-IDPLKLST          PIC S9(3)   VALUE ZERO  COMP-3.          
520500*                                                                         
520600   03  W-IDDC-X.                                                          
520700     05  W-IDDC                  PIC X(2).                                
520800*                                                                         
520900   03  W-KDFRAKT-X.                                                       
521000     05  W-KDFRAKT               PIC S9(3)   VALUE ZERO  COMP-3.          
521100*                                                                         
521200   03  W-IDPRODNR-X.                                                      
521300     05  W-IDPRODNR              PIC S9(7)   VALUE ZERO  COMP-3.          
521400*                                                                         
521500   03  W-IDKOLLI-X.                                                       
521600     05  W-IDKOLLI               PIC S9(5)   VALUE ZERO  COMP-3.          
521700*                                                                         
521800   03  W-IDORDER-X.                                                       
521900      05 W-IDORDER               PIC S9(7)   COMP-3.                      
522000*                                                                         
522100   03  W-IDARTNR-X.                                                       
522200      05 W-IDARTNR               PIC S9(9)   COMP-3.                      
522300*                                                                         
522400   03  W-4503-WDGXKEY-X.                                                  
522500     05  W-4503-IDHTYP           PIC X(4)    VALUE '4503'.                
522600     05  W-4503-NYCKEL-VALFRI    PIC X(26)   VALUE LOW-VALUE.             
522700   03  W-4504-WDGXKEY-X.                                                  
522800      05 W-KY4504-IDPRODNR       PIC S9(7)   VALUE ZERO COMP-3.           
522900      05 W-KY4504-IDKOLLI        PIC S9(5)   VALUE ZERO COMP-3.           
523000*                                                                         
523100   03  W-4545-WDGXKEY-X.                                                  
523200     05  W-4545-IDHTYP           PIC X(4)    VALUE '4545'.                
523300     05  W-4545-NYCKEL-VALFRI    PIC X(26)   VALUE LOW-VALUE.             
523400   03  W-4546-WDGXKEY-X.                                                  
523500      05 W-KY4546-IDPRODNR       PIC S9(7)   VALUE ZERO COMP-3.           
523600      05 W-KY4546-IDKOLLI        PIC S9(5)   VALUE ZERO COMP-3.           
523700*                                                                         
523800   03  W-WDGXKEY-4523-X.                                                  
523900     05  W-IDHTYP-4523           PIC X(4)    VALUE '4523'.                
524000     05  FILLER                  PIC X(26)   VALUE LOW-VALUE.             
524100*                                                                         
524200   03  W-WDGXKEY-4524-X.                                                  
524300     05  W-KDSEGKEY              PIC X(1)    VALUE '1'.                   
524400                                                                          
524500   03  W-IDDC-B6-X.                                                       
524600     05  W-IDDC-B6               PIC X(2).                                
524700******************************************************************        
524800   01  FILLER                  PIC X(16) VALUE 'MID W4I33901AREA'.        
524900   01  4339-MSG-IO-AREA.                                                  
525000                                                                          
525100       03  4339-KVLL             PIC S9(4)   COMP SYNC.                   
525200       03  4339-Z1               PIC X.                                   
525300       03  4339-Z2               PIC X.                                   
525400       03  4339-TRANSKOD         PIC X(8)    VALUE 'W4T339X '.            
525500       03  4339-IDTRANS          PIC X(4)    VALUE '4333'.                
525600       03  4339-KDMFSFOR         PIC X.                                   
525700*      03  MID -COPY W4I33901  -PRE 4339-.                                
525800     SKIP2                                                                
525900******************************************************************        
527300     EJECT                                                                
527400 01  MEDDELANDE.                                                          
527500   03  FEL1.                                                              
527600     05 FILLER                   PIC X(40)                                
527700          VALUE '749 FEL NYCKEL'.                                         
527800     05 FILLER                   PIC X(40)                                
527900          VALUE '749 WRONG KEY                   '.                       
528000   03  FILLER REDEFINES FEL1.                                             
528100     05  FEL-1                   PIC X(40)   OCCURS 2.                    
528200                                                                          
528300   03  FEL2A.                                                             
528400     05 FILLER                   PIC X(40)                                
528500          VALUE '701 ORDERN SAKNAS'.                                      
528600     05 FILLER                   PIC X(40)                                
528700          VALUE '701 ORDER MISSING  '.                                    
528800   03  FILLER REDEFINES FEL2A.                                            
528900     05  FEL-2A                  PIC X(40)   OCCURS 2.                    
529000                                                                          
529100   03  FEL2B.                                                             
529200     05 FILLER                   PIC X(40)                                
529300          VALUE '702 ORDERN SAKNAS'.                                      
529400     05 FILLER                   PIC X(40)                                
529500          VALUE '702 ORDER MISSING  '.                                    
529600   03  FILLER REDEFINES FEL2B.                                            
529700     05  FEL-2B                  PIC X(40)   OCCURS 2.                    
529800                                                                          
529900   03  FEL2C.                                                             
530000     05 FILLER                   PIC X(40)                                
530100          VALUE '702 ORDERN SAKNAS'.                                      
530200     05 FILLER                   PIC X(40)                                
530300          VALUE '702 ORDER MISSING  '.                                    
530400   03  FILLER REDEFINES FEL2C.                                            
530500     05  FEL-2C                  PIC X(40)   OCCURS 2.                    
530600                                                                          
530700   03  FEL3.                                                              
530800     05 FILLER                   PIC X(40)                                
530900          VALUE '758 KOLLI SAKNAS'.                                       
531000     05 FILLER                   PIC X(40)                                
531100          VALUE '758 CASE MISSING  '.                                     
531200   03  FILLER REDEFINES FEL3.                                             
531300     05  FEL-3                   PIC X(40)   OCCURS 2.                    
531400                                                                          
531500   03  FEL4.                                                              
531600     05 FILLER                   PIC X(40)                                
531700          VALUE '772 FELAKTIG PRINTER '.                                  
531800     05 FILLER                   PIC X(40)                                
531900          VALUE '772 WRONG PRINTER    '.                                  
532000   03  FILLER REDEFINES FEL4.                                             
532100     05  FEL-4                   PIC X(40)   OCCURS 2.                    
532200                                                                          
532300   03  FEL5A.                                                             
532400     05 FILLER                   PIC X(40)                                
532500          VALUE '773A GODKÄNT KOLLI SAKNAS '.                             
532600     05 FILLER                   PIC X(40)                                
532700          VALUE '773A APPROVED CASE MISSING.'.                            
532800   03  FILLER REDEFINES FEL5A.                                            
532900     05  FEL-5A                  PIC X(40)   OCCURS 2.                    
533000                                                                          
533100   03  FEL5B.                                                             
533200     05 FILLER                   PIC X(40)                                
533300          VALUE '773B FRAKTKOD INFO SAKNAS PÅ KUNDREG.'.                  
533400     05 FILLER                   PIC X(40)                                
533500          VALUE '773B FREIGHT CODE MISSING ON CUST.FILE'.                 
533600   03  FILLER REDEFINES FEL5B.                                            
533700     05  FEL-5B                  PIC X(40)   OCCURS 2.                    
533800                                                                          
533900   03  FEL6.                                                              
534000     05 FILLER                   PIC X(40)                                
534100          VALUE 'XXX RADER SAKNAS         '.                              
534200     05 FILLER                   PIC X(40)                                
534300          VALUE 'XXX LINES MISSING        '.                              
534400   03  FILLER REDEFINES FEL6.                                             
534500     05  FEL-6                   PIC X(40)   OCCURS 2.                    
534600                                                                          
534700   03  FEL7.                                                              
534800     05 FILLER                   PIC X(40)                                
534900          VALUE 'FEL NYCKEL -DC           '.                              
535000     05 FILLER                   PIC X(40)                                
535100          VALUE 'WRONG KEY  -DC           '.                              
535200   03  FILLER REDEFINES FEL7.                                             
535300     05  FEL-7                   PIC X(40)   OCCURS 2.                    
535400                                                                          
535500   03  FEL8.                                                              
535600     05 FILLER                   PIC X(40)                                
535700          VALUE 'KUND SAKNAS - KUNDREG.   '.                              
535800     05 FILLER                   PIC X(40)                                
535900          VALUE 'DEALER MISSING SCREEN 4417'.                             
536000   03  FILLER REDEFINES FEL8.                                             
536100     05  FEL-8                   PIC X(40)   OCCURS 2.                    
536200                                                                          
536300   03  FEL9.                                                              
536400     05 FILLER                   PIC X(40)                                
536500          VALUE 'FEL I BERÄKNING IDKLI.   '.                              
536600     05 FILLER                   PIC X(40)                                
536700          VALUE 'ERROR IN COMPUTE IDKLI   '.                              
536800   03  FILLER REDEFINES FEL9.                                             
536900     05  FEL-9                   PIC X(40)   OCCURS 2.                    
537000                                                                          
537100   03  MED1.                                                              
537200     05 FILLER                   PIC X(40)                                
537300          VALUE 'KOLLIFLAGGA UTSKRIVEN    '.                              
537400     05 FILLER                   PIC X(40)                                
537500          VALUE 'CASE LABEL PRINTED          '.                           
537600   03  FILLER REDEFINES MED1.                                             
537700     05  MED-1                   PIC X(40)   OCCURS 2.                    
537800                                                                          
537900     EJECT                                                                
538000******************************************************************        
538100*                                                                         
538200*                AREOR FÖR MFS OCH SKÄRMHANTERING                         
538300*                                                                         
538400 01  FILLER                      PIC X(16)   VALUE 'MFS-WS'.              
538500     SKIP3                                                                
538600*01  MID -COPY W4I33301                                                   
538700     EJECT                                                                
538800*01  -COPY WMSGAREA                                                       
538900     EJECT                                                                
539000*  03  MOD -COPY W4O33301           -RED MSG-AREA.                        
539100     EJECT                                                                
539200*01  -COPY WMFSAREA                                                       
539300     EJECT                                                                
539400*01  -COPY W006PRAR                                                       
539500     EJECT                                                                
539600******************************************************************        
539700*                                                                         
539800*        ARBETS-AREOR TILL IMS-SEKTIONERNA                                
539900*                                                                         
540000 01  IMS-WS.                                                              
540100   03  FILLER                    PIC X(16)   VALUE 'IMS-WS     '.         
540200     SKIP3                                                                
540300*                        **** STATUS-KOD FRÅN IMS                         
540400   03  STATUS-KUNDORDER-SEK-WS   PIC XX.                                  
540500     88  KUNDORDER-SEK-FINNS                 VALUE '  '.                  
540600     88  KUNDORDER-SEK-SAKNAS                VALUE 'GE' 'GB'.             
540700   03  STATUS-WS                 PIC XX.                                  
540800     88  SEGMENT-FINNS                       VALUE '  '.                  
540900     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
541000     SKIP3                                                                
541100   03  GODK-STATUSKODER.                                                  
541200     05  GODK-STATUS OCCURS 5 INDEXED BY STATUS-INDX PIC XX.              
541300     SKIP3                                                                
541400 01    SSA1                      PIC X(160).                              
541500 01    SSA2                      PIC X(160).                              
541600     EJECT                                                                
541700*                            IMS FUNKTIONSKODER                           
541800*01    -COPY W0003                                                        
541900     EJECT                                                                
542000*                            DLI INPUT-OUTPUT AREA                        
542100 01  DLI-IO-AREA.                                                         
542200   03  IO-AREA                   PIC X(320)  VALUE SPACE.                 
542300     SKIP3                                                                
542400*  03  WDE401    -COPY WDE401  -RED IO-AREA.                              
542500     EJECT                                                                
542600 01  DLI-IO-AREA3.                                                        
542700   03  IO-AREA3                  PIC X(288)  VALUE SPACE.                 
542800     SKIP3                                                                
542900*  03  WLORQA01  -COPY WDQ301  -RED IO-AREA3.                             
543000     EJECT                                                                
543100 01  FILLER                      PIC X(16)  VALUE 'Q201-AREA'.            
543200 01  DLI-IO-Q201.                                                         
543300*  03  -COPY WDQ201                                                       
543400     EJECT                                                                
543500 01  FILLER                      PIC X(16)  VALUE 'Q212-AREA'.            
543600 01  DLI-IO-Q212.                                                         
543700*  03  -COPY WDQ212                                                       
543800     EJECT                                                                
543900 01  FILLER                      PIC X(16)  VALUE 'WDB1-AREA'.            
544000*01  -COPY WDB101                                                         
544100     EJECT                                                                
544200 01  FILLER                      PIC X(16)  VALUE 'WDB2-AREA'.            
544300*01  -COPY WDB201                                                         
544400     EJECT                                                                
544500 01  FILLER                      PIC X(16)  VALUE 'WDB5-AREA'.            
544600*01  -COPY WDB501                                                         
544700     EJECT                                                                
544800 01  FILLER                      PIC X(16)  VALUE 'WDK701-AREA'.          
544900*01  -COPY WDK701                                                         
545000     EJECT                                                                
545100 01  FILLER                      PIC X(16)  VALUE 'WDK711-AREA'.          
545200*01  -COPY WDK711                                                         
545300     EJECT                                                                
545400 01  FILLER                    PIC X(16) VALUE 'WDE611         '.         
545500 01    DLI-IO-AREA4.                                                      
545600   03    IO-AREA                 PIC X(300)  VALUE SPACE.                 
545700*  03    WDE611    COPY WDE611  -PRE XE6-  -RED IO-AREA.                  
545800*DIRLEV                                                                   
545900 01  DLI-IO-AREA5.                                                        
546000   03  IO-AREA                   PIC X(320)  VALUE SPACE.                 
546100     SKIP3                                                                
546200 01  FILLER                   PIC X(18) VALUE 'DLI-IOAREA-WDE601'.        
546300 01  DLI-IOAREA-WDE601.                                                   
546400*  03  WDE601    -COPY WDE601                                             
546500*                                                                         
546600 01  DLI-IOAREA-WDE611.                                                   
546700*     03            -COPY WDE611                                          
546800     EJECT                                                                
546900 01  DLI-IOAREA-WDE411.                                                   
547000*     03            -COPY WDE411                                          
547100 01  DLI-IOAREA-WDE421.                                                   
547200*     03            -COPY WDE421                                          
547300     EJECT                                                                
547400 01  DLI-IOAREA-WDE4E1.                                                   
547500*     03            -COPY WDE4E1                                          
547600     EJECT                                                                
547700 01  DLI-IO-GX01.                                                         
547800*    03  -COPY WDGX01                                                     
547900     EJECT                                                                
548000 01  FILLER                      PIC X(08)   VALUE 'WDGX4504'.            
548100 01  WDGX4504    -COPY WDGX4504.                                          
548200     EJECT                                                                
548300 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-4524'.         
548400     SKIP3                                                                
548500 01  DLI-IO-4524.                                                         
548600*    03  -COPY WDGX4524                                                   
548700                                                                          
548800 01  FILLER                      PIC X(16)   VALUE 'WDB601 AREA'.         
548900 01   DLI-IO-AREA-B601.                                                   
549000*     03  -COPY WDB601                                                    
549100     EJECT                                                                
549200 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-4546'.         
549300 01   DLI-IO-4546.                                                        
549400      03  -COPY WDGX4546.                                                 
549500     EJECT                                                                
549600 01  FILLER                      PIC X(16)   VALUE 'WDI2-AREA'.           
549700 01   DLI-IO-WDI2.                                                        
549800      03  -COPY WDI201.                                                   
549900     EJECT                                                                
550000 LINKAGE                   SECTION.                                       
550100*01  -COPY W0009     -PRE MSG-                                            
550200     SKIP2                                                                
550300*01  -COPY W0009     -PRE ALT-                                            
550400     SKIP2                                                                
550500*01  -COPY W0009     -PRE ALT4339-                                        
550600     SKIP2                                                                
550900*01  -COPY W0008     -PRE USEA-                                           
551000     05  FILLER                  PIC X.                                   
551100     SKIP2                                                                
551200*01  -COPY W0008     -PRE WDE42-                                          
551300     05  FILLER                  PIC X.                                   
551400     SKIP2                                                                
551500*01  -COPY W0008     -PRE WDE4A-                                          
551600     05  FILLER                  PIC X.                                   
551700     SKIP2                                                                
551800*01  -COPY W0008     -PRE ORQI-                                           
551900     05  FILLER                  PIC X.                                   
552000     SKIP2                                                                
552100*01  -COPY W0008     -PRE GMTA-                                           
552200     05  FILLER                  PIC X.                                   
552300     SKIP2                                                                
552400*01  -COPY W0008     -PRE ORQA-                                           
552500     05  FILLER                  PIC X.                                   
552600     SKIP2                                                                
552700*01  -COPY W0008     -PRE BETC-                                           
552800     05  FILLER                  PIC X.                                   
552900     SKIP2                                                                
553000*01  -COPY W0008     -PRE GMTC-                                           
553100     05  FILLER                  PIC X.                                   
553200     SKIP2                                                                
553300*01  -COPY W0008     -PRE ARTS-                                           
553400     05  FILLER                  PIC X.                                   
553500     SKIP2                                                                
553600*01  -COPY W0008     -PRE WDE6-                                           
553700     05  FILLER                  PIC X.                                   
553800     SKIP2                                                                
553900*01  -COPY W0008     -PRE ORDD-                                           
554000     05  FILLER                  PIC X.                                   
554100     SKIP2                                                                
554200*01  -COPY W0008     -PRE WDE4-                                           
554300     05  FILLER                  PIC X.                                   
554400     SKIP2                                                                
554500*01  -COPY W0008     -PRE WDE4E-                                          
554600     05  FILLER                  PIC X.                                   
554700     SKIP2                                                                
554800*01  -COPY W0008     -PRE 4503-                                           
554900     05  FILLER                  PIC X.                                   
555000     SKIP2                                                                
555100*01  -COPY W0008     -PRE 4523-                                           
555200     05  FILLER                  PIC X.                                   
555300     SKIP2                                                                
555400*01  -COPY W0008     -PRE WDB6-                                           
555500     05  FILLER                  PIC X.                                   
555600     EJECT                                                                
555700*01  -COPY W0008     -PRE 4545-                                           
555800     05  FILLER                  PIC X.                                   
555900     EJECT                                                                
556000*01  -COPY W0008     -PRE 4547-                                           
556100     05  FILLER                  PIC X.                                   
556200     EJECT                                                                
556300*01  -COPY W0008     -PRE WDI2-                                           
556400     05  FILLER                  PIC X.                                   
556500     EJECT                                                                
556600 PROCEDURE DIVISION USING MSG-PCB ALT-PCB   ALT4339-PCB                   
556800                                            USEA-PCB                      
556900                                            WDE42-PCB WDE4A-PCB           
557000                                            ORQI-PCB GMTA-PCB             
557100                                            ORQA-PCB BETC-PCB             
557200                                            GMTC-PCB ARTS-PCB             
557300                                            WDE6-PCB ORDD-PCB             
557400                                            WDE4-PCB WDE4E-PCB            
557500                                            4503-PCB 4523-PCB             
557600                                            WDB6-PCB 4545-PCB             
557700                                            4547-PCB WDI2-PCB.            
557800 MAIN SECTION.                                                            
557900     ENTRY 'DLITCBL' USING MSG-PCB ALT-PCB  ALT4339-PCB                   
558100                                            USEA-PCB                      
558200                                            WDE42-PCB WDE4A-PCB           
558300                                            ORQI-PCB GMTA-PCB             
558400                                            ORQI-PCB GMTA-PCB             
558500                                            ORQA-PCB BETC-PCB             
558600                                            GMTC-PCB ARTS-PCB             
558700                                            WDE6-PCB ORDD-PCB             
558800                                            WDE4-PCB WDE4E-PCB            
558900                                            4503-PCB 4523-PCB             
559000                                            WDB6-PCB 4545-PCB             
559100                                            4547-PCB WDI2-PCB.            
559110                                                                          
559200     PERFORM IMS-GET-MSG                                                  
559400     IF SEGMENT-FINNS                                                     
559500       PERFORM A-INIT                                                     
559600*DIRLEV                                                                   
559700       IF WS-GODKAEND-BILD                                                
559800         IF WS-IDDISTR  NUMERIC AND WS-IDKUNDNR NUMERIC AND               
559900            WS-IDORDNR  NUMERIC AND WS-IDKOLLI  NUMERIC AND               
560000            WS-IDPRODNR NUMERIC AND DC-OK                                 
560100                                                                          
560200           IF IDPRODNR-IFYLLT                                             
560300           AND WS-IDPRODNR > ZERO                                         
560400*DETTA BEHÖVS OM MAN ANV. BILD 4333 OCH END. FYLLER I PRODNR.             
560500                                                                          
560600             PERFORM IMS-GU-ORDD-WDE601                                   
560700             IF SEGMENT-FINNS                                             
560800               MOVE VORD-IDDISTR     TO WS-IDDISTR                        
560900               MOVE VORD-IDKUNDNR    TO WS-IDKUNDNR-7                     
561000               MOVE WS-IDKUNDNR-7(2:6)    TO WS-IDKUNDNR                  
561100               MOVE VORD-IDPRODNR TO W-IDPRODNR-WDE4E-MAX                 
561200               MOVE VORD-IDPRODNR TO W-IDPRODNR-WDE4E-MIN                 
561300               PERFORM IMS-GET-WDE4E                                      
561400               IF SEGMENT-FINNS                                           
561500                 MOVE SEQE-IDORDNR5  TO WS-IDORDNR                        
561600                                                                          
561700               END-IF                                                     
561800             END-IF                                                       
561900           END-IF                                                         
562000                                                                          
562100           MOVE 'N'          TO WS-SLINGA-KLAR                            
562200           MOVE WS-IDDISTR   TO W-4A1-IDDISTR                             
562300                                W-IDDISTR                                 
562310                                                                          
562400           MOVE WS-IDKUNDNR  TO W-4A1-IDKUNDNR                            
562500                                W-IDKUNDNR                                
562510                                                                          
562600           MOVE SPACE        TO W-4A1-IDKUNDRF                            
562700           MOVE WS-IDORDNR   TO W-4A1-IDORDNR                             
562710                                                                          
562800           PERFORM IMS-GU-WDE401-ASEQ                                     
562900                                                                          
563000           IF SEGMENT-FINNS                                               
563100             PERFORM UNTIL KUNDORDER-SEK-SAKNAS OR                        
563200                 SLINGA-KLAR                                              
563300               MOVE KORD-IDDISTR  TO W-401-IDDISTR                        
563400               MOVE KORD-IDKUNDNR TO W-401-IDKUNDNR                       
563500                                                                          
563600               MOVE SPACE         TO W-401-IDKUNDRF                       
563700               MOVE KORD-IDORDNR5 TO W-401-IDORDNR                        
563800               MOVE KORD-IDPRODNR TO W-401-IDPRODNR                       
563900                                     W-301-IDPRODNR                       
564000                                     W-IDPRODNR-WDE611                    
564100                                     W-IDPRODNR                           
564200               MOVE KORD-IDPLKLST TO W-401-IDPLKLST                       
564300                                     W-301-IDPLKLST                       
564400               MOVE KORD-IDDC        TO W-301-IDDC                        
564500               PERFORM IMS-GU-WDE401                                      
564600               MOVE KORD-KVORDRAD-LEVPL   TO                              
564700                                          WS-SPAR-KVORDRAD-LEVPL          
564800               MOVE KORD-IDORDER          TO W-IDORDER                    
564900                                             W-301-IDORDER                
565000*FIX START - FÖR DIRLEV ORDDEL                                            
565100*DIRLEV                                                                   
565200               IF IDPRODNR-IFYLLT                                         
565300               AND WS-IDPRODNR > ZERO                                     
565400                 PERFORM IMS-GU-ORDD-WDE601                               
565500                 IF VORD-IDDC   = WS-IDDC                                 
565600                 OR GOOD-DDC                                              
565700                   MOVE 'J' TO WS-SLINGA-KLAR                             
565800                 END-IF                                                   
565900               ELSE                                                       
566000                 PERFORM IMS-GU-ORDD-WDE601                               
566100                 IF (VORD-IDDC   = WS-IDDC   AND                          
566200                     WS-SPAR-KVORDRAD-LEVPL = ZERO)                       
566300                   MOVE 'J' TO WS-SLINGA-KLAR                             
566400                 END-IF                                                   
566500               END-IF                                                     
566600               IF SLINGA-KLAR                                             
566700                 MOVE VORD-KDFRAKT  TO MOD-KDFRAKT                        
566800                                       LISTA-KDFRAKT                      
566900                                       WS-KDFRAKT                         
567000                                       W-KDFRAKT                          
567100                                       W-501-KDFRAKT                      
567200                                       W-501-KDFRAKT-DEFAULT              
567300                 MOVE VORD-IDPRODNR TO W-IDPRODNR                         
567400                                       W-IDPRODNR-WDE611                  
567500                                              WS-IDPRODNR-7               
567600                 MOVE WS-IDPRODNR-7        TO WS-IDPRODNR                 
567700                 MOVE WS-IDKOLLI TO WS-IDKOLLI-PRT                        
567800                 PERFORM UNTIL WS-IDKOLLI-PRT > WS-IDKOLLI-TOM            
567900                            OR WS-IDKOLLI-PRT = 0                         
568000                            OR BREAK                                      
568100                   MOVE WS-IDKOLLI-PRT TO W-IDKOLLI-WDE611                
568200                   PERFORM IMS-GU-WDE611                                  
568300                   IF SEGMENT-FINNS                                       
568400                     IF KOLLI-KDKOLSTA =                                  
568500                              1 OR 2 OR 6 OR 7 OR 8 OR 9                  
568600                       PERFORM C-SHOW-INFO                                
568700                       IF ADRESS-INTE-HAMTAD                              
568800                         PERFORM F-HAEMTA-ADRESS                          
568900                         MOVE JA      TO SW-ADRESS-HAMTAD                 
569000                       END-IF                                             
569100                       PERFORM H-CONTROL-TRANSPORT-POSTEN                 
569200                       IF MOD-TEMFSFEL > SPACE                            
569300                         CONTINUE                                         
569400                       ELSE                                               
569500                         PERFORM G-HAEMTA-TIRFS                           
569600                         EVALUATE TRUE                                    
569700                           WHEN MFS-IDTRANS = '433A' OR                   
569800                                    '433B' OR '433E' OR                   
569900                                    '433F' OR '431E' OR                   
570000                                    '431C' OR '431D' OR                   
570100                                    '433Z' OR 'L199' OR                   
570100                                    '4327' OR 'L197'                      
570200                             PERFORM D-PRINT-LIST                         
570300                           WHEN MFS-IDTRANS = '4333' AND                  
570400                                MFS-IDPFK = '4'                           
570500                             PERFORM D-PRINT-LIST                         
570600                           WHEN MFS-UPD-X                                 
570700                             PERFORM D-PRINT-LIST                         
570800                         END-EVALUATE                                     
570900                       END-IF                                             
571000                     ELSE                                                 
571100                       MOVE FEL-5A(SPRAK-IX) TO MOD-TEMFSFEL              
571200                       MOVE MFS-RENSA-FAELT TO MOD-RAD2                   
571300                     END-IF                                               
571400                   ELSE                                                   
571500                     MOVE FEL-3 (SPRAK-IX) TO MOD-TEMFSFEL                
571600                     MOVE MFS-RENSA-FAELT TO MOD-RAD2                     
571700                   END-IF                                                 
571800                   ADD +1 TO WS-IDKOLLI-PRT                               
571900                   IF ONLY-ONE-CASE                                       
572000                     MOVE JA               TO BREAK-SW                    
572100                   END-IF                                                 
572200                 END-PERFORM                                              
572300               END-IF                                                     
572400                 PERFORM IMS-GN-WDE401                                    
572500             END-PERFORM                                                  
572600           ELSE                                                           
572700             MOVE FEL-2A(SPRAK-IX) TO MOD-TEMFSFEL                        
572800             MOVE MFS-RENSA-FAELT TO MOD-RAD2                             
572900           END-IF                                                         
573000         ELSE                                                             
573100           IF DC-WRONG                                                    
573200             MOVE FEL-6 (SPRAK-IX) TO MOD-TEMFSFEL                        
573300           ELSE                                                           
573400             MOVE FEL-1 (SPRAK-IX) TO MOD-TEMFSFEL                        
573500           END-IF                                                         
573600           MOVE MFS-RENSA-FAELT TO MOD-RAD2                               
573700         END-IF                                                           
573800       ELSE                                                               
573900         PERFORM E-RENSA-NYCKLAR                                          
574000       END-IF                                                             
574100                                                                          
574200       IF NOT SLINGA-KLAR                                                 
574300         MOVE FEL-2B(SPRAK-IX) TO MOD-TEMFSFEL                            
574400         MOVE MFS-RENSA-FAELT TO MOD-RAD2                                 
574500       END-IF                                                             
574600       IF (MFS-IDTRANS = '431C' OR '433A' OR '433B' OR                    
574700                         '433E' OR '433F' OR '431E' OR                    
574800                         '431D' OR '433Z' OR 'L199' OR                    
574800                         '4327' OR 'L197')                                
574900           OR MFS-UPD-X                                                   
575000          CONTINUE                                                        
575100       ELSE                                                               
575200         COMPUTE MSG-KVLL = LENGTH OF MOD-W4O33301 + 4                    
575300         PERFORM IMS-ISRT-MSG                                             
575400       END-IF                                                             
575500     END-IF                                                               
575600     MOVE ZERO TO RETURN-CODE                                             
575700                                                                          
575800     GOBACK                                                               
575900     .                                                                    
576000     EJECT                                                                
576100 A-INIT                    SECTION.                                       
576200                                                                          
576300     IF MSG-DUBBLA-TRANSKODER                                             
576400       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W4I33301                 
576500       MOVE MSG-IDTRANS-2 TO MFS-IDTRANS                                  
576600       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
576700     ELSE                                                                 
576800       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W4I33301                  
576900       MOVE MSG-IDTRANS-1 TO MFS-IDTRANS                                  
577000       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
577100     END-IF                                                               
577200                                                                          
577300     ACCEPT WS-DAGENS-DATUM               FROM DATE                       
577400     MOVE FUNCTION CURRENT-DATE(1:8)      TO WS-DAGENS-DATUM-HIT          
577500     ACCEPT WS-TIDPUNKT                   FROM TIME                       
577600                                                                          
577700     IF FROM-4333-BILD                                                    
577800       MOVE ALL '+'           TO MSGI-WMSGINIT                            
577900       MOVE '001'             TO MSGI-KDCALL                              
578000       MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                              
578100       MOVE '4333'            TO MSGI-IDTRANS                             
578200       MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                        
578300       MOVE WS-DAGENS-DATUM   TO MSGI-TILOKDAT                            
578400       MOVE WS-TIDPUNKT       TO MSGI-TILOKTID                            
578500       CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                         
578600       MOVE MSGI-KDMATT       TO WS-KDMATT                                
578700       MOVE MSGI-TILOKDAT     TO WS-DAGENS-DATUM                          
578800       MOVE MSGI-TILOKTID     TO WS-TIDPUNKT (1:4)                        
578900     ELSE                                                                 
579000       IF WS-GODKAEND-BILD                                                
579100         PERFORM E-RENSA-NYCKLAR                                          
579200       END-IF                                                             
579300     END-IF                                                               
579400                                                                          
579500     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
579600     MOVE MSG-IDPFK TO MFS-IDPFK                                          
579700     MOVE MFS-IDTRANS TO WS-IDTRANS                                       
579800     MOVE MFS-KDMFSFOR TO WS-MFS-KDMFSFOR                                 
579900                                                                          
580000     MOVE LOW-VALUE TO MSG-AREA                                           
580100     MOVE 'W4O333N1' TO MFS-IDMOD                                         
580200     MOVE '4333' TO MOD-IDTRANS                                           
580300     MOVE +00001            TO W-IDPURAD                                  
580400     MOVE NEJ               TO IDPRODNR-IFYLLT-SW                         
580500     MOVE NEJ               TO SW-ADRESS-HAMTAD                           
580600     MOVE NEJ               TO ONLY-ONE-CASE-SW                           
580700     MOVE NEJ               TO BREAK-SW                                   
580800                                                                          
580900     IF ENGLISH-TEXT                                                      
581000       MOVE +2 TO SPRAK-IX                                                
581100     ELSE                                                                 
581200       MOVE +1 TO SPRAK-IX                                                
581300     END-IF                                                               
581400                                                                          
581500     IF MID-IDDISTR-IN = ALL '+'                                          
581600       INSPECT MID-IDDISTR-UT REPLACING LEADING SPACE BY ZERO             
581700       MOVE MID-IDDISTR-UT TO WS-IDDISTR MOD-IDDISTR-UT                   
581800     ELSE                                                                 
581900       MOVE MID-IDDISTR-IN TO WS-IDDISTR MOD-IDDISTR-UT                   
582000     END-IF                                                               
582110                                                                          
582200     IF MID-IDKUNDNR-IN = ALL '+'                                         
582300       INSPECT MID-IDKUNDNR-UT REPLACING LEADING SPACE BY ZERO            
582400       MOVE MID-IDKUNDNR-UT TO WS-IDKUNDNR MOD-IDKUNDNR-UT                
582500     ELSE                                                                 
582600       MOVE MID-IDKUNDNR-IN TO WS-IDKUNDNR MOD-IDKUNDNR-UT                
582700     END-IF                                                               
582800                                                                          
582900     IF MID-IDORDNR-IN = ALL '+'                                          
583000       INSPECT MID-IDORDNR-UT REPLACING LEADING SPACE BY ZERO             
583100       MOVE MID-IDORDNR-UT TO WS-IDORDNR MOD-IDORDNR-UT                   
583200     ELSE                                                                 
583300       MOVE MID-IDORDNR-IN TO WS-IDORDNR MOD-IDORDNR-UT                   
583400     END-IF                                                               
583500                                                                          
583600     IF MID-IDKOLLI-IN = ALL '+'                                          
583700       INSPECT MID-IDKOLLI-UT REPLACING LEADING SPACE BY ZERO             
583800       IF MID-IDKOLLI-UT NOT NUMERIC                                      
583900         MOVE ZERO         TO WS-IDKOLLI                                  
584000         MOVE ZERO         TO W-IDKOLLI                                   
584100       ELSE                                                               
584200         MOVE MID-IDKOLLI-UT TO WS-IDKOLLI MOD-IDKOLLI-UT                 
584300                                W-IDKOLLI                                 
584400       END-IF                                                             
584500     ELSE                                                                 
584600       IF MID-IDKOLLI-IN NUMERIC                                          
584700         MOVE MID-IDKOLLI-IN TO WS-IDKOLLI MOD-IDKOLLI-UT                 
584800                                W-IDKOLLI                                 
584900       ELSE                                                               
585000         MOVE ZERO         TO WS-IDKOLLI                                  
585100         MOVE ZERO         TO W-IDKOLLI                                   
585200       END-IF                                                             
585300     END-IF                                                               
585400                                                                          
585500     IF WS-IDKOLLI > MID-IDKOLLI-TOM                                      
585600       MOVE WS-IDKOLLI      TO WS-IDKOLLI-TOM                             
585700       MOVE JA              TO ONLY-ONE-CASE-SW                           
585800     ELSE                                                                 
585900       MOVE MID-IDKOLLI-TOM TO WS-IDKOLLI-TOM                             
586000       IF WS-IDKOLLI-TOM = 99999                                          
586100         MOVE WS-IDKOLLI    TO WS-IDKOLLI-TOM                             
586200       END-IF                                                             
586300     END-IF                                                               
586400                                                                          
586500     MOVE JA                              TO WS-DC-TEST                   
586600     IF MID-IDDC-IN = ALL '+'                                             
586700       IF MID-IDDC-UT = SPACE                                             
586800       OR MID-IDDC-UT = ZERO                                              
586900         MOVE NEJ                         TO WS-DC-TEST                   
587000           MOVE ZERO                      TO WS-IDDC                      
587100                                             MOD-IDDC-UT                  
587200                                             W-IDDC                       
587300                                             W-301-IDDC                   
587400       ELSE                                                               
587500         IF MSGI-IDDC = WS-IDDC                                           
587600           MOVE MID-IDDC-UT               TO WS-IDDC                      
587700                                             MOD-IDDC-UT                  
587800                                             W-IDDC                       
587900                                             W-301-IDDC                   
588000         ELSE                                                             
588100           MOVE NEJ                       TO WS-DC-TEST                   
588200           MOVE ZERO                      TO WS-IDDC                      
588300                                             MOD-IDDC-UT                  
588400                                             W-IDDC                       
588500                                             W-301-IDDC                   
588600         END-IF                                                           
588700       END-IF                                                             
588800     ELSE                                                                 
588900       IF MSGI-IDDC = WS-IDDC                                             
589000         MOVE MID-IDDC-IN                 TO WS-IDDC                      
589100                                             MOD-IDDC-UT                  
589200                                             W-IDDC                       
589300                                             W-301-IDDC                   
589400       ELSE                                                               
589500         MOVE NEJ                         TO WS-DC-TEST                   
589600       END-IF                                                             
589700     END-IF                                                               
589800                                                                          
589900     IF WS-IDDC > SPACE                                                   
590000       MOVE WS-IDDC                       TO W-IDDC-B6                    
590100                                                                          
590200       IF GOOD-DC OR GOOD-DDC                                             
590300         IF DDC-SE OR DDC-NO OR DDC-FI OR DDC-BE OR DDC-DE                
590400         OR DDC-NL OR DDC-GB OR DDC-FR OR DDC-KR                          
590500         OR DDC-TR OR DDC-HU OR DDC-PL OR DDC-MA OR DDC-AU                
590600           MOVE NEJ                       TO WS-DC-TEST                   
590700         ELSE                                                             
590800           PERFORM IMS-GU-WDB601                                          
590900           MOVE JA                        TO WS-DC-TEST                   
591000         END-IF                                                           
591100       ELSE                                                               
591200         MOVE NEJ                         TO WS-DC-TEST                   
591300       END-IF                                                             
591400       IF DC-OK                                                           
591500         IF NOT FROM-4333-BILD                                            
591600           IF NDC-NA                                                      
591700             MOVE 'U'                     TO WS-KDMATT                    
591800           ELSE                                                           
591900             MOVE 'S'                     TO WS-KDMATT                    
592000           END-IF                                                         
592100         END-IF                                                           
592200       END-IF                                                             
592300     ELSE                                                                 
592400       MOVE NEJ                           TO WS-DC-TEST                   
592500     END-IF                                                               
592600*DIRLEV                                                                   
592700     IF MID-IDPRODNR-IN = ALL '+'                                         
592800       INSPECT MID-IDPRODNR-UT REPLACING LEADING SPACE BY ZERO            
592900         IF MID-IDPRODNR-IN NUMERIC                                       
593000         MOVE MID-IDPRODNR-UT TO W-IDPRODNR MOD-IDPRODNR-UT               
593100                                 WS-IDPRODNR                              
593200         IF MID-IDPRODNR-UT > ZERO                                        
593300           MOVE JA          TO IDPRODNR-IFYLLT-SW                         
593400         END-IF                                                           
593500       ELSE                                                               
593600         MOVE ZERO            TO W-IDPRODNR                               
593700                                 WS-IDPRODNR                              
593800       END-IF                                                             
593900     ELSE                                                                 
594000       IF MID-IDPRODNR-IN NUMERIC                                         
594100         MOVE MID-IDPRODNR-IN TO W-IDPRODNR MOD-IDPRODNR-UT               
594200                                 WS-IDPRODNR                              
594300         INSPECT WS-IDPRODNR REPLACING LEADING SPACE BY ZERO              
594400         MOVE JA            TO IDPRODNR-IFYLLT-SW                         
594500       ELSE                                                               
594600         MOVE ZERO            TO W-IDPRODNR                               
594700                                 WS-IDPRODNR                              
594800       END-IF                                                             
594900     END-IF                                                               
595000                                                                          
595100     IF MID-KDPRTVAL-IN = ALL '+'                                         
595200       MOVE MID-KDPRTVAL-UT TO MOD-KDPRTVAL-UT                            
595300                               WS-KDPRTVAL                                
595400     ELSE                                                                 
595500       MOVE MID-KDPRTVAL-IN TO MOD-KDPRTVAL-UT                            
595600                               WS-KDPRTVAL                                
595700     END-IF                                                               
595800                                                                          
595900     MOVE MFS-RENSA-FAELT      TO MOD-IDDISTR-IN                          
596000                                  MOD-IDKUNDNR-IN                         
596100                                  MOD-IDORDNR-IN                          
596200                                  MOD-IDKOLLI-IN                          
596300                                  MOD-IDDC-IN                             
596400                                  MOD-IDPRODNR-IN                         
596500                                  MOD-KDPRTVAL-IN                         
596600                                                                          
596700     MOVE MFS-RENSA-FAELT      TO MOD-TEMFSFEL MOD-TEMFSINF               
596800                                                                          
596900     INSPECT MOD-IDDISTR-UT REPLACING                                     
597000                            LEADING ZEROES BY SPACE                       
597100     INSPECT MOD-IDKUNDNR-UT REPLACING                                    
597200                            LEADING ZEROES BY SPACE                       
597300     INSPECT MOD-IDORDNR-UT REPLACING                                     
597400                            LEADING ZEROES BY SPACE                       
597500     INSPECT MOD-IDKOLLI-UT REPLACING                                     
597600                            LEADING ZEROES BY SPACE                       
597700     INSPECT MOD-IDPRODNR-UT REPLACING                                    
597800                            LEADING ZEROES BY SPACE                       
597900     .                                                                    
598000     EJECT                                                                
598100 C-SHOW-INFO               SECTION.                                       
598200                                                                          
598300     MOVE KOLLI-ADFLGEO             TO MOD-ADFLGEO                        
598400                                       LISTA-ADFLGEO                      
598500                                       WS-KOLLI-ADFLGEO                   
598600     MOVE KOLLI-ADFLOMR             TO MOD-ADFLOMR                        
598700                                       LISTA-ADFLOMR                      
598800                                       WS-KOLLI-ADFLOMR                   
598900     MOVE KOLLI-ADRUTNIV            TO MOD-ADRUTNIV                       
599000                                       LISTA-ADRUTNIV                     
599100     MOVE KOLLI-ADVMODUL            TO MOD-ADVMODUL                       
599200                                       LISTA-ADVMODUL                     
599300     MOVE KOLLI-ADHMODUL            TO MOD-ADHMODUL                       
599400                                       LISTA-ADHMODUL                     
599500                                                                          
599600     MOVE KOLLI-IDKOLLI-FLER        TO WS-IDKOLLI-FLER                    
599700     MOVE KOLLI-VKORDBTO-KOLLI      TO LISTA-VKORDBTO                     
599800                                       WS-VKORDBTO                        
599900                                       A6-ZEBRA-JAPAN-VKORDBTO            
600000     MOVE KOLLI-KDEMBTYP            TO WS-KDEMBTYP                        
600100     MOVE KOLLI-KDORDKL             TO WS-KDORDKL                         
600200                                       A6-ZEBRA-JAPAN-KDORDKL             
600300     MOVE KOLLI-IDPSN(1)            TO WS-IDPSN                           
600400     MOVE KOLLI-IDTRPTNR            TO WS-IDTRPTNR                        
600500                                       A6-ZEBRA-JAPAN-IDTRPTNR            
600600     MOVE KOLLI-IDPLOCK             TO A6-ZEBRA-IDPLOCK                   
600700     MOVE SPACE                     TO WS-KDORDKL-TXT                     
600800     MOVE KOLLI-KVORDRAD            TO WS-KOLLI-KVORDRAD                  
600900     IF KOLLI-KDORDKL =  0                                                
601000        MOVE 'VOR'                  TO WS-KDORDKL-TXT                     
601100     END-IF                                                               
601200     MOVE KOLLI-VLORDBTO-KOLLI      TO A6-ZEBRA-JAPAN-VLORDBTO            
601300     MOVE KOLLI-DIKOLLIL            TO WS-KOLLI-DIKOLLIL                  
601400     MOVE KOLLI-DIKOLLIB            TO WS-KOLLI-DIKOLLIB                  
601500     MOVE KOLLI-DIKOLLIH            TO WS-KOLLI-DIKOLLIH                  
601600                                                                          
601700     MOVE KOLLI-KDKOLLI             TO WS-KOLLI-KDKOLLI                   
601800     .                                                                    
601900     EJECT                                                                
602000 H-CONTROL-TRANSPORT-POSTEN          SECTION.                             
602100     MOVE 'STA H-CONTROL-'    TO PGM-POS                                  
602200                                                                          
602300*KONTROLL FÖR FLAGGA MED UPPGIFTER FÖR TRANSPORT MED POSTEN.              
602400*                                                                         
602500*POSTEN-SVERIGE                                                           
602600*                                                                         
602700     IF CDC                                                               
602800     AND (DIST13-SVERIGE)                                                 
602900                                                                          
603000       MOVE KOLLI-IDTRPTNR               TO TRP01-IDTRP                   
603100       IF TRP01-TRP-MED-POSTEN                                            
603200                                                                          
603300         MOVE VORD-IDPRODNR              TO W-KY4504-IDPRODNR             
603400         MOVE KOLLI-IDKOLLI              TO W-KY4504-IDKOLLI              
603500         PERFORM IMS-GHU-WDGX4503-04                                      
603600                                                                          
603700         IF SEGMENT-FINNS                                                 
603800                                                                          
603900           MOVE KOLLI-VKORDBTO-KOLLI     TO 4504-VKORDBTO-KOLLI           
604000           MOVE KOLLI-VLORDBTO-KOLLI     TO 4504-VLORDBTO-KOLLI           
604100           MOVE WS-DAGENS-DATUM-HIT      TO 4504-DAREGDAT                 
604200           MOVE WS-TIDPUNKT(1:4)         TO 4504-TIHHMM                   
604300           IF  4504-IDKLIID NUMERIC                                       
604400           AND 4504-IDKLIID > ZERO                                        
604500             MOVE 4504-IDKLIID           TO WS-IDKLIID-1-11               
604600             MOVE 'SE'                   TO WS-IDKLIID-12                 
604700           ELSE                                                           
604800             PERFORM HA-GENERATE-IDKLIID                                  
604900             MOVE WS-IDKLIID-1-11        TO 4504-IDKLIID                  
605000           END-IF                                                         
605100                                                                          
605200           PERFORM IMS-REPL-WDGX4503-04                                   
605300         ELSE                                                             
605400                                                                          
605500           MOVE VORD-IDPRODNR            TO 4504-IDPRODNR                 
605600           MOVE KOLLI-IDKOLLI            TO 4504-IDKOLLI                  
605700           MOVE WS-ADGMT-PADR            TO 4504-ADGMT-PADR               
605800           MOVE KORD-IDGMTREF            TO 4504-IDGMTREF                 
605900                                                                          
606000           IF VORD-KDORDKL = 0 OR                                         
606100              VORD-KDORDKL = 1 OR                                         
606200              VORD-KDORDKL = 2                                            
606300             MOVE 06                     TO 4504-IDGODS                   
606400           ELSE                                                           
606500             IF VORD-KDORDKL = 3 OR                                       
606600                VORD-KDORDKL = 4                                          
606700               MOVE 07                   TO 4504-IDGODS                   
606800             END-IF                                                       
606900           END-IF                                                         
607000                                                                          
607100           MOVE WS-DAGENS-DATUM-HIT      TO 4504-DAREGDAT                 
607200           MOVE WS-TIDPUNKT(1:4)         TO 4504-TIHHMM                   
607300           MOVE KOLLI-VKORDBTO-KOLLI     TO 4504-VKORDBTO-KOLLI           
607400           MOVE KOLLI-VLORDBTO-KOLLI     TO 4504-VLORDBTO-KOLLI           
607500                                                                          
607600           PERFORM HA-GENERATE-IDKLIID                                    
607700           MOVE WS-IDKLIID-1-11          TO 4504-IDKLIID                  
607800                                                                          
607900           PERFORM HE-HAMTA-SORT-POS                                      
608000                                                                          
608100           PERFORM IMS-ISRT-WDGX4503-04                                   
608200         END-IF                                                           
608300       END-IF                                                             
608400     ELSE                                                                 
608500*                                                                         
608600*POSTEN-FINLAND                                                           
608700*                                                                         
608800       IF CDC                                                             
608900       AND DIST83-HIT-FI                                                  
609000                                                                          
609100         IF KOLLI-IDTRPTNR > WS-FLYG-TRP                                  
609200                                                                          
609300           MOVE VORD-IDPRODNR            TO W-KY4504-IDPRODNR             
609400           MOVE KOLLI-IDKOLLI            TO W-KY4504-IDKOLLI              
609500           PERFORM IMS-GHU-WDGX4503-04                                    
609600                                                                          
609700           IF SEGMENT-FINNS                                               
609800                                                                          
609900             MOVE KOLLI-VKORDBTO-KOLLI   TO 4504-VKORDBTO-KOLLI           
610000             MOVE KOLLI-VLORDBTO-KOLLI   TO 4504-VLORDBTO-KOLLI           
610100             MOVE WS-DAGENS-DATUM-HIT    TO 4504-DAREGDAT                 
610200             MOVE WS-TIDPUNKT(1:4)       TO 4504-TIHHMM                   
610300             IF 4504-IDKLIID NUMERIC                                      
610400             AND 4504-IDKLIID > ZERO                                      
610500               MOVE 4504-IDKLIID         TO WS-IDKLIID-1-11               
610600               MOVE 'SE'                 TO WS-IDKLIID-12                 
610700             ELSE                                                         
610800               PERFORM HB-GENERATE-IDKLIID-FI                             
610900                                                                          
611000               MOVE WS-IDKLIID-1-11      TO 4504-IDKLIID                  
611100             END-IF                                                       
611200                                                                          
611300             PERFORM IMS-REPL-WDGX4503-04                                 
611400           ELSE                                                           
611500                                                                          
611600             MOVE VORD-IDPRODNR          TO 4504-IDPRODNR                 
611700             MOVE KOLLI-IDKOLLI          TO 4504-IDKOLLI                  
611800             MOVE WS-ADGMT-PADR          TO 4504-ADGMT-PADR               
611900             MOVE KORD-IDGMTREF          TO 4504-IDGMTREF                 
612000                                                                          
612100             IF VORD-KDORDKL = 0 OR                                       
612200                VORD-KDORDKL = 1 OR                                       
612300                VORD-KDORDKL = 2                                          
612400               MOVE 06                   TO 4504-IDGODS                   
612500             ELSE                                                         
612600               IF VORD-KDORDKL = 3 OR                                     
612700                  VORD-KDORDKL = 4                                        
612800                 MOVE 07                 TO 4504-IDGODS                   
612900               END-IF                                                     
613000             END-IF                                                       
613100                                                                          
613200             MOVE WS-DAGENS-DATUM-HIT    TO 4504-DAREGDAT                 
613300             MOVE WS-TIDPUNKT(1:4)       TO 4504-TIHHMM                   
613400             MOVE KOLLI-VKORDBTO-KOLLI   TO 4504-VKORDBTO-KOLLI           
613500             MOVE KOLLI-VLORDBTO-KOLLI   TO 4504-VLORDBTO-KOLLI           
613600                                                                          
613700             PERFORM HB-GENERATE-IDKLIID-FI                               
613800             MOVE WS-IDKLIID-1-11        TO 4504-IDKLIID                  
613900                                                                          
614000             PERFORM HE-HAMTA-SORT-POS                                    
614100                                                                          
614200             PERFORM IMS-ISRT-WDGX4503-04                                 
614300           END-IF                                                         
614400         END-IF                                                           
614500       END-IF                                                             
614600*                                                                         
614700*POSTEN-NORGE                                                             
614800*                                                                         
614900       IF CDC                                                             
615000       AND DIST83-HIT-NO                                                  
615100       AND VORD-KDFRAKT NOT = +31                                         
615200                                                                          
615300         MOVE VORD-IDPRODNR              TO W-KY4504-IDPRODNR             
615400         MOVE KOLLI-IDKOLLI              TO W-KY4504-IDKOLLI              
615500         PERFORM IMS-GHU-WDGX4503-04                                      
615600                                                                          
615700         IF SEGMENT-FINNS                                                 
615800                                                                          
615900           MOVE KOLLI-VKORDBTO-KOLLI     TO 4504-VKORDBTO-KOLLI           
616000           MOVE KOLLI-VLORDBTO-KOLLI     TO 4504-VLORDBTO-KOLLI           
616100           MOVE WS-DAGENS-DATUM-HIT      TO 4504-DAREGDAT                 
616200           MOVE WS-TIDPUNKT(1:4)         TO 4504-TIHHMM                   
616300           IF 4504-IDKLIID NUMERIC                                        
616400           AND 4504-IDKLIID > ZERO                                        
616500             MOVE 4504-IDKLIID           TO WS-IDKLIID-1-11               
616600             MOVE 'SE'                   TO WS-IDKLIID-12                 
616700           ELSE                                                           
616800             PERFORM HD-GENERATE-IDKLIID-NO                               
616900                                                                          
617000             MOVE WS-IDKLIID-1-11        TO 4504-IDKLIID                  
617100           END-IF                                                         
617200                                                                          
617300           PERFORM IMS-REPL-WDGX4503-04                                   
617400         ELSE                                                             
617500                                                                          
617600           MOVE VORD-IDPRODNR            TO 4504-IDPRODNR                 
617700           MOVE KOLLI-IDKOLLI            TO 4504-IDKOLLI                  
617800           MOVE WS-ADGMT-PADR            TO 4504-ADGMT-PADR               
617900           MOVE KORD-IDGMTREF            TO 4504-IDGMTREF                 
618000                                                                          
618100           IF VORD-KDORDKL = 0 OR                                         
618200              VORD-KDORDKL = 1 OR                                         
618300              VORD-KDORDKL = 2                                            
618400             MOVE 06                     TO 4504-IDGODS                   
618500           ELSE                                                           
618600             IF VORD-KDORDKL = 3 OR                                       
618700                VORD-KDORDKL = 4                                          
618800               MOVE 07                   TO 4504-IDGODS                   
618900             END-IF                                                       
619000           END-IF                                                         
619100                                                                          
619200           MOVE WS-DAGENS-DATUM-HIT      TO 4504-DAREGDAT                 
619300           MOVE WS-TIDPUNKT(1:4)         TO 4504-TIHHMM                   
619400           MOVE KOLLI-VKORDBTO-KOLLI     TO 4504-VKORDBTO-KOLLI           
619500           MOVE KOLLI-VLORDBTO-KOLLI     TO 4504-VLORDBTO-KOLLI           
619600                                                                          
619700           PERFORM HD-GENERATE-IDKLIID-NO                                 
619800           MOVE WS-IDKLIID-1-11          TO 4504-IDKLIID                  
619900                                                                          
620000*                                                                         
620100*   NORGE FÅR SORT.POS BEROENDE AV FRAKT                                  
620200*                                                                         
620300           IF VORD-KDFRAKT = WS-FLYG-FRAKT                                
620400             MOVE WS-AIR                 TO 4504-ADFLGEO                  
620500           ELSE                                                           
620600             MOVE WS-LHS                 TO 4504-ADFLGEO                  
620700           END-IF                                                         
620800                                                                          
620900           PERFORM IMS-ISRT-WDGX4503-04                                   
621000         END-IF                                                           
621100       END-IF                                                             
621200     END-IF                                                               
621300     MOVE 'END H-CONTROL-'    TO PGM-POS                                  
621400     .                                                                    
621500     EJECT                                                                
621600 HA-GENERATE-IDKLIID SECTION.                                             
621700     MOVE 'STA HA-GENERATE-'    TO PGM-POS                                
621800                                                                          
621900     PERFORM IMS-GHU-WDGX4523-24                                          
622000                                                                          
622100     MOVE ZERO TO WS-SUMMA                                                
622200     MOVE 4524-IDKLIID-AKT TO WS-IDKLIID-1-10                             
622300                                                                          
622400     MOVE 1 TO KLIID-IX                                                   
622500     PERFORM UNTIL KLIID-IX = 11                                          
622600       MOVE WS-IDKLIID-1-10(KLIID-IX:1) TO WS-SIFFRA                      
622700       MOVE WS-VIKT(KLIID-IX:1) TO WS-VIKT-SIFFRA                         
622800                                                                          
622900       COMPUTE WS-SUMMA = WS-SUMMA + WS-SIFFRA * WS-VIKT-SIFFRA           
623000                                                                          
623100       ADD +1 TO KLIID-IX                                                 
623200     END-PERFORM                                                          
623300                                                                          
623400     DIVIDE 11 INTO WS-SUMMA GIVING WS-HELTAL                             
623500                             REMAINDER WS-REST                            
623600                                                                          
623700     COMPUTE WS-CD    = 11 - WS-REST                                      
623800                                                                          
623900     EVALUATE TRUE                                                        
624000     WHEN WS-CD     = 11                                                  
624100       MOVE 5            TO WS-IDKLIID-11                                 
624200     WHEN WS-CD     = 10                                                  
624300       MOVE 0            TO WS-IDKLIID-11                                 
624400     WHEN OTHER                                                           
624500       MOVE WS-CD        TO WS-IDKLIID-11                                 
624600     END-EVALUATE                                                         
624700                                                                          
624800     ADD +1              TO 4524-IDKLIID-AKT                              
624900                                                                          
625000*    IF 4524-IDKLIID-AKT > 4524-IDKLIID-MAX - 20000                       
625100*      SKICKA MAIL TILL LINJEN                                            
625200*      CONTINUE                                                           
625300*    END-IF                                                               
625400                                                                          
625500     PERFORM IMS-REPL-WDGX4524                                            
625600     MOVE 'END HA-GENERATE-'  TO PGM-POS                                  
625700     .                                                                    
625800     EJECT                                                                
625900 HB-GENERATE-IDKLIID-FI SECTION.                                          
626000     MOVE 'STA HB-GENERATE-'    TO PGM-POS                                
626100                                                                          
626200     PERFORM IMS-GHU-WDGX4523-24                                          
626300                                                                          
626400     MOVE ZERO TO WS-SUMMA                                                
626500     MOVE 4524-IDKLIID-AKT-FI TO WS-IDKLIID-1-10                          
626600                                                                          
626700     MOVE 1 TO KLIID-IX                                                   
626800     PERFORM UNTIL KLIID-IX = 11                                          
626900       MOVE WS-IDKLIID-1-10(KLIID-IX:1) TO WS-SIFFRA                      
627000       MOVE WS-VIKT(KLIID-IX:1) TO WS-VIKT-SIFFRA                         
627100                                                                          
627200       COMPUTE WS-SUMMA = WS-SUMMA + WS-SIFFRA * WS-VIKT-SIFFRA           
627300                                                                          
627400       ADD +1 TO KLIID-IX                                                 
627500     END-PERFORM                                                          
627600                                                                          
627700     DIVIDE 11 INTO WS-SUMMA GIVING WS-HELTAL                             
627800                             REMAINDER WS-REST                            
627900                                                                          
628000     COMPUTE WS-CD    = 11 - WS-REST                                      
628100                                                                          
628200     EVALUATE TRUE                                                        
628300     WHEN WS-CD     = 11                                                  
628400       MOVE 5            TO WS-IDKLIID-11                                 
628500     WHEN WS-CD     = 10                                                  
628600       MOVE 0            TO WS-IDKLIID-11                                 
628700     WHEN OTHER                                                           
628800       MOVE WS-CD        TO WS-IDKLIID-11                                 
628900     END-EVALUATE                                                         
629000                                                                          
629100     ADD +1              TO 4524-IDKLIID-AKT-FI                           
629200                                                                          
629300*    IF 4524-IDKLIID-AKT-FI > 4524-IDKLIID-MAX-FI - 20000                 
629400*      SKICKA MAIL TILL LINJEN                                            
629500*      CONTINUE                                                           
629600*    END-IF                                                               
629700                                                                          
629800     PERFORM IMS-REPL-WDGX4524                                            
629900     MOVE 'END HB-GENERATE-'  TO PGM-POS                                  
630000     .                                                                    
630100     EJECT                                                                
630200 HD-GENERATE-IDKLIID-NO SECTION.                                          
630300     MOVE 'STA HC-GENERATE-'    TO PGM-POS                                
630400                                                                          
630500     PERFORM IMS-GHU-WDGX4523-24                                          
630600                                                                          
630700     MOVE ZERO TO WS-SUMMA                                                
630800     MOVE 4524-IDKLIID-AKT-NO TO WS-IDKLIID-1-10                          
630900                                                                          
631000     MOVE 1 TO KLIID-IX                                                   
631100     PERFORM UNTIL KLIID-IX = 11                                          
631200       MOVE WS-IDKLIID-1-10(KLIID-IX:1) TO WS-SIFFRA                      
631300       MOVE WS-VIKT(KLIID-IX:1) TO WS-VIKT-SIFFRA                         
631400                                                                          
631500       COMPUTE WS-SUMMA = WS-SUMMA + WS-SIFFRA * WS-VIKT-SIFFRA           
631600                                                                          
631700       ADD +1 TO KLIID-IX                                                 
631800     END-PERFORM                                                          
631900                                                                          
632000     DIVIDE 11 INTO WS-SUMMA GIVING WS-HELTAL                             
632100                             REMAINDER WS-REST                            
632200                                                                          
632300     COMPUTE WS-CD    = 11 - WS-REST                                      
632400                                                                          
632500     EVALUATE TRUE                                                        
632600     WHEN WS-CD     = 11                                                  
632700       MOVE 5            TO WS-IDKLIID-11                                 
632800     WHEN WS-CD     = 10                                                  
632900       MOVE 0            TO WS-IDKLIID-11                                 
633000     WHEN OTHER                                                           
633100       MOVE WS-CD        TO WS-IDKLIID-11                                 
633200     END-EVALUATE                                                         
633300                                                                          
633400     ADD +1              TO 4524-IDKLIID-AKT-NO                           
633500                                                                          
633600*    IF 4524-IDKLIID-AKT-NO > 4524-IDKLIID-MAX-NO - 20000                 
633700*      SKICKA MAIL TILL LINJEN                                            
633800*      CONTINUE                                                           
633900*    END-IF                                                               
634000                                                                          
634100     PERFORM IMS-REPL-WDGX4524                                            
634200     MOVE 'END HC-GENERATE-'  TO PGM-POS                                  
634300     .                                                                    
634400     EJECT                                                                
634500 HE-HAMTA-SORT-POS SECTION.                                               
634600     MOVE 'STA HD-HAMTA-'     TO PGM-POS                                  
634700                                                                          
634800     MOVE WS-IDKUNDNR           TO W-IDKUNDNR                             
634900     MOVE WS-IDDISTR            TO W-IDDISTR                              
635000     PERFORM IMS-GU-GMTA01-WDB201                                         
635100                                                                          
635200     MOVE GMT-IDROUTE           TO WS-IDROUTE-HIT                         
635300     MOVE GMT-IDDEPOT           TO WS-IDDEPOT-HIT                         
635400                                                                          
635500     MOVE WS-ADFLGEO-HIT        TO 4504-ADFLGEO                           
635600     .                                                                    
635700     EJECT                                                                
635800 D-PRINT-LIST              SECTION.                                       
635900                                                                          
636000     PERFORM DA-REDIGERA-KOLLIFLAGGA                                      
636100                                                                          
636200     IF DONT-PRINT-CASE-LABEL                                             
636300       MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDPRTVAL-IN-ATTR                  
636400*DHL                                                                      
636500       MOVE WS-IDDISTR         TO TEST-IDDISTR                            
636600       MOVE WS-IDDC            TO WS-DC                                   
636700       IF CDC-SE                                                          
636800         CONTINUE                                                         
636900*        PERFORM S10-EV-START-W4339                                       
637000*        MOVE MED-1 (SPRAK-IX) TO MOD-TEMFSINF                            
637100       END-IF                                                             
637200*DHL                                                                      
637300     ELSE                                                                 
637400       MOVE '4'                TO WS-SYSTDEL                              
637500       MOVE 'KF'               TO WS-LISTTYP                              
637600       MOVE WS-IDDC            TO WS-DC                                   
637700       MOVE WS-KDPRTVAL        TO WS-KDPRT                                
637800                                                                          
637900       MOVE 001                TO PRT-KDCALL                              
638000       MOVE WS-IDPRTLST        TO PRT-IDPRTLST                            
638100                                                                          
638200       CALL W006PRT USING PRT-W006PRT                                     
638300       MOVE WS-IDDISTR         TO TEST-IDDISTR                            
638400       MOVE WS-IDDISTR         TO DIST75-IDDISTR                          
638500       MOVE WS-IDKUNDNR        TO DIST75-IDKUNDNR                         
638600                                                                          
638700       IF PRT-KDSVAR = RAETT                                              
638800          MOVE PRT-IDPRTLST    TO LISTVAL                                 
638900                                                                          
639000          EVALUATE TRUE                                                   
639100             WHEN CDC-SE AND (DIST35-REFILL                               
639200                          OR DIST75-Q-CASE-LABEL                          
639300                          OR DIST75-Q-CASE-LABEL2)                        
639400*OBS! OM DISTRIKT LÄGGS TILL SKALL DET ÄVEN GÖRAS I SEKTION DE-.          
639500                          PERFORM DE-PRT-REFILL-A6-TERMO                  
639600             WHEN CDC-SE AND SKRIV-KOLLIFLAGGA-A6-FORMAT                  
639700                  IF DIST13-SVERIGE                                       
639800*POSTNORD                                                                 
639900                    PERFORM S09-MARKP-A6-NL-GBG-POSTNORD                  
640000                  ELSE                                                    
640010*POSTNORD                                                                 
640100                    PERFORM S03-MARKP-A6-NL-CDC-SDC                       
640200*                   PERFORM S10-EV-START-W4339                            
640300*DHL                                                                      
640400                  END-IF                                                  
640500             WHEN CDC-SE                                                  
640600                                                                          
640700                  IF DIST13-SVERIGE                                       
640800                    IF SKRIV-PA-NOVA-SKRIVARE                             
640900                      PERFORM DB-PRINT-NOVA-LABEL                         
641000                    ELSE                                                  
641100                      IF (OHUV-IDSYSTEM = 'LDC' OR 'TACD')                
641200                      AND GMT-FLLDCKND = JA                               
641300*TACDIS                                                                   
641400                        PERFORM DC-TACDIS                                 
641500                      ELSE                                                
641600                        PERFORM S08-MARKPOINT-7INC-SVERIGE                
641700                      END-IF                                              
641800                    END-IF                                                
641900                  ELSE                                                    
642000                    IF SKRIV-PA-NOVA-SKRIVARE                             
642100                      PERFORM S11-PRINT-NOVA-LABEL                        
642200                    ELSE                                                  
642300                      PERFORM S01-PRINT-MARKPOINT-TERMO7INC               
642400                    END-IF                                                
642500*                   PERFORM S10-EV-START-W4339                            
642600*DHL                                                                      
642700                  END-IF                                                  
642800*PGA ATT DET FINNS MARKPOINT-SKRIVARE MED 7INCH EL. A6 -FORMAT I          
642900*DC11 SÅ FINNS DET TVÅ SEKTIONER  S01-. OCH S03-. FÖR DC11.)              
643000*OM DU LÄGGER TILL PRT-ALIAS I W006PRT FÖR DC11 OCH A6-MARKPOINT          
643100*SÅ SKALL ÄVEN PRT-ALIAS LÄGGAS TILL SOM GODK. WS-KDPRTVAL OVAN.          
643200*DETTA FÖR ATT SKILJA VAD SOM SKALL SKRIVAS PÅ 7INCH RESP.A6 PRT.         
643300             WHEN NDC-NA                                                  
643400                  PERFORM DF-PRT-NDC-ZEBRA-A6                             
643500             WHEN NDC-JP                                                  
643600**                MOVE 700           TO WS-TIME-DELAY                     
643700**                CALL W009WAIT USING WS-TIME-DELAY                       
643800                                                                          
643900                  PERFORM DF-PRT-NDC-ZEBRA-A6                             
644000             WHEN OTHER                                                   
644100                  CONTINUE                                                
644200          END-EVALUATE                                                    
644300                                                                          
644400          MOVE MED-1 (SPRAK-IX) TO MOD-TEMFSINF                           
644500       ELSE                                                               
644600          MOVE FEL-4 (SPRAK-IX) TO MOD-TEMFSFEL                           
644700          MOVE MFS-ALFA-FAELT-FEL TO MOD-KDPRTVAL-IN-ATTR                 
644800       END-IF                                                             
644900     END-IF                                                               
645000     .                                                                    
645100     EJECT                                                                
645200 DA-REDIGERA-KOLLIFLAGGA   SECTION.                                       
645300                                                                          
645400     MOVE WS-IDDISTR         TO LISTA-IDDISTR                             
645500     MOVE WS-IDDISTR         TO DIST29-IDDISTR                            
645600                                DIST35-IDDISTR                            
645700     IF (OHUV-IDSYSTEM = 'LDC' OR 'TACD')                                 
645800     AND (DIST13-SVERIGE OR DIST05-NORGE)                                 
645900     AND GMT-FLLDCKND = JA                                                
646000     AND OHUV-IDGROSS > ZERO                                              
646100       IF TAKF-FINNS-SW = JA                                              
646200         CONTINUE                                                         
646300       ELSE                                                               
646400         MOVE WS-IDKUNDNR    TO LISTA-IDKUNDNR                            
646500       END-IF                                                             
646600     ELSE                                                                 
646700       MOVE WS-IDKUNDNR      TO LISTA-IDKUNDNR                            
646800     END-IF                                                               
646900                                                                          
647000     MOVE WS-IDDISTR         TO DIST05-IDDISTR                            
647100                                                                          
647200     MOVE OHUV-IDBILREG      TO LISTA-IDBILREG                            
647300     MOVE OHUV-BEKUNDRF      TO WS-OHUV-BEKUNDRF                          
647400                                                                          
647500     MOVE WS-IDORDNR         TO LISTA-IDORDNR                             
647600                                                                          
647700     IF WS-IDKOLLI-FLER < ZERO                                            
647800       MOVE ZERO             TO LISTA-IDKOLLI                             
647900     ELSE                                                                 
648000       MOVE WS-IDKOLLI-PRT   TO LISTA-IDKOLLI                             
648100     END-IF                                                               
648200                                                                          
648300     MOVE WS-IDDISTR         TO DIST83-IDDISTR                            
648400     .                                                                    
648500     SKIP2                                                                
648600 DF-PRT-NDC-ZEBRA-A6      SECTION.                                        
648700                                                                          
648800     MOVE LISTA-IDDISTR  TO A6-ZEBRA-IDDISTR                              
648900                            A6-ZEBRA-CODE-IDDISTR                         
649000     MOVE LISTA-IDKUNDNR TO A6-ZEBRA-IDKUNDNR                             
649100                            A6-ZEBRA-CODE-IDKUNDNR                        
649200                            A6-ZEBRA-JAPAN-IDKUNDNR                       
649300     MOVE LISTA-IDORDNR  TO A6-ZEBRA-IDORDNR                              
649400                            A6-ZEBRA-CODE-IDORDNR                         
649500                            A6-ZEBRA-JAPAN-IDORDNR                        
649600     MOVE WS-IDKOLLI-PRT TO A6-ZEBRA-IDKOLLI                              
649700                            A6-ZEBRA-CODE-IDKOLLI                         
649800                            A6-ZEBRA-JAPAN-IDKOLLI                        
649900     MOVE LISTA-KDFRAKT  TO A6-ZEBRA-KDFRAKT                              
650000                                                                          
650100     IF NDC-AU                                                            
650200         EVALUATE WS-KDORDKL                                              
650300            WHEN 0 THRU 2                                                 
650400              MOVE '     D  ' TO A6-ZEBRA-ORDER-TYPE                      
650500            WHEN 3 THRU 4                                                 
650600              MOVE '     B  ' TO A6-ZEBRA-ORDER-TYPE                      
650700         END-EVALUATE                                                     
650800     ELSE                                                                 
650900       IF NDC-JP                                                          
651000         EVALUATE WS-KDORDKL                                              
651100*           WHEN 0                                                        
651200*             MOVE 'VOR'  TO A6-ZEBRA-ORDER-TYPE-JP                       
651300            WHEN 0 THRU 1                                                 
651400              MOVE 'D'    TO A6-ZEBRA-ORDER-TYPE-JP                       
651500            WHEN 2 THRU 4                                                 
651600              MOVE 'B'    TO A6-ZEBRA-ORDER-TYPE-JP                       
651700            WHEN OTHER                                                    
651800              MOVE WS-KDORDKL TO A6-ZEBRA-ORDER-TYPE-JP                   
651900         END-EVALUATE                                                     
652000       ELSE                                                               
652100         EVALUATE WS-KDORDKL                                              
652200            WHEN 0                                                        
652300              MOVE 'VOR     ' TO A6-ZEBRA-ORDER-TYPE                      
652400            WHEN 1                                                        
652500              MOVE 'CRITICAL' TO A6-ZEBRA-ORDER-TYPE                      
652600            WHEN 2 THRU 4                                                 
652700              MOVE 'STOCK   ' TO A6-ZEBRA-ORDER-TYPE                      
652800         END-EVALUATE                                                     
652900       END-IF                                                             
653000     END-IF                                                               
653100                                                                          
653200     IF NDC-JP                                                            
653300                                                                          
653400       MOVE LISTA-ADRESS-1     TO A6-ZEBRA-ADRESS-1                       
653500       MOVE LISTA-ADRESS-2     TO A6-ZEBRA-ADRESS-2                       
653600       MOVE LISTA-ADRESS-4     TO A6-ZEBRA-ADRESS-3                       
653700       MOVE LISTA-ADRESS-3     TO A6-ZEBRA-ADRESS-4                       
653800*      MOVE WS-LISTA-ADRESS-5  TO A6-ZEBRA-ADRESS-3                       
653900     ELSE                                                                 
654000       MOVE LISTA-ADRESS-1     TO A6-ZEBRA-ADRESS-1                       
654100       MOVE LISTA-ADRESS-2     TO A6-ZEBRA-ADRESS-2                       
654200       MOVE LISTA-ADRESS-3     TO A6-ZEBRA-ADRESS-3                       
654300       MOVE LISTA-ADRESS-4     TO A6-ZEBRA-ADRESS-4                       
654400       MOVE WS-LISTA-ADRESS-5  TO A6-ZEBRA-ADRESS-5                       
654500     END-IF                                                               
654600*USA OCH KANADA                                                           
654700     IF NDC-US                                                            
654800       COMPUTE WS-VKORDBTO ROUNDED =                                      
654900               WS-VKORDBTO * CONV-KG-TO-LB                                
655000       END-COMPUTE                                                        
655100     END-IF                                                               
655200     MOVE WS-KILO           TO A6-ZEBRA-KILO                              
655300     MOVE WS-HEKTO          TO A6-ZEBRA-HEKTO                             
655400*                                                                         
655500*    MOVE WS-VKORDBTO(1:5)  TO A6-ZEBRA-VKORDBTO                          
655600                                                                          
655700     IF NDC-AU                                                            
655800     OR NDC-JP                                                            
655900       MOVE WS-DARFS-YYMMDD     TO A6-ZEBRA-TIRFS                         
656000     ELSE                                                                 
656100       MOVE WS-DARFS-YYMMDD(1:2) TO WS-TIRFS-YY                           
656200       MOVE WS-DARFS-YYMMDD(3:2) TO WS-TIRFS-MM                           
656300       MOVE WS-DARFS-YYMMDD(5:2) TO WS-TIRFS-DD                           
656400       MOVE WS-TIRFS-MMDDYY     TO A6-ZEBRA-TIRFS                         
656500     END-IF                                                               
656600                                                                          
656700     MOVE WS-IDTRPTNR           TO A6-ZEBRA-IDTRPTNR                      
656800                                                                          
656900     IF WS-IDKOLLI-PRT = WS-IDKOLLI                                       
657000       CALL W006PRS1 USING PRT-SPOOL-A4S PRT-OPEN LISTVAL                 
657100                           ALT-PCB DUMMY-AREA DUMMY-AREA                  
657200     END-IF                                                               
657300                                                                          
657400     MOVE SPACE       TO A6-ZEBRA-RAD                                     
657500                                                                          
657600     MOVE A6-ZEBRA-STYR-01 TO A6-ZEBRA-RAD                                
657700     CALL W006PRS1 USING PRT-SPOOL-A4S PRT-WRITE LISTVAL                  
657800                         ALT-PCB PRT-NYSIDA-RAD1 A6-ZEBRA-RAD             
657900                                                                          
658000     IF NOT NDC-JP                                                        
658100       IF WS-IDPSN NOT = 000                                              
658200                                                                          
658300         MOVE A6-ZEBRA-RUB-DANGEROUS TO A6-ZEBRA-RAD                      
658400         CALL W006PRS1 USING PRT-SPOOL-A4S PRT-WRITE LISTVAL              
658500                             ALT-PCB PRT-AFTER-1 A6-ZEBRA-RAD             
658600                                                                          
658700         MOVE 'H'               TO A6-ZEBRA-H                             
658800                                                                          
658900         MOVE A6-ZEBRA-DATA-H   TO A6-ZEBRA-RAD                           
659000         CALL W006PRS1 USING PRT-SPOOL-A4S PRT-WRITE LISTVAL              
659100                             ALT-PCB PRT-AFTER-1 A6-ZEBRA-RAD             
659200       END-IF                                                             
659300     END-IF                                                               
659400                                                                          
659500     MOVE A6-ZEBRA-RUB-DISTRICT TO A6-ZEBRA-RAD                           
659600     CALL W006PRS1 USING PRT-SPOOL-A4S PRT-WRITE LISTVAL                  
659700                         ALT-PCB PRT-AFTER-1 A6-ZEBRA-RAD                 
659800                                                                          
659900     IF NDC-AU                                                            
660000     OR NDC-JP                                                            
660100       MOVE A6-ZEBRA-RUB-DEALER   TO A6-ZEBRA-RAD                         
660200     ELSE                                                                 
660300       MOVE A6-ZEBRA-RUB-RETAILER TO A6-ZEBRA-RAD                         
660400     END-IF                                                               
660500     CALL W006PRS1 USING PRT-SPOOL-A4S PRT-WRITE LISTVAL                  
660600                         ALT-PCB PRT-AFTER-1 A6-ZEBRA-RAD                 
660700                                                                          
660800     MOVE A6-ZEBRA-RUB-ORDER-NUMBER TO A6-ZEBRA-RAD                       
660900     CALL W006PRS1 USING PRT-SPOOL-A4S PRT-WRITE LISTVAL                  
661000                         ALT-PCB PRT-AFTER-1 A6-ZEBRA-RAD                 
661100                                                                          
661200     MOVE A6-ZEBRA-RUB-CASE-NUMBER TO A6-ZEBRA-RAD                        
661300     CALL W006PRS1 USING PRT-SPOOL-A4S PRT-WRITE LISTVAL                  
661400                         ALT-PCB PRT-AFTER-1 A6-ZEBRA-RAD                 
661500                                                                          
661600     IF NDC-NA                                                            
661700       MOVE A6-ZEBRA-RUB-PICKER  TO A6-ZEBRA-RAD                          
661800       CALL W006PRS1 USING PRT-SPOOL-A4S PRT-WRITE LISTVAL                
661900                           ALT-PCB PRT-AFTER-1 A6-ZEBRA-RAD               
662000     END-IF                                                               
662100                                                                          
662200     MOVE A6-ZEBRA-RUB-WEIGHT TO A6-ZEBRA-RAD                             
662300     CALL W006PRS1 USING PRT-SPOOL-A4S PRT-WRITE LISTVAL                  
662400                         ALT-PCB PRT-AFTER-1 A6-ZEBRA-RAD                 
662500                                                                          
662600     MOVE A6-ZEBRA-RUB-CONSIGNEE TO A6-ZEBRA-RAD                          
662700     CALL W006PRS1 USING PRT-SPOOL-A4S PRT-WRITE LISTVAL                  
662800                         ALT-PCB PRT-AFTER-1 A6-ZEBRA-RAD                 
662900                                                                          
663000     MOVE A6-ZEBRA-RUB-RFS TO A6-ZEBRA-RAD                                
663100     CALL W006PRS1 USING PRT-SPOOL-A4S PRT-WRITE LISTVAL                  
663200                         ALT-PCB PRT-AFTER-1 A6-ZEBRA-RAD                 
663300                                                                          
663400     MOVE A6-ZEBRA-RUB-FREIGHT-CODE TO A6-ZEBRA-RAD                       
663500     CALL W006PRS1 USING PRT-SPOOL-A4S PRT-WRITE LISTVAL                  
663600                         ALT-PCB PRT-AFTER-1 A6-ZEBRA-RAD                 
663700                                                                          
663800     MOVE A6-ZEBRA-RUB-ORDER-TYPE TO A6-ZEBRA-RAD                         
663900     CALL W006PRS1 USING PRT-SPOOL-A4S PRT-WRITE LISTVAL                  
664000                         ALT-PCB PRT-AFTER-1 A6-ZEBRA-RAD                 
664100                                                                          
664200     MOVE A6-ZEBRA-RUB-TRANSPORT  TO A6-ZEBRA-RAD                         
664300     CALL W006PRS1 USING PRT-SPOOL-A4S PRT-WRITE LISTVAL                  
664400                         ALT-PCB PRT-AFTER-1 A6-ZEBRA-RAD                 
664500                                                                          
664600     MOVE A6-ZEBRA-RUB-SHIPPER TO A6-ZEBRA-RAD                            
664700     CALL W006PRS1 USING PRT-SPOOL-A4S PRT-WRITE LISTVAL                  
664800                         ALT-PCB PRT-AFTER-1 A6-ZEBRA-RAD                 
664900*DATA-FIELDS                                                              
665000     MOVE A6-ZEBRA-DATA-IDDISTR   TO A6-ZEBRA-RAD                         
665100     CALL W006PRS1 USING PRT-SPOOL-A4S PRT-WRITE LISTVAL                  
665200                         ALT-PCB PRT-AFTER-1 A6-ZEBRA-RAD                 
665300                                                                          
665400     MOVE A6-ZEBRA-DATA-IDKUNDNR  TO A6-ZEBRA-RAD                         
665500     CALL W006PRS1 USING PRT-SPOOL-A4S PRT-WRITE LISTVAL                  
665600                         ALT-PCB PRT-AFTER-1 A6-ZEBRA-RAD                 
665700                                                                          
665800     MOVE A6-ZEBRA-DATA-IDORDNR   TO A6-ZEBRA-RAD                         
665900     CALL W006PRS1 USING PRT-SPOOL-A4S PRT-WRITE LISTVAL                  
666000                         ALT-PCB PRT-AFTER-1 A6-ZEBRA-RAD                 
666100                                                                          
666200     MOVE A6-ZEBRA-DATA-IDKOLLI     TO A6-ZEBRA-RAD                       
666300     CALL W006PRS1 USING PRT-SPOOL-A4S PRT-WRITE LISTVAL                  
666400                         ALT-PCB PRT-AFTER-1 A6-ZEBRA-RAD                 
666500                                                                          
666600     MOVE A6-ZEBRA-DATA-ADRESS-1       TO A6-ZEBRA-RAD                    
666700     CALL W006PRS1 USING PRT-SPOOL-A4S PRT-WRITE LISTVAL                  
666800                         ALT-PCB PRT-AFTER-1 A6-ZEBRA-RAD                 
666900                                                                          
667000     MOVE A6-ZEBRA-DATA-ADRESS-2       TO A6-ZEBRA-RAD                    
667100     CALL W006PRS1 USING PRT-SPOOL-A4S PRT-WRITE LISTVAL                  
667200                         ALT-PCB PRT-AFTER-1 A6-ZEBRA-RAD                 
667300                                                                          
667400     MOVE A6-ZEBRA-DATA-ADRESS-3       TO A6-ZEBRA-RAD                    
667500     CALL W006PRS1 USING PRT-SPOOL-A4S PRT-WRITE LISTVAL                  
667600                         ALT-PCB PRT-AFTER-1 A6-ZEBRA-RAD                 
667700                                                                          
667800     MOVE A6-ZEBRA-DATA-ADRESS-4       TO A6-ZEBRA-RAD                    
667900     CALL W006PRS1 USING PRT-SPOOL-A4S PRT-WRITE LISTVAL                  
668000                         ALT-PCB PRT-AFTER-1 A6-ZEBRA-RAD                 
668100                                                                          
668200     MOVE A6-ZEBRA-DATA-ADRESS-5       TO A6-ZEBRA-RAD                    
668300     CALL W006PRS1 USING PRT-SPOOL-A4S PRT-WRITE LISTVAL                  
668400                         ALT-PCB PRT-AFTER-1 A6-ZEBRA-RAD                 
668500                                                                          
668600     MOVE A6-ZEBRA-DATA-TIRFS    TO A6-ZEBRA-RAD                          
668700     CALL W006PRS1 USING PRT-SPOOL-A4S PRT-WRITE LISTVAL                  
668800                         ALT-PCB PRT-AFTER-1 A6-ZEBRA-RAD                 
668900                                                                          
669000*    MOVE A6-ZEBRA-DATA-VKORDBTO   TO A6-ZEBRA-RAD                        
669100     MOVE A6-ZEBRA-DATA-WEIGHT     TO A6-ZEBRA-RAD                        
669200     CALL W006PRS1 USING PRT-SPOOL-A4S PRT-WRITE LISTVAL                  
669300                         ALT-PCB PRT-AFTER-1 A6-ZEBRA-RAD                 
669400                                                                          
669500     IF NDC-JP                                                            
669600       MOVE A6-ZEBRA-DATA-ORDER-TYPE-JP  TO A6-ZEBRA-RAD                  
669700       CALL W006PRS1 USING PRT-SPOOL-A4S PRT-WRITE LISTVAL                
669800                         ALT-PCB PRT-AFTER-1 A6-ZEBRA-RAD                 
669900     ELSE                                                                 
670000       MOVE A6-ZEBRA-DATA-ORDER-TYPE     TO A6-ZEBRA-RAD                  
670100       CALL W006PRS1 USING PRT-SPOOL-A4S PRT-WRITE LISTVAL                
670200                         ALT-PCB PRT-AFTER-1 A6-ZEBRA-RAD                 
670300     END-IF                                                               
670400                                                                          
670500     MOVE A6-ZEBRA-DATA-KDFRAKT      TO A6-ZEBRA-RAD                      
670600     CALL W006PRS1 USING PRT-SPOOL-A4S PRT-WRITE LISTVAL                  
670700                         ALT-PCB PRT-AFTER-1 A6-ZEBRA-RAD                 
670800                                                                          
670900     MOVE A6-ZEBRA-DATA-IDTRPTNR     TO A6-ZEBRA-RAD                      
671000     CALL W006PRS1 USING PRT-SPOOL-A4S PRT-WRITE LISTVAL                  
671100                         ALT-PCB PRT-AFTER-1 A6-ZEBRA-RAD                 
671200                                                                          
671300                                                                          
671400     MOVE A6-ZEBRA-DATA-IDPSN        TO A6-ZEBRA-RAD                      
671500     CALL W006PRS1 USING PRT-SPOOL-A4S PRT-WRITE LISTVAL                  
671600                         ALT-PCB PRT-AFTER-1 A6-ZEBRA-RAD                 
671700                                                                          
671800*SHIPPER-INFO FINNS I SHIPPER-TAB I PROGRAMMET.                           
671900     EVALUATE TRUE                                                        
672000       WHEN NDC-US-RU                                                     
672100         MOVE +1                     TO SHIP-INDX                         
672200       WHEN NDC-US-BAT                                                    
672300         MOVE +2                     TO SHIP-INDX                         
672400       WHEN NDC-US-LA                                                     
672500         MOVE +3                     TO SHIP-INDX                         
672600       WHEN NDC-US-SE                                                     
672700         MOVE +4                     TO SHIP-INDX                         
672800       WHEN NDC-US-CH                                                     
672900         MOVE +5                     TO SHIP-INDX                         
673000       WHEN NDC-US-JA                                                     
673100         MOVE +6                     TO SHIP-INDX                         
673200       WHEN NDC-CA                                                        
673300         MOVE +7                     TO SHIP-INDX                         
673400       WHEN NDC-JP                                                        
673500         MOVE +8                     TO SHIP-INDX                         
673600       WHEN NDC-AU                                                        
673700         MOVE +9                     TO SHIP-INDX                         
673800     END-EVALUATE                                                         
673900                                                                          
674000     MOVE SHIPPER-COMPANY (SHIP-INDX) TO A6-ZEBRA-SHIPPER-COMPANY         
674100     MOVE A6-ZEBRA-DATA-SHIPPER-COMPANY TO A6-ZEBRA-RAD                   
674200     CALL W006PRS1 USING PRT-SPOOL-A4S PRT-WRITE LISTVAL                  
674300                         ALT-PCB PRT-AFTER-1 A6-ZEBRA-RAD                 
674400                                                                          
674500     MOVE SHIPPER-NAME (SHIP-INDX)   TO A6-ZEBRA-SHIPPER-NAME             
674600     MOVE A6-ZEBRA-DATA-SHIPPER-NAME TO A6-ZEBRA-RAD                      
674700     CALL W006PRS1 USING PRT-SPOOL-A4S PRT-WRITE LISTVAL                  
674800                         ALT-PCB PRT-AFTER-1 A6-ZEBRA-RAD                 
674900                                                                          
675000     MOVE SHIPPER-STREET (SHIP-INDX)   TO A6-ZEBRA-SHIPPER-STREET         
675100     MOVE A6-ZEBRA-DATA-SHIPPER-STREET TO A6-ZEBRA-RAD                    
675200     CALL W006PRS1 USING PRT-SPOOL-A4S PRT-WRITE LISTVAL                  
675300                         ALT-PCB PRT-AFTER-1 A6-ZEBRA-RAD                 
675400                                                                          
675500     MOVE SHIPPER-CITY (SHIP-INDX)   TO A6-ZEBRA-SHIPPER-CITY             
675600     MOVE A6-ZEBRA-DATA-SHIPPER-CITY TO A6-ZEBRA-RAD                      
675700     CALL W006PRS1 USING PRT-SPOOL-A4S PRT-WRITE LISTVAL                  
675800                         ALT-PCB PRT-AFTER-1 A6-ZEBRA-RAD                 
675900                                                                          
676000     MOVE SHIPPER-COUNTRY (SHIP-INDX) TO A6-ZEBRA-SHIPPER-COUNTRY         
676100     MOVE A6-ZEBRA-DATA-SHIPPER-COUNTRY TO A6-ZEBRA-RAD                   
676200     CALL W006PRS1 USING PRT-SPOOL-A4S PRT-WRITE LISTVAL                  
676300                         ALT-PCB PRT-AFTER-1 A6-ZEBRA-RAD                 
676400                                                                          
676500     IF NDC-US                                                            
676600       MOVE A6-ZEBRA-DATA-IDPLOCK  TO A6-ZEBRA-RAD                        
676700       CALL W006PRS1 USING PRT-SPOOL-A4S PRT-WRITE LISTVAL                
676800                           ALT-PCB PRT-AFTER-1 A6-ZEBRA-RAD               
676900     END-IF                                                               
677000                                                                          
677100     MOVE A6-ZEBRA-STYR-02 TO A6-ZEBRA-RAD                                
677200     CALL W006PRS1 USING PRT-SPOOL-A4S PRT-WRITE LISTVAL                  
677300                         ALT-PCB PRT-AFTER-1 A6-ZEBRA-RAD                 
677400                                                                          
677500     IF NDC-JP                                                            
677600       MOVE A6-ZEBRA-BARCODE-JAPAN TO A6-ZEBRA-RAD                        
677700     ELSE                                                                 
677800       MOVE A6-ZEBRA-BARCODE       TO A6-ZEBRA-RAD                        
677900     END-IF                                                               
678000     CALL W006PRS1 USING PRT-SPOOL-A4S PRT-WRITE LISTVAL                  
678100                         ALT-PCB PRT-AFTER-1 A6-ZEBRA-RAD                 
678200                                                                          
678300     MOVE A6-ZEBRA-STYR-03 TO A6-ZEBRA-RAD                                
678400     CALL W006PRS1 USING PRT-SPOOL-A4S PRT-WRITE LISTVAL                  
678500                         ALT-PCB PRT-AFTER-1 A6-ZEBRA-RAD                 
678600                                                                          
678700     IF WS-IDKOLLI-PRT = WS-IDKOLLI-TOM                                   
678800       CALL W006PRS1 USING PRT-SPOOL-A4S PRT-CLOSE LISTVAL                
678900                           ALT-PCB DUMMY-AREA DUMMY-AREA                  
679000     END-IF                                                               
679100     .                                                                    
679200     EJECT                                                                
679300 DE-PRT-REFILL-A6-TERMO     SECTION.                                      
679400                                                                          
679500     IF WS-KOLLI-KVORDRAD = +1                                            
679600       PERFORM S05-GET-INFO-FR-WDE420                                     
679700       IF SEGMENT-FINNS                                                   
679800*OBS! OM DISTRIKT LÄGGS TILL SKALL DET ÄVEN GÖRAS I SEKTION D-.           
679900         IF WS-IDDISTR = 5220                                             
680000           MOVE ZERO             TO NOVA-LBL-ADLAGOMR                     
680100                                    NOVA-LBL-ADLAGOMR-NDC                 
680200                                    NOVA-LBL-ADGANG                       
680300                                    NOVA-LBL-ADGANG-NDC                   
680400                                    NOVA-LBL-ADPLATS                      
680500                                    NOVA-LBL-ADPLATS-NDC                  
680600                                    A6REFIL-ADLAGOMR                      
680700                                    A6REFIL-ADLAGOMR-NDC                  
680800                                    A6REFIL-ADGANG                        
680900                                    A6REFIL-ADGANG-NDC                    
681000                                    A6REFIL-ADPLATS                       
681100                                    A6REFIL-ADPLATS-NDC                   
681200         ELSE                                                             
681300           PERFORM DEB-GET-INFO-FR-WDK711                                 
681400         END-IF                                                           
681500*POSTEN                                                                   
681600         IF SKRIV-KOLLIFLAGGA-A6-FORMAT                                   
681700            PERFORM DED-PRT-REFILL-LBL-A6                                 
681800         ELSE                                                             
681900           IF SKRIV-PA-NOVA-SKRIVARE                                      
682000             PERFORM DEA-PRT-REFILL-LBL-NOVA                              
682100           ELSE                                                           
682200             PERFORM DEC-PRT-REFILL-LBL-7I                                
682300           END-IF                                                         
682400         END-IF                                                           
682500       ELSE                                                               
682600         MOVE FEL-6 (SPRAK-IX) TO MOD-TEMFSFEL                            
682700         MOVE MFS-RENSA-FAELT TO MOD-RAD2                                 
682800       END-IF                                                             
682900     ELSE                                                                 
683000       IF SKRIV-KOLLIFLAGGA-A6-FORMAT                                     
683100          PERFORM S03-MARKP-A6-NL-CDC-SDC                                 
683200       ELSE                                                               
683300         IF SKRIV-PA-NOVA-SKRIVARE                                        
683400           PERFORM S11-PRINT-NOVA-LABEL                                   
683500         ELSE                                                             
683600           PERFORM S01-PRINT-MARKPOINT-TERMO7INC                          
683700         END-IF                                                           
683800       END-IF                                                             
683900     END-IF                                                               
684000     .                                                                    
684100     SKIP2                                                                
684200 DEB-GET-INFO-FR-WDK711   SECTION.                                        
684300                                                                          
684400     SEARCH ALL DIST57-REFILL-DC                                          
684500        AT END                                                            
684600           MOVE 'EJ TRÄFF I REFILLTAB WWDIST57'                           
684700                            TO FELTEXT                                    
684800*          CALL FELLOG                                                    
684900        WHEN DIST57-SOK-IDDISTR(DIST57-IX) = TEST-IDDISTR                 
685000           MOVE DIST57-REFILL-TO-DC(DIST57-IX) TO W-IDDC                  
685100     END-SEARCH                                                           
685200                                                                          
685300     PERFORM IMS-GU-ARTS01                                                
685400     IF SEGMENT-FINNS                                                     
685500       PERFORM IMS-GNP-ARTS11                                             
685600       IF SEGMENT-FINNS                                                   
685700         MOVE SLAG-ADLAGOMR      TO CL7INCH-ADLAGOMR                      
685800                                    A6REFIL-ADLAGOMR                      
685900                                    A6REFIL-ADLAGOMR-POST                 
686000                                    NOVA-LBL-ADLAGOMR                     
686100                                    NOVA-LBL-ADLAGOMR-NDC                 
686200         MOVE SLAG-ADLAGOMR      TO CL7INCH-ADLAGOMR-NDC                  
686300                                    A6REFIL-ADLAGOMR-NDC                  
686400                                    A6REFIL-ADLAGOMR-POST                 
686500         MOVE SLAG-ADGANG        TO CL7INCH-ADGANG                        
686600                                    A6REFIL-ADGANG                        
686700         MOVE SLAG-ADGANG        TO CL7INCH-ADGANG-NDC                    
686800                                    A6REFIL-ADGANG-NDC                    
686900                                    A6REFIL-ADGANG-POST                   
687000                                    NOVA-LBL-ADGANG                       
687100                                    NOVA-LBL-ADGANG-NDC                   
687200         MOVE SLAG-ADPLATS       TO CL7INCH-ADPLATS                       
687300                                    A6REFIL-ADPLATS                       
687400                                    A6REFIL-ADPLATS-POST                  
687500         MOVE SLAG-ADPLATS       TO CL7INCH-ADPLATS-NDC                   
687600                                    A6REFIL-ADPLATS-NDC                   
687700                                    NOVA-LBL-ADPLATS                      
687800                                    NOVA-LBL-ADPLATS-NDC                  
687900       END-IF                                                             
688000     END-IF                                                               
688100     .                                                                    
688200     SKIP2                                                                
688300 DEA-PRT-REFILL-LBL-NOVA                  SECTION.                        
688400                                                                          
688500                                                                          
688600     MOVE LISTA-IDDISTR  TO NOVA-LBL-IDDISTR                              
688700     MOVE LISTA-IDDISTR  TO NOVA-LBL-IDDISTR-SE                           
688800                            NOVA-LBL-DISTR                                
688900                            NOVA-LBL-DIST                                 
689000                            DIST29-IDDISTR                                
689100                                                                          
689200     MOVE LISTA-IDKUNDNR TO NOVA-LBL-IDKUNDNR                             
689300                            NOVA-LBL-KUNDNR                               
689400                            NOVA-LBL-KUNDN                                
689500                                                                          
689600     MOVE LISTA-IDORDNR  TO NOVA-LBL-IDORDNR                              
689700                            NOVA-LBL-ORDNR                                
689800                            NOVA-LBL-ORDN                                 
689900                                                                          
690000     MOVE WS-IDPRODNR    TO NOVA-LBL-IDPRODNR                             
690100     MOVE LISTA-IDKOLLI  TO NOVA-LBL-IDKOLLI                              
690200                            NOVA-LBL-KOLLI                                
690300                            NOVA-LBL-KOLI                                 
690400     MOVE LISTA-KDFRAKT  TO NOVA-LBL-KDFRAKT                              
690500     MOVE LISTA-KDFRAKT  TO NOVA-LBL-KDFRAKT-REFILL                       
690600                                                                          
690700     MOVE LISTA-ADRESS-1 TO NOVA-LBL-ADRESS1                              
690800     MOVE LISTA-ADRESS-2 TO NOVA-LBL-ADRESS2                              
690900     MOVE LISTA-ADRESS-3 TO NOVA-LBL-ADRESS3                              
691000     MOVE LISTA-ADRESS-4 TO NOVA-LBL-ADRESS4                              
691100     MOVE WS-LISTA-ADRESS-5 TO NOVA-LBL-ADRESS5                           
691200                                                                          
691300     IF  WS-KILO < 10                                                     
691400     AND (WS-KILO > ZERO OR WS-HEKTO > ZERO)                              
691500       MOVE WS-KILO      TO NOVA-LBL-KILO                                 
691600                            NOVA-LBL-KILO-REFILL                          
691700       MOVE WS-HEKTO     TO NOVA-LBL-HEKTO                                
691800                            NOVA-LBL-HEKTO-REFILL                         
691900     ELSE                                                                 
692000       MOVE WS-VKORDBTO  TO NOVA-LBL-VKORDBTO                             
692100                            NOVA-LBL-VKORDBTO-REFILL                      
692200     END-IF                                                               
692300                                                                          
692400     MOVE W-IDARTNR           TO NOVA-LBL-IDARTNR                         
692500     MOVE W-IDARTNR           TO NOVA-LBL-IDARTNR-NDC                     
692600     INSPECT NOVA-LBL-IDARTNR REPLACING LEADING ZERO BY SPACE             
692700     INSPECT NOVA-LBL-IDARTNR-NDC REPLACING LEADING ZERO BY SPACE         
692800     MOVE WS-ORAD-BERADREF    TO NOVA-LBL-BERADREF                        
692900     MOVE LISTA-TIRFS         TO NOVA-LBL-TIRFS                           
693000                                                                          
693100*ST-ADRESS                                                                
693200       MOVE WS-KOLLI-ADFLGEO  TO NOVA-LBL-ADFLGEO-REFILL                  
693300       MOVE WS-KOLLI-ADFLOMR  TO NOVA-LBL-ADFLOMR-REFILL                  
693400                                                                          
693500     MOVE WS-ORAD-KVLEVART    TO NOVA-LBL-KVLEVART                        
693600     MOVE WS-ORAD-KVLEVART    TO NOVA-LBL-KVLEVART-NDC                    
693700                                                                          
693800     IF WS-IDKOLLI-PRT = WS-IDKOLLI                                       
693900       CALL W006PRS1 USING PRT-SPOOL-OVR PRT-OPEN LISTVAL                 
694000                           ALT-PCB DUMMY-AREA DUMMY-AREA                  
694100     END-IF                                                               
694200                                                                          
694300     MOVE SPACE       TO NOVA-LBL-RAD                                     
694400     MOVE NOVA-LBL-STYR-01 TO NOVA-LBL-RAD                                
694500     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                  
694600                         ALT-PCB PRT-NYSIDA-RAD1 NOVA-LBL-RAD             
694700                                                                          
694800     MOVE NOVA-LBL-STYR-01 TO NOVA-LBL-RAD                                
694900     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                  
695000                         ALT-PCB PRT-AFTER-1 NOVA-LBL-RAD                 
695100                                                                          
695200     MOVE NOVA-LBL-STYR-42 TO NOVA-LBL-RAD                                
695300     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                  
695400                         ALT-PCB PRT-AFTER-1 NOVA-LBL-RAD                 
695500                                                                          
695600*RUB DISTRICT                                                             
695700     MOVE NOVA-LBL-RUB-1-1 TO NOVA-LBL-RAD                                
695800     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                  
695900                         ALT-PCB PRT-AFTER-1 NOVA-LBL-RAD                 
696000                                                                          
696100     EVALUATE TRUE                                                        
696200       WHEN DIST35-REFILL-NA                                              
696300         MOVE NOVA-LBL-RUB-RETAILER TO NOVA-LBL-RAD                       
696400       WHEN DIST35-CDC-GB-REFILL                                          
696500         MOVE NOVA-LBL-RUB-DEALER  TO NOVA-LBL-RAD                        
696600       WHEN DIST35-CDC-GB-3A-REFILL                                       
696700         MOVE NOVA-LBL-RUB-DEALER  TO NOVA-LBL-RAD                        
696800       WHEN OTHER                                                         
696900         MOVE NOVA-LBL-RUB-CUSTOMER TO NOVA-LBL-RAD                       
697000     END-EVALUATE                                                         
697100                                                                          
697200     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                  
697300                         ALT-PCB PRT-AFTER-1 NOVA-LBL-RAD                 
697400                                                                          
697500     MOVE NOVA-LBL-RUB-1-3 TO NOVA-LBL-RAD                                
697600     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                  
697700                         ALT-PCB PRT-AFTER-1 NOVA-LBL-RAD                 
697800                                                                          
697900     MOVE NOVA-LBL-RUB-2-1 TO NOVA-LBL-RAD                                
698000     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                  
698100                         ALT-PCB PRT-AFTER-1 NOVA-LBL-RAD                 
698200                                                                          
698300     MOVE NOVA-LBL-RUB-2-5 TO NOVA-LBL-RAD                                
698400     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                  
698500                         ALT-PCB PRT-AFTER-1 NOVA-LBL-RAD                 
698600                                                                          
698700     MOVE NOVA-LBL-RUB-FREIGHT-REFILL TO NOVA-LBL-RAD                     
698800     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                  
698900                         ALT-PCB PRT-AFTER-1 NOVA-LBL-RAD                 
699000                                                                          
699100     MOVE NOVA-LBL-RUB-3-1-WEIGHT-REFILL    TO NOVA-LBL-RAD               
699200     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                  
699300                         ALT-PCB PRT-AFTER-1 NOVA-LBL-RAD                 
699400                                                                          
699500     MOVE NOVA-LBL-RUB-RFS-POST TO NOVA-LBL-RAD                           
699600*    MOVE NOVA-LBL-RUB-4-1-RFS    TO NOVA-LBL-RAD                         
699700     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                  
699800                         ALT-PCB PRT-AFTER-1 NOVA-LBL-RAD                 
699900                                                                          
700000*IDDISTR                                                                  
700100     IF CDC-SE                                                            
700200       MOVE NOVA-LBL-DATA-1-1-SE TO NOVA-LBL-RAD                          
700300     ELSE                                                                 
700400       MOVE NOVA-LBL-DATA-1-1    TO NOVA-LBL-RAD                          
700500     END-IF                                                               
700600     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                  
700700                         ALT-PCB PRT-AFTER-1 NOVA-LBL-RAD                 
700800                                                                          
700900     IF DIST35-REFILL-NA                                                  
701000     OR DIST35-REFILL-CN                                                  
701100     OR DIST35-REFILL-NA-JAP                                              
701200     OR DIST35-CDC-AU-REFILL                                              
701300       MOVE NOVA-LBL-REFILL-Q-NDC           TO NOVA-LBL-RAD               
701400       CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                
701500                         ALT-PCB PRT-AFTER-1 NOVA-LBL-RAD                 
701600     END-IF                                                               
701700                                                                          
701800     IF DIST35-REFILL-NA                                                  
701900     OR DIST35-REFILL-CN                                                  
702000     OR DIST35-REFILL-NA-JAP                                              
702100     OR DIST35-CDC-AU-REFILL                                              
702200       MOVE NOVA-LBL-RUB-PARTNUMBER-NDC TO NOVA-LBL-RAD                   
702300       CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                
702400                           ALT-PCB PRT-AFTER-1 NOVA-LBL-RAD               
702500     ELSE                                                                 
702600       MOVE NOVA-LBL-RUB-PARTNUMBER     TO NOVA-LBL-RAD                   
702700       CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                
702800                           ALT-PCB PRT-AFTER-1 NOVA-LBL-RAD               
702900     END-IF                                                               
703000                                                                          
703100     IF DIST35-REFILL-NA                                                  
703200     OR DIST35-REFILL-CN                                                  
703300     OR DIST35-REFILL-NA-JAP                                              
703400     OR DIST35-CDC-AU-REFILL                                              
703500       MOVE NOVA-LBL-RUB-DC-WH-ADRS-NDC TO NOVA-LBL-RAD                   
703600       CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                
703700                           ALT-PCB PRT-AFTER-1 NOVA-LBL-RAD               
703800     ELSE                                                                 
703900       MOVE NOVA-LBL-RUB-DC-WH-ADDRESS   TO NOVA-LBL-RAD                  
704000       CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                
704100                           ALT-PCB PRT-AFTER-1 NOVA-LBL-RAD               
704200     END-IF                                                               
704300                                                                          
704400     MOVE NOVA-LBL-RUB-4-3-ST-ADDRESS     TO NOVA-LBL-RAD                 
704500     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                  
704600                         ALT-PCB PRT-AFTER-1 NOVA-LBL-RAD                 
704700                                                                          
704800     MOVE NOVA-LBL-DATA-1-2 TO NOVA-LBL-RAD                               
704900     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                  
705000                         ALT-PCB PRT-AFTER-1 NOVA-LBL-RAD                 
705100                                                                          
705200     MOVE NOVA-LBL-DATA-1-3 TO NOVA-LBL-RAD                               
705300     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                  
705400                         ALT-PCB PRT-AFTER-1 NOVA-LBL-RAD                 
705500                                                                          
705600     MOVE NOVA-LBL-DATA-ADRESS1 TO NOVA-LBL-RAD                           
705700     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                  
705800                         ALT-PCB PRT-AFTER-1 NOVA-LBL-RAD                 
705900                                                                          
706000     MOVE NOVA-LBL-DATA-ADRESS2 TO NOVA-LBL-RAD                           
706100     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                  
706200                         ALT-PCB PRT-AFTER-1 NOVA-LBL-RAD                 
706300                                                                          
706400     IF DIST35-REFILL-NA                                                  
706500     OR DIST35-REFILL-CN                                                  
706600     OR DIST35-REFILL-NA-JAP                                              
706700     OR DIST35-CDC-AU-REFILL                                              
706800       CONTINUE                                                           
706900     ELSE                                                                 
707000       MOVE NOVA-LBL-DATA-ADRESS3 TO NOVA-LBL-RAD                         
707100       CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                
707200                           ALT-PCB PRT-AFTER-1 NOVA-LBL-RAD               
707300                                                                          
707400       MOVE NOVA-LBL-DATA-ADRESS4 TO NOVA-LBL-RAD                         
707500       CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                
707600                           ALT-PCB PRT-AFTER-1 NOVA-LBL-RAD               
707700     END-IF                                                               
707800                                                                          
707900     MOVE NOVA-LBL-DATA-2-5 TO NOVA-LBL-RAD                               
708000     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                  
708100                         ALT-PCB PRT-AFTER-1 NOVA-LBL-RAD                 
708200*KDFRAKT                                                                  
708300     MOVE NOVA-LBL-DATA-FREIGHT-REFILL TO NOVA-LBL-RAD                    
708400     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                  
708500                         ALT-PCB PRT-AFTER-1 NOVA-LBL-RAD                 
708600                                                                          
708700*KDORDKL                                                                  
708800     MOVE WS-KDORDKL             TO NOVA-LBL-KDORDKL-DEA                  
708900     MOVE WS-KDORDKL             TO NOVA-LBL-KDORDKL-DEA-NDC              
709000     IF DIST35-REFILL-NA                                                  
709100     OR DIST35-REFILL-CN                                                  
709200     OR DIST35-REFILL-NA-JAP                                              
709300     OR DIST35-CDC-AU-REFILL                                              
709400       MOVE NOVA-LBL-RUB-KDORDKL-DEA-NDC TO NOVA-LBL-RAD                  
709500       CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                
709600                             ALT-PCB PRT-AFTER-1 NOVA-LBL-RAD             
709700                                                                          
709800       MOVE NOVA-LBL-DATA-KDORDKL-DEA-NDC TO NOVA-LBL-RAD                 
709900       CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                
710000                             ALT-PCB PRT-AFTER-1 NOVA-LBL-RAD             
710100     ELSE                                                                 
710200       MOVE NOVA-LBL-RUB-KDORDKL-DEA  TO NOVA-LBL-RAD                     
710300       CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                
710400                             ALT-PCB PRT-AFTER-1 NOVA-LBL-RAD             
710500                                                                          
710600       MOVE NOVA-LBL-DATA-KDORDKL-DEA TO NOVA-LBL-RAD                     
710700       CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                
710800                             ALT-PCB PRT-AFTER-1 NOVA-LBL-RAD             
710900     END-IF                                                               
711000                                                                          
711100     IF    WS-KILO < 10                                                   
711200     AND (WS-KILO > ZERO OR WS-HEKTO > ZERO)                              
711300       MOVE NOVA-LBL-DATA-KILO-HEKTO-REF        TO NOVA-LBL-RAD           
711400     ELSE                                                                 
711500       MOVE NOVA-LBL-DATA-WEIGHT-REFILL         TO NOVA-LBL-RAD           
711600     END-IF                                                               
711700     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                  
711800                         ALT-PCB PRT-AFTER-1 NOVA-LBL-RAD                 
711900*TIRFS                                                                    
712000     MOVE NOVA-LBL-DATA-4-1       TO NOVA-LBL-RAD                         
712100     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                  
712200                         ALT-PCB PRT-AFTER-1 NOVA-LBL-RAD                 
712300                                                                          
712400     IF DIST35-REFILL-NA                                                  
712500     OR DIST35-REFILL-CN                                                  
712600     OR DIST35-REFILL-NA-JAP                                              
712700     OR DIST35-CDC-AU-REFILL                                              
712800       MOVE NOVA-LBL-REFILL-ARTNR-NDC       TO NOVA-LBL-RAD               
712900       CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                
713000                           ALT-PCB PRT-AFTER-1 NOVA-LBL-RAD               
713100     ELSE                                                                 
713200       MOVE NOVA-LBL-REFILL-ARTNR           TO NOVA-LBL-RAD               
713300       CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                
713400                           ALT-PCB PRT-AFTER-1 NOVA-LBL-RAD               
713500     END-IF                                                               
713600                                                                          
713700     MOVE NOVA-LBL-REFILL-DC-WH-ADR      TO NOVA-LBL-RAD                  
713800     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                  
713900                       ALT-PCB PRT-AFTER-1 NOVA-LBL-RAD                   
714000                                                                          
714100     MOVE NOVA-LBL-ST-ADRESS              TO NOVA-LBL-RAD                 
714200     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                  
714300                         ALT-PCB PRT-AFTER-1 NOVA-LBL-RAD                 
714400                                                                          
714500*    IF DIST35-REFILL-NA                                                  
714600*    OR DIST35-REFILL-CN                                                  
714700*    OR DIST35-REFILL-NA-JAP                                              
714800*    OR DIST35-CDC-AU-REFILL                                              
714900*      MOVE NOVA-LBL-REFILL-KVLEVART-NDC  TO NOVA-LBL-RAD                 
715000*      CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                
715100*                          ALT-PCB PRT-AFTER-1 NOVA-LBL-RAD               
715200*    ELSE                                                                 
715300       MOVE NOVA-LBL-REFILL-KVLEVART      TO NOVA-LBL-RAD                 
715400       CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                
715500                           ALT-PCB PRT-AFTER-1 NOVA-LBL-RAD               
715600*    END-IF                                                               
715700                                                                          
715800     MOVE NOVA-LBL-BARCODE                  TO NOVA-LBL-RAD               
715900     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                  
716000                         ALT-PCB PRT-AFTER-1 NOVA-LBL-RAD                 
716100                                                                          
716200     MOVE NOVA-LBL-TEXT-BELOW-BARCODE       TO NOVA-LBL-RAD               
716300     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                  
716400                         ALT-PCB PRT-AFTER-1 NOVA-LBL-RAD                 
716500                                                                          
716600     IF CDC-SE                                                            
716700        MOVE NOVA-LBL-TEXT-SHIPPER-CDC      TO NOVA-LBL-RAD               
716800        CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL               
716900                            ALT-PCB PRT-AFTER-1 NOVA-LBL-RAD              
717000                                                                          
717100     END-IF                                                               
717200                                                                          
717300     MOVE NOVA-LBL-STYR-91                  TO NOVA-LBL-RAD               
717400     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                  
717500                         ALT-PCB PRT-AFTER-1 NOVA-LBL-RAD                 
717600                                                                          
717700     IF WS-IDKOLLI-PRT = WS-IDKOLLI-TOM                                   
717800     OR PRINT-TWO-CASE-LABELS                                             
717900       CALL W006PRS1 USING PRT-SPOOL-OVR PRT-CLOSE LISTVAL                
718000                           ALT-PCB DUMMY-AREA DUMMY-AREA                  
718100     END-IF                                                               
718200     .                                                                    
718300     EJECT                                                                
718400 DEC-PRT-REFILL-LBL-7I                    SECTION.                        
718500                                                                          
718600     MOVE LISTA-IDDISTR  TO CL7INCH-IDDISTR                               
718700     MOVE LISTA-IDDISTR  TO CL7INCH-IDDISTR-SE                            
718800                            CL7INCH-DISTR                                 
718900                            CL7INCH-DIST                                  
719000                            DIST29-IDDISTR                                
719100                                                                          
719200     MOVE LISTA-IDKUNDNR TO CL7INCH-IDKUNDNR                              
719300     MOVE LISTA-IDKUNDNR TO CL7INCH-IDKUNDNR-B                            
719400                            CL7INCH-KUNDNR                                
719500                            CL7INCH-KUNDN                                 
719600                                                                          
719700     MOVE LISTA-IDORDNR  TO CL7INCH-IDORDNR                               
719800                            CL7INCH-ORDNR                                 
719900                            CL7INCH-ORDN                                  
720000                                                                          
720100     MOVE WS-IDPRODNR    TO CL7INCH-IDPRODNR                              
720200     MOVE LISTA-IDKOLLI  TO CL7INCH-IDKOLLI                               
720300                            CL7INCH-KOLLI                                 
720400                            CL7INCH-KOLI                                  
720500     MOVE LISTA-KDFRAKT  TO CL7INCH-KDFRAKT                               
720600     MOVE LISTA-KDFRAKT  TO CL7INCH-KDFRAKT-REFILL                        
720700                                                                          
720800     MOVE LISTA-ADRESS-1 TO CL7INCH-ADRESS1                               
720900     MOVE LISTA-ADRESS-2 TO CL7INCH-ADRESS2                               
721000     MOVE LISTA-ADRESS-3 TO CL7INCH-ADRESS3                               
721100     MOVE LISTA-ADRESS-4 TO CL7INCH-ADRESS4                               
721200     MOVE WS-LISTA-ADRESS-5 TO CL7INCH-ADRESS5                            
721300                                                                          
721400     MOVE W-IDARTNR           TO CL7INCH-IDARTNR                          
721500     MOVE W-IDARTNR           TO CL7INCH-IDARTNR-NDC                      
721600     INSPECT CL7INCH-IDARTNR REPLACING LEADING ZERO BY SPACE              
721700     INSPECT CL7INCH-IDARTNR-NDC REPLACING LEADING ZERO BY SPACE          
721800     MOVE WS-ORAD-BERADREF    TO CL7INCH-BERADREF                         
721900                                                                          
722000     MOVE LISTA-TIRFS         TO CL7INCH-TIRFS                            
722100                                                                          
722200*ST-ADRESS                                                                
722300     MOVE WS-KOLLI-ADFLGEO    TO CL7INCH-ADFLGEO-REFILL                   
722400     MOVE WS-KOLLI-ADFLOMR    TO CL7INCH-ADFLOMR-REFILL                   
722500                                                                          
722600     MOVE WS-ORAD-KVLEVART    TO CL7INCH-KVLEVART                         
722700     MOVE WS-ORAD-KVLEVART    TO CL7INCH-KVLEVART-NDC                     
722800                                                                          
722900     IF WS-IDKOLLI-PRT = WS-IDKOLLI                                       
723000       CALL W006PRS1 USING PRT-SPOOL-OVR PRT-OPEN LISTVAL                 
723100                           ALT-PCB DUMMY-AREA DUMMY-AREA                  
723200     END-IF                                                               
723300                                                                          
723400     MOVE SPACE       TO CL7INCH-RAD                                      
723500     MOVE CL7INCH-STYR-01 TO CL7INCH-RAD                                  
723600     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                  
723700                         ALT-PCB PRT-NYSIDA-RAD1 CL7INCH-RAD              
723800                                                                          
723900     MOVE CL7INCH-STYR-01 TO CL7INCH-RAD                                  
724000     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                  
724100                         ALT-PCB PRT-AFTER-1 CL7INCH-RAD                  
724200*RUB DISTRICT                                                             
724300     MOVE CL7INCH-RUB-1-1 TO CL7INCH-RAD                                  
724400     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                  
724500                         ALT-PCB PRT-AFTER-1 CL7INCH-RAD                  
724600                                                                          
724700     EVALUATE TRUE                                                        
724800       WHEN DIST35-REFILL-NA                                              
724900         MOVE CL7INCH-RUB-RETAILER TO CL7INCH-RAD                         
725000       WHEN DIST35-CDC-GB-REFILL                                          
725100         MOVE CL7INCH-RUB-DEALER   TO CL7INCH-RAD                         
725200       WHEN DIST35-CDC-GB-3A-REFILL                                       
725300         MOVE CL7INCH-RUB-DEALER   TO CL7INCH-RAD                         
725400       WHEN OTHER                                                         
725500         MOVE CL7INCH-RUB-CUSTOMER TO CL7INCH-RAD                         
725600     END-EVALUATE                                                         
725700                                                                          
725800     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                  
725900                         ALT-PCB PRT-AFTER-1 CL7INCH-RAD                  
726000                                                                          
726100     MOVE CL7INCH-RUB-1-3 TO CL7INCH-RAD                                  
726200     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                  
726300                         ALT-PCB PRT-AFTER-1 CL7INCH-RAD                  
726400                                                                          
726500     MOVE CL7INCH-RUB-2-1 TO CL7INCH-RAD                                  
726600     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                  
726700                         ALT-PCB PRT-AFTER-1 CL7INCH-RAD                  
726800                                                                          
726900     MOVE CL7INCH-RUB-2-5 TO CL7INCH-RAD                                  
727000     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                  
727100                         ALT-PCB PRT-AFTER-1 CL7INCH-RAD                  
727200                                                                          
727300     MOVE CL7INCH-RUB-FREIGHT-REFILL    TO CL7INCH-RAD                    
727400     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                  
727500                         ALT-PCB PRT-AFTER-1 CL7INCH-RAD                  
727600                                                                          
727700     MOVE CL7INCH-RUB-WEIGHT-REFILL     TO CL7INCH-RAD                    
727800     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                  
727900                         ALT-PCB PRT-AFTER-1 CL7INCH-RAD                  
728000                                                                          
728100     MOVE CL7INCH-RUB-REFILL-RFS TO CL7INCH-RAD                           
728200     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                  
728300                         ALT-PCB PRT-AFTER-1 CL7INCH-RAD                  
728400                                                                          
728500*IDDISTR                                                                  
728600     IF CDC-SE                                                            
728700       MOVE CL7INCH-DATA-1-1-SE  TO CL7INCH-RAD                           
728800     ELSE                                                                 
728900       MOVE CL7INCH-DATA-1-1     TO CL7INCH-RAD                           
729000     END-IF                                                               
729100     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                  
729200                         ALT-PCB PRT-AFTER-1 CL7INCH-RAD                  
729300                                                                          
729400     IF DIST35-REFILL-NA                                                  
729500       MOVE CL7INCH-RUB-4-1-PARTNUMBER-NDC TO CL7INCH-RAD                 
729600       CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                
729700                             ALT-PCB PRT-AFTER-1 CL7INCH-RAD              
729800     ELSE                                                                 
729900       MOVE CL7INCH-RUB-4-1-PARTNUMBER    TO CL7INCH-RAD                  
730000       CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                
730100                             ALT-PCB PRT-AFTER-1 CL7INCH-RAD              
730200     END-IF                                                               
730300                                                                          
730400     MOVE CL7INCH-RUB-4-2-DC-WH-ADDRESS    TO CL7INCH-RAD                 
730500     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                  
730600                         ALT-PCB PRT-AFTER-1 CL7INCH-RAD                  
730700                                                                          
730800     MOVE CL7INCH-RUB-4-3-ST-ADDRESS      TO CL7INCH-RAD                  
730900     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                  
731000                         ALT-PCB PRT-AFTER-1 CL7INCH-RAD                  
731100                                                                          
731200     MOVE CL7INCH-DATA-1-2 TO CL7INCH-RAD                                 
731300     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                  
731400                         ALT-PCB PRT-AFTER-1 CL7INCH-RAD                  
731500                                                                          
731600     MOVE CL7INCH-IDORDNR-GRP   TO CL7INCH-RAD                            
731700     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                  
731800                         ALT-PCB PRT-AFTER-1 CL7INCH-RAD                  
731900                                                                          
732000     MOVE CL7INCH-DATA-ADRESS1 TO CL7INCH-RAD                             
732100     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                  
732200                         ALT-PCB PRT-AFTER-1 CL7INCH-RAD                  
732300                                                                          
732400     MOVE CL7INCH-DATA-ADRESS2 TO CL7INCH-RAD                             
732500     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                  
732600                         ALT-PCB PRT-AFTER-1 CL7INCH-RAD                  
732700                                                                          
732800     IF DIST35-REFILL-NA                                                  
732900       CONTINUE                                                           
733000     ELSE                                                                 
733100       MOVE CL7INCH-DATA-ADRESS3 TO CL7INCH-RAD                           
733200       CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                
733300                           ALT-PCB PRT-AFTER-1 CL7INCH-RAD                
733400                                                                          
733500       MOVE CL7INCH-DATA-ADRESS4 TO CL7INCH-RAD                           
733600       CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                
733700                           ALT-PCB PRT-AFTER-1 CL7INCH-RAD                
733800                                                                          
733900       MOVE CL7INCH-DATA-ADRESS5 TO CL7INCH-RAD                           
734000       CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                
734100                           ALT-PCB PRT-AFTER-1 CL7INCH-RAD                
734200     END-IF                                                               
734300                                                                          
734400     MOVE CL7INCH-DATA-IDKOLLI TO CL7INCH-RAD                             
734500     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                  
734600                         ALT-PCB PRT-AFTER-1 CL7INCH-RAD                  
734700                                                                          
734800     MOVE CL7INCH-DATA-KDFRAKT-REFILL TO CL7INCH-RAD                      
734900     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                  
735000                         ALT-PCB PRT-AFTER-1 CL7INCH-RAD                  
735100*WEIGHT                                                                   
735200     IF   WS-KILO < 10                                                    
735300     AND (WS-KILO > ZERO OR WS-HEKTO > ZERO)                              
735400       MOVE WS-KILO      TO CL7INCH-KILO                                  
735500                            CL7INCH-KILO-REFILL                           
735600       MOVE WS-HEKTO     TO CL7INCH-HEKTO                                 
735700                            CL7INCH-HEKTO-REFILL                          
735800       MOVE CL7INCH-DATA-KILO-HEKTO-REFILL      TO CL7INCH-RAD            
735900     ELSE                                                                 
736000       MOVE WS-VKORDBTO  TO CL7INCH-VKORDBTO                              
736100                            CL7INCH-VKORDBTO-REFILL                       
736200       MOVE CL7INCH-DATA-WEIGHT-REFILL    TO CL7INCH-RAD                  
736300     END-IF                                                               
736400     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                  
736500                         ALT-PCB PRT-AFTER-1 CL7INCH-RAD                  
736600*TIRFS                                                                    
736700     MOVE CL7INCH-DATA-TIRFS      TO CL7INCH-RAD                          
736800     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                  
736900                         ALT-PCB PRT-AFTER-1 CL7INCH-RAD                  
737000*KDORDKL                                                                  
737100     IF DIST35-REFILL-NA                                                  
737200       MOVE CL7INCH-RUB-KDORDKL2-NDC TO CL7INCH-RAD                       
737300       CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                
737400                             ALT-PCB PRT-AFTER-1 CL7INCH-RAD              
737500                                                                          
737600       MOVE WS-KDORDKL            TO CL7INCH-KDORDKL2-NDC                 
737700       MOVE CL7INCH-DATA-KDORDKL2-NDC       TO CL7INCH-RAD                
737800       CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                
737900                             ALT-PCB PRT-AFTER-1 CL7INCH-RAD              
738000     ELSE                                                                 
738100       MOVE CL7INCH-RUB-KDORDKL2  TO CL7INCH-RAD                          
738200       CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                
738300                             ALT-PCB PRT-AFTER-1 CL7INCH-RAD              
738400                                                                          
738500       MOVE WS-KDORDKL            TO CL7INCH-KDORDKL2                     
738600       MOVE CL7INCH-DATA-KDORDKL2           TO CL7INCH-RAD                
738700       CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                
738800                             ALT-PCB PRT-AFTER-1 CL7INCH-RAD              
738900     END-IF                                                               
739000                                                                          
739100     MOVE CL7INCH-REFILL-DC-WH-ADR       TO CL7INCH-RAD                   
739200     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                  
739300                         ALT-PCB PRT-AFTER-1 CL7INCH-RAD                  
739400                                                                          
739500     MOVE CL7INCH-ST-ADRESS                 TO CL7INCH-RAD                
739600     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                  
739700                         ALT-PCB PRT-AFTER-1 CL7INCH-RAD                  
739800                                                                          
739900     IF DIST35-REFILL-NA                                                  
740000       MOVE CL7INCH-REFILL-Q-NDC            TO CL7INCH-RAD                
740100       CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                
740200                           ALT-PCB PRT-AFTER-1 CL7INCH-RAD                
740300                                                                          
740400       MOVE CL7INCH-REFILL-ARTNR-NDC        TO CL7INCH-RAD                
740500       CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                
740600                           ALT-PCB PRT-AFTER-1 CL7INCH-RAD                
740700                                                                          
740800     ELSE                                                                 
740900       MOVE CL7INCH-REFILL-ARTNR            TO CL7INCH-RAD                
741000       CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                
741100                           ALT-PCB PRT-AFTER-1 CL7INCH-RAD                
741200     END-IF                                                               
741300                                                                          
741400     MOVE CL7INCH-REFILL-KVLEVART         TO CL7INCH-RAD                  
741500     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                  
741600                         ALT-PCB PRT-AFTER-1 CL7INCH-RAD                  
741700                                                                          
741800     MOVE CL7INCH-BARCODE                   TO CL7INCH-RAD                
741900     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                  
742000                         ALT-PCB PRT-AFTER-1 CL7INCH-RAD                  
742100                                                                          
742200     MOVE CL7INCH-TEXT-BELOW-BARCODE        TO CL7INCH-RAD                
742300     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                  
742400                         ALT-PCB PRT-AFTER-1 CL7INCH-RAD                  
742500                                                                          
742600     IF CDC-SE                                                            
742700        MOVE CL7INCH-TEXT-SHIPPER-CDC       TO CL7INCH-RAD                
742800        CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL               
742900                            ALT-PCB PRT-AFTER-1 CL7INCH-RAD               
743000                                                                          
743100     END-IF                                                               
743200                                                                          
743300     MOVE CL7INCH-STYR-91                   TO CL7INCH-RAD                
743400     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                  
743500                         ALT-PCB PRT-AFTER-1 CL7INCH-RAD                  
743600                                                                          
743700     IF WS-IDKOLLI-PRT = WS-IDKOLLI-TOM                                   
743800       CALL W006PRS1 USING PRT-SPOOL-OVR PRT-CLOSE LISTVAL                
743900                           ALT-PCB DUMMY-AREA DUMMY-AREA                  
744000     END-IF                                                               
744100     .                                                                    
744200     EJECT                                                                
744300 DED-PRT-REFILL-LBL-A6                    SECTION.                        
744400                                                                          
744500     MOVE LISTA-IDDISTR  TO A6REFIL-IDDISTR                               
744600                            A6REFIL-DISTR                                 
744700                            A6REFIL-DIST                                  
744800     MOVE LISTA-IDKUNDNR TO A6REFIL-IDKUNDNR                              
744900                            A6REFIL-KUNDNR                                
745000                            A6REFIL-KUNDN                                 
745100     MOVE LISTA-IDORDNR  TO A6REFIL-IDORDNR                               
745200                            A6REFIL-ORDNR                                 
745300                            A6REFIL-ORDN                                  
745400     MOVE LISTA-IDKOLLI  TO A6REFIL-IDKOLLI                               
745500                            A6REFIL-KOLLI                                 
745600                            A6REFIL-KOLI                                  
745700     MOVE LISTA-KDFRAKT  TO A6REFIL-KDFRAKT                               
745800     MOVE LISTA-KDFRAKT  TO A6REFIL-KDFRAKT-NDC                           
745900     MOVE WS-KDORDKL     TO A6REFIL-KDORDKL                               
746000     MOVE WS-KDORDKL     TO A6REFIL-KDORDKL-NDC                           
746100                                                                          
746200     MOVE LISTA-ADRESS-1 TO A6REFIL-ADRESS1                               
746300     MOVE LISTA-ADRESS-2 TO A6REFIL-ADRESS2                               
746400     MOVE LISTA-ADRESS-3 TO A6REFIL-ADRESS3                               
746500     MOVE LISTA-ADRESS-4 TO A6REFIL-ADRESS4                               
746600                                                                          
746700     IF   WS-KILO < 10                                                    
746800     AND (WS-KILO > ZERO OR WS-HEKTO > ZERO)                              
746900       MOVE WS-KILO      TO A6REFIL-KILO-REFILL                           
747000       MOVE WS-HEKTO     TO A6REFIL-HEKTO-REFILL                          
747100     ELSE                                                                 
747200       MOVE WS-VKORDBTO  TO A6REFIL-VKORDBTO-REFILL                       
747300     END-IF                                                               
747400                                                                          
747500     MOVE W-IDARTNR           TO A6REFIL-IDARTNR                          
747600     MOVE W-IDARTNR           TO A6REFIL-IDARTNR-NDC                      
747700     INSPECT A6REFIL-IDARTNR REPLACING LEADING ZERO BY SPACE              
747800     INSPECT A6REFIL-IDARTNR-NDC REPLACING LEADING ZERO BY SPACE          
747900                                                                          
748000*ST-ADRESS                                                                
748100     MOVE WS-KOLLI-ADFLGEO    TO A6REFIL-ADFLGEO-REFILL                   
748200     MOVE WS-KOLLI-ADFLGEO    TO A6REFIL-ADFLGEO-REFILL-POST              
748300     MOVE WS-KOLLI-ADFLOMR    TO A6REFIL-ADFLOMR-REFILL                   
748400     MOVE WS-KOLLI-ADFLOMR    TO A6REFIL-ADFLOMR-REFILL-POST              
748500                                                                          
748600     MOVE WS-ORAD-KVLEVART    TO A6REFIL-KVLEVART                         
748700     MOVE WS-ORAD-KVLEVART    TO A6REFIL-KVLEVART-NDC                     
748800                                                                          
748900     IF WS-IDKOLLI-PRT = WS-IDKOLLI                                       
749000       CALL W006PRS1 USING PRT-SPOOL-OVR PRT-OPEN LISTVAL                 
749100                           ALT-PCB DUMMY-AREA DUMMY-AREA                  
749200     END-IF                                                               
749300                                                                          
749400     MOVE SPACE       TO A6REFIL-RAD                                      
749500     MOVE A6REFIL-STYR-01 TO A6REFIL-RAD                                  
749600     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                  
749700                         ALT-PCB PRT-NYSIDA-RAD1 A6REFIL-RAD              
749800                                                                          
749900     MOVE A6REFIL-STYR-01 TO A6REFIL-RAD                                  
750000     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                  
750100                         ALT-PCB PRT-AFTER-1 A6REFIL-RAD                  
750200*RUB DISTRIKT                                                             
750300     MOVE A6REFIL-RUB-DISTRICT TO A6REFIL-RAD                             
750400     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                  
750500                         ALT-PCB PRT-AFTER-1 A6REFIL-RAD                  
750600                                                                          
750700     EVALUATE TRUE                                                        
750800       WHEN DIST35-REFILL-NA                                              
750900         MOVE A6REFIL-RUB-RETAILER TO A6REFIL-RAD                         
751000       WHEN DIST35-CDC-GB-REFILL                                          
751100         MOVE A6REFIL-RUB-DEALER   TO A6REFIL-RAD                         
751200       WHEN DIST35-CDC-GB-3A-REFILL                                       
751300         MOVE A6REFIL-RUB-DEALER   TO A6REFIL-RAD                         
751400       WHEN OTHER                                                         
751500         MOVE A6REFIL-RUB-CUSTOMER TO A6REFIL-RAD                         
751600     END-EVALUATE                                                         
751700                                                                          
751800     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                  
751900                         ALT-PCB PRT-AFTER-1 A6REFIL-RAD                  
752000                                                                          
752100     MOVE A6REFIL-RUB-ORDER-NUMBER TO A6REFIL-RAD                         
752200     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                  
752300                         ALT-PCB PRT-AFTER-1 A6REFIL-RAD                  
752400                                                                          
752500     MOVE A6REFIL-RUB-ADDRESS TO A6REFIL-RAD                              
752600     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                  
752700                         ALT-PCB PRT-AFTER-1 A6REFIL-RAD                  
752800                                                                          
752900     MOVE A6REFIL-RUB-CASE TO A6REFIL-RAD                                 
753000     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                  
753100                         ALT-PCB PRT-AFTER-1 A6REFIL-RAD                  
753200                                                                          
753300     MOVE A6REFIL-RUB-WEIGHT-REFILL     TO A6REFIL-RAD                    
753400     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                  
753500                         ALT-PCB PRT-AFTER-1 A6REFIL-RAD                  
753600                                                                          
753700*RFS                                                                      
753800     MOVE A6REFIL-RUB-RFS     TO A6REFIL-RAD                              
753900     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                  
754000                         ALT-PCB PRT-AFTER-1 A6REFIL-RAD                  
754100*IDDISTR                                                                  
754200     MOVE A6REFIL-DATA-IDDISTR   TO A6REFIL-RAD                           
754300     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                  
754400                         ALT-PCB PRT-AFTER-1 A6REFIL-RAD                  
754500                                                                          
754600     MOVE A6REFIL-RUB-DC-WH-ADDRESS    TO A6REFIL-RAD                     
754700     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                  
754800                         ALT-PCB PRT-AFTER-1 A6REFIL-RAD                  
754900                                                                          
755000     MOVE A6REFIL-RUB-ST-ADDRESS      TO A6REFIL-RAD                      
755100     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                  
755200                         ALT-PCB PRT-AFTER-1 A6REFIL-RAD                  
755300                                                                          
755400     MOVE A6REFIL-DATA-IDKUNDNR TO A6REFIL-RAD                            
755500     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                  
755600                         ALT-PCB PRT-AFTER-1 A6REFIL-RAD                  
755700                                                                          
755800     MOVE A6REFIL-DATA-IDORDNR TO A6REFIL-RAD                             
755900     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                  
756000                         ALT-PCB PRT-AFTER-1 A6REFIL-RAD                  
756100                                                                          
756200     MOVE A6REFIL-DATA-ADRESS1 TO A6REFIL-RAD                             
756300     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                  
756400                         ALT-PCB PRT-AFTER-1 A6REFIL-RAD                  
756500                                                                          
756600     MOVE A6REFIL-DATA-ADRESS2 TO A6REFIL-RAD                             
756700     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                  
756800                         ALT-PCB PRT-AFTER-1 A6REFIL-RAD                  
756900                                                                          
757000     IF DIST35-REFILL-NA                                                  
757100     OR DIST35-REFILL-CN                                                  
757200     OR DIST35-REFILL-NA-JAP                                              
757300     OR DIST35-CDC-AU-REFILL                                              
757400       CONTINUE                                                           
757500*NOT ADDRESS LINE 3 & 4 FOR Q-MARKED LABEL.                               
757600     ELSE                                                                 
757700       MOVE A6REFIL-DATA-ADRESS3 TO A6REFIL-RAD                           
757800       CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                
757900                           ALT-PCB PRT-AFTER-1 A6REFIL-RAD                
758000                                                                          
758100       MOVE A6REFIL-DATA-ADRESS4 TO A6REFIL-RAD                           
758200       CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                
758300                           ALT-PCB PRT-AFTER-1 A6REFIL-RAD                
758400     END-IF                                                               
758500*IDKOLLI                                                                  
758600     MOVE A6REFIL-DATA-IDKOLLI TO A6REFIL-RAD                             
758700     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                  
758800                         ALT-PCB PRT-AFTER-1 A6REFIL-RAD                  
758900*KDORDKL                                                                  
759000     IF DIST35-REFILL-NA                                                  
759100     OR DIST35-REFILL-CN                                                  
759200     OR DIST35-REFILL-NA-JAP                                              
759300     OR DIST35-CDC-AU-REFILL                                              
759400                                                                          
759500       MOVE A6REFIL-RUB-KDORDKL-NDC  TO A6REFIL-RAD                       
759600       CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                
759700                           ALT-PCB PRT-AFTER-1 A6REFIL-RAD                
759800                                                                          
759900       MOVE A6REFIL-DATA-KDORDKL-NDC TO A6REFIL-RAD                       
760000       CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                
760100                           ALT-PCB PRT-AFTER-1 A6REFIL-RAD                
760200     ELSE                                                                 
760300       MOVE A6REFIL-RUB-KDORDKL      TO A6REFIL-RAD                       
760400       CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                
760500                           ALT-PCB PRT-AFTER-1 A6REFIL-RAD                
760600                                                                          
760700       MOVE A6REFIL-DATA-KDORDKL     TO A6REFIL-RAD                       
760800       CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                
760900                           ALT-PCB PRT-AFTER-1 A6REFIL-RAD                
761000     END-IF                                                               
761100*KDFRAKT                                                                  
761200     IF DIST35-REFILL-NA                                                  
761300     OR DIST35-REFILL-CN                                                  
761400     OR DIST35-REFILL-NA-JAP                                              
761500     OR DIST35-CDC-AU-REFILL                                              
761600       MOVE A6REFIL-RUB-FREIGHT-CODE-NDC TO A6REFIL-RAD                   
761700       CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                
761800                           ALT-PCB PRT-AFTER-1 A6REFIL-RAD                
761900                                                                          
762000       MOVE A6REFIL-DATA-KDFRAKT-NDC TO A6REFIL-RAD                       
762100       CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                
762200                           ALT-PCB PRT-AFTER-1 A6REFIL-RAD                
762300     ELSE                                                                 
762400       MOVE A6REFIL-RUB-FREIGHT-CODE TO A6REFIL-RAD                       
762500       CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                
762600                           ALT-PCB PRT-AFTER-1 A6REFIL-RAD                
762700                                                                          
762800       MOVE A6REFIL-DATA-KDFRAKT     TO A6REFIL-RAD                       
762900       CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                
763000                           ALT-PCB PRT-AFTER-1 A6REFIL-RAD                
763100     END-IF                                                               
763200                                                                          
763300     IF   WS-KILO < 10                                                    
763400     AND (WS-KILO > ZERO OR WS-HEKTO > ZERO)                              
763500       MOVE A6REFIL-DATA-KILO-HEKTO-REFILL     TO A6REFIL-RAD             
763600       CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                
763700                         ALT-PCB PRT-AFTER-1 A6REFIL-RAD                  
763800     ELSE                                                                 
763900       MOVE A6REFIL-DATA-WEIGHT-REFILL         TO A6REFIL-RAD             
764000       CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                
764100                         ALT-PCB PRT-AFTER-1 A6REFIL-RAD                  
764200     END-IF                                                               
764300                                                                          
764400     MOVE SPACE               TO A6REFIL-RAD                              
764500     MOVE LISTA-TIRFS         TO A6REFIL-TIRFS                            
764600                                                                          
764700     MOVE A6REFIL-DATA-RFS        TO A6REFIL-RAD                          
764800                                                                          
764900     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                  
765000                         ALT-PCB PRT-AFTER-1 A6REFIL-RAD                  
765100                                                                          
765200     MOVE WS-IDDISTR         TO DIST35-IDDISTR                            
765300     IF DIST35-REFILL-NA                                                  
765400       MOVE A6REFIL-PART-NUMBER-TXT-NDC   TO A6REFIL-RAD                  
765500       CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                
765600                           ALT-PCB PRT-AFTER-1 A6REFIL-RAD                
765700                                                                          
765800       MOVE A6REFIL-REFILL-ARTNR-NDC      TO A6REFIL-RAD                  
765900       CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                
766000                           ALT-PCB PRT-AFTER-1 A6REFIL-RAD                
766100                                                                          
766200       MOVE A6REFIL-REFILL-Q-NDC          TO A6REFIL-RAD                  
766300       CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                
766400                           ALT-PCB PRT-AFTER-1 A6REFIL-RAD                
766500                                                                          
766600       MOVE A6REFIL-REFILL-DC-WH-ADR-NDC TO A6REFIL-RAD                   
766700       CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                
766800                         ALT-PCB PRT-AFTER-1 A6REFIL-RAD                  
766900     ELSE                                                                 
767000       MOVE A6REFIL-REFILL-ARTNR          TO A6REFIL-RAD                  
767100       CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                
767200                           ALT-PCB PRT-AFTER-1 A6REFIL-RAD                
767300                                                                          
767400       MOVE A6REFIL-PART-NUMBER-TXT       TO A6REFIL-RAD                  
767500       CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                
767600                           ALT-PCB PRT-AFTER-1 A6REFIL-RAD                
767700                                                                          
767800       MOVE A6REFIL-REFILL-DC-WH-ADR     TO A6REFIL-RAD                   
767900       CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                
768000                         ALT-PCB PRT-AFTER-1 A6REFIL-RAD                  
768100     END-IF                                                               
768200                                                                          
768300     MOVE A6REFIL-ST-ADRESS               TO A6REFIL-RAD                  
768400     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                  
768500                         ALT-PCB PRT-AFTER-1 A6REFIL-RAD                  
768600                                                                          
768700     IF DIST35-REFILL-NA                                                  
768800       MOVE A6REFIL-REFILL-KVLEVART-NDC   TO A6REFIL-RAD                  
768900     ELSE                                                                 
769000       MOVE A6REFIL-REFILL-KVLEVART       TO A6REFIL-RAD                  
769100     END-IF                                                               
769200     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                  
769300                         ALT-PCB PRT-AFTER-1 A6REFIL-RAD                  
769400                                                                          
769500     MOVE A6REFIL-BARCODE                   TO A6REFIL-RAD                
769600     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                  
769700                         ALT-PCB PRT-AFTER-1 A6REFIL-RAD                  
769800                                                                          
769900     MOVE A6REFIL-TEXT-BELOW-BARCODE        TO A6REFIL-RAD                
770000     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                  
770100                         ALT-PCB PRT-AFTER-1 A6REFIL-RAD                  
770200                                                                          
770300     IF CDC-SE                                                            
770400        MOVE A6REFIL-TEXT-SHIPPER-CDC       TO A6REFIL-RAD                
770500        CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL               
770600                            ALT-PCB PRT-AFTER-1 A6REFIL-RAD               
770700     END-IF                                                               
770800                                                                          
770900     MOVE A6REFIL-STYR-91                   TO A6REFIL-RAD                
771000     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                  
771100                         ALT-PCB PRT-AFTER-1 A6REFIL-RAD                  
771200                                                                          
771300     IF WS-IDKOLLI-PRT = WS-IDKOLLI-TOM                                   
771400       CALL W006PRS1 USING PRT-SPOOL-OVR PRT-CLOSE LISTVAL                
771500                           ALT-PCB DUMMY-AREA DUMMY-AREA                  
771600     END-IF                                                               
771700     .                                                                    
771800     EJECT                                                                
771900*TACDIS                                                                   
772000 DC-TACDIS      SECTION.                                                  
772100     MOVE 'DC-TACDIS         '    TO PGM-POS                              
772200                                                                          
772300     MOVE LISTA-IDDISTR  TO TACDIS-IDDISTR                                
772400     MOVE LISTA-IDDISTR  TO TACDIS-DISTR                                  
772500     MOVE LISTA-IDDISTR  TO TACDIS-DIST                                   
772600                                DIST35-IDDISTR                            
772700                                                                          
772800     IF (OHUV-IDSYSTEM = 'LDC' OR 'TACD')                                 
772900     AND (DIST13-SVERIGE)                                                 
773000     AND GMT-FLLDCKND = JA                                                
773100     AND OHUV-IDGROSS > ZERO                                              
773200                                                                          
773300*LISTA-IDKUNDNR IS CHANGED IN F-SECTION                                   
773400       MOVE WS-IDKUNDNR    TO TACDIS-KUNDNR                               
773500       MOVE WS-IDKUNDNR    TO TACDIS-KUNDN                                
773600     ELSE                                                                 
773700       MOVE LISTA-IDKUNDNR TO TACDIS-KUNDNR                               
773800       MOVE LISTA-IDKUNDNR TO TACDIS-KUNDN                                
773900     END-IF                                                               
774000                                                                          
774100     MOVE LISTA-IDORDNR  TO TACDIS-IDORDNR                                
774200     MOVE LISTA-IDORDNR  TO TACDIS-ORDNR                                  
774300     MOVE LISTA-IDORDNR  TO TACDIS-ORDN                                   
774400                                                                          
774500     MOVE LISTA-IDKOLLI  TO TACDIS-IDKOLLI                                
774600     MOVE LISTA-IDKOLLI  TO TACDIS-KOLLI                                  
774700     MOVE LISTA-IDKOLLI  TO TACDIS-KOLI                                   
774800                                                                          
774900     MOVE LISTA-KDFRAKT  TO TACDIS-KDFRAKT                                
775000     MOVE LISTA-IDBILREG TO TACDIS-IDBILREG                               
775100                                                                          
775200     MOVE WS-IDDEPOT     TO TACDIS-IDDEPOT                                
775300     MOVE WS-IDDISTR     TO TEST-IDDISTR                                  
775400     MOVE WS-IDDISTR     TO DIST29-IDDISTR                                
775500     MOVE WS-IDKUNDNR    TO KUND12-IDKUNDNR                               
775600*                                                                         
775700*TACDIS                                                                   
775800                                                                          
775900     MOVE WS-BEGMT-RAD1        TO TACD-BEGMT-RAD1                         
776000     MOVE WS-BEGMT-RAD2        TO TACD-BEGMT-RAD2                         
776100     MOVE WS-ADGMT-GATA        TO TACD-ADGMT-GATA                         
776200     MOVE WS-ADGMT-PADR        TO TACD-ADGMT-PADR                         
776300*    MOVE WS-ADGMT-LAND        TO TACD-ADGMT-LAND                         
776400*                                                                         
776500*UPPGIFTER FÖR NEDRE DELEN AV TACDIS KOLLIFLAGGA FLYTTAS                  
776600*I SECTION F-HAEMTA-ADRESS                                                
776700*                                                                         
776800     IF DIST-KUND-LDC                                                     
776900     OR DIST-KUND-SDC-NL                                                  
777000     OR DIST-KUND-LDC-GB-3A                                               
777100     OR DIST-KUND-SDC-IT                                                  
777200     OR GMT-FLLDCKND = JA                                                 
777300                                                                          
777400       PERFORM S05-GET-INFO-FR-WDE420                                     
777500     END-IF                                                               
777600                                                                          
777700     MOVE WS-ORAD-BERADREF    TO TACDIS-BERADREF                          
777800                                                                          
777900     MOVE WS-KDORDKL TO TACDIS-KDORDKL                                    
778000                                                                          
778100     MOVE WS-OHUV-IDDEPT    TO TACDIS-IDDEPT                              
778200     MOVE WS-OHUV-TIREPDAT  TO TACDIS-TIREPDAT                            
778300                                                                          
778400     MOVE LISTA-TIRFS       TO TACDIS-TIRFS                               
778500                                                                          
778600     IF WS-IDKOLLI-PRT = WS-IDKOLLI                                       
778700       CALL W006PRS1 USING PRT-SPOOL-OVR PRT-OPEN LISTVAL                 
778800                           ALT-PCB DUMMY-AREA DUMMY-AREA                  
778900     END-IF                                                               
779000                                                                          
779100     MOVE SPACE       TO CL7INCH-RAD                                      
779200     MOVE CL7INCH-STYR-01 TO CL7INCH-RAD                                  
779300     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                  
779400                         ALT-PCB PRT-NYSIDA-RAD1 CL7INCH-RAD              
779500                                                                          
779600     MOVE CL7INCH-STYR-01 TO CL7INCH-RAD                                  
779700     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                  
779800                         ALT-PCB PRT-AFTER-1 CL7INCH-RAD                  
779900                                                                          
780000     MOVE TACDIS-RUB-DISTRICT TO CL7INCH-RAD                              
780100     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                  
780200                         ALT-PCB PRT-AFTER-1 CL7INCH-RAD                  
780300                                                                          
780400     MOVE TACDIS-RUB-CUSTOMER TO CL7INCH-RAD                              
780500     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                  
780600                       ALT-PCB PRT-AFTER-1 CL7INCH-RAD                    
780700                                                                          
780800     MOVE TACDIS-RUB-ORDER-NUMBER TO CL7INCH-RAD                          
780900     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                  
781000                         ALT-PCB PRT-AFTER-1 CL7INCH-RAD                  
781100                                                                          
781200     MOVE TACDIS-RUB-ADDRESS TO CL7INCH-RAD                               
781300     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                  
781400                         ALT-PCB PRT-AFTER-1 CL7INCH-RAD                  
781500                                                                          
781600     MOVE TACDIS-RUB-CASE-NUMBER TO CL7INCH-RAD                           
781700     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                  
781800                         ALT-PCB PRT-AFTER-1 CL7INCH-RAD                  
781900                                                                          
782000     MOVE TACDIS-RUB-FREIGHT-CODE  TO CL7INCH-RAD                         
782100     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                  
782200                         ALT-PCB PRT-AFTER-1 CL7INCH-RAD                  
782300                                                                          
782400     MOVE TACDIS-RUB-STRECK        TO CL7INCH-RAD                         
782500     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                  
782600                         ALT-PCB PRT-AFTER-1 CL7INCH-RAD                  
782700                                                                          
782800     MOVE TACDIS-RUB-STRECK2       TO CL7INCH-RAD                         
782900     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                  
783000                         ALT-PCB PRT-AFTER-1 CL7INCH-RAD                  
783100                                                                          
783200     MOVE TACDIS-RUB-STRECK3       TO CL7INCH-RAD                         
783300     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                  
783400                         ALT-PCB PRT-AFTER-1 CL7INCH-RAD                  
783500                                                                          
783600     MOVE TACDIS-RUB-STRECK4       TO CL7INCH-RAD                         
783700     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                  
783800                         ALT-PCB PRT-AFTER-1 CL7INCH-RAD                  
783900                                                                          
784000     MOVE TACDIS-DATA-IDKOLLI         TO CL7INCH-RAD                      
784100     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                  
784200                         ALT-PCB PRT-AFTER-1 CL7INCH-RAD                  
784300                                                                          
784400     MOVE TACDIS-DATA-IDBILREG        TO CL7INCH-RAD                      
784500     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                  
784600                         ALT-PCB PRT-AFTER-1 CL7INCH-RAD                  
784700                                                                          
784800     MOVE TACDIS-RUB-RFS-DATUM  TO CL7INCH-RAD                            
784900     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                  
785000                         ALT-PCB PRT-AFTER-1 CL7INCH-RAD                  
785100                                                                          
785200     MOVE TACDIS-DATA-TIRFS      TO CL7INCH-RAD                           
785300     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                  
785400                         ALT-PCB PRT-AFTER-1 CL7INCH-RAD                  
785500                                                                          
785600     MOVE TACDIS-RUB-REP-DATUM  TO CL7INCH-RAD                            
785700     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                  
785800                         ALT-PCB PRT-AFTER-1 CL7INCH-RAD                  
785900                                                                          
786000     MOVE TACDIS-DATA-TIREPDAT   TO CL7INCH-RAD                           
786100     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                  
786200                         ALT-PCB PRT-AFTER-1 CL7INCH-RAD                  
786300                                                                          
786400     MOVE TACDIS-DATA-TIHHMM     TO CL7INCH-RAD                           
786500     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                  
786600                         ALT-PCB PRT-AFTER-1 CL7INCH-RAD                  
786700                                                                          
786800     MOVE TACDIS-RUB-IDDEPT TO CL7INCH-RAD                                
786900     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                  
787000                         ALT-PCB PRT-AFTER-1 CL7INCH-RAD                  
787100                                                                          
787200     MOVE TACDIS-DATA-IDDEPT TO CL7INCH-RAD                               
787300     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                  
787400                         ALT-PCB PRT-AFTER-1 CL7INCH-RAD                  
787500                                                                          
787600     MOVE TACDIS-RUB-KDORDKL TO CL7INCH-RAD                               
787700     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                  
787800                           ALT-PCB PRT-AFTER-1 CL7INCH-RAD                
787900                                                                          
788000     MOVE TACDIS-DATA-KDORDKL TO CL7INCH-RAD                              
788100     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                  
788200                           ALT-PCB PRT-AFTER-1 CL7INCH-RAD                
788300                                                                          
788400     MOVE TACDIS-RUB-ORDER-NUMMER   TO CL7INCH-RAD                        
788500     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                  
788600                           ALT-PCB PRT-AFTER-1 CL7INCH-RAD                
788700                                                                          
788800     MOVE TACDIS-RUB-MEKANIKERPLATS TO CL7INCH-RAD                        
788900     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                  
789000                           ALT-PCB PRT-AFTER-1 CL7INCH-RAD                
789100                                                                          
789200     MOVE TACDIS-RUB-REG-NUMMER     TO CL7INCH-RAD                        
789300     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                  
789400                           ALT-PCB PRT-AFTER-1 CL7INCH-RAD                
789500                                                                          
789600     MOVE TACDIS-RUB-FORPLOCK       TO CL7INCH-RAD                        
789700     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                  
789800                           ALT-PCB PRT-AFTER-1 CL7INCH-RAD                
789900                                                                          
790000     MOVE TACDIS-RUB-KUNDINFO       TO CL7INCH-RAD                        
790100     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                  
790200                           ALT-PCB PRT-AFTER-1 CL7INCH-RAD                
790300                                                                          
790400     MOVE TACDIS-DATA-IDDISTR   TO CL7INCH-RAD                            
790500     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                  
790600                         ALT-PCB PRT-AFTER-1 CL7INCH-RAD                  
790700                                                                          
790800     MOVE TACDIS-DATA-IDKUNDNR  TO CL7INCH-RAD                            
790900     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                  
791000                         ALT-PCB PRT-AFTER-1 CL7INCH-RAD                  
791100                                                                          
791200     MOVE TACDIS-DATA-IDORDNR   TO CL7INCH-RAD                            
791300     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                  
791400                         ALT-PCB PRT-AFTER-1 CL7INCH-RAD                  
791500*ADDRESS                                                                  
791600     MOVE TACDIS-BEGMT-RAD1               TO CL7INCH-RAD                  
791700     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                  
791800                         ALT-PCB PRT-AFTER-1 CL7INCH-RAD                  
791900                                                                          
792000     MOVE TACDIS-BEGMT-RAD2               TO CL7INCH-RAD                  
792100     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                  
792200                         ALT-PCB PRT-AFTER-1 CL7INCH-RAD                  
792300                                                                          
792400     MOVE TACDIS-ADGMT-GATA               TO CL7INCH-RAD                  
792500     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                  
792600                         ALT-PCB PRT-AFTER-1 CL7INCH-RAD                  
792700                                                                          
792800     MOVE TACDIS-ADGMT-PADR               TO CL7INCH-RAD                  
792900     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                  
793000                         ALT-PCB PRT-AFTER-1 CL7INCH-RAD                  
793100                                                                          
793200*    MOVE TACDIS-ADGMT-LAND               TO CL7INCH-RAD                  
793300*    CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                  
793400*                        ALT-PCB PRT-AFTER-1 CL7INCH-RAD                  
793500*                                                                         
793600*                                                                         
793700*BETEXT                                                                   
793800     IF WS-GMT-BETEXT-INFO > SPACE                                        
793900       MOVE WS-GMT-BETEXT-INFO     TO CL7INCH-DATA-BETEXT-INFO-DC         
794000       MOVE CL7INCH-DATA-BETEXT-DC  TO CL7INCH-RAD                        
794100       CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                
794200                           ALT-PCB PRT-AFTER-1 CL7INCH-RAD                
794300     END-IF                                                               
794400*                                                                         
794500*KUNDINFO1                                                                
794600     MOVE TACDIS-DATA-KUNDINFO1           TO CL7INCH-RAD                  
794700     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                  
794800                         ALT-PCB PRT-AFTER-1 CL7INCH-RAD                  
794900                                                                          
795000*KUNDINFO2                                                                
795100     MOVE TACDIS-DATA-KUNDINFO2           TO CL7INCH-RAD                  
795200     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                  
795300                         ALT-PCB PRT-AFTER-1 CL7INCH-RAD                  
795400                                                                          
795500     MOVE TACDIS-DATA-BETELNR-TACD        TO CL7INCH-RAD                  
795600     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                  
795700                         ALT-PCB PRT-AFTER-1 CL7INCH-RAD                  
795800                                                                          
795900     MOVE TACDIS-DATA-TETACDBO            TO CL7INCH-RAD                  
796000     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                  
796100                         ALT-PCB PRT-AFTER-1 CL7INCH-RAD                  
796200*                                                                         
796300                                                                          
796400*KDFRAKT                                                                  
796500     MOVE TACDIS-DATA-KDFRAKT      TO CL7INCH-RAD                         
796600     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                  
796700                         ALT-PCB PRT-AFTER-1 CL7INCH-RAD                  
796800                                                                          
796900     MOVE TACDIS-BARCODE            TO CL7INCH-RAD                        
797000     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                  
797100                         ALT-PCB PRT-AFTER-1 CL7INCH-RAD                  
797200                                                                          
797300                                                                          
797400     MOVE TACDIS-TEXT-BELOW-BARCODE TO CL7INCH-RAD                        
797500     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                  
797600                         ALT-PCB PRT-AFTER-1 CL7INCH-RAD                  
797700                                                                          
797800*     MOVE CL7INCH-TEXT-SHIPPER-CDC TO CL7INCH-RAD                        
797900*     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                 
798000*                         ALT-PCB PRT-AFTER-1 CL7INCH-RAD                 
798100*                                                                         
798200                                                                          
798300*MEKANIKERPLATS                                                           
798400     MOVE TACDIS-DATA-MEKANIKERPLATS TO CL7INCH-RAD                       
798500     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                  
798600                         ALT-PCB PRT-AFTER-1 CL7INCH-RAD                  
798700                                                                          
798800*BERADREF                                                                 
798900     MOVE TACDIS-DATA-BERADREF      TO CL7INCH-RAD                        
799000     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                  
799100                         ALT-PCB PRT-AFTER-1 CL7INCH-RAD                  
799200                                                                          
799300*FORPLOCK - PRE PICKING SWITCH LETTER 'F' ON LABEL                        
799400     MOVE TACDIS-DATA-FORPLOCK      TO CL7INCH-RAD                        
799500     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                  
799600                         ALT-PCB PRT-AFTER-1 CL7INCH-RAD                  
799700                                                                          
799800     MOVE CL7INCH-STYR-91 TO CL7INCH-RAD                                  
799900     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                  
800000                         ALT-PCB PRT-AFTER-1 CL7INCH-RAD                  
800100                                                                          
800200     IF WS-IDKOLLI-PRT = WS-IDKOLLI-TOM                                   
800300       CALL W006PRS1 USING PRT-SPOOL-OVR PRT-CLOSE LISTVAL                
800400                           ALT-PCB DUMMY-AREA DUMMY-AREA                  
800500     END-IF                                                               
800600                                                                          
800700     .                                                                    
800800     EJECT                                                                
800900                                                                          
801000 E-RENSA-NYCKLAR                    SECTION.                              
801100     MOVE MFS-RENSA-FAELT      TO MOD-IDDISTR-UT                          
801200                                  MOD-IDKUNDNR-UT                         
801300                                  MOD-IDORDNR-UT                          
801400                                  MOD-IDKOLLI-UT                          
801500                                  MOD-IDPRODNR-UT                         
801600                                  MOD-KDPRTVAL-UT                         
801700                                  MOD-IDDC-UT                             
801800     .                                                                    
801900     SKIP2                                                                
802000 F-HAEMTA-ADRESS           SECTION.                                       
802100     MOVE 'STA F-HAEMTA-ADRESS'    TO PGM-POS                             
802200                                                                          
802300     MOVE SPACE           TO WS-BEGMT-RAD1                                
802400                             WS-BEGMT-RAD2                                
802500                             WS-ADGMT-GATA                                
802600                             WS-ADGMT-PADR                                
802700                             WS-ADGMT-LAND                                
802800     MOVE WS-IDKUNDNR     TO KUND12-IDKUNDNR                              
802900                                                                          
803000     PERFORM IMS-GET-WLORQI01                                             
803100     PERFORM S06-DIST-KUND-LDC                                            
803200     MOVE OHUV-IDDISTR    TO DIST03-IDDISTR                               
803300     MOVE OHUV-IDDISTR    TO DIST13-IDDISTR                               
803400     MOVE OHUV-IDSYSTEM TO WS-OHUV-IDSYSTEM                               
803500                                                                          
803600     MOVE OHUV-IDDEPT         TO WS-OHUV-IDDEPT                           
803700     MOVE OHUV-TIREPDAT       TO WS-OHUV-TIREPDAT                         
803800                                                                          
803900     MOVE WS-IDKUNDNR           TO W-IDKUNDNR                             
804000     MOVE WS-IDDISTR            TO W-IDDISTR                              
804100     PERFORM IMS-GU-GMTA01-WDB201                                         
804200                                                                          
804300     IF ((OHUV-IDSYSTEM = 'LDC' OR 'TACD')                                
804400     AND (DIST13-SVERIGE)                                                 
804500     AND OHUV-IDGROSS NUMERIC                                             
804600     AND OHUV-IDGROSS > ZERO )                                            
804700                                                                          
804800        MOVE WS-IDKUNDNR (4:3)  TO LISTA-IDKUNDNR (1:3)                   
804900        MOVE OHUV-IDGROSS       TO LISTA-IDKUNDNR (4:3)                   
805000        MOVE WS-IDKUNDNR (4:3)  TO TACDIS-IDKUNDNR (1:3)                  
805100        MOVE OHUV-IDGROSS       TO TACDIS-IDKUNDNR (4:3)                  
805200                                                                          
805300        MOVE  LISTA-IDKUNDNR    TO WS-IDKUNDNR-IDGROSS                    
805400                                                                          
805500       IF OHUV-BEGMT-RAD1 = SPACE                                         
805600         MOVE GMT-BEGMT-RAD1  TO WS-BEGMT-RAD1                            
805700       ELSE                                                               
805800         MOVE OHUV-BEGMT-RAD1 TO WS-BEGMT-RAD1                            
805900       END-IF                                                             
806000         MOVE WS-BEGMT-RAD1   TO LISTA-ADRESS-1                           
806100       IF OHUV-BEGMT-RAD2 = SPACE                                         
806200         MOVE GMT-BEGMT-RAD2  TO WS-BEGMT-RAD2                            
806300       ELSE                                                               
806400         MOVE OHUV-BEGMT-RAD2 TO WS-BEGMT-RAD2                            
806500       END-IF                                                             
806600         MOVE WS-BEGMT-RAD2   TO LISTA-ADRESS-2                           
806700       IF OHUV-ADGMT-GATA = SPACE                                         
806800         MOVE GMT-ADGMT-GATA  TO WS-ADGMT-GATA                            
806900       ELSE                                                               
807000         MOVE OHUV-ADGMT-GATA TO WS-ADGMT-GATA                            
807100       END-IF                                                             
807200         MOVE WS-ADGMT-GATA   TO LISTA-ADRESS-3                           
807300       IF OHUV-ADGMT-PADR = SPACE                                         
807400         MOVE GMT-ADGMT-PADR  TO WS-ADGMT-PADR                            
807500       ELSE                                                               
807600         MOVE OHUV-ADGMT-PADR TO WS-ADGMT-PADR                            
807700       END-IF                                                             
807800         MOVE WS-ADGMT-PADR   TO LISTA-ADRESS-4                           
807900       IF OHUV-ADGMT-LAND = SPACE                                         
808000         MOVE GMT-ADGMT-LAND  TO WS-ADGMT-LAND                            
808100       ELSE                                                               
808200         MOVE OHUV-ADGMT-LAND TO WS-ADGMT-LAND                            
808300       END-IF                                                             
808400         MOVE WS-ADGMT-LAND   TO WS-LISTA-ADRESS-5                        
808500     ELSE                                                                 
808600                                                                          
808700*KUNDREG UPPGIFTER TILL ÖVRE DELEN AV TACDIS CASE LABEL                   
808800       MOVE GMT-BEGMT-RAD1      TO WS-BEGMT-RAD1                          
808900                                   LISTA-ADRESS-1                         
809000       MOVE GMT-BEGMT-RAD2      TO WS-BEGMT-RAD2                          
809100                                   LISTA-ADRESS-2                         
809200       MOVE GMT-ADGMT-GATA      TO WS-ADGMT-GATA                          
809300                                   LISTA-ADRESS-3                         
809400       MOVE GMT-ADGMT-PADR      TO WS-ADGMT-PADR                          
809500                                   LISTA-ADRESS-4                         
809600       MOVE GMT-ADGMT-LAND      TO WS-ADGMT-LAND                          
809700                                   WS-LISTA-ADRESS-5                      
809800     END-IF                                                               
809900                                                                          
810000     MOVE GMT-BETEXT            TO WS-GMT-BETEXT-INFO                     
810100                                                                          
810200*TACDIS                                                                   
810300     MOVE NEJ                  TO TAKF-FINNS-SW                           
810400     IF (OHUV-IDSYSTEM = 'LDC' OR 'TACD')                                 
810500     AND (DIST13-SVERIGE OR DIST05-NORGE)                                 
810600     AND GMT-FLLDCKND = JA                                                
810700                                                                          
810800       MOVE OHUV-IDGMTREF       TO W-IDGMTREF                             
810900       PERFORM IMS-GU-WDI201                                              
811000*TAKF                                                                     
811100       IF SEGMENT-FINNS                                                   
811200         MOVE JA                TO TAKF-FINNS-SW                          
811300         MOVE TAKF-BEMEKAN      TO TACDIS-BEMEKAN                         
811400         MOVE TAKF-FLFPLOCK     TO TACDIS-FLFPLOCK                        
811500         MOVE TAKF-TETACDBO     TO TACDIS-TETACDBO                        
811600         MOVE TAKF-BETELNR-TACD TO TACDIS-BETELNR-TACD                    
811700         MOVE TAKF-TIHHMM       TO WS-TID                                 
811800         MOVE WS-TID(3:2)       TO TACDIS-TIHH                            
811900         MOVE WS-TID(5:2)       TO TACDIS-TIMM                            
812000                                                                          
812100         IF TAKF-IDGROSS > ZERO                                           
812200           MOVE WS-IDKUNDNR (4:3) TO LISTA-IDKUNDNR (1:3)                 
812300           MOVE TAKF-IDGROSS      TO LISTA-IDKUNDNR (4:3)                 
812400           MOVE WS-IDKUNDNR (4:3) TO TACDIS-IDKUNDNR (1:3)                
812500           MOVE TAKF-IDGROSS      TO TACDIS-IDKUNDNR (4:3)                
812600         ELSE                                                             
812700           MOVE WS-IDKUNDNR       TO LISTA-IDKUNDNR                       
812800           MOVE WS-IDKUNDNR       TO TACDIS-IDKUNDNR                      
812900         END-IF                                                           
813000         MOVE TAKF-BEGMT-RAD1   TO WS-TAKF-BEGMT-RAD1                     
813100         MOVE TAKF-BEGMT-RAD2   TO WS-TAKF-BEGMT-RAD2                     
813200       ELSE                                                               
813300         PERFORM FA-STD-CASE-LABEL                                        
813400                                                                          
813500         MOVE SPACE             TO TACDIS-BEMEKAN                         
813600         MOVE SPACE             TO TACDIS-FLFPLOCK                        
813700         MOVE SPACE             TO TACDIS-TETACDBO                        
813800         MOVE SPACE             TO TACDIS-BETELNR-TACD                    
813900         MOVE SPACE             TO TACDIS-TIHH                            
814000         MOVE SPACE             TO TACDIS-TIMM                            
814100       END-IF                                                             
814200                                                                          
814300*OHUV-UPPGIFTER TILL NEDRE DELEN AV TACDIS CASE LABEL                     
814400*                                                                         
814500       IF WS-TAKF-BEGMT-RAD1 = SPACE                                      
814600         IF OHUV-BEGMT-RAD1 = SPACE                                       
814700           MOVE GMT-BEGMT-RAD1  TO TACDIS-KUNDINFO1                       
814800         ELSE                                                             
814900           MOVE OHUV-BEGMT-RAD1 TO TACDIS-KUNDINFO1                       
815000         END-IF                                                           
815100       ELSE                                                               
815200         MOVE WS-TAKF-BEGMT-RAD1  TO TACDIS-KUNDINFO1                     
815300       END-IF                                                             
815400                                                                          
815500       IF WS-TAKF-BEGMT-RAD2 = SPACE                                      
815600         IF OHUV-BEGMT-RAD2 = SPACE                                       
815700           MOVE GMT-BEGMT-RAD2  TO TACDIS-KUNDINFO2                       
815800         ELSE                                                             
815900           MOVE OHUV-BEGMT-RAD2 TO TACDIS-KUNDINFO2                       
816000         END-IF                                                           
816100       ELSE                                                               
816200         MOVE WS-TAKF-BEGMT-RAD2  TO TACDIS-KUNDINFO2                     
816300       END-IF                                                             
816400                                                                          
816500     ELSE                                                                 
816600*NOT   TACDIS                                                             
816700       PERFORM FA-STD-CASE-LABEL                                          
816800                                                                          
816900     END-IF                                                               
817000     .                                                                    
817100     SKIP2                                                                
817200                                                                          
817300 FA-STD-CASE-LABEL         SECTION.                                       
817400                                                                          
817500     PERFORM IMS-GNP-WLORQI12                                             
817600                                                                          
817700     IF (GMT-FLLDCKND = JA AND KDFRAKT-FINNS)                             
817800     OR KUND12-LDC-AKUT                                                   
817900       IF OHUV-IDDEPT > ZERO                                              
818000         MOVE OHUV-IDDEPT TO WS-OHUV-IDDEPT                               
818100       END-IF                                                             
818200                                                                          
818300       MOVE OHUV-BEGMT-RAD1 TO WS-BEGMT-RAD1                              
818400                                 LISTA-ADRESS-1                           
818500       MOVE OHUV-BEGMT-RAD2 TO WS-BEGMT-RAD2                              
818600                                 LISTA-ADRESS-2                           
818700       MOVE OHUV-ADGMT-GATA TO WS-ADGMT-GATA                              
818800                                 LISTA-ADRESS-3                           
818900       MOVE OHUV-ADGMT-PADR TO WS-ADGMT-PADR                              
819000                                 LISTA-ADRESS-4                           
819100       MOVE OHUV-ADGMT-LAND TO WS-ADGMT-LAND                              
819200     ELSE                                                                 
819300       MOVE OHUV-BEGMT-RAD1 TO WS-BEGMT-RAD1                              
819400                                 LISTA-ADRESS-1                           
819500       MOVE OHUV-BEGMT-RAD2 TO WS-BEGMT-RAD2                              
819600                                 LISTA-ADRESS-2                           
819700       MOVE OHUV-ADGMT-GATA TO WS-ADGMT-GATA                              
819800                                 LISTA-ADRESS-3                           
819900       MOVE OHUV-ADGMT-PADR TO WS-ADGMT-PADR                              
820000                                 LISTA-ADRESS-4                           
820100       MOVE OHUV-ADGMT-LAND TO WS-ADGMT-LAND                              
820200     END-IF                                                               
820300                                                                          
820400     IF GMT-FLLDCKND = JA AND KDFRAKT-FINNS                               
820500       IF SEGMENT-FINNS                                                   
820600         IF DIST-KUND-LDC-GB                                              
820700           MOVE WS-IDKUNDNR   TO W-IDKUNDNR                               
820800           MOVE WS-IDDISTR    TO W-IDDISTR                                
820900           PERFORM IMS-GU-GMTA01-WDB201                                   
821000                                                                          
821100           MOVE GMT-IDZON     TO WS-IDZON                                 
821200           MOVE GMT-IDROUTE   TO WS-IDROUTE                               
821300           MOVE GMT-IDDEPOT   TO WS-IDDEPOT                               
821400           MOVE GMT-IDPARTNR  TO W-WDB1-IDPARTNR                          
821500           MOVE DCS-IDFTG     TO W-WDB1-IDFTG                             
821600         END-IF                                                           
821700       END-IF                                                             
821800     ELSE                                                                 
821900       IF SEGMENT-FINNS                                                   
822000         IF ARB-BEGMRK           = 'SPECIALMÄRKNING'                      
822100           CONTINUE                                                       
822200         ELSE                                                             
822300           IF ARB-BEGMRK          = SPACE                                 
822400           MOVE WS-IDKUNDNR   TO W-IDKUNDNR                               
822500           MOVE WS-IDDISTR    TO W-IDDISTR                                
822600           PERFORM IMS-GU-GMTA01-WDB201                                   
822700                                                                          
822800           MOVE GMT-IDZON     TO WS-IDZON                                 
822900           MOVE GMT-IDROUTE   TO WS-IDROUTE                               
823000           MOVE GMT-IDDEPOT   TO WS-IDDEPOT                               
823100           MOVE GMT-IDPARTNR  TO W-WDB1-IDPARTNR                          
823200           MOVE DCS-IDFTG     TO W-WDB1-IDFTG                             
823300                                                                          
823400             PERFORM IMS-GU-BETC01-WDB101                                 
823500             IF SEGMENT-FINNS                                             
823600               MOVE BET-BEBETRAD-1 TO WS-BEBETRAD-1                       
823700               MOVE BET-BEBETRAD-2 TO WS-BEBETRAD-2                       
823800               MOVE BET-ADBETRAD-1 TO WS-ADBETRAD-1                       
823900               MOVE BET-ADBETRAD-2 TO WS-ADBETRAD-2                       
824000             END-IF                                                       
824100                                                                          
824200             MOVE WS-IDDC       TO W-501-IDDC                             
824300                                     W-501-IDDC-DEFAULT                   
824400*            KDFRAKT FLYTTAS I HUVUDSLINGAN.                              
824500             MOVE WS-IDDISTR    TO W-501-IDDISTR                          
824600                                     W-501-IDDISTR-DEFAULT                
824700             MOVE WS-IDKUNDNR   TO W-501-IDKUNDNR                         
824800             PERFORM IMS-GU-GMTC01-WDB501                                 
824900             IF SEGMENT-FINNS                                             
825000                                                                          
825100               IF FK-BEGMRK-RAD1 = 'SPECIALMÄRKNING'                      
825200                 CONTINUE                                                 
825300               ELSE                                                       
825400                 IF FK-BEGMRK-RAD1 = SPACE                                
825500                   IF WS-ADGMT-GATA = SPACE AND                           
825600                      WS-ADGMT-PADR = SPACE AND                           
825700                      WS-BEGMT-RAD1 = SPACE AND                           
825800                      WS-BEGMT-RAD2 = SPACE                               
825900                     MOVE WS-BEBETRAD-1 TO LISTA-ADRESS-1                 
826000                     MOVE WS-BEBETRAD-2 TO LISTA-ADRESS-2                 
826100                     MOVE WS-ADBETRAD-1 TO LISTA-ADRESS-3                 
826200                     MOVE WS-ADBETRAD-2 TO LISTA-ADRESS-4                 
826300                   ELSE                                                   
826400                     MOVE WS-BEGMT-RAD1 TO LISTA-ADRESS-1                 
826500                     MOVE WS-BEGMT-RAD2 TO LISTA-ADRESS-2                 
826600                     MOVE WS-ADGMT-GATA TO LISTA-ADRESS-3                 
826700                     MOVE WS-ADGMT-PADR TO LISTA-ADRESS-4                 
826800                     MOVE WS-ADGMT-LAND TO WS-LISTA-ADRESS-5              
826900                   END-IF                                                 
827000                 ELSE                                                     
827100                   MOVE FK-BEGMRK-RAD1 TO LISTA-ADRESS-1                  
827200                   MOVE FK-BEGMRK-RAD2 TO LISTA-ADRESS-2                  
827300                 END-IF                                                   
827400               END-IF                                                     
827500             ELSE                                                         
827600               MOVE FEL-5B(SPRAK-IX) TO MOD-TEMFSFEL                      
827700               MOVE MFS-RENSA-FAELT TO MOD-RAD2                           
827800             END-IF                                                       
827900           ELSE                                                           
828000             MOVE ARB-BEGMRK     TO WS-BEGMRK                             
828100             MOVE WS-BEGMRK-RAD1 TO LISTA-ADRESS-1                        
828200             MOVE WS-BEGMRK-RAD2 TO LISTA-ADRESS-2                        
828300             MOVE WS-BEGMRK-RAD3 TO LISTA-ADRESS-3                        
828400             MOVE WS-BEGMRK-RAD4 TO LISTA-ADRESS-4                        
828500                                                                          
828600             MOVE WS-IDKUNDNR TO W-IDKUNDNR                               
828700             MOVE WS-IDDISTR TO W-IDDISTR                                 
828800             PERFORM IMS-GU-GMTA01-WDB201                                 
828900                                                                          
829000             MOVE GMT-IDZON TO WS-IDZON                                   
829100             MOVE GMT-IDROUTE TO WS-IDROUTE                               
829200             MOVE GMT-IDDEPOT TO WS-IDDEPOT                               
829300                                                                          
829400             MOVE GMT-IDPARTNR  TO W-WDB1-IDPARTNR                        
829500             MOVE DCS-IDFTG     TO W-WDB1-IDFTG                           
829600           END-IF                                                         
829700         END-IF                                                           
829800                                                                          
829900       ELSE                                                               
830000         MOVE FEL-2C(SPRAK-IX) TO MOD-TEMFSFEL                            
830100         MOVE MFS-RENSA-FAELT TO MOD-RAD2                                 
830200       END-IF                                                             
830300     END-IF                                                               
830400     .                                                                    
830500     SKIP2                                                                
830600                                                                          
830700 G-HAEMTA-TIRFS            SECTION.                                       
830800                                                                          
830900     PERFORM IMS-GU-WLORQA01                                              
831000     MOVE SPACE                TO  WS-IDPRCVAR-TXT                        
831100     IF ODEL-IDPRCVAR = 'I'                                               
831200        MOVE 'INT'                 TO  WS-IDPRCVAR-TXT                    
831300     END-IF                                                               
831400     MOVE ODEL-DARFS           TO WS-DARFS                                
831500     MOVE WS-DARFS-YYMMDD      TO LISTA-TIRFS                             
831600     .                                                                    
831700     EJECT                                                                
831800 S11-PRINT-NOVA-LABEL                       SECTION.                      
831900                                                                          
832000     MOVE LISTA-IDDISTR  TO NOVA-LBL-IDDISTR                              
832100                            NOVA-LBL-IDDISTR-SE                           
832200                            NOVA-LBL-DISTR                                
832300                            NOVA-LBL-DIST                                 
832400                            NOVA-LBL-DISTR-IT                             
832500                            NOVA-LBL-DIST-IT                              
832600                            NOVA-LBL-DISTR-LONG                           
832700     MOVE LISTA-IDKUNDNR TO NOVA-LBL-IDKUNDNR                             
832800     MOVE WS-IDKUNDNR TO    NOVA-LBL-KUNDNR                               
832900*                           NOVA-LBL-KUNDN                                
833000*                           NOVA-LBL-KUNDNR-IT                            
833100*                           NOVA-LBL-KUNDN-IT                             
833200*                           NOVA-LBL-KUNDNR-LONG                          
833300                                                                          
833400     IF (OHUV-IDSYSTEM = 'LDC' OR 'TACD')                                 
833500     AND (DIST13-SVERIGE)                                                 
833600     AND GMT-FLLDCKND = JA                                                
833700     AND OHUV-IDGROSS > ZERO                                              
833800                                                                          
833900*LISTA-IDKUNDNR IS CHANGED IN F-SECTION                                   
834000       MOVE WS-IDKUNDNR TO NOVA-LBL-KUNDNR                                
834100       MOVE WS-IDKUNDNR TO NOVA-LBL-KUNDN                                 
834200       MOVE WS-IDKUNDNR TO NOVA-LBL-KUNDNR-IT                             
834300       MOVE WS-IDKUNDNR TO NOVA-LBL-KUNDN-IT                              
834400     ELSE                                                                 
834500*BA                                                                       
834600       IF DIST05-NORGE                                                    
834700         MOVE WS-IDKUNDNR    TO NOVA-LBL-KUNDNR                           
834800         MOVE WS-IDKUNDNR    TO NOVA-LBL-KUNDN                            
834900       ELSE                                                               
835000         MOVE LISTA-IDKUNDNR TO NOVA-LBL-KUNDN                            
835100         MOVE LISTA-IDKUNDNR TO NOVA-LBL-KUNDNR-IT                        
835200         MOVE LISTA-IDKUNDNR TO NOVA-LBL-KUNDN-IT                         
835300         MOVE LISTA-IDKUNDNR TO NOVA-LBL-KUNDNR-LONG                      
835400       END-IF                                                             
835500     END-IF                                                               
835600                                                                          
835700     MOVE LISTA-IDORDNR  TO NOVA-LBL-IDORDNR                              
835800                            NOVA-LBL-ORDNR                                
835900                            NOVA-LBL-ORDN                                 
836000                            NOVA-LBL-ORDNR-IT                             
836100                            NOVA-LBL-ORDN-IT                              
836200                            NOVA-LBL-ORDNR-LONG                           
836300     MOVE WS-IDPRODNR    TO NOVA-LBL-IDPRODNR                             
836400     MOVE LISTA-IDKOLLI  TO NOVA-LBL-IDKOLLI                              
836500                            NOVA-LBL-KOLLI                                
836600                            NOVA-LBL-KOLI                                 
836700                            NOVA-LBL-KOLLI-IT                             
836800                            NOVA-LBL-KOLI-IT                              
836900                            NOVA-LBL-KOLLI-LONG                           
837000     MOVE LISTA-KDFRAKT  TO NOVA-LBL-KDFRAKT                              
837100     MOVE LISTA-KDFRAKT  TO NOVA-LBL-KDFRAKT-SE                           
837200                            WS-WRITE-ZONA                                 
837300     MOVE WS-IDZON       TO NOVA-LBL-IDZON                                
837400     MOVE WS-IDROUTE     TO NOVA-LBL-IDROUTE                              
837500     MOVE WS-IDDEPOT     TO NOVA-LBL-IDDEPOT                              
837600     MOVE LISTA-ADFLGEO  TO NOVA-LBL-8700-ADFLGEO-SORT                    
837700     MOVE WS-IDKUNDNR    TO KUND12-IDKUNDNR                               
837800     MOVE LISTA-TIRFS    TO NOVA-LBL-TIRFS-POST                           
837900     MOVE LISTA-IDBILREG TO NOVA-LBL-IDBILREG-S11                         
838000                                                                          
838100     IF DIST83-HIT-FI                                                     
838200       MOVE 4504-ADFLGEO TO NOVA-LBL-ADFLGEO-SORT-1090                    
838300     END-IF                                                               
838400                                                                          
838500     IF DIST83-HIT-NO                                                     
838600     AND VORD-KDFRAKT NOT = +31                                           
838700       MOVE 4504-ADFLGEO TO NOVA-LBL-ADFLGEO-SORT-NO                      
838800     END-IF                                                               
838900                                                                          
839000     MOVE WS-IDDISTR                TO DIST05-IDDISTR                     
839100     MOVE WS-IDDISTR                TO DIST30-IDDISTR                     
839200                                                                          
839300                                                                          
839400     IF DIST-KUND-LDC                                                     
839500     OR DIST-KUND-SDC-NL                                                  
839600     OR DIST-KUND-LDC-GB-3A                                               
839700     OR DIST-KUND-SDC-IT                                                  
839800     OR (DIST-KUND-LDC-GB-3A AND (WS-KDORDKL = 0 OR 1))                   
839900                                                                          
840000       PERFORM S05-GET-INFO-FR-WDE420                                     
840100       MOVE WS-ORAD-BERADREF      TO NOVA-LBL-WIP                         
840200     END-IF                                                               
840300                                                                          
840400     MOVE WS-KDORDKL TO NOVA-LBL-KDORDKL                                  
840500                                                                          
840600     MOVE WS-IDDISTR     TO TEST-IDDISTR                                  
840700     MOVE WS-IDDISTR     TO DIST29-IDDISTR                                
840800     IF  DIST03-SVERIGE                                                   
840900     AND CDC-SE                                                           
841000                                                                          
841100       IF LISTA-ADRESS-1 > SPACE                                          
841200         MOVE LISTA-ADRESS-1 TO NOVA-LBL-ADRESS-1-SWE                     
841300         MOVE LISTA-ADRESS-2 TO NOVA-LBL-ADRESS-2-SWE                     
841400         MOVE LISTA-ADRESS-3 TO NOVA-LBL-ADRESS-3-SWE                     
841500         MOVE LISTA-ADRESS-4 TO NOVA-LBL-ADRESS-4-SWE                     
841600         MOVE WS-OHUV-IDDEPT TO NOVA-LBL-IDDEPT                           
841700         IF LDC                                                           
841800         OR (CDC-SE AND DIST-KUND-LDC)                                    
841900           MOVE WS-ORAD-BERADREF TO NOVA-LBL-ADRESS-5-SWE                 
842000         ELSE                                                             
842100           MOVE WS-LISTA-ADRESS-5 TO NOVA-LBL-ADRESS-5-SWE                
842200         END-IF                                                           
842300       ELSE                                                               
842400         IF LISTA-ADRESS-2 > SPACE                                        
842500           MOVE LISTA-ADRESS-2 TO NOVA-LBL-ADRESS-1-SWE                   
842600           MOVE LISTA-ADRESS-3 TO NOVA-LBL-ADRESS-3-SWE                   
842700           MOVE LISTA-ADRESS-4 TO NOVA-LBL-ADRESS-4-SWE                   
842800           MOVE WS-OHUV-IDDEPT TO NOVA-LBL-IDDEPT                         
842900           IF LDC                                                         
843000           OR (CDC-SE AND DIST-KUND-LDC)                                  
843100             MOVE WS-ORAD-BERADREF TO NOVA-LBL-ADRESS-5-SWE               
843200           ELSE                                                           
843300             MOVE WS-LISTA-ADRESS-5 TO NOVA-LBL-ADRESS-5-SWE              
843400           END-IF                                                         
843500         ELSE                                                             
843600           IF LISTA-ADRESS-3 > SPACE                                      
843700             MOVE LISTA-ADRESS-3    TO NOVA-LBL-ADRESS-1-SWE              
843800             MOVE LISTA-ADRESS-4    TO NOVA-LBL-ADRESS-4-SWE              
843900             MOVE WS-LISTA-ADRESS-5 TO NOVA-LBL-ADRESS-5-SWE              
844000             MOVE WS-OHUV-IDDEPT    TO NOVA-LBL-IDDEPT                    
844100                                                                          
844200             IF (CDC-SE AND DIST-KUND-LDC)                                
844300               MOVE WS-ORAD-BERADREF TO NOVA-LBL-ADRESS-5-SWE             
844400             ELSE                                                         
844500               MOVE ZERO              TO NOVA-LBL-ADRESS-5-SWE            
844600             END-IF                                                       
844700           ELSE                                                           
844800             IF LISTA-ADRESS-4 > SPACE                                    
844900               MOVE LISTA-ADRESS-4      TO NOVA-LBL-ADRESS-1-SWE          
845000               MOVE WS-OHUV-IDDEPT      TO NOVA-LBL-IDDEPT                
845100                                                                          
845200               IF (CDC-SE AND DIST-KUND-LDC)                              
845300                 MOVE WS-ORAD-BERADREF  TO NOVA-LBL-ADRESS-5-SWE          
845400               ELSE                                                       
845500                 MOVE WS-LISTA-ADRESS-5 TO NOVA-LBL-ADRESS-5-SWE          
845600               END-IF                                                     
845700             ELSE                                                         
845800               MOVE LISTA-ADRESS-1    TO NOVA-LBL-ADRESS-1                
845900               MOVE LISTA-ADRESS-2    TO NOVA-LBL-ADRESS-2                
846000               MOVE LISTA-ADRESS-3    TO NOVA-LBL-ADRESS-3                
846100               MOVE LISTA-ADRESS-4    TO NOVA-LBL-ADRESS-4                
846200               MOVE WS-LISTA-ADRESS-5 TO NOVA-LBL-ADRESS-5                
846300               MOVE WS-OHUV-IDDEPT    TO NOVA-LBL-IDDEPT                  
846400                                                                          
846500               IF (CDC-SE AND DIST-KUND-LDC)                              
846600                 MOVE WS-ORAD-BERADREF  TO NOVA-LBL-ADRESS-5              
846700               ELSE                                                       
846800                 MOVE WS-LISTA-ADRESS-5 TO NOVA-LBL-ADRESS-5              
846900               END-IF                                                     
847000             END-IF                                                       
847100           END-IF                                                         
847200         END-IF                                                           
847300       END-IF                                                             
847400     ELSE                                                                 
847500                                                                          
847600       IF DIST-KUND-LDC                                                   
847700       OR DIST-KUND-SDC-NL                                                
847800       OR DIST-KUND-SDC-IT                                                
847900       OR (DIST-KUND-LDC-GB-3A AND (WS-KDORDKL = 0 OR 1))                 
848000       OR (DIST05-NORGE AND (WS-OHUV-IDSYSTEM = 'LDC' OR 'TACD'))         
848100         MOVE LISTA-ADRESS-1     TO NOVA-LBL-ADRESS-1                     
848200         MOVE LISTA-ADRESS-3     TO NOVA-LBL-ADRESS-2                     
848300         MOVE LISTA-ADRESS-4     TO NOVA-LBL-ADRESS-3                     
848400         MOVE WS-ORAD-BERADREF   TO NOVA-LBL-ADRESS-5                     
848500         IF LDC-SE                                                        
848600           MOVE WS-OHUV-IDDEPT   TO NOVA-LBL-IDDEPT                       
848700         END-IF                                                           
848800           MOVE WS-KDORDKL       TO NOVA-LBL-KDORDKL                      
848900       ELSE                                                               
849000         MOVE LISTA-ADRESS-1         TO NOVA-LBL-ADRESS-1                 
849100         MOVE LISTA-ADRESS-2         TO NOVA-LBL-ADRESS-2                 
849200         MOVE LISTA-ADRESS-3         TO NOVA-LBL-ADRESS-3                 
849300         MOVE LISTA-ADRESS-4         TO NOVA-LBL-ADRESS-4                 
849400         MOVE WS-LISTA-ADRESS-5      TO NOVA-LBL-ADRESS-5                 
849500*                                                                         
849600*BEKUNDRF                                                                 
849700         IF (CDC-SE AND DIST30-FRANCE)                                    
849800           PERFORM S05-GET-INFO-FR-WDE420                                 
849900           MOVE WS-ORAD-BERADREF    TO NOVA-1478-BERADREF                 
850000           MOVE WS-OHUV-BEKUNDRF    TO NOVA-1478-BEKUNDRF                 
850100         END-IF                                                           
850200                                                                          
850300         MOVE LISTA-BEGMRKTXT-RAD1   TO NOVA-LBL-BEGMRKTXT-RAD1           
850400         MOVE LISTA-BEGMRKBC-RAD1    TO NOVA-LBL-BEGMRKBC-RAD1            
850500       END-IF                                                             
850600*      MOVE WS-IDDISTR       TO DIST05-IDDISTR                            
850700*      IF DIST05-NORGE                                                    
850800*      IF (WS-OHUV-IDSYSTEM = 'LDC' OR 'TACD')                            
850900*        MOVE WS-ORAD-BERADREF   TO NOVA-LBL-ADRESS-5                     
851000*      END-IF                                                             
851100*      END-IF                                                             
851200     END-IF                                                               
851300                                                                          
851400     IF  WS-KILO < 10                                                     
851500     AND (WS-KILO > ZERO OR WS-HEKTO > ZERO)                              
851600       MOVE WS-KILO      TO NOVA-LBL-KILO                                 
851700                            NOVA-LBL-KILO-GB                              
851800       MOVE WS-KILO      TO NOVA-LBL-KILO-S11                             
851900                                                                          
852000       MOVE WS-HEKTO     TO NOVA-LBL-HEKTO-S11                            
852100                            NOVA-LBL-HEKTO-GB                             
852200       MOVE WS-HEKTO     TO NOVA-LBL-HEKTO                                
852300     ELSE                                                                 
852400       MOVE WS-VKORDBTO  TO NOVA-LBL-VKORDBTO                             
852500                            NOVA-LBL-VKORDBTO-GB                          
852600                            NOVA-LBL-VKORDBTO-S11                         
852700     END-IF                                                               
852800                                                                          
852900     MOVE LISTA-TIRFS    TO NOVA-LBL-TIRFS                                
853000     MOVE LISTA-ADFLGEO  TO NOVA-LBL-ADFLGEO-STD                          
853100     MOVE LISTA-ADFLGEO  TO NOVA-LBL-ADFLGEO-GB                           
853200     MOVE LISTA-ADFLOMR  TO NOVA-LBL-ADFLOMR-STD                          
853300     MOVE LISTA-ADFLOMR  TO NOVA-LBL-ADFLOMR-GB                           
853400     MOVE LISTA-ADRUTNIV TO NOVA-LBL-ADRUTNIV-STD                         
853500     MOVE LISTA-ADRUTNIV TO NOVA-LBL-ADRUTNIV-GB                          
853600                            NOVA-LBL-ADRUTNIV-SPA                         
853700                                                                          
853800     IF WS-IDKOLLI-PRT = WS-IDKOLLI                                       
853900       CALL W006PRS1 USING PRT-SPOOL-OVR PRT-OPEN LISTVAL                 
854000                           ALT-PCB DUMMY-AREA DUMMY-AREA                  
854100     END-IF                                                               
854200                                                                          
854300     MOVE SPACE       TO NOVA-LBL-RAD                                     
854400     MOVE NOVA-LBL-STYR-01 TO NOVA-LBL-RAD                                
854500     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                  
854600                         ALT-PCB PRT-NYSIDA-RAD1 NOVA-LBL-RAD             
854700                                                                          
854800     MOVE NOVA-LBL-STYR-01 TO NOVA-LBL-RAD                                
854900     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                  
855000                         ALT-PCB PRT-AFTER-1 NOVA-LBL-RAD                 
855100                                                                          
855200     MOVE NOVA-LBL-STYR-42 TO NOVA-LBL-RAD                                
855300     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                  
855400                         ALT-PCB PRT-AFTER-1 NOVA-LBL-RAD                 
855500                                                                          
855600     MOVE NOVA-LBL-RUB-1-1 TO NOVA-LBL-RAD                                
855700     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                  
855800                         ALT-PCB PRT-AFTER-1 NOVA-LBL-RAD                 
855900                                                                          
856000     IF DIST34-ENGLAND-SDC                                                
856100       MOVE NOVA-LBL-RUB-DEALER  TO NOVA-LBL-RAD                          
856200     ELSE                                                                 
856300       MOVE NOVA-LBL-RUB-CUSTOMER TO NOVA-LBL-RAD                         
856400     END-IF                                                               
856500     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                  
856600                         ALT-PCB PRT-AFTER-1 NOVA-LBL-RAD                 
856700                                                                          
856800     MOVE NOVA-LBL-RUB-1-3 TO NOVA-LBL-RAD                                
856900     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                  
857000                         ALT-PCB PRT-AFTER-1 NOVA-LBL-RAD                 
857100                                                                          
857200     MOVE NOVA-LBL-RUB-2-1 TO NOVA-LBL-RAD                                
857300     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                  
857400                         ALT-PCB PRT-AFTER-1 NOVA-LBL-RAD                 
857500                                                                          
857600     MOVE NOVA-LBL-RUB-2-5 TO NOVA-LBL-RAD                                
857700     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                  
857800                         ALT-PCB PRT-AFTER-1 NOVA-LBL-RAD                 
857900                                                                          
858000     MOVE NOVA-LBL-RUB-2-6 TO NOVA-LBL-RAD                                
858100     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                  
858200                         ALT-PCB PRT-AFTER-1 NOVA-LBL-RAD                 
858300                                                                          
858400     IF  CDC-SE                                                           
858500     AND DIST34-ENGLAND-SDC                                               
858600       MOVE NOVA-LBL-RUB-3-1-WEIGHT-GB   TO NOVA-LBL-RAD                  
858700     ELSE                                                                 
858800       IF CDC-SE                                                          
858900         MOVE NOVA-LBL-RUB-3-1-WEIGHT-S11  TO NOVA-LBL-RAD                
859000       ELSE                                                               
859100         MOVE NOVA-LBL-RUB-3-1-WEIGHT      TO NOVA-LBL-RAD                
859200       END-IF                                                             
859300     END-IF                                                               
859400     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                  
859500                         ALT-PCB PRT-AFTER-1 NOVA-LBL-RAD                 
859600                                                                          
859700     MOVE OHUV-IDDISTR  TO DIST03-IDDISTR                                 
859800     IF DIST03-DANMARK-900                                                
859900     OR (OHUV-IDSYSTEM = 'LDC' OR 'TACD')                                 
860000                                                                          
860100       MOVE NOVA-LBL-RUB-REPDAT-S11      TO NOVA-LBL-RAD                  
860200       CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                
860300                           ALT-PCB PRT-AFTER-1 NOVA-LBL-RAD               
860400                                                                          
860500       MOVE WS-OHUV-TIREPDAT             TO NOVA-TIREPDAT-S11             
860600       INSPECT NOVA-TIREPDAT-S11 REPLACING LEADING ZERO BY SPACE          
860700       MOVE NOVA-LBL-TIREPDAT-S11        TO NOVA-LBL-RAD                  
860800       CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                
860900                           ALT-PCB PRT-AFTER-1 NOVA-LBL-RAD               
861000                                                                          
861100       MOVE NOVA-LBL-RUB-CAR-REG-S11     TO NOVA-LBL-RAD                  
861200       CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                
861300                           ALT-PCB PRT-AFTER-1 NOVA-LBL-RAD               
861400                                                                          
861500       MOVE NOVA-LBL-IDBILREG-LDC-S11    TO NOVA-LBL-RAD                  
861600       CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                
861700                           ALT-PCB PRT-AFTER-1 NOVA-LBL-RAD               
861800     END-IF                                                               
861900                                                                          
862000     IF CDC-SE                                                            
862100     OR GMT-FLLDCKND = JA                                                 
862200*                                                                         
862300       IF DIST34-ENGLAND-SDC                                              
862400         MOVE NOVA-LBL-RUB-4-1-RFS TO NOVA-LBL-RAD                        
862500       ELSE                                                               
862600         MOVE NOVA-LBL-RUB-RFS-POST TO NOVA-LBL-RAD                       
862700       END-IF                                                             
862800       CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                
862900                           ALT-PCB PRT-AFTER-1 NOVA-LBL-RAD               
863000                                                                          
863100       IF (GMT-FLLDCKND = JA AND KDFRAKT-FINNS)                           
863200       OR KUND12-LDC-AKUT                                                 
863300         IF WS-OHUV-IDDEPT > ZERO                                         
863400           MOVE NOVA-LBL-RUB-IDDEPT TO NOVA-LBL-RAD                       
863500           CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL            
863600                               ALT-PCB PRT-AFTER-1 NOVA-LBL-RAD           
863700                                                                          
863800           MOVE NOVA-LBL-DATA-IDDEPT TO NOVA-LBL-RAD                      
863900           CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL            
864000                               ALT-PCB PRT-AFTER-1 NOVA-LBL-RAD           
864100         END-IF                                                           
864200                                                                          
864300       END-IF                                                             
864400     END-IF                                                               
864500                                                                          
864600     MOVE WS-KDORDKL           TO NOVA-LBL-KDORDKL-S11                    
864700     MOVE NOVA-LBL-RUB-KDORDKL-S11    TO NOVA-LBL-RAD                     
864800     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                  
864900                           ALT-PCB PRT-AFTER-1 NOVA-LBL-RAD               
865000                                                                          
865100     MOVE NOVA-LBL-DATA-KDORDKL-S11   TO NOVA-LBL-RAD                     
865200     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                  
865300                           ALT-PCB PRT-AFTER-1 NOVA-LBL-RAD               
865400                                                                          
865500     IF CDC-SE                                                            
865600       MOVE NOVA-LBL-DATA-1-1-SE TO NOVA-LBL-RAD                          
865700     ELSE                                                                 
865800       MOVE NOVA-LBL-DATA-1-1    TO NOVA-LBL-RAD                          
865900     END-IF                                                               
866000     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                  
866100                         ALT-PCB PRT-AFTER-1 NOVA-LBL-RAD                 
866200                                                                          
866300     MOVE NOVA-LBL-DATA-1-2 TO NOVA-LBL-RAD                               
866400     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                  
866500                         ALT-PCB PRT-AFTER-1 NOVA-LBL-RAD                 
866600                                                                          
866700     MOVE NOVA-LBL-DATA-1-3 TO NOVA-LBL-RAD                               
866800     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                  
866900                         ALT-PCB PRT-AFTER-1 NOVA-LBL-RAD                 
867000                                                                          
867100     IF NOVA-LBL-ADRESS-1-SWE > SPACE                                     
867200*    AND WS-IDKUNDNR NOT = '000715'                                       
867300       MOVE NOVA-LBL-DATA-2-1-SWE         TO NOVA-LBL-RAD                 
867400       CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                
867500                           ALT-PCB PRT-AFTER-1 NOVA-LBL-RAD               
867600                                                                          
867700       MOVE NOVA-LBL-DATA-2-2-SWE         TO NOVA-LBL-RAD                 
867800       CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                
867900                           ALT-PCB PRT-AFTER-1 NOVA-LBL-RAD               
868000                                                                          
868100       MOVE NOVA-LBL-DATA-2-3-SWE         TO NOVA-LBL-RAD                 
868200       CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                
868300                           ALT-PCB PRT-AFTER-1 NOVA-LBL-RAD               
868400                                                                          
868500       MOVE NOVA-LBL-DATA-2-4-SWE         TO NOVA-LBL-RAD                 
868600       CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                
868700                           ALT-PCB PRT-AFTER-1 NOVA-LBL-RAD               
868800                                                                          
868900       MOVE NOVA-LBL-DATA-2-5-SWE         TO NOVA-LBL-RAD                 
869000       CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                
869100                           ALT-PCB PRT-AFTER-1 NOVA-LBL-RAD               
869200     ELSE                                                                 
869300       MOVE NOVA-LBL-DATA-2-1 TO NOVA-LBL-RAD                             
869400       CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                
869500                           ALT-PCB PRT-AFTER-1 NOVA-LBL-RAD               
869600                                                                          
869700       MOVE NOVA-LBL-DATA-2-2 TO NOVA-LBL-RAD                             
869800       CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                
869900                           ALT-PCB PRT-AFTER-1 NOVA-LBL-RAD               
870000                                                                          
870100       MOVE NOVA-LBL-DATA-2-3 TO NOVA-LBL-RAD                             
870200       CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                
870300                           ALT-PCB PRT-AFTER-1 NOVA-LBL-RAD               
870400                                                                          
870500       MOVE NOVA-LBL-DATA-2-4 TO NOVA-LBL-RAD                             
870600       CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                
870700                           ALT-PCB PRT-AFTER-1 NOVA-LBL-RAD               
870800                                                                          
870900       IF DIST-KUND-LDC                                                   
871000       OR DIST-KUND-SDC-NL                                                
871100       OR DIST-KUND-SDC-IT                                                
871200       OR (DIST-KUND-LDC-GB-3A AND (WS-KDORDKL = 0 OR 1))                 
871300         MOVE NOVA-LBL-ADRESS-2-5-LDC    TO NOVA-LBL-RAD                  
871400       ELSE                                                               
871500         MOVE NOVA-LBL-ADRESS-2-5        TO NOVA-LBL-RAD                  
871600       END-IF                                                             
871700                                                                          
871800         CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL              
871900                             ALT-PCB PRT-AFTER-1 NOVA-LBL-RAD             
872000     END-IF                                                               
872100                                                                          
872200*BETEXT                                                                   
872300     IF WS-GMT-BETEXT-INFO > SPACE                                        
872400       MOVE WS-GMT-BETEXT-INFO     TO NOVA-DATA-BETEXT-NOVA-S11           
872500       MOVE NOVA-DATA-BETEXT-S11    TO NOVA-LBL-RAD                       
872600       CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                
872700                           ALT-PCB PRT-AFTER-1 NOVA-LBL-RAD               
872800     END-IF                                                               
872900                                                                          
873000     IF (CDC-SE AND DIST30-FRANCE)                                        
873100                                                                          
873200       MOVE NOVA-LBL-RUB-LINEREF         TO NOVA-LBL-RAD                  
873300       CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                
873400                           ALT-PCB PRT-AFTER-1 NOVA-LBL-RAD               
873500                                                                          
873600       MOVE NOVA-LBL-RUB-CUSTREF         TO NOVA-LBL-RAD                  
873700       CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                
873800                           ALT-PCB PRT-AFTER-1 NOVA-LBL-RAD               
873900                                                                          
874000       MOVE NOVA-LBL-1478-BERADREF       TO NOVA-LBL-RAD                  
874100       CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                
874200                           ALT-PCB PRT-AFTER-1 NOVA-LBL-RAD               
874300                                                                          
874400       MOVE NOVA-LBL-1478-BEKUNDRF       TO NOVA-LBL-RAD                  
874500       CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                
874600                           ALT-PCB PRT-AFTER-1 NOVA-LBL-RAD               
874700     END-IF                                                               
874800                                                                          
874900     MOVE NOVA-LBL-DATA-2-5 TO NOVA-LBL-RAD                               
875000     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                  
875100                         ALT-PCB PRT-AFTER-1 NOVA-LBL-RAD                 
875200                                                                          
875300     MOVE NOVA-LBL-DATA-2-6-SE      TO NOVA-LBL-RAD                       
875400     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                  
875500                         ALT-PCB PRT-AFTER-1 NOVA-LBL-RAD                 
875600                                                                          
875700     IF  CDC-SE                                                           
875800     AND DIST34-ENGLAND-SDC                                               
875900       IF  WS-KILO < 10                                                   
876000       AND (WS-KILO > ZERO OR WS-HEKTO > ZERO)                            
876100         MOVE NOVA-LBL-DATA-KILO-HEKTO-GB TO NOVA-LBL-RAD                 
876200         CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL              
876300                           ALT-PCB PRT-AFTER-1 NOVA-LBL-RAD               
876400       ELSE                                                               
876500         MOVE NOVA-LBL-DATA-WEIGHT-GB TO NOVA-LBL-RAD                     
876600         CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL              
876700                           ALT-PCB PRT-AFTER-1 NOVA-LBL-RAD               
876800       END-IF                                                             
876900     ELSE                                                                 
877000       IF  WS-KILO < 10                                                   
877100       AND (WS-KILO > ZERO OR WS-HEKTO > ZERO)                            
877200         MOVE NOVA-LBL-DATA-KILO-HEKTO-S11 TO NOVA-LBL-RAD                
877300         CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL              
877400                           ALT-PCB PRT-AFTER-1 NOVA-LBL-RAD               
877500       ELSE                                                               
877600         MOVE NOVA-LBL-DATA-WEIGHT-S11 TO NOVA-LBL-RAD                    
877700         CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL              
877800                           ALT-PCB PRT-AFTER-1 NOVA-LBL-RAD               
877900       END-IF                                                             
878000     END-IF                                                               
878100                                                                          
878200     IF CDC-SE                                                            
878300*TIRFS                                                                    
878400       IF NOT DIST34-ENGLAND-SDC                                          
878500         MOVE NOVA-LBL-DATA-4-1 TO NOVA-LBL-RAD                           
878600         CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL              
878700                             ALT-PCB PRT-AFTER-1 NOVA-LBL-RAD             
878800       END-IF                                                             
878900                                                                          
879000       MOVE WS-IDDISTR     TO DIST34-IDDISTR                              
879100*DENNA TEST GER FEL/BLANK ADFLGEO PÅ KF VID RAD MED RESTORDER.            
879200*KOMMENTARMÄRKT FÖR ATT EV ANVÄNDAS OM BEHOV UPPSTÅR.                     
879300*      IF GMT-FLLDCKND = JA AND RO-JA                                     
879400*        CONTINUE                                                         
879500*      ELSE                                                               
879600         IF DIST34-ENGLAND-SDC                                            
879700           MOVE NOVA-LBL-RUB-GB-ST-ADDRESS TO NOVA-LBL-RAD                
879800           CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL            
879900                         ALT-PCB PRT-AFTER-1 NOVA-LBL-RAD                 
880000                                                                          
880100           MOVE NOVA-LBL-ADFLGEO-GB-GRP TO NOVA-LBL-RAD                   
880200         ELSE                                                             
880300           MOVE NOVA-LBL-ADFLGEO-STD-GRP TO NOVA-LBL-RAD                  
880400         END-IF                                                           
880500         CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL              
880600                         ALT-PCB PRT-AFTER-1 NOVA-LBL-RAD                 
880700*      END-IF                                                             
880800                                                                          
880900       IF LISTA-IDDISTR = ' 878'                                          
881000       AND LISTA-IDKUNDNR = '  1117'                                      
881100       AND NOVA-LBL-ADFLGEO-STD = 'FLY'                                   
881200         CONTINUE                                                         
881300       ELSE                                                               
881400*DENNA TEST GER FEL/BLANK ADFLGEO PÅ KF VID RAD MED RESTORDER.            
881500*KOMMENTARMÄRKT FÖR ATT EV ANVÄNDAS OM BEHOV UPPSTÅR.                     
881600*        IF GMT-FLLDCKND = JA AND RO-JA                                   
881700*          MOVE NOVA-LBL-DATA-RESTORDER TO NOVA-LBL-RAD                   
881800*        ELSE                                                             
881900           MOVE WS-IDDISTR TO DIST34-IDDISTR                              
882000           IF DIST34-ENGLAND-SDC                                          
882100             MOVE NOVA-LBL-ADFLOMR-GB-GRP TO NOVA-LBL-RAD                 
882200           ELSE                                                           
882300             MOVE NOVA-LBL-ADFLOMR-STD-GRP TO NOVA-LBL-RAD                
882400           END-IF                                                         
882500*        END-IF                                                           
882600         CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL              
882700                           ALT-PCB PRT-AFTER-1 NOVA-LBL-RAD               
882800       END-IF                                                             
882900                                                                          
883000*      IF LISTA-IDDISTR = ' 878'                                          
883100*      AND LISTA-IDKUNDNR = '  1117'                                      
883200*      AND (NOVA-LBL-ADFLGEO-STD = 'RUT'                                  
883300*      OR NOVA-LBL-ADFLGEO-STD = 'FLY')                                   
883400*        CONTINUE                                                         
883500*      ELSE                                                               
883600*DENNA TEST GER FEL/BLANK ADFLGEO PÅ KF VID RAD MED RESTORDER.            
883700*KOMMENTARMÄRKT FÖR ATT EV ANVÄNDAS OM BEHOV UPPSTÅR.                     
883800*        IF GMT-FLLDCKND = JA AND RO-JA                                   
883900*          CONTINUE                                                       
884000*        ELSE                                                             
884100*          IF DIST34-ENGLAND-SDC                                          
884200*            CONTINUE                                                     
884300*            MOVE NOVA-LBL-ADRUTNIV-GB-GRP TO NOVA-LBL-RAD                
884400*REMOVE    ELSE                                                           
884500*            MOVE NOVA-LBL-ADRUTNIV-STD-GRP TO NOVA-LBL-RAD               
884600*          CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL            
884700*                              ALT-PCB PRT-AFTER-1 NOVA-LBL-RAD           
884800*          END-IF                                                         
884900*        END-IF                                                           
885000*      END-IF                                                             
885100                                                                          
885200     END-IF                                                               
885300                                                                          
885400*HIT-FI SCR 5619825 + HIT-NO SCR                                          
885500*POSTEN-DIST-1090 START                                                   
885600                                                                          
885700     MOVE WS-IDDISTR       TO TEST-IDDISTR                                
885800     IF CDC-SE AND DIST83-HIT-FI                                          
885900       MOVE NOVA-LBL-TEXT-PRODUKT-1090 TO NOVA-LBL-RAD                    
886000       CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                
886100                           ALT-PCB PRT-AFTER-1 NOVA-LBL-RAD               
886200*48   HIT  FÖR 1090                                                       
886300       MOVE NOVA-LBL-TEXT-48       TO NOVA-LBL-RAD                        
886400       CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                
886500                           ALT-PCB PRT-AFTER-1 NOVA-LBL-RAD               
886600       MOVE NOVA-LBL-TEXT-HIT-1090 TO NOVA-LBL-RAD                        
886700       CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                
886800                             ALT-PCB PRT-AFTER-1 NOVA-LBL-RAD             
886900                                                                          
887000       MOVE NOVA-LBL-SORTERINGSKOD-1090 TO NOVA-LBL-RAD                   
887100       CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                
887200                           ALT-PCB PRT-AFTER-1 NOVA-LBL-RAD               
887300                                                                          
887400       MOVE WS-IDKLIID   TO NOVA-LBL-POSTEN1-1090                         
887500                            NOVA-LBL-POSTEN1-TEXT-1090                    
887600                                                                          
887700       MOVE NOVA-LBL-BARCODE-POSTEN-1090 TO NOVA-LBL-RAD                  
887800       CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                
887900                           ALT-PCB PRT-AFTER-1 NOVA-LBL-RAD               
888000                                                                          
888100       MOVE NOVA-LBL-TEXT-POSTEN-1090 TO NOVA-LBL-RAD                     
888200       CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                
888300                           ALT-PCB PRT-AFTER-1 NOVA-LBL-RAD               
888400     END-IF                                                               
888500*                                                                         
888600     IF CDC-SE AND DIST83-HIT-NO                                          
888700       MOVE NOVA-LBL-TEXT-PRODUKT-NO TO NOVA-LBL-RAD                      
888800       CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                
888900                           ALT-PCB PRT-AFTER-1 NOVA-LBL-RAD               
889000*48   HIT  FÖR NO                                                         
889100       MOVE NOVA-LBL-TEXT-48       TO NOVA-LBL-RAD                        
889200       CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                
889300                           ALT-PCB PRT-AFTER-1 NOVA-LBL-RAD               
889400       MOVE NOVA-LBL-TEXT-HIT-1090 TO NOVA-LBL-RAD                        
889500       CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                
889600                             ALT-PCB PRT-AFTER-1 NOVA-LBL-RAD             
889700                                                                          
889800       MOVE NOVA-LBL-SORTERINGSKOD-NO   TO NOVA-LBL-RAD                   
889900       CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                
890000                           ALT-PCB PRT-AFTER-1 NOVA-LBL-RAD               
890100                                                                          
890200       MOVE WS-IDKLIID   TO NOVA-LBL-POSTEN1-1090                         
890300                            NOVA-LBL-POSTEN1-TEXT-1090                    
890400                                                                          
890500       MOVE NOVA-LBL-BARCODE-POSTEN-1090 TO NOVA-LBL-RAD                  
890600       CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                
890700                           ALT-PCB PRT-AFTER-1 NOVA-LBL-RAD               
890800                                                                          
890900       MOVE NOVA-LBL-TEXT-POSTEN-1090 TO NOVA-LBL-RAD                     
891000       CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                
891100                           ALT-PCB PRT-AFTER-1 NOVA-LBL-RAD               
891200     END-IF                                                               
891300*POSTEN-A5-DIST-1090 END                                                  
891400                                                                          
891500     MOVE WS-IDDISTR       TO DIST34-IDDISTR                              
891600     IF  CDC-SE                                                           
891700     AND DIST34-ENGLAND-SDC                                               
891800                                                                          
891900       MOVE NOVA-LBL-RUB-4-3-TRANSPORT TO NOVA-LBL-RAD                    
892000       CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                
892100                           ALT-PCB PRT-AFTER-1 NOVA-LBL-RAD               
892200                                                                          
892300       MOVE NOVA-LBL-TRPINFO-SDC23-GB      TO NOVA-LBL-RAD                
892400       CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                
892500                           ALT-PCB PRT-AFTER-1 NOVA-LBL-RAD               
892600                                                                          
892700       MOVE NOVA-LBL-DATA-POST TO NOVA-LBL-RAD                            
892800       CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                
892900                           ALT-PCB PRT-AFTER-1 NOVA-LBL-RAD               
893000                                                                          
893100       MOVE NOVA-LBL-RUB-WIP  TO NOVA-LBL-RAD                             
893200       CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                
893300                           ALT-PCB PRT-AFTER-1 NOVA-LBL-RAD               
893400                                                                          
893500       MOVE NOVA-LBL-DATA-WIP TO NOVA-LBL-RAD                             
893600       CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                
893700                           ALT-PCB PRT-AFTER-1 NOVA-LBL-RAD               
893800     END-IF                                                               
893900*BARCODE                                                                  
894000     IF  CDC-SE                                                           
894100     AND DIST34-ENGLAND-SDC                                               
894200*                                                                         
894300       MOVE NOVA-LBL-BARCODE TO NOVA-LBL-RAD                              
894400       CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                
894500                           ALT-PCB PRT-AFTER-1 NOVA-LBL-RAD               
894600                                                                          
894700       MOVE NOVA-LBL-TEXT-BELOW-BARCODE TO NOVA-LBL-RAD                   
894800       CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                
894900                           ALT-PCB PRT-AFTER-1 NOVA-LBL-RAD               
895000     ELSE                                                                 
895100       IF SKRIV-LANG-BARCODE                                              
895200         MOVE NOVA-LBL-BARCODE-LONG TO NOVA-LBL-RAD                       
895300         CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL              
895400                             ALT-PCB PRT-AFTER-1 NOVA-LBL-RAD             
895500       ELSE                                                               
895600         MOVE NOVA-LBL-BARCODE TO NOVA-LBL-RAD                            
895700         CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL              
895800                             ALT-PCB PRT-AFTER-1 NOVA-LBL-RAD             
895900       END-IF                                                             
896000                                                                          
896100       MOVE NOVA-LBL-TEXT-BELOW-BARCODE TO NOVA-LBL-RAD                   
896200       CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                
896300                           ALT-PCB PRT-AFTER-1 NOVA-LBL-RAD               
896400     END-IF                                                               
896500*SHIPPER                                                                  
896600     IF CDC-SE                                                            
896700       EVALUATE TRUE                                                      
896800       WHEN DIST34-ENGLAND-SDC                                            
896900        MOVE NOVA-LBL-TEXT-SDC23-SHIP-CDC TO NOVA-LBL-RAD                 
897000        CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL               
897100                            ALT-PCB PRT-AFTER-1 NOVA-LBL-RAD              
897200       WHEN OTHER                                                         
897300        MOVE NOVA-LBL-TEXT-SHIPPER-CDC TO NOVA-LBL-RAD                    
897400        CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL               
897500                            ALT-PCB PRT-AFTER-1 NOVA-LBL-RAD              
897600       END-EVALUATE                                                       
897700     END-IF                                                               
897800                                                                          
897900     MOVE NOVA-LBL-STYR-91 TO NOVA-LBL-RAD                                
898000     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                  
898100                         ALT-PCB PRT-AFTER-1 NOVA-LBL-RAD                 
898200                                                                          
898300*VID VISSA PRINTERVAL                                                     
898400*VILL CDC11 RA-LAGRET FÅ TVÅ FLAGGOR UTSKRIVNA.                           
898500     MOVE WS-IDDISTR     TO TEST-IDDISTR                                  
898600     MOVE WS-IDDISTR     TO DIST29-IDDISTR                                
898700                                                                          
898800     IF WS-IDKOLLI-PRT = WS-IDKOLLI-TOM                                   
898900     OR PRINT-TWO-CASE-LABELS                                             
899000       CALL W006PRS1 USING PRT-SPOOL-OVR PRT-CLOSE LISTVAL                
899100                           ALT-PCB DUMMY-AREA DUMMY-AREA                  
899200     END-IF                                                               
899300     .                                                                    
899400     EJECT                                                                
899500 S01-PRINT-MARKPOINT-TERMO7INC              SECTION.                      
899600                                                                          
899700     MOVE LISTA-IDDISTR  TO CL7INCH-IDDISTR                               
899800                            CL7INCH-IDDISTR-SE                            
899900                            CL7INCH-DISTR                                 
900000                            CL7INCH-DIST                                  
900100                            CL7INCH-DISTR-IT                              
900200                            CL7INCH-DIST-IT                               
900300                            CL7INCH-DISTR-LONG                            
900400     MOVE LISTA-IDKUNDNR TO CL7INCH-IDKUNDNR                              
900500     MOVE LISTA-IDKUNDNR TO CL7INCH-IDKUNDNR-B                            
900600*                           CL7INCH-KUNDNR                                
900700*                           CL7INCH-KUNDN                                 
900800*                           CL7INCH-KUNDNR-IT                             
900900*                           CL7INCH-KUNDN-IT                              
901000*                           CL7INCH-KUNDNR-LONG                           
901100                                                                          
901200     IF (OHUV-IDSYSTEM = 'LDC' OR 'TACD')                                 
901300     AND (DIST13-SVERIGE OR DIST05-NORGE)                                 
901400     AND GMT-FLLDCKND = JA                                                
901500     AND OHUV-IDGROSS > ZERO                                              
901600                                                                          
901700*LISTA-IDKUNDNR IS CHANGED IN F-SECTION                                   
901800       MOVE WS-IDKUNDNR TO CL7INCH-KUNDNR                                 
901900       MOVE WS-IDKUNDNR TO CL7INCH-KUNDN                                  
902000       MOVE WS-IDKUNDNR TO CL7INCH-KUNDNR-IT                              
902100       MOVE WS-IDKUNDNR TO CL7INCH-KUNDN-IT                               
902200       MOVE WS-IDKUNDNR TO CL7INCH-KUNDNR-LONG                            
902300     ELSE                                                                 
902400       MOVE LISTA-IDKUNDNR TO CL7INCH-KUNDNR                              
902500       MOVE LISTA-IDKUNDNR TO CL7INCH-KUNDN                               
902600       MOVE LISTA-IDKUNDNR TO CL7INCH-KUNDNR-IT                           
902700       MOVE LISTA-IDKUNDNR TO CL7INCH-KUNDN-IT                            
902800       MOVE LISTA-IDKUNDNR TO CL7INCH-KUNDNR-LONG                         
902900     END-IF                                                               
903000                                                                          
903100     MOVE LISTA-IDORDNR  TO CL7INCH-IDORDNR                               
903200                            CL7INCH-ORDNR                                 
903300                            CL7INCH-ORDN                                  
903400                            CL7INCH-ORDNR-IT                              
903500                            CL7INCH-ORDN-IT                               
903600                            CL7INCH-ORDNR-LONG                            
903700     MOVE WS-IDPRODNR    TO CL7INCH-IDPRODNR                              
903800     MOVE LISTA-IDKOLLI  TO CL7INCH-IDKOLLI                               
903900                            CL7INCH-KOLLI                                 
904000                            CL7INCH-KOLI                                  
904100                            CL7INCH-KOLLI-IT                              
904200                            CL7INCH-KOLI-IT                               
904300                            CL7INCH-KOLLI-LONG                            
904400     MOVE LISTA-KDFRAKT  TO CL7INCH-KDFRAKT                               
904500     MOVE LISTA-KDFRAKT  TO CL7INCH-KDFRAKT-SE                            
904600                            WS-WRITE-ZONA                                 
904700     MOVE WS-IDZON       TO CL7INCH-IDZON                                 
904800     MOVE WS-IDROUTE     TO CL7INCH-IDROUTE                               
904900     MOVE WS-IDDEPOT     TO CL7INCH-IDDEPOT                               
905000     MOVE LISTA-ADFLGEO  TO CL7INCH-8700-ADFLGEO-SORT                     
905100     MOVE WS-IDKUNDNR    TO KUND12-IDKUNDNR                               
905200     MOVE LISTA-IDBILREG TO CL7INCH-IDBILREG-S01                          
905300                                                                          
905400     IF DIST83-HIT-FI                                                     
905500       MOVE 4504-ADFLGEO TO CL7INCH-ADFLGEO-SORT-1090                     
905600     END-IF                                                               
905700                                                                          
905800     IF DIST83-HIT-NO                                                     
905900     AND VORD-KDFRAKT NOT = +31                                           
906000       MOVE 4504-ADFLGEO TO CL7INCH-ADFLGEO-SORT-NO                       
906100     END-IF                                                               
906200                                                                          
906300     MOVE WS-IDDISTR                TO DIST05-IDDISTR                     
906400*    IF DIST05-NORGE                                                      
906500*    IF (WS-OHUV-IDSYSTEM = 'LDC' OR 'TACD')                              
906600*      PERFORM S05-GET-INFO-FR-WDE420                                     
906700*    END-IF                                                               
906800*    END-IF                                                               
906900                                                                          
907000     IF DIST-KUND-LDC                                                     
907100     OR DIST-KUND-SDC-NL                                                  
907200     OR DIST-KUND-LDC-GB-3A                                               
907300     OR DIST-KUND-SDC-IT                                                  
907400     OR (DIST-KUND-LDC-GB-3A AND (WS-KDORDKL = 0 OR 1))                   
907500     OR (DIST05-NORGE AND (WS-OHUV-IDSYSTEM = 'LDC' OR 'TACD'))           
907600                                                                          
907700       PERFORM S05-GET-INFO-FR-WDE420                                     
907800     END-IF                                                               
907900                                                                          
908000     MOVE WS-KDORDKL TO CL7INCH-KDORDKL-S01                               
908100                                                                          
908200     MOVE WS-IDDISTR     TO TEST-IDDISTR                                  
908300     MOVE WS-IDDISTR     TO DIST29-IDDISTR                                
908400     IF  DIST03-SVERIGE                                                   
908500     AND CDC-SE                                                           
908600                                                                          
908700       IF LISTA-ADRESS-1 > SPACE                                          
908800         MOVE LISTA-ADRESS-1 TO CL7INCH-ADRESS-1-SWE                      
908900         MOVE LISTA-ADRESS-2 TO CL7INCH-ADRESS-2-SWE                      
909000         MOVE LISTA-ADRESS-3 TO CL7INCH-ADRESS-3-SWE                      
909100         MOVE LISTA-ADRESS-4 TO CL7INCH-ADRESS-4-SWE                      
909200         MOVE WS-LISTA-ADRESS-5 TO CL7INCH-ADRESS-5-SWE                   
909300         MOVE WS-OHUV-IDDEPT TO CL7INCH-IDDEPT                            
909400                                                                          
909500         IF (CDC-SE AND DIST-KUND-LDC)                                    
909600           MOVE WS-ORAD-BERADREF TO CL7INCH-BERADREF-S01                  
909700         END-IF                                                           
909800       ELSE                                                               
909900         IF LISTA-ADRESS-2 > SPACE                                        
910000           MOVE LISTA-ADRESS-2 TO CL7INCH-ADRESS-1-SWE                    
910100           MOVE LISTA-ADRESS-3 TO CL7INCH-ADRESS-3-SWE                    
910200           MOVE LISTA-ADRESS-4 TO CL7INCH-ADRESS-4-SWE                    
910300           MOVE WS-LISTA-ADRESS-5 TO CL7INCH-ADRESS-5-SWE                 
910400           MOVE WS-OHUV-IDDEPT TO CL7INCH-IDDEPT                          
910500                                                                          
910600           IF (CDC-SE AND DIST-KUND-LDC)                                  
910700             MOVE WS-ORAD-BERADREF TO CL7INCH-BERADREF-S01                
910800           END-IF                                                         
910900         ELSE                                                             
911000           IF LISTA-ADRESS-3 > SPACE                                      
911100             MOVE LISTA-ADRESS-3    TO CL7INCH-ADRESS-1-SWE               
911200             MOVE LISTA-ADRESS-4    TO CL7INCH-ADRESS-4-SWE               
911300             MOVE WS-LISTA-ADRESS-5 TO CL7INCH-ADRESS-5-SWE               
911400             MOVE WS-OHUV-IDDEPT    TO CL7INCH-IDDEPT                     
911500                                                                          
911600             IF (CDC-SE AND DIST-KUND-LDC)                                
911700               MOVE WS-ORAD-BERADREF TO CL7INCH-BERADREF-S01              
911800             END-IF                                                       
911900           ELSE                                                           
912000             IF LISTA-ADRESS-4 > SPACE                                    
912100               MOVE LISTA-ADRESS-4      TO CL7INCH-ADRESS-1-SWE           
912200               MOVE WS-LISTA-ADRESS-5 TO CL7INCH-ADRESS-5-SWE             
912300               MOVE WS-OHUV-IDDEPT      TO CL7INCH-IDDEPT                 
912400                                                                          
912500               IF (CDC-SE AND DIST-KUND-LDC)                              
912600                 MOVE WS-ORAD-BERADREF  TO CL7INCH-BERADREF-S01           
912700               END-IF                                                     
912800             ELSE                                                         
912900               MOVE LISTA-ADRESS-1    TO CL7INCH-ADRESS-1                 
913000               MOVE LISTA-ADRESS-2    TO CL7INCH-ADRESS-2                 
913100               MOVE LISTA-ADRESS-3    TO CL7INCH-ADRESS-3                 
913200               MOVE LISTA-ADRESS-4    TO CL7INCH-ADRESS-4                 
913300               MOVE WS-LISTA-ADRESS-5 TO CL7INCH-ADRESS-5                 
913400               MOVE WS-OHUV-IDDEPT    TO CL7INCH-IDDEPT                   
913500                                                                          
913600               IF (CDC-SE AND DIST-KUND-LDC)                              
913700                 MOVE WS-ORAD-BERADREF  TO CL7INCH-BERADREF-S01           
913800               END-IF                                                     
913900             END-IF                                                       
914000           END-IF                                                         
914100         END-IF                                                           
914200       END-IF                                                             
914300     ELSE                                                                 
914400                                                                          
914500       IF DIST-KUND-LDC                                                   
914600       OR DIST-KUND-SDC-NL                                                
914700       OR DIST-KUND-SDC-IT                                                
914800       OR (DIST-KUND-LDC-GB-3A AND (WS-KDORDKL = 0 OR 1))                 
914900     OR (DIST05-NORGE AND (WS-OHUV-IDSYSTEM = 'LDC' OR 'TACD'))           
915000         MOVE LISTA-ADRESS-1     TO CL7INCH-ADRESS-1                      
915100         MOVE LISTA-ADRESS-3     TO CL7INCH-ADRESS-2                      
915200         MOVE LISTA-ADRESS-4     TO CL7INCH-ADRESS-3                      
915300         MOVE WS-LISTA-ADRESS-5  TO CL7INCH-ADRESS-5                      
915400         MOVE WS-ORAD-BERADREF   TO CL7INCH-BERADREF-S01                  
915500         IF LDC-SE                                                        
915600           MOVE WS-OHUV-IDDEPT   TO CL7INCH-IDDEPT                        
915700         END-IF                                                           
915800           MOVE WS-KDORDKL       TO CL7INCH-KDORDKL-S01                   
915900       ELSE                                                               
916000         MOVE LISTA-ADRESS-1         TO CL7INCH-ADRESS-1                  
916100         MOVE LISTA-ADRESS-2         TO CL7INCH-ADRESS-2                  
916200         MOVE LISTA-ADRESS-3         TO CL7INCH-ADRESS-3                  
916300         MOVE LISTA-ADRESS-4         TO CL7INCH-ADRESS-4                  
916400         MOVE WS-LISTA-ADRESS-5      TO CL7INCH-ADRESS-5                  
916500*BERADREF                                                                 
916600         IF (CDC-SE AND DIST30-FRANCE)                                    
916700           PERFORM S05-GET-INFO-FR-WDE420                                 
916800           MOVE WS-ORAD-BERADREF TO CL7INCH-1478-BERADREF                 
916900           MOVE WS-OHUV-BEKUNDRF TO CL7INCH-1478-BEKUNDRF                 
917000         END-IF                                                           
917100                                                                          
917200       END-IF                                                             
917300                                                                          
917400*      MOVE WS-IDDISTR              TO DIST05-IDDISTR                     
917500*      IF DIST05-NORGE                                                    
917600*      IF (WS-OHUV-IDSYSTEM = 'LDC' OR 'TACD')                            
917700*        MOVE WS-ORAD-BERADREF   TO CL7INCH-ADRESS-5-LDC                  
917800*      END-IF                                                             
917900*      END-IF                                                             
918000     END-IF                                                               
918100                                                                          
918200     IF  WS-KILO < 10                                                     
918300     AND (WS-KILO > ZERO OR WS-HEKTO > ZERO)                              
918400       MOVE WS-KILO      TO CL7INCH-KILO                                  
918500                            CL7INCH-KILO-GB                               
918600       MOVE WS-HEKTO     TO CL7INCH-HEKTO                                 
918700                            CL7INCH-HEKTO-GB                              
918800     ELSE                                                                 
918900       MOVE WS-VKORDBTO  TO CL7INCH-VKORDBTO                              
919000                            CL7INCH-VKORDBTO-GB                           
919100     END-IF                                                               
919200                                                                          
919300     MOVE LISTA-TIRFS    TO CL7INCH-TIRFS                                 
919400     MOVE LISTA-ADFLGEO  TO CL7INCH-ADFLGEO                               
919500     MOVE LISTA-ADFLOMR  TO CL7INCH-ADFLOMR                               
919600     MOVE LISTA-ADRUTNIV TO CL7INCH-ADRUTNIV                              
919700                            CL7INCH-ADRUTNIV-SPA                          
919800                                                                          
919900     IF WS-IDKOLLI-PRT = WS-IDKOLLI                                       
920000       CALL W006PRS1 USING PRT-SPOOL-OVR PRT-OPEN LISTVAL                 
920100                           ALT-PCB DUMMY-AREA DUMMY-AREA                  
920200     END-IF                                                               
920300                                                                          
920400     MOVE SPACE       TO CL7INCH-RAD                                      
920500     MOVE CL7INCH-STYR-01 TO CL7INCH-RAD                                  
920600     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                  
920700                         ALT-PCB PRT-NYSIDA-RAD1 CL7INCH-RAD              
920800                                                                          
920900     MOVE CL7INCH-STYR-01 TO CL7INCH-RAD                                  
921000     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                  
921100                         ALT-PCB PRT-AFTER-1 CL7INCH-RAD                  
921200                                                                          
921300     MOVE CL7INCH-RUB-1-1 TO CL7INCH-RAD                                  
921400     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                  
921500                         ALT-PCB PRT-AFTER-1 CL7INCH-RAD                  
921600                                                                          
921700     MOVE CL7INCH-STYR-42 TO CL7INCH-RAD                                  
921800     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                  
921900                         ALT-PCB PRT-AFTER-1 CL7INCH-RAD                  
922000                                                                          
922100     IF DIST34-ENGLAND-SDC                                                
922200       MOVE CL7INCH-RUB-DEALER   TO CL7INCH-RAD                           
922300     ELSE                                                                 
922400       MOVE CL7INCH-RUB-CUSTOMER TO CL7INCH-RAD                           
922500     END-IF                                                               
922600     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                  
922700                         ALT-PCB PRT-AFTER-1 CL7INCH-RAD                  
922800                                                                          
922900     MOVE CL7INCH-RUB-1-3 TO CL7INCH-RAD                                  
923000     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                  
923100                         ALT-PCB PRT-AFTER-1 CL7INCH-RAD                  
923200                                                                          
923300     MOVE CL7INCH-RUB-2-1 TO CL7INCH-RAD                                  
923400     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                  
923500                         ALT-PCB PRT-AFTER-1 CL7INCH-RAD                  
923600                                                                          
923700     MOVE CL7INCH-RUB-2-5 TO CL7INCH-RAD                                  
923800     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                  
923900                         ALT-PCB PRT-AFTER-1 CL7INCH-RAD                  
924000                                                                          
924100     MOVE CL7INCH-RUB-2-6 TO CL7INCH-RAD                                  
924200     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                  
924300                         ALT-PCB PRT-AFTER-1 CL7INCH-RAD                  
924400                                                                          
924500     IF  CDC-SE                                                           
924600     AND DIST34-ENGLAND-SDC                                               
924700       MOVE CL7INCH-RUB-3-1-WEIGHT-GB    TO CL7INCH-RAD                   
924800     ELSE                                                                 
924900       IF CDC-SE                                                          
925000         MOVE CL7INCH-RUB-3-1-WEIGHT-GB    TO CL7INCH-RAD                 
925100       ELSE                                                               
925200         MOVE CL7INCH-RUB-3-1-WEIGHT       TO CL7INCH-RAD                 
925300       END-IF                                                             
925400     END-IF                                                               
925500     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                  
925600                         ALT-PCB PRT-AFTER-1 CL7INCH-RAD                  
925700                                                                          
925800     MOVE OHUV-IDDISTR  TO DIST03-IDDISTR                                 
925900                                                                          
926000     IF DIST03-DANMARK-900                                                
926100                                                                          
926200       MOVE CL7INCH-TIREPDAT-RUB-S01      TO CL7INCH-RAD                  
926300       CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                
926400                           ALT-PCB PRT-AFTER-1 CL7INCH-RAD                
926500                                                                          
926600       MOVE WS-OHUV-TIREPDAT        TO CL7INCH-TIREPDAT-S01               
926700     INSPECT CL7INCH-TIREPDAT-S01 REPLACING LEADING ZERO BY SPACE         
926800       MOVE CL7INCH-TIREPDAT-LDC-S01      TO CL7INCH-RAD                  
926900       CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                
927000                           ALT-PCB PRT-AFTER-1 CL7INCH-RAD                
927100                                                                          
927200       MOVE CL7INCH-IDBILREG-RUB-S01      TO CL7INCH-RAD                  
927300       CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                
927400                           ALT-PCB PRT-AFTER-1 CL7INCH-RAD                
927500                                                                          
927600       MOVE CL7INCH-IDBILREG-LDC-S01      TO CL7INCH-RAD                  
927700       CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                
927800                           ALT-PCB PRT-AFTER-1 CL7INCH-RAD                
927900     END-IF                                                               
928000                                                                          
928100     IF CDC-SE                                                            
928200     OR LDC-SE                                                            
928300     OR GMT-FLLDCKND = JA                                                 
928400*                                                                         
928500       MOVE CL7INCH-RUB-4-1-RFS TO CL7INCH-RAD                            
928600       CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                
928700                           ALT-PCB PRT-AFTER-1 CL7INCH-RAD                
928800                                                                          
928900       IF (GMT-FLLDCKND = JA AND KDFRAKT-FINNS)                           
929000       OR KUND12-LDC-AKUT                                                 
929100         IF WS-OHUV-IDDEPT > ZERO                                         
929200           MOVE CL7INCH-RUB-IDDEPT TO CL7INCH-RAD                         
929300           CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL            
929400                               ALT-PCB PRT-AFTER-1 CL7INCH-RAD            
929500                                                                          
929600           MOVE CL7INCH-DATA-IDDEPT TO CL7INCH-RAD                        
929700           CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL            
929800                               ALT-PCB PRT-AFTER-1 CL7INCH-RAD            
929900         END-IF                                                           
930000                                                                          
930100       END-IF                                                             
930200     END-IF                                                               
930300                                                                          
930400     MOVE CL7INCH-RUB-KDORDKL-S01 TO CL7INCH-RAD                          
930500     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                  
930600                           ALT-PCB PRT-AFTER-1 CL7INCH-RAD                
930700                                                                          
930800     MOVE WS-KDORDKL           TO CL7INCH-KDORDKL-S01                     
930900     MOVE CL7INCH-DATA-KDORDKL-S01         TO CL7INCH-RAD                 
931000     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                  
931100                           ALT-PCB PRT-AFTER-1 CL7INCH-RAD                
931200                                                                          
931300     IF CDC-SE                                                            
931400       MOVE CL7INCH-DATA-1-1-SE  TO CL7INCH-RAD                           
931500     ELSE                                                                 
931600       MOVE CL7INCH-DATA-1-1     TO CL7INCH-RAD                           
931700     END-IF                                                               
931800     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                  
931900                         ALT-PCB PRT-AFTER-1 CL7INCH-RAD                  
932000                                                                          
932100     MOVE CL7INCH-IDKUNDNR-GRP   TO CL7INCH-RAD                           
932200     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                  
932300                         ALT-PCB PRT-AFTER-1 CL7INCH-RAD                  
932400                                                                          
932500     MOVE CL7INCH-IDORDNR-GRP  TO CL7INCH-RAD                             
932600     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                  
932700                         ALT-PCB PRT-AFTER-1 CL7INCH-RAD                  
932800                                                                          
932900     IF CL7INCH-ADRESS-1-SWE > SPACE                                      
933000*    AND WS-IDKUNDNR NOT = '000715'                                       
933100       MOVE CL7INCH-ADRESS-1-GRP          TO CL7INCH-RAD                  
933200       CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                
933300                           ALT-PCB PRT-AFTER-1 CL7INCH-RAD                
933400                                                                          
933500       MOVE CL7INCH-ADRESS-2-GRP          TO CL7INCH-RAD                  
933600       CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                
933700                           ALT-PCB PRT-AFTER-1 CL7INCH-RAD                
933800                                                                          
933900       MOVE CL7INCH-ADRESS-3-GRP          TO CL7INCH-RAD                  
934000       CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                
934100                           ALT-PCB PRT-AFTER-1 CL7INCH-RAD                
934200                                                                          
934300       MOVE CL7INCH-ADRESS-4-GRP          TO CL7INCH-RAD                  
934400       CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                
934500                           ALT-PCB PRT-AFTER-1 CL7INCH-RAD                
934600                                                                          
934700                                                                          
934800       MOVE CL7INCH-ADRESS-5-SWE          TO CL7INCH-RAD                  
934900       CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                
935000                           ALT-PCB PRT-AFTER-1 CL7INCH-RAD                
935100                                                                          
935200*BETEXT                                                                   
935300       IF WS-GMT-BETEXT-INFO > SPACE                                      
935400         MOVE WS-GMT-BETEXT-INFO  TO CL7INCH-DATA-BETEXT-SWE-S01          
935500         MOVE CL7INCH-DATA-BETEXT-GRP-S01  TO CL7INCH-RAD                 
935600         CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL              
935700                             ALT-PCB PRT-AFTER-1 CL7INCH-RAD              
935800       END-IF                                                             
935900     ELSE                                                                 
936000       MOVE CL7INCH-DATA-2-1 TO CL7INCH-RAD                               
936100       CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                
936200                           ALT-PCB PRT-AFTER-1 CL7INCH-RAD                
936300                                                                          
936400       MOVE CL7INCH-DATA-2-2 TO CL7INCH-RAD                               
936500       CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                
936600                           ALT-PCB PRT-AFTER-1 CL7INCH-RAD                
936700                                                                          
936800       MOVE CL7INCH-DATA-2-3 TO CL7INCH-RAD                               
936900       CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                
937000                           ALT-PCB PRT-AFTER-1 CL7INCH-RAD                
937100                                                                          
937200       MOVE CL7INCH-DATA-2-4 TO CL7INCH-RAD                               
937300       CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                
937400                           ALT-PCB PRT-AFTER-1 CL7INCH-RAD                
937500                                                                          
937600       MOVE CL7INCH-ADRESS-5-SWE          TO CL7INCH-RAD                  
937700       CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                
937800                           ALT-PCB PRT-AFTER-1 CL7INCH-RAD                
937900                                                                          
938000*BETEXT                                                                   
938100       IF WS-GMT-BETEXT-INFO > SPACE                                      
938200         MOVE WS-GMT-BETEXT-INFO  TO CL7INCH-DATA-BETEXT-SWE-S01          
938300         MOVE CL7INCH-DATA-BETEXT-GRP-S01  TO CL7INCH-RAD                 
938400         CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL              
938500                             ALT-PCB PRT-AFTER-1 CL7INCH-RAD              
938600       END-IF                                                             
938700                                                                          
938800       MOVE WS-IDDISTR       TO DIST05-IDDISTR                            
938900       IF DIST-KUND-LDC                                                   
939000       OR (DIST05-NORGE AND (WS-OHUV-IDSYSTEM = 'LDC' OR 'TACD'))         
939100         MOVE CL7INCH-ADRESS-2-5-S08     TO CL7INCH-RAD                   
939200       ELSE                                                               
939300         MOVE CL7INCH-ADRESS-2-5         TO CL7INCH-RAD                   
939400       END-IF                                                             
939500*                                                                         
939600*      IF DIST05-NORGE                                                    
939700*      IF (WS-OHUV-IDSYSTEM = 'LDC' OR 'TACD')                            
939800*        MOVE CL7INCH-ADRESS-2-5-LDC     TO CL7INCH-RAD                   
939900*      END-IF                                                             
940000*      END-IF                                                             
940100                                                                          
940200       CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                
940300                           ALT-PCB PRT-AFTER-1 CL7INCH-RAD                
940400     END-IF                                                               
940500                                                                          
940600     MOVE CL7INCH-DATA-IDKOLLI TO CL7INCH-RAD                             
940700     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                  
940800                         ALT-PCB PRT-AFTER-1 CL7INCH-RAD                  
940900                                                                          
941000     IF (CDC-SE AND DIST30-FRANCE)                                        
941100*BERADREF RUB                                                             
941200       MOVE CL7INCH-RUB-LINEREF        TO CL7INCH-RAD                     
941300       CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                
941400                           ALT-PCB PRT-AFTER-1 CL7INCH-RAD                
941500                                                                          
941600*BERADREF                                                                 
941700       MOVE CL7INCH-DATA-BERADREF       TO CL7INCH-RAD                    
941800       CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                
941900                           ALT-PCB PRT-AFTER-1 CL7INCH-RAD                
942000                                                                          
942100       MOVE CL7INCH-RUB-CUSTREF        TO CL7INCH-RAD                     
942200       CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                
942300                           ALT-PCB PRT-AFTER-1 CL7INCH-RAD                
942400                                                                          
942500       MOVE CL7INCH-DATA-BEKUNDRF       TO CL7INCH-RAD                    
942600       CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                
942700                           ALT-PCB PRT-AFTER-1 CL7INCH-RAD                
942800     ELSE                                                                 
942900*BERADREF                                                                 
943000       IF CL7INCH-BERADREF-S01 > SPACE                                    
943100         MOVE CL7INCH-BERADREF-GRP     TO CL7INCH-RAD                     
943200         CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL              
943300                             ALT-PCB PRT-AFTER-1 CL7INCH-RAD              
943400                                                                          
943500         MOVE CL7INCH-BERADREF-RUB-S01 TO CL7INCH-RAD                     
943600         CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL              
943700                             ALT-PCB PRT-AFTER-1 CL7INCH-RAD              
943800       END-IF                                                             
943900     END-IF                                                               
944000                                                                          
944100*KDFRAKT                                                                  
944200     IF CDC-SE                                                            
944300       MOVE CL7INCH-DATA-2-6-SE     TO CL7INCH-RAD                        
944400     ELSE                                                                 
944500       MOVE CL7INCH-DATA-2-6        TO CL7INCH-RAD                        
944600     END-IF                                                               
944700     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                  
944800                         ALT-PCB PRT-AFTER-1 CL7INCH-RAD                  
944900                                                                          
945000     IF  CDC-SE                                                           
945100     AND DIST34-ENGLAND-SDC                                               
945200       IF  WS-KILO < 10                                                   
945300       AND (WS-KILO > ZERO OR WS-HEKTO > ZERO)                            
945400         MOVE CL7INCH-DATA-KILO-HEKTO-GB  TO CL7INCH-RAD                  
945500         CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL              
945600                           ALT-PCB PRT-AFTER-1 CL7INCH-RAD                
945700       ELSE                                                               
945800         MOVE CL7INCH-DATA-WEIGHT-GB      TO CL7INCH-RAD                  
945900         CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL              
946000                           ALT-PCB PRT-AFTER-1 CL7INCH-RAD                
946100       END-IF                                                             
946200     ELSE                                                                 
946300       IF  WS-KILO < 10                                                   
946400       AND (WS-KILO > ZERO OR WS-HEKTO > ZERO)                            
946500         MOVE CL7INCH-DATA-KILO-HEKTO-GB  TO CL7INCH-RAD                  
946600         CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL              
946700                           ALT-PCB PRT-AFTER-1 CL7INCH-RAD                
946800       ELSE                                                               
946900         MOVE CL7INCH-DATA-WEIGHT-GB      TO CL7INCH-RAD                  
947000         CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL              
947100                           ALT-PCB PRT-AFTER-1 CL7INCH-RAD                
947200       END-IF                                                             
947300     END-IF                                                               
947400                                                                          
947500     IF CDC-SE                                                            
947600     OR LDC-SE                                                            
947700       MOVE CL7INCH-DATA-TIRFS TO CL7INCH-RAD                             
947800       CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                
947900                           ALT-PCB PRT-AFTER-1 CL7INCH-RAD                
948000                                                                          
948100       IF LDC-SE                                                          
948200         CONTINUE                                                         
948300       ELSE                                                               
948400*DENNA TEST GER FEL/BLANK ADFLGEO PÅ KF VID RAD MED RESTORDER.            
948500*KOMMENTARMÄRKT FÖR ATT EV ANVÄNDAS OM BEHOV UPPSTÅR.                     
948600*        IF GMT-FLLDCKND = JA AND RO-JA                                   
948700*          CONTINUE                                                       
948800*        ELSE                                                             
948900           MOVE CL7INCH-DATA-4-2 TO CL7INCH-RAD                           
949000           CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL            
949100                           ALT-PCB PRT-AFTER-1 CL7INCH-RAD                
949200*        END-IF                                                           
949300       END-IF                                                             
949400                                                                          
949500       IF LISTA-IDDISTR = ' 878'                                          
949600       AND LISTA-IDKUNDNR = '  1117'                                      
949700       AND CL7INCH-ADFLGEO = 'FLY'                                        
949800         CONTINUE                                                         
949900       ELSE                                                               
950000         IF LDC-SE                                                        
950100           CONTINUE                                                       
950200         ELSE                                                             
950300*DENNA TEST GER FEL/BLANK ADFLGEO PÅ KF VID RAD MED RESTORDER.            
950400*KOMMENTARMÄRKT FÖR ATT EV ANVÄNDAS OM BEHOV UPPSTÅR.                     
950500*          IF GMT-FLLDCKND = JA AND RO-JA                                 
950600*            MOVE CL7INCH-DATA-RESTORDER TO CL7INCH-RAD                   
950700*          ELSE                                                           
950800             MOVE CL7INCH-DATA-4-3 TO CL7INCH-RAD                         
950900*          END-IF                                                         
951000           CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL            
951100                             ALT-PCB PRT-AFTER-1 CL7INCH-RAD              
951200         END-IF                                                           
951300       END-IF                                                             
951400                                                                          
951500       IF LISTA-IDDISTR = ' 878'                                          
951600       AND LISTA-IDKUNDNR = '  1117'                                      
951700       AND (CL7INCH-ADFLGEO = 'RUT'                                       
951800       OR CL7INCH-ADFLGEO = 'FLY')                                        
951900         CONTINUE                                                         
952000       ELSE                                                               
952100         IF LDC-SE                                                        
952200           CONTINUE                                                       
952300         ELSE                                                             
952400*DENNA TEST GER FEL/BLANK ADFLGEO PÅ KF VID RAD MED RESTORDER.            
952500*KOMMENTARMÄRKT FÖR ATT EV ANVÄNDAS OM BEHOV UPPSTÅR.                     
952600*          IF GMT-FLLDCKND = JA AND RO-JA                                 
952700*            CONTINUE                                                     
952800*          ELSE                                                           
952900             MOVE CL7INCH-DATA-4-4 TO CL7INCH-RAD                         
953000             CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL          
953100                                 ALT-PCB PRT-AFTER-1 CL7INCH-RAD          
953200*          END-IF                                                         
953300         END-IF                                                           
953400       END-IF                                                             
953500                                                                          
953600     END-IF                                                               
953700                                                                          
953800*HIT-FI SCR 5619825 + HIT-NO SCR 10271301                                 
953900*POSTEN-DIST-1090 START                                                   
954000                                                                          
954100       MOVE WS-IDDISTR     TO TEST-IDDISTR                                
954200     IF CDC-SE AND DIST83-HIT-FI                                          
954300       MOVE CL7INCH-TEXT-PRODUKT-1090 TO CL7INCH-RAD                      
954400       CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                
954500                           ALT-PCB PRT-AFTER-1 CL7INCH-RAD                
954600*48   HIT  FÖR 1090                                                       
954700       MOVE CL7INCH-TEXT-48-EJ-SE TO CL7INCH-RAD                          
954800       CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                
954900                           ALT-PCB PRT-AFTER-1 CL7INCH-RAD                
955000       MOVE CL7INCH-TEXT-HIT-1090 TO CL7INCH-RAD                          
955100       CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                
955200                             ALT-PCB PRT-AFTER-1 CL7INCH-RAD              
955300                                                                          
955400       MOVE CL7INCH-SORTERINGSKOD-1090 TO CL7INCH-RAD                     
955500       CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                
955600                           ALT-PCB PRT-AFTER-1 CL7INCH-RAD                
955700       MOVE WS-IDKLIID   TO CL7INCH-POSTEN1-1090                          
955800                            CL7INCH-POSTEN1-TEXT-1090                     
955900                                                                          
956000       MOVE CL7INCH-BARCODE-POSTEN-1090 TO CL7INCH-RAD                    
956100       CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                
956200                           ALT-PCB PRT-AFTER-1 CL7INCH-RAD                
956300                                                                          
956400       MOVE CL7INCH-TEXT-POSTEN-1090  TO CL7INCH-RAD                      
956500       CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                
956600                           ALT-PCB PRT-AFTER-1 CL7INCH-RAD                
956700     END-IF                                                               
956800*                                                                         
956900*POSTEN-A5-DIST-1090 END                                                  
957000*                                                                         
957100     IF CDC-SE AND DIST83-HIT-NO                                          
957200       MOVE CL7INCH-TEXT-PRODUKT-1090 TO CL7INCH-RAD                      
957300       CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                
957400                           ALT-PCB PRT-AFTER-1 CL7INCH-RAD                
957500*48   HIT FÖR NO                                                          
957600       MOVE CL7INCH-TEXT-48-EJ-SE TO CL7INCH-RAD                          
957700       CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                
957800                           ALT-PCB PRT-AFTER-1 CL7INCH-RAD                
957900       MOVE CL7INCH-TEXT-HIT-1090 TO CL7INCH-RAD                          
958000       CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                
958100                             ALT-PCB PRT-AFTER-1 CL7INCH-RAD              
958200                                                                          
958300       MOVE CL7INCH-SORTERINGSKOD-NO   TO CL7INCH-RAD                     
958400       CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                
958500                           ALT-PCB PRT-AFTER-1 CL7INCH-RAD                
958600                                                                          
958700       MOVE WS-IDKLIID   TO CL7INCH-POSTEN1-1090                          
958800                            CL7INCH-POSTEN1-TEXT-1090                     
958900                                                                          
959000       MOVE CL7INCH-BARCODE-POSTEN-1090 TO CL7INCH-RAD                    
959100       CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                
959200                           ALT-PCB PRT-AFTER-1 CL7INCH-RAD                
959300                                                                          
959400       MOVE CL7INCH-TEXT-POSTEN-1090  TO CL7INCH-RAD                      
959500       CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                
959600                           ALT-PCB PRT-AFTER-1 CL7INCH-RAD                
959700     END-IF                                                               
959800*POSTEN-A5-DIST-NO   END                                                  
959900                                                                          
960000     MOVE WS-IDDISTR       TO DIST34-IDDISTR                              
960100     IF  CDC-SE                                                           
960200     AND DIST34-ENGLAND-SDC                                               
960300                                                                          
960400       MOVE CL7INCH-RUB-3-3-DEPOT TO CL7INCH-RAD                          
960500       CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                
960600                           ALT-PCB PRT-AFTER-1 CL7INCH-RAD                
960700                                                                          
960800       MOVE CL7INCH-RUB-3-4-ROUTE TO CL7INCH-RAD                          
960900       CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                
961000                           ALT-PCB PRT-AFTER-1 CL7INCH-RAD                
961100                                                                          
961200       MOVE CL7INCH-TRPINFO-SDC23-GB       TO CL7INCH-RAD                 
961300       CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                
961400                           ALT-PCB PRT-AFTER-1 CL7INCH-RAD                
961500                                                                          
961600     END-IF                                                               
961700                                                                          
961800     IF SKRIV-LANG-BARCODE                                                
961900       MOVE CL7INCH-BARCODE-LONG TO CL7INCH-RAD                           
962000       CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                
962100                           ALT-PCB PRT-AFTER-1 CL7INCH-RAD                
962200     ELSE                                                                 
962300       MOVE CL7INCH-BARCODE TO CL7INCH-RAD                                
962400       CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                
962500                           ALT-PCB PRT-AFTER-1 CL7INCH-RAD                
962600     END-IF                                                               
962700                                                                          
962800     MOVE CL7INCH-TEXT-BELOW-BARCODE TO CL7INCH-RAD                       
962900     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                  
963000                         ALT-PCB PRT-AFTER-1 CL7INCH-RAD                  
963100                                                                          
963200     IF CDC-SE                                                            
963300       EVALUATE TRUE                                                      
963400       WHEN DIST34-ENGLAND-SDC                                            
963500        MOVE CL7INCH-TEXT-SDC23-SHIPPER-CDC TO CL7INCH-RAD                
963600        CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL               
963700                            ALT-PCB PRT-AFTER-1 CL7INCH-RAD               
963800       WHEN OTHER                                                         
963900        MOVE CL7INCH-TEXT-SHIPPER-CDC TO CL7INCH-RAD                      
964000        CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL               
964100                            ALT-PCB PRT-AFTER-1 CL7INCH-RAD               
964200       END-EVALUATE                                                       
964300     END-IF                                                               
964400                                                                          
964500     MOVE CL7INCH-STYR-91 TO CL7INCH-RAD                                  
964600     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                  
964700                         ALT-PCB PRT-AFTER-1 CL7INCH-RAD                  
964800                                                                          
964900     IF WS-IDKOLLI-PRT = WS-IDKOLLI-TOM                                   
965000       CALL W006PRS1 USING PRT-SPOOL-OVR PRT-CLOSE LISTVAL                
965100                           ALT-PCB DUMMY-AREA DUMMY-AREA                  
965200     END-IF                                                               
965300     .                                                                    
965400     EJECT                                                                
965500 DB-PRINT-NOVA-LABEL                        SECTION.                      
965600                                                                          
965700     MOVE LISTA-IDDISTR  TO NOVA-LBL-IDDISTR                              
965800                            NOVA-LBL-IDDISTR-SE                           
965900                            NOVA-LBL-DISTR                                
966000                            NOVA-LBL-DIST                                 
966100                            NOVA-LBL-DIST-POST                            
966200                            NOVA-LBL-DISTR-POST                           
966300                            NOVA-LBL-DISTR-LONG                           
966400*IDGROSS                                                                  
966500     IF (OHUV-IDSYSTEM = 'LDC' OR 'TACD')                                 
966600     AND (DIST13-SVERIGE)                                                 
966700     AND GMT-FLLDCKND = JA                                                
966800     AND OHUV-IDGROSS > ZERO                                              
966900       MOVE WS-IDKUNDNR-IDGROSS TO NOVA-LBL-IDKUNDNR                      
967000       MOVE WS-IDKUNDNR         TO NOVA-LBL-KUNDN                         
967100                                   NOVA-LBL-KUNDN-POST                    
967200                                   NOVA-LBL-KUNDNR-POST                   
967300                                   NOVA-LBL-KUNDNR-LONG                   
967400     ELSE                                                                 
967500       MOVE LISTA-IDKUNDNR TO NOVA-LBL-IDKUNDNR                           
967600       MOVE LISTA-IDKUNDNR TO NOVA-LBL-KUNDNR                             
967700                              NOVA-LBL-KUNDN                              
967800                              NOVA-LBL-KUNDN-POST                         
967900                              NOVA-LBL-KUNDNR-POST                        
968000                              NOVA-LBL-KUNDNR-LONG                        
968100     END-IF                                                               
968200                                                                          
968300     MOVE LISTA-IDORDNR  TO NOVA-LBL-IDORDNR                              
968400                            NOVA-LBL-ORDNR                                
968500                            NOVA-LBL-ORDN                                 
968600                            NOVA-LBL-ORDN-POST                            
968700                            NOVA-LBL-ORDNR-POST                           
968800                            NOVA-LBL-ORDNR-LONG                           
968900     MOVE WS-IDPRODNR    TO NOVA-LBL-IDPRODNR                             
969000     MOVE LISTA-IDKOLLI  TO NOVA-LBL-IDKOLLI                              
969100                            NOVA-LBL-KOLLI                                
969200                            NOVA-LBL-KOLI                                 
969300                            NOVA-LBL-KOLI-POST                            
969400                            NOVA-LBL-KOLLI-POST                           
969500                            NOVA-LBL-KOLLI-LONG                           
969600     MOVE LISTA-KDFRAKT  TO NOVA-LBL-KDFRAKT                              
969700     MOVE LISTA-KDFRAKT  TO NOVA-LBL-KDFRAKT-SE                           
969800                            WS-WRITE-ZONA                                 
969900     MOVE WS-IDZON       TO NOVA-LBL-IDZON                                
970000     MOVE WS-IDROUTE     TO NOVA-LBL-IDROUTE                              
970100     MOVE WS-IDDEPOT     TO NOVA-LBL-IDDEPOT                              
970200     MOVE WS-IDDISTR     TO TEST-IDDISTR                                  
970300     MOVE WS-IDDISTR     TO DIST29-IDDISTR                                
970400     MOVE WS-IDKUNDNR    TO KUND12-IDKUNDNR                               
970500     MOVE LISTA-IDBILREG TO NOVA-LBL-IDBILREG                             
970600                                                                          
970700     IF DIST-KUND-LDC                                                     
970800     OR DIST-KUND-SDC-NL                                                  
970900     OR DIST-KUND-LDC-GB-3A                                               
971000     OR DIST-KUND-SDC-IT                                                  
971100     OR GMT-FLLDCKND = JA                                                 
971200                                                                          
971300       PERFORM S05-GET-INFO-FR-WDE420                                     
971400     END-IF                                                               
971500                                                                          
971600     MOVE WS-KDORDKL TO NOVA-LBL-KDORDKL                                  
971700                                                                          
971800                                                                          
971900     IF LISTA-ADRESS-1 > SPACE                                            
972000       MOVE LISTA-ADRESS-1 TO NOVA-LBL-ADRESS-1-SWE                       
972100       MOVE LISTA-ADRESS-2 TO NOVA-LBL-ADRESS-2-SWE                       
972200       MOVE LISTA-ADRESS-3 TO NOVA-LBL-ADRESS-3-SWE                       
972300       MOVE LISTA-ADRESS-4 TO NOVA-LBL-ADRESS-4-SWE                       
972400       MOVE WS-OHUV-IDDEPT TO NOVA-LBL-IDDEPT                             
972500                                                                          
972600       IF GMT-FLLDCKND = JA                                               
972700         MOVE WS-ORAD-BERADREF TO NOVA-LBL-ADRESS-5-SWE                   
972800       ELSE                                                               
972900         MOVE WS-LISTA-ADRESS-5 TO NOVA-LBL-ADRESS-5-SWE                  
973000       END-IF                                                             
973100     ELSE                                                                 
973200       IF LISTA-ADRESS-2 > SPACE                                          
973300         MOVE LISTA-ADRESS-2 TO NOVA-LBL-ADRESS-1-SWE                     
973400         MOVE LISTA-ADRESS-3 TO NOVA-LBL-ADRESS-3-SWE                     
973500         MOVE LISTA-ADRESS-4 TO NOVA-LBL-ADRESS-4-SWE                     
973600         MOVE WS-OHUV-IDDEPT TO NOVA-LBL-IDDEPT                           
973700                                                                          
973800         IF GMT-FLLDCKND = JA                                             
973900           MOVE WS-ORAD-BERADREF    TO NOVA-LBL-ADRESS-5-SWE              
974000         ELSE                                                             
974100           MOVE WS-LISTA-ADRESS-5   TO NOVA-LBL-ADRESS-5-SWE              
974200         END-IF                                                           
974300       ELSE                                                               
974400         IF LISTA-ADRESS-3 > SPACE                                        
974500           MOVE LISTA-ADRESS-3      TO NOVA-LBL-ADRESS-1-SWE              
974600           MOVE LISTA-ADRESS-4      TO NOVA-LBL-ADRESS-4-SWE              
974700           MOVE WS-LISTA-ADRESS-5   TO NOVA-LBL-ADRESS-5-SWE              
974800           MOVE WS-OHUV-IDDEPT      TO NOVA-LBL-IDDEPT                    
974900                                                                          
975000           IF GMT-FLLDCKND = JA                                           
975100             MOVE WS-ORAD-BERADREF  TO NOVA-LBL-ADRESS-5-SWE              
975200           ELSE                                                           
975300             MOVE ZERO              TO NOVA-LBL-ADRESS-5-SWE              
975400           END-IF                                                         
975500         ELSE                                                             
975600           IF LISTA-ADRESS-4 > SPACE                                      
975700             MOVE LISTA-ADRESS-4        TO NOVA-LBL-ADRESS-1-SWE          
975800             MOVE WS-OHUV-IDDEPT        TO NOVA-LBL-IDDEPT                
975900                                                                          
976000             IF GMT-FLLDCKND = JA                                         
976100               MOVE WS-ORAD-BERADREF  TO NOVA-LBL-ADRESS-5-SWE            
976200             ELSE                                                         
976300               MOVE WS-LISTA-ADRESS-5 TO NOVA-LBL-ADRESS-5-SWE            
976400             END-IF                                                       
976500           ELSE                                                           
976600             MOVE LISTA-ADRESS-1      TO NOVA-LBL-ADRESS-1                
976700             MOVE LISTA-ADRESS-2      TO NOVA-LBL-ADRESS-2                
976800             MOVE LISTA-ADRESS-3      TO NOVA-LBL-ADRESS-3                
976900             MOVE LISTA-ADRESS-4      TO NOVA-LBL-ADRESS-4                
977000             MOVE WS-LISTA-ADRESS-5   TO NOVA-LBL-ADRESS-5                
977100             MOVE WS-OHUV-IDDEPT      TO NOVA-LBL-IDDEPT                  
977200                                                                          
977300             IF GMT-FLLDCKND = JA                                         
977400               MOVE WS-ORAD-BERADREF  TO NOVA-LBL-ADRESS-5                
977500             ELSE                                                         
977600               MOVE WS-LISTA-ADRESS-5 TO NOVA-LBL-ADRESS-5                
977700             END-IF                                                       
977800           END-IF                                                         
977900         END-IF                                                           
978000       END-IF                                                             
978100     END-IF                                                               
978200                                                                          
978300                                                                          
978400     IF  WS-KILO < 10                                                     
978500     AND (WS-KILO > ZERO OR WS-HEKTO > ZERO)                              
978600       MOVE WS-KILO      TO NOVA-LBL-KILO                                 
978700                            NOVA-LBL-KILO-GB                              
978800       MOVE WS-HEKTO     TO NOVA-LBL-HEKTO                                
978900                            NOVA-LBL-HEKTO-GB                             
979000     ELSE                                                                 
979100       MOVE WS-VKORDBTO  TO NOVA-LBL-VKORDBTO                             
979200                            NOVA-LBL-VKORDBTO-GB                          
979300     END-IF                                                               
979400                                                                          
979500     MOVE LISTA-TIRFS    TO NOVA-LBL-TIRFS                                
979600     MOVE LISTA-TIRFS    TO NOVA-LBL-TIRFS-POST                           
979700     MOVE LISTA-ADFLGEO  TO NOVA-LBL-ADFLGEO-SORT                         
979800     MOVE LISTA-ADFLGEO  TO NOVA-LBL-REFILL-ADFLGEO-SORT                  
979900     MOVE LISTA-ADFLGEO  TO NOVA-LBL-ADFLGEO-STD                          
980000     MOVE LISTA-ADFLGEO  TO NOVA-LBL-ADFLGEO-GB                           
980100     MOVE LISTA-ADFLGEO  TO NOVA-LBL-ADFLGEO-POST                         
980200     MOVE LISTA-ADFLOMR  TO NOVA-LBL-ADFLOMR-STD                          
980300     MOVE LISTA-ADFLOMR  TO NOVA-LBL-ADFLOMR-GB                           
980400     MOVE LISTA-ADFLOMR  TO NOVA-LBL-ADFLOMR-POST                         
980500     MOVE LISTA-ADRUTNIV TO NOVA-LBL-ADRUTNIV-STD                         
980600     MOVE LISTA-ADRUTNIV TO NOVA-LBL-ADRUTNIV-GB                          
980700     MOVE LISTA-ADRUTNIV TO NOVA-LBL-ADRUTNIV-POST                        
980800                            NOVA-LBL-ADRUTNIV-SPA                         
980900     IF DIST83-HIT-FI                                                     
981000       MOVE 4504-ADFLGEO TO NOVA-LBL-ADFLGEO-SORT-1090                    
981100     END-IF                                                               
981200                                                                          
981300     IF DIST83-HIT-NO                                                     
981400     AND VORD-KDFRAKT NOT = +31                                           
981500       MOVE 4504-ADFLGEO TO NOVA-LBL-ADFLGEO-SORT-NO                      
981600     END-IF                                                               
981700                                                                          
981800     IF WS-IDKOLLI-PRT = WS-IDKOLLI                                       
981900       CALL W006PRS1 USING PRT-SPOOL-OVR PRT-OPEN LISTVAL                 
982000                           ALT-PCB DUMMY-AREA DUMMY-AREA                  
982100     END-IF                                                               
982200                                                                          
982300     MOVE SPACE       TO NOVA-LBL-RAD                                     
982400     MOVE NOVA-LBL-STYR-01 TO NOVA-LBL-RAD                                
982500     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                  
982600                         ALT-PCB PRT-NYSIDA-RAD1 NOVA-LBL-RAD             
982700                                                                          
982800     MOVE NOVA-LBL-STYR-01 TO NOVA-LBL-RAD                                
982900     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                  
983000                         ALT-PCB PRT-AFTER-1 NOVA-LBL-RAD                 
983100                                                                          
983200     MOVE NOVA-LBL-STYR-42 TO NOVA-LBL-RAD                                
983300     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                  
983400                         ALT-PCB PRT-AFTER-1 NOVA-LBL-RAD                 
983500                                                                          
983600     MOVE NOVA-LBL-RUB-1-1 TO NOVA-LBL-RAD                                
983700     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                  
983800                         ALT-PCB PRT-AFTER-1 NOVA-LBL-RAD                 
983900                                                                          
984000     MOVE NOVA-LBL-RUB-CUSTOMER TO NOVA-LBL-RAD                           
984100     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                  
984200                       ALT-PCB PRT-AFTER-1 NOVA-LBL-RAD                   
984300                                                                          
984400     MOVE NOVA-LBL-RUB-1-3 TO NOVA-LBL-RAD                                
984500     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                  
984600                         ALT-PCB PRT-AFTER-1 NOVA-LBL-RAD                 
984700                                                                          
984800     MOVE NOVA-LBL-RUB-2-1 TO NOVA-LBL-RAD                                
984900     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                  
985000                         ALT-PCB PRT-AFTER-1 NOVA-LBL-RAD                 
985100                                                                          
985200     MOVE NOVA-LBL-RUB-2-5 TO NOVA-LBL-RAD                                
985300     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                  
985400                         ALT-PCB PRT-AFTER-1 NOVA-LBL-RAD                 
985500                                                                          
985600     MOVE NOVA-LBL-RUB-2-6 TO NOVA-LBL-RAD                                
985700     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                  
985800                         ALT-PCB PRT-AFTER-1 NOVA-LBL-RAD                 
985900                                                                          
986000     IF CDC-SE                                                            
986100       MOVE NOVA-LBL-RUB-3-1-WEIGHT-SE  TO NOVA-LBL-RAD                   
986200     ELSE                                                                 
986300       MOVE NOVA-LBL-RUB-3-1-WEIGHT     TO NOVA-LBL-RAD                   
986400     END-IF                                                               
986500     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                  
986600                           ALT-PCB PRT-AFTER-1 NOVA-LBL-RAD               
986700                                                                          
986800     MOVE OHUV-IDDISTR  TO DIST03-IDDISTR                                 
986900                                                                          
987000     IF DIST03-DANMARK-900                                                
987100     OR (OHUV-IDSYSTEM = 'LDC' OR 'TACD')                                 
987200       MOVE NOVA-LBL-RUB-REPDAT          TO NOVA-LBL-RAD                  
987300       CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                
987400                           ALT-PCB PRT-AFTER-1 NOVA-LBL-RAD               
987500                                                                          
987600       MOVE WS-OHUV-TIREPDAT             TO NOVA-TIREPDAT                 
987700       INSPECT NOVA-TIREPDAT REPLACING LEADING ZERO BY SPACE              
987800       MOVE NOVA-LBL-TIREPDAT            TO NOVA-LBL-RAD                  
987900       CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                
988000                           ALT-PCB PRT-AFTER-1 NOVA-LBL-RAD               
988100                                                                          
988200       MOVE NOVA-LBL-RUB-CAR-REG         TO NOVA-LBL-RAD                  
988300       CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                
988400                           ALT-PCB PRT-AFTER-1 NOVA-LBL-RAD               
988500                                                                          
988600       MOVE NOVA-LBL-IDBILREG-LDC        TO NOVA-LBL-RAD                  
988700       CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                
988800                           ALT-PCB PRT-AFTER-1 NOVA-LBL-RAD               
988900     END-IF                                                               
989000                                                                          
989100*POSTEN                                                                   
989200     MOVE NOVA-LBL-RUB-RFS-POST TO NOVA-LBL-RAD                           
989300     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                  
989400                         ALT-PCB PRT-AFTER-1 NOVA-LBL-RAD                 
989500                                                                          
989600     IF (GMT-FLLDCKND = JA AND KDFRAKT-FINNS)                             
989700     OR KUND12-LDC-AKUT                                                   
989800       IF WS-OHUV-IDDEPT > ZERO                                           
989900         MOVE NOVA-LBL-RUB-IDDEPT TO NOVA-LBL-RAD                         
990000         CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL              
990100                             ALT-PCB PRT-AFTER-1 NOVA-LBL-RAD             
990200                                                                          
990300         MOVE NOVA-LBL-DATA-IDDEPT TO NOVA-LBL-RAD                        
990400         CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL              
990500                             ALT-PCB PRT-AFTER-1 NOVA-LBL-RAD             
990600       END-IF                                                             
990700                                                                          
990800     END-IF                                                               
990900                                                                          
991000     MOVE NOVA-LBL-RUB-KDORDKL TO NOVA-LBL-RAD                            
991100     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                  
991200                           ALT-PCB PRT-AFTER-1 NOVA-LBL-RAD               
991300                                                                          
991400     MOVE NOVA-LBL-DATA-KDORDKL TO NOVA-LBL-RAD                           
991500     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                  
991600                           ALT-PCB PRT-AFTER-1 NOVA-LBL-RAD               
991700                                                                          
991800     IF CDC-SE                                                            
991900       MOVE NOVA-LBL-DATA-1-1-SE TO NOVA-LBL-RAD                          
992000     ELSE                                                                 
992100       MOVE NOVA-LBL-DATA-1-1    TO NOVA-LBL-RAD                          
992200     END-IF                                                               
992300     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                  
992400                         ALT-PCB PRT-AFTER-1 NOVA-LBL-RAD                 
992500                                                                          
992600     MOVE NOVA-LBL-DATA-1-2 TO NOVA-LBL-RAD                               
992700     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                  
992800                         ALT-PCB PRT-AFTER-1 NOVA-LBL-RAD                 
992900                                                                          
993000     MOVE NOVA-LBL-DATA-1-3 TO NOVA-LBL-RAD                               
993100     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                  
993200                         ALT-PCB PRT-AFTER-1 NOVA-LBL-RAD                 
993300                                                                          
993400     IF NOVA-LBL-ADRESS-1-SWE > SPACE                                     
993500*    AND WS-IDKUNDNR NOT = '000715'                                       
993600       MOVE NOVA-LBL-DATA-2-1-SWE         TO NOVA-LBL-RAD                 
993700       CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                
993800                           ALT-PCB PRT-AFTER-1 NOVA-LBL-RAD               
993900                                                                          
994000       MOVE NOVA-LBL-DATA-2-2-SWE         TO NOVA-LBL-RAD                 
994100       CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                
994200                           ALT-PCB PRT-AFTER-1 NOVA-LBL-RAD               
994300                                                                          
994400       MOVE NOVA-LBL-DATA-2-3-SWE         TO NOVA-LBL-RAD                 
994500       CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                
994600                           ALT-PCB PRT-AFTER-1 NOVA-LBL-RAD               
994700                                                                          
994800       MOVE NOVA-LBL-DATA-2-4-SWE         TO NOVA-LBL-RAD                 
994900       CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                
995000                           ALT-PCB PRT-AFTER-1 NOVA-LBL-RAD               
995100                                                                          
995200       MOVE NOVA-LBL-DATA-2-5-SWE         TO NOVA-LBL-RAD                 
995300       CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                
995400                           ALT-PCB PRT-AFTER-1 NOVA-LBL-RAD               
995500     ELSE                                                                 
995600       MOVE NOVA-LBL-DATA-2-1 TO NOVA-LBL-RAD                             
995700       CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                
995800                           ALT-PCB PRT-AFTER-1 NOVA-LBL-RAD               
995900                                                                          
996000       MOVE NOVA-LBL-DATA-2-2 TO NOVA-LBL-RAD                             
996100       CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                
996200                           ALT-PCB PRT-AFTER-1 NOVA-LBL-RAD               
996300                                                                          
996400       MOVE NOVA-LBL-DATA-2-3 TO NOVA-LBL-RAD                             
996500       CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                
996600                           ALT-PCB PRT-AFTER-1 NOVA-LBL-RAD               
996700                                                                          
996800       MOVE NOVA-LBL-DATA-2-4 TO NOVA-LBL-RAD                             
996900       CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                
997000                           ALT-PCB PRT-AFTER-1 NOVA-LBL-RAD               
997100                                                                          
997200                                                                          
997300       IF DIST-KUND-LDC                                                   
997400         MOVE NOVA-LBL-ADRESS-2-5-LDC    TO NOVA-LBL-RAD                  
997500         CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL              
997600                             ALT-PCB PRT-AFTER-1 NOVA-LBL-RAD             
997700       ELSE                                                               
997800         MOVE NOVA-LBL-ADRESS-2-5        TO NOVA-LBL-RAD                  
997900         CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL              
998000                             ALT-PCB PRT-AFTER-1 NOVA-LBL-RAD             
998100       END-IF                                                             
998200     END-IF                                                               
998300                                                                          
998400*BETEXT                                                                   
998500     IF WS-GMT-BETEXT-INFO > SPACE                                        
998600       MOVE WS-GMT-BETEXT-INFO     TO NOVA-DATA-BETEXT-NOVA               
998700       MOVE NOVA-DATA-BETEXT        TO NOVA-LBL-RAD                       
998800       CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                
998900                           ALT-PCB PRT-AFTER-1 NOVA-LBL-RAD               
999000     END-IF                                                               
999100                                                                          
999200     MOVE NOVA-LBL-DATA-2-5 TO NOVA-LBL-RAD                               
999300     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                  
999400                         ALT-PCB PRT-AFTER-1 NOVA-LBL-RAD                 
999500*KDFRAKT                                                                  
999600     IF CDC-SE                                                            
999700       MOVE NOVA-LBL-DATA-2-6-SE    TO NOVA-LBL-RAD                       
999800     ELSE                                                                 
999900       MOVE NOVA-LBL-DATA-2-6       TO NOVA-LBL-RAD                       
000000     END-IF                                                               
000100     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                  
000200                         ALT-PCB PRT-AFTER-1 NOVA-LBL-RAD                 
000300                                                                          
000400     IF    WS-KILO < 10                                                   
000500     AND (WS-KILO > ZERO OR WS-HEKTO > ZERO)                              
000600       MOVE NOVA-LBL-DATA-KILO-HEKTO TO NOVA-LBL-RAD                      
000700     ELSE                                                                 
000800       MOVE NOVA-LBL-DATA-WEIGHT TO NOVA-LBL-RAD                          
000900     END-IF                                                               
001000     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                  
001100                         ALT-PCB PRT-AFTER-1 NOVA-LBL-RAD                 
001200                                                                          
001300     IF CDC-SE                                                            
001400*POSTEN                                                                   
001500*      MOVE NOVA-LBL-DATA-POST TO NOVA-LBL-RAD                            
001600       MOVE NOVA-LBL-DATA-4-1 TO NOVA-LBL-RAD                             
001700       CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                
001800                           ALT-PCB PRT-AFTER-1 NOVA-LBL-RAD               
001900                                                                          
002000*DENNA TEST GER FEL/BLANK ADFLGEO PÅ KF VID RAD MED RESTORDER.            
002100*KOMMENTARMÄRKT FÖR ATT EV ANVÄNDAS OM BEHOV UPPSTÅR.                     
002200*      IF GMT-FLLDCKND = JA AND RO-JA                                     
002300*        CONTINUE                                                         
002400*      ELSE                                                               
002500*POSTEN                                                                   
002600         MOVE KOLLI-IDTRPTNR             TO TRP01-IDTRP                   
002700         IF TRP01-TRP-MED-POSTEN                                          
002800           MOVE 4504-ADFLGEO            TO NOVA-LBL-ADFLGEO-SORT          
002900         END-IF                                                           
003000         MOVE NOVA-LBL-ADFLGEO-POSTEN   TO NOVA-LBL-RAD                   
003100*        MOVE NOVA-LBL-ADFLGEO-STD-GRP TO NOVA-LBL-RAD                    
003200         CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL              
003300                         ALT-PCB PRT-AFTER-1 NOVA-LBL-RAD                 
003400*      END-IF                                                             
003500                                                                          
003600       IF LISTA-IDDISTR = ' 878'                                          
003700       AND LISTA-IDKUNDNR = '  1117'                                      
003800       AND NOVA-LBL-ADFLGEO-STD = 'FLY'                                   
003900         CONTINUE                                                         
004000       ELSE                                                               
004100*THIS TEST GIVES BLANK ADFLGEO ON CASE LABEL WITH BACKORDER LINE          
004200*STAR MARKED TO BE USED IF NEEDED.                                        
004300*        IF GMT-FLLDCKND = JA AND RO-JA                                   
004400*          MOVE NOVA-LBL-DATA-RESTORDER TO NOVA-LBL-RAD                   
004500*        ELSE                                                             
004600*POSTEN                                                                   
004700           MOVE NOVA-LBL-ADFLOMR-POSTEN   TO NOVA-LBL-RAD                 
004800*        END-IF                                                           
004900         CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL              
005000                           ALT-PCB PRT-AFTER-1 NOVA-LBL-RAD               
005100       END-IF                                                             
005200                                                                          
005300     END-IF                                                               
005400                                                                          
005500     IF SKRIV-LANG-BARCODE                                                
005600       MOVE NOVA-LBL-BARCODE-LONG TO NOVA-LBL-RAD                         
005700       CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                
005800                           ALT-PCB PRT-AFTER-1 NOVA-LBL-RAD               
005900     ELSE                                                                 
006000*POSTEN                                                                   
006100       MOVE NOVA-LBL-BARCODE-POST    TO NOVA-LBL-RAD                      
006200*      MOVE NOVA-LBL-BARCODE TO NOVA-LBL-RAD                              
006300       CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                
006400                           ALT-PCB PRT-AFTER-1 NOVA-LBL-RAD               
006500     END-IF                                                               
006600                                                                          
006700*POSTEN                                                                   
006800     MOVE NOVA-LBL-TXT-BLW-BARCODE-POST    TO NOVA-LBL-RAD                
006900     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                  
007000                         ALT-PCB PRT-AFTER-1 NOVA-LBL-RAD                 
007100                                                                          
007200*POSTEN                                                                   
007300      MOVE NOVA-LBL-TEXT-SHIPPER-CDC-POST TO NOVA-LBL-RAD                 
007400*     MOVE NOVA-LBL-TEXT-SHIPPER-CDC TO NOVA-LBL-RAD                      
007500      CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                 
007600                          ALT-PCB PRT-AFTER-1 NOVA-LBL-RAD                
007700                                                                          
007800*POSTEN-A5-CL7                                                            
007900     MOVE KOLLI-IDTRPTNR         TO TRP01-IDTRP                           
008000     IF TRP01-TRP-MED-POSTEN                                              
008100       MOVE NOVA-LBL-TEXT-PRODUKT TO NOVA-LBL-RAD                         
008200       CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                
008300                           ALT-PCB PRT-AFTER-1 NOVA-LBL-RAD               
008400       IF WS-KDORDKL = '1'                                                
008500*48 HIT                                                                   
008600         MOVE NOVA-LBL-TEXT-48     TO NOVA-LBL-RAD                        
008700         CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL              
008800                             ALT-PCB PRT-AFTER-1 NOVA-LBL-RAD             
008900         MOVE NOVA-LBL-TEXT-HIT    TO NOVA-LBL-RAD                        
009000         CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL              
009100                             ALT-PCB PRT-AFTER-1 NOVA-LBL-RAD             
009200       ELSE                                                               
009300         IF (WS-KDORDKL = '3' OR '4')                                     
009400*69 PAK                                                                   
009500           MOVE NOVA-LBL-TEXT-69     TO NOVA-LBL-RAD                      
009600           CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL            
009700                               ALT-PCB PRT-AFTER-1 NOVA-LBL-RAD           
009800           MOVE NOVA-LBL-TEXT-PAK    TO NOVA-LBL-RAD                      
009900           CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL            
010000                               ALT-PCB PRT-AFTER-1 NOVA-LBL-RAD           
010100         END-IF                                                           
010200       END-IF                                                             
010300                                                                          
010400       MOVE NOVA-LBL-SORTERINGSKOD TO NOVA-LBL-RAD                        
010500       CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                
010600                           ALT-PCB PRT-AFTER-1 NOVA-LBL-RAD               
010700                                                                          
010800*POSTENS KOLLINR                                                          
010900       MOVE WS-IDKLIID   TO NOVA-LBL-POSTEN1                              
011000                            NOVA-LBL-POSTEN1-TEXT                         
011100                                                                          
011200       MOVE NOVA-LBL-BARCODE-POSTEN TO NOVA-LBL-RAD                       
011300       CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                
011400                           ALT-PCB PRT-AFTER-1 NOVA-LBL-RAD               
011500                                                                          
011600       MOVE NOVA-LBL-TEXT-POSTEN TO NOVA-LBL-RAD                          
011700       CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                
011800                           ALT-PCB PRT-AFTER-1 NOVA-LBL-RAD               
011900     END-IF                                                               
012000                                                                          
012100     MOVE NOVA-LBL-STYR-91 TO NOVA-LBL-RAD                                
012200     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                  
012300                         ALT-PCB PRT-AFTER-1 NOVA-LBL-RAD                 
012400                                                                          
012500     IF WS-IDKOLLI-PRT = WS-IDKOLLI-TOM                                   
012600     OR PRINT-TWO-CASE-LABELS                                             
012700       CALL W006PRS1 USING PRT-SPOOL-OVR PRT-CLOSE LISTVAL                
012800                           ALT-PCB DUMMY-AREA DUMMY-AREA                  
012900     END-IF                                                               
013000     .                                                                    
013100     EJECT                                                                
013200 S08-MARKPOINT-7INC-SVERIGE          SECTION.                             
013300                                                                          
013400     MOVE LISTA-IDDISTR  TO CL7INCH-IDDISTR                               
013500                            CL7INCH-IDDISTR-SE                            
013600                            CL7INCH-DISTR                                 
013700                            CL7INCH-DIST                                  
013800                            CL7INCH-DIST-POST                             
013900                            CL7INCH-DISTR-POST                            
014000                            CL7INCH-DISTR-LONG                            
014100                                                                          
014200     MOVE LISTA-IDKUNDNR TO CL7INCH-IDKUNDNR                              
014300*                           CL7INCH-KUNDNR                                
014400*                           CL7INCH-KUNDN                                 
014500*                           CL7INCH-KUNDN-POST                            
014600*                           CL7INCH-KUNDNR-POST                           
014700*                           CL7INCH-KUNDNR-LONG                           
014800                                                                          
014900     IF (OHUV-IDSYSTEM = 'LDC' OR 'TACD')                                 
015000     AND (DIST13-SVERIGE OR DIST05-NORGE)                                 
015100     AND GMT-FLLDCKND = JA                                                
015200     AND OHUV-IDGROSS > ZERO                                              
015300                                                                          
015400*LISTA-IDKUNDNR IS CHANGED IN F-SECTION                                   
015500       MOVE WS-IDKUNDNR TO CL7INCH-KUNDNR                                 
015600       MOVE WS-IDKUNDNR TO CL7INCH-KUNDN                                  
015700       MOVE WS-IDKUNDNR TO CL7INCH-KUNDN-POST                             
015800       MOVE WS-IDKUNDNR TO CL7INCH-KUNDNR-POST                            
015900       MOVE WS-IDKUNDNR TO CL7INCH-KUNDNR-LONG                            
016000     ELSE                                                                 
016100       MOVE LISTA-IDKUNDNR TO CL7INCH-KUNDNR                              
016200       MOVE LISTA-IDKUNDNR TO CL7INCH-KUNDN                               
016300       MOVE LISTA-IDKUNDNR TO CL7INCH-KUNDN-POST                          
016400       MOVE LISTA-IDKUNDNR TO CL7INCH-KUNDNR-POST                         
016500       MOVE LISTA-IDKUNDNR TO CL7INCH-KUNDNR-LONG                         
016600     END-IF                                                               
016700                                                                          
016800     MOVE LISTA-IDORDNR  TO CL7INCH-IDORDNR                               
016900                            CL7INCH-ORDNR                                 
017000                            CL7INCH-ORDN                                  
017100                            CL7INCH-ORDN-POST                             
017200                            CL7INCH-ORDNR-POST                            
017300                            CL7INCH-ORDNR-LONG                            
017400     MOVE WS-IDPRODNR    TO CL7INCH-IDPRODNR                              
017500     MOVE LISTA-IDKOLLI  TO CL7INCH-IDKOLLI                               
017600                            CL7INCH-KOLLI                                 
017700                            CL7INCH-KOLI                                  
017800                            CL7INCH-KOLI-POST                             
017900                            CL7INCH-KOLLI-POST                            
018000                            CL7INCH-KOLLI-LONG                            
018100     MOVE LISTA-KDFRAKT  TO CL7INCH-KDFRAKT                               
018200     MOVE LISTA-KDFRAKT  TO CL7INCH-KDFRAKT-SE                            
018300                            WS-WRITE-ZONA                                 
018400     MOVE LISTA-IDBILREG TO CL7INCH-IDBILREG                              
018500     MOVE WS-IDZON       TO CL7INCH-IDZON                                 
018600     MOVE WS-IDROUTE     TO CL7INCH-IDROUTE                               
018700     MOVE WS-IDDEPOT     TO CL7INCH-IDDEPOT                               
018800     MOVE WS-IDDISTR     TO TEST-IDDISTR                                  
018900     MOVE WS-IDDISTR     TO DIST29-IDDISTR                                
019000     MOVE WS-IDKUNDNR    TO KUND12-IDKUNDNR                               
019100                                                                          
019200     IF DIST-KUND-LDC                                                     
019300     OR DIST-KUND-SDC-NL                                                  
019400     OR DIST-KUND-LDC-GB-3A                                               
019500     OR DIST-KUND-SDC-IT                                                  
019600     OR GMT-FLLDCKND = JA                                                 
019700                                                                          
019800       PERFORM S05-GET-INFO-FR-WDE420                                     
019900     END-IF                                                               
020000                                                                          
020100     MOVE WS-KDORDKL TO CL7INCH-KDORDKL-S08                               
020200                                                                          
020300     IF LISTA-ADRESS-1 > SPACE                                            
020400       MOVE LISTA-ADRESS-1 TO CL7INCH-ADRESS-1-SWE-S08                    
020500       MOVE LISTA-ADRESS-2 TO CL7INCH-ADRESS-2-SWE-S08                    
020600       MOVE LISTA-ADRESS-3 TO CL7INCH-ADRESS-3-SWE-S08                    
020700       MOVE LISTA-ADRESS-4 TO CL7INCH-ADRESS-4-SWE-S08                    
020800       MOVE WS-OHUV-IDDEPT TO CL7INCH-IDDEPT                              
020900       MOVE WS-LISTA-ADRESS-5 TO CL7INCH-ADRESS-5-SWE-S08                 
021000                                                                          
021100       IF GMT-FLLDCKND = JA                                               
021200*        BERADREF   MOVED TO *BERADREF-S08                                
021300         MOVE LISTA-IDBILREG TO CL7INCH-IDBILREG                          
021400       ELSE                                                               
021500         MOVE WS-LISTA-ADRESS-5 TO CL7INCH-ADRESS-5-SWE-S08               
021600       END-IF                                                             
021700     ELSE                                                                 
021800       IF LISTA-ADRESS-2 > SPACE                                          
021900         MOVE LISTA-ADRESS-2 TO CL7INCH-ADRESS-1-SWE-S08                  
022000         MOVE LISTA-ADRESS-3 TO CL7INCH-ADRESS-3-SWE-S08                  
022100         MOVE LISTA-ADRESS-4 TO CL7INCH-ADRESS-4-SWE-S08                  
022200         MOVE WS-LISTA-ADRESS-5 TO CL7INCH-ADRESS-5-SWE-S08               
022300         MOVE WS-OHUV-IDDEPT TO CL7INCH-IDDEPT                            
022400                                                                          
022500         IF GMT-FLLDCKND = JA                                             
022600*          BERADREF MOVED TO *BERADREF-S08                                
022700           MOVE LISTA-IDBILREG TO CL7INCH-IDBILREG                        
022800         ELSE                                                             
022900           MOVE WS-LISTA-ADRESS-5 TO CL7INCH-ADRESS-5-SWE-S08             
023000         END-IF                                                           
023100       ELSE                                                               
023200         IF LISTA-ADRESS-3 > SPACE                                        
023300           MOVE LISTA-ADRESS-3      TO CL7INCH-ADRESS-1-SWE-S08           
023400           MOVE LISTA-ADRESS-4      TO CL7INCH-ADRESS-4-SWE-S08           
023500           MOVE WS-LISTA-ADRESS-5   TO CL7INCH-ADRESS-5-SWE-S08           
023600           MOVE WS-OHUV-IDDEPT      TO CL7INCH-IDDEPT                     
023700                                                                          
023800           IF GMT-FLLDCKND = JA                                           
023900*            BERADREF   MOVED TO *BERADREF-S08                            
024000             MOVE LISTA-IDBILREG   TO CL7INCH-IDBILREG                    
024100           ELSE                                                           
024200             MOVE ZERO             TO CL7INCH-ADRESS-5-SWE-S08            
024300           END-IF                                                         
024400         ELSE                                                             
024500           IF LISTA-ADRESS-4 > SPACE                                      
024600             MOVE LISTA-ADRESS-4      TO CL7INCH-ADRESS-1-SWE-S08         
024700             MOVE WS-OHUV-IDDEPT      TO CL7INCH-IDDEPT                   
024800             MOVE WS-LISTA-ADRESS-5 TO CL7INCH-ADRESS-5-SWE-S08           
024900                                                                          
025000             IF GMT-FLLDCKND = JA                                         
025100*              BERADREF   MOVED TO *BERADREF-S08                          
025200               MOVE LISTA-IDBILREG    TO CL7INCH-IDBILREG                 
025300             ELSE                                                         
025400               MOVE WS-LISTA-ADRESS-5 TO CL7INCH-ADRESS-5-SWE-S08         
025500             END-IF                                                       
025600           ELSE                                                           
025700             MOVE LISTA-ADRESS-1      TO CL7INCH-ADRESS-1                 
025800             MOVE LISTA-ADRESS-2      TO CL7INCH-ADRESS-2                 
025900             MOVE LISTA-ADRESS-3      TO CL7INCH-ADRESS-3                 
026000             MOVE LISTA-ADRESS-4      TO CL7INCH-ADRESS-4-SWE-S08         
026100             MOVE WS-OHUV-IDDEPT      TO CL7INCH-IDDEPT                   
026200                                                                          
026300             IF GMT-FLLDCKND = JA                                         
026400*              BERADREF   MOVED TO *BERADREF-S08                          
026500               MOVE LISTA-IDBILREG    TO CL7INCH-IDBILREG                 
026600             END-IF                                                       
026700           END-IF                                                         
026800         END-IF                                                           
026900       END-IF                                                             
027000     END-IF                                                               
027100                                                                          
027200                                                                          
027300     IF  WS-KILO < 10                                                     
027400     AND (WS-KILO > ZERO OR WS-HEKTO > ZERO)                              
027500       MOVE WS-KILO      TO CL7INCH-KILO                                  
027600                            CL7INCH-KILO-GB                               
027700       MOVE WS-HEKTO     TO CL7INCH-HEKTO                                 
027800                            CL7INCH-HEKTO-GB                              
027900     ELSE                                                                 
028000       MOVE WS-VKORDBTO  TO CL7INCH-VKORDBTO                              
028100                            CL7INCH-VKORDBTO-GB                           
028200     END-IF                                                               
028300                                                                          
028400     MOVE LISTA-TIRFS    TO CL7INCH-TIRFS                                 
028500     MOVE LISTA-TIRFS    TO CL7INCH-TIRFS-POST                            
028600                                                                          
028700     MOVE LISTA-ADFLGEO  TO CL7INCH-ADFLGEO-SORT                          
028800     MOVE LISTA-ADFLGEO  TO CL7INCH-REFILL-ADFLGEO-SORT                   
028900     MOVE LISTA-ADFLGEO  TO CL7INCH-ADFLGEO                               
029000     MOVE LISTA-ADFLGEO  TO CL7INCH-ADFLGEO-POST                          
029100     MOVE LISTA-ADFLOMR  TO CL7INCH-ADFLOMR                               
029200     MOVE LISTA-ADFLOMR  TO CL7INCH-ADFLOMR-POST                          
029300     MOVE LISTA-ADRUTNIV TO CL7INCH-ADRUTNIV                              
029400     MOVE LISTA-ADRUTNIV TO CL7INCH-ADRUTNIV-POST                         
029500                            CL7INCH-ADRUTNIV-SPA                          
029600     IF DIST83-HIT-FI                                                     
029700       MOVE 4504-ADFLGEO TO CL7INCH-ADFLGEO-SORT-1090                     
029800     END-IF                                                               
029900                                                                          
030000     IF DIST83-HIT-NO                                                     
030100     AND VORD-KDFRAKT NOT = +31                                           
030200       MOVE 4504-ADFLGEO TO CL7INCH-ADFLGEO-SORT-NO                       
030300     END-IF                                                               
030400                                                                          
030500     IF WS-IDKOLLI-PRT = WS-IDKOLLI                                       
030600       CALL W006PRS1 USING PRT-SPOOL-OVR PRT-OPEN LISTVAL                 
030700                           ALT-PCB DUMMY-AREA DUMMY-AREA                  
030800     END-IF                                                               
030900                                                                          
031000     MOVE SPACE       TO CL7INCH-RAD                                      
031100     MOVE CL7INCH-STYR-01 TO CL7INCH-RAD                                  
031200     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                  
031300                         ALT-PCB PRT-NYSIDA-RAD1 CL7INCH-RAD              
031400                                                                          
031500     MOVE CL7INCH-STYR-01 TO CL7INCH-RAD                                  
031600     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                  
031700                         ALT-PCB PRT-AFTER-1 CL7INCH-RAD                  
031800                                                                          
031900     MOVE CL7INCH-RUB-1-1 TO CL7INCH-RAD                                  
032000     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                  
032100                         ALT-PCB PRT-AFTER-1 CL7INCH-RAD                  
032200                                                                          
032300     MOVE CL7INCH-RUB-CUSTOMER TO CL7INCH-RAD                             
032400     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                  
032500                       ALT-PCB PRT-AFTER-1 CL7INCH-RAD                    
032600                                                                          
032700     MOVE CL7INCH-RUB-1-3 TO CL7INCH-RAD                                  
032800     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                  
032900                         ALT-PCB PRT-AFTER-1 CL7INCH-RAD                  
033000                                                                          
033100     MOVE CL7INCH-RUB-2-1 TO CL7INCH-RAD                                  
033200     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                  
033300                         ALT-PCB PRT-AFTER-1 CL7INCH-RAD                  
033400                                                                          
033500     MOVE CL7INCH-RUB-2-5 TO CL7INCH-RAD                                  
033600     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                  
033700                         ALT-PCB PRT-AFTER-1 CL7INCH-RAD                  
033800                                                                          
033900     MOVE CL7INCH-RUB-2-6 TO CL7INCH-RAD                                  
034000     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                  
034100                         ALT-PCB PRT-AFTER-1 CL7INCH-RAD                  
034200                                                                          
034300     IF GMT-FLLDCKND = JA                                                 
034400       MOVE CL7INCH-IDBILREG-LDC         TO CL7INCH-RAD                   
034500       CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                
034600                           ALT-PCB PRT-AFTER-1 CL7INCH-RAD                
034700     END-IF                                                               
034800                                                                          
034900     IF CDC-SE                                                            
035000       MOVE CL7INCH-RUB-3-1-WEIGHT-GB   TO CL7INCH-RAD                    
035100     ELSE                                                                 
035200       MOVE CL7INCH-RUB-3-1-WEIGHT      TO CL7INCH-RAD                    
035300     END-IF                                                               
035400     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                  
035500                           ALT-PCB PRT-AFTER-1 CL7INCH-RAD                
035600                                                                          
035700*POSTEN                                                                   
035800     MOVE CL7INCH-RUB-RFS-POST TO CL7INCH-RAD                             
035900     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                  
036000                         ALT-PCB PRT-AFTER-1 CL7INCH-RAD                  
036100                                                                          
036200     IF (GMT-FLLDCKND = JA AND KDFRAKT-FINNS)                             
036300     OR KUND12-LDC-AKUT                                                   
036400       IF WS-OHUV-IDDEPT > ZERO                                           
036500         MOVE CL7INCH-RUB-IDDEPT TO CL7INCH-RAD                           
036600         CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL              
036700                             ALT-PCB PRT-AFTER-1 CL7INCH-RAD              
036800                                                                          
036900         MOVE CL7INCH-DATA-IDDEPT TO CL7INCH-RAD                          
037000         CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL              
037100                             ALT-PCB PRT-AFTER-1 CL7INCH-RAD              
037200       END-IF                                                             
037300                                                                          
037400     END-IF                                                               
037500                                                                          
037600     MOVE CL7INCH-RUB-KDORDKL-S08      TO CL7INCH-RAD                     
037700     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                  
037800                           ALT-PCB PRT-AFTER-1 CL7INCH-RAD                
037900                                                                          
038000     MOVE CL7INCH-DATA-KDORDKL-S08 TO CL7INCH-RAD                         
038100     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                  
038200                           ALT-PCB PRT-AFTER-1 CL7INCH-RAD                
038300                                                                          
038400     IF CDC-SE                                                            
038500       MOVE CL7INCH-DATA-1-1-SE  TO CL7INCH-RAD                           
038600     ELSE                                                                 
038700       MOVE CL7INCH-DATA-1-1     TO CL7INCH-RAD                           
038800     END-IF                                                               
038900     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                  
039000                         ALT-PCB PRT-AFTER-1 CL7INCH-RAD                  
039100                                                                          
039200     MOVE CL7INCH-DATA-1-2 TO CL7INCH-RAD                                 
039300     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                  
039400                         ALT-PCB PRT-AFTER-1 CL7INCH-RAD                  
039500                                                                          
039600     MOVE CL7INCH-IDORDNR-GRP  TO CL7INCH-RAD                             
039700     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                  
039800                         ALT-PCB PRT-AFTER-1 CL7INCH-RAD                  
039900                                                                          
040000     IF CL7INCH-ADRESS-1-SWE-S08 > SPACE                                  
040100*    AND WS-IDKUNDNR NOT = '000715'                                       
040200       MOVE CL7INCH-ADRESS-1-GRP-S08      TO CL7INCH-RAD                  
040300       CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                
040400                           ALT-PCB PRT-AFTER-1 CL7INCH-RAD                
040500                                                                          
040600       MOVE CL7INCH-ADRESS-2-GRP-S08      TO CL7INCH-RAD                  
040700       CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                
040800                           ALT-PCB PRT-AFTER-1 CL7INCH-RAD                
040900                                                                          
041000       MOVE CL7INCH-ADRESS-3-GRP-S08      TO CL7INCH-RAD                  
041100       CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                
041200                           ALT-PCB PRT-AFTER-1 CL7INCH-RAD                
041300                                                                          
041400       MOVE CL7INCH-ADRESS-4-GRP-S08      TO CL7INCH-RAD                  
041500       CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                
041600                           ALT-PCB PRT-AFTER-1 CL7INCH-RAD                
041700                                                                          
041800*BETEXT                                                                   
041900       IF WS-GMT-BETEXT-INFO > SPACE                                      
042000         MOVE WS-GMT-BETEXT-INFO  TO CL7INCH-DATA-BETEXT-S082             
042100         MOVE CL7INCH-INFO-BETEXT-S082 TO CL7INCH-RAD                     
042200         CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL              
042300                             ALT-PCB PRT-AFTER-1 CL7INCH-RAD              
042400       END-IF                                                             
042500     ELSE                                                                 
042600       MOVE CL7INCH-DATA-2-1 TO CL7INCH-RAD                               
042700       CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                
042800                           ALT-PCB PRT-AFTER-1 CL7INCH-RAD                
042900                                                                          
043000       MOVE CL7INCH-DATA-2-2 TO CL7INCH-RAD                               
043100       CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                
043200                           ALT-PCB PRT-AFTER-1 CL7INCH-RAD                
043300                                                                          
043400       MOVE CL7INCH-DATA-2-3 TO CL7INCH-RAD                               
043500       CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                
043600                           ALT-PCB PRT-AFTER-1 CL7INCH-RAD                
043700                                                                          
043800       MOVE CL7INCH-ADRESS-4-GRP-S08      TO CL7INCH-RAD                  
043900       MOVE CL7INCH-DATA-2-4 TO CL7INCH-RAD                               
044000       CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                
044100                           ALT-PCB PRT-AFTER-1 CL7INCH-RAD                
044200                                                                          
044300*BETEXT                                                                   
044400       IF WS-GMT-BETEXT-INFO > SPACE                                      
044500         MOVE WS-GMT-BETEXT-INFO  TO CL7INCH-DATA-BETEXT-INF-S08          
044600         MOVE CL7INCH-DATA-BETEXT-S08 TO CL7INCH-RAD                      
044700         CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL              
044800                             ALT-PCB PRT-AFTER-1 CL7INCH-RAD              
044900       END-IF                                                             
045000     END-IF                                                               
045100                                                                          
045200*BERADREF-S08                                                             
045300     IF GMT-FLLDCKND = JA                                                 
045400       MOVE WS-ORAD-BERADREF TO CL7INCH-BERADREF-S08                      
045500       MOVE CL7INCH-BERADREF-GRP-S08   TO CL7INCH-RAD                     
045600       CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                
045700                           ALT-PCB PRT-AFTER-1 CL7INCH-RAD                
045800                                                                          
045900       MOVE CL7INCH-BERADREF-RUB-S08   TO CL7INCH-RAD                     
046000       CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                
046100                           ALT-PCB PRT-AFTER-1 CL7INCH-RAD                
046200     END-IF                                                               
046300                                                                          
046400     MOVE CL7INCH-DATA-IDKOLLI TO CL7INCH-RAD                             
046500     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                  
046600                         ALT-PCB PRT-AFTER-1 CL7INCH-RAD                  
046700                                                                          
046800*KDFRAKT                                                                  
046900     IF CDC-SE                                                            
047000       MOVE CL7INCH-DATA-2-6-SE     TO CL7INCH-RAD                        
047100     ELSE                                                                 
047200       MOVE CL7INCH-DATA-2-6        TO CL7INCH-RAD                        
047300     END-IF                                                               
047400     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                  
047500                         ALT-PCB PRT-AFTER-1 CL7INCH-RAD                  
047600                                                                          
047700     IF WS-KILO < 10                                                      
047800     AND (WS-KILO > ZERO OR WS-HEKTO > ZERO)                              
047900       MOVE CL7INCH-DATA-KILO-HEKTO TO CL7INCH-RAD                        
048000     ELSE                                                                 
048100       MOVE CL7INCH-DATA-WEIGHT TO CL7INCH-RAD                            
048200     END-IF                                                               
048300     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                  
048400                         ALT-PCB PRT-AFTER-1 CL7INCH-RAD                  
048500                                                                          
048600     IF CDC-SE                                                            
048700*POSTEN                                                                   
048800*      MOVE CL7INCH-DATA-POST TO CL7INCH-RAD                              
048900       MOVE CL7INCH-DATA-TIRFS TO CL7INCH-RAD                             
049000                                                                          
049100       CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                
049200                           ALT-PCB PRT-AFTER-1 CL7INCH-RAD                
049300                                                                          
049400*THIS TEST GIVES BLANK ADFLGEO ON CASE LABEL WITH BACKORDER LINE          
049500*STAR MARKED TO BE USED IF NEEDED.                                        
049600       IF GMT-FLLDCKND = JA AND RO-JA                                     
049700         CONTINUE                                                         
049800       ELSE                                                               
049900*POSTEN                                                                   
050000         MOVE KOLLI-IDTRPTNR             TO TRP01-IDTRP                   
050100         IF TRP01-TRP-MED-POSTEN                                          
050200           MOVE 4504-ADFLGEO            TO CL7INCH-ADFLGEO-SORT           
050300         END-IF                                                           
050400         MOVE CL7INCH-ADFLGEO-POSTEN    TO CL7INCH-RAD                    
050500*        MOVE CL7INCH-DATA-4-2 TO CL7INCH-RAD                             
050600         CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL              
050700                         ALT-PCB PRT-AFTER-1 CL7INCH-RAD                  
050800       END-IF                                                             
050900                                                                          
051000       IF LISTA-IDDISTR = ' 878'                                          
051100       AND LISTA-IDKUNDNR = '  1117'                                      
051200       AND CL7INCH-ADFLGEO = 'FLY'                                        
051300         CONTINUE                                                         
051400       ELSE                                                               
051500                                                                          
051600*THIS TEST GIVES BLANK ADFLGEO ON CASE LABEL WITH BACKORDER LINE          
051700*STAR MARKED TO BE USED IF NEEDED.                                        
051800         IF GMT-FLLDCKND = JA AND RO-JA                                   
051900           MOVE CL7INCH-DATA-RESTORDER TO CL7INCH-RAD                     
052000         ELSE                                                             
052100*POSTEN                                                                   
052200           MOVE CL7INCH-ADFLOMR-POSTEN    TO CL7INCH-RAD                  
052300*          MOVE CL7INCH-DATA-4-3 TO CL7INCH-RAD                           
052400         END-IF                                                           
052500         CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL              
052600                           ALT-PCB PRT-AFTER-1 CL7INCH-RAD                
052700       END-IF                                                             
052800                                                                          
052900       IF LISTA-IDDISTR = ' 878'                                          
053000       AND LISTA-IDKUNDNR = '  1117'                                      
053100       AND (CL7INCH-ADFLGEO = 'RUT'                                       
053200       OR CL7INCH-ADFLGEO = 'FLY')                                        
053300         CONTINUE                                                         
053400       ELSE                                                               
053500                                                                          
053600*THIS TEST GIVES BLANK ADFLGEO ON CASE LABEL WITH BACKORDER LINE          
053700*STAR MARKED TO BE USED IF NEEDED.                                        
053800*        IF GMT-FLLDCKND = JA AND RO-JA                                   
053900           CONTINUE                                                       
054000*        ELSE                                                             
054100*POSTEN                                                                   
054200*          MOVE CL7INCH-ADRUTNIV-POSTEN    TO  CL7INCH-RAD                
054300*          MOVE CL7INCH-DATA-4-4 TO CL7INCH-RAD                           
054400*          CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL            
054500*                          ALT-PCB PRT-AFTER-1 CL7INCH-RAD                
054600*        END-IF                                                           
054700       END-IF                                                             
054800     END-IF                                                               
054900                                                                          
055000     IF SKRIV-LANG-BARCODE                                                
055100       MOVE CL7INCH-BARCODE-LONG TO CL7INCH-RAD                           
055200       CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                
055300                           ALT-PCB PRT-AFTER-1 CL7INCH-RAD                
055400     ELSE                                                                 
055500*POSTEN                                                                   
055600*      MOVE CL7INCH-BARCODE-POST     TO CL7INCH-RAD                       
055700       MOVE CL7INCH-BARCODE          TO CL7INCH-RAD                       
055800       CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                
055900                           ALT-PCB PRT-AFTER-1 CL7INCH-RAD                
056000     END-IF                                                               
056100                                                                          
056200*POSTEN                                                                   
056300*    MOVE CL7INCH-TXT-BLW-BARCODE-POST     TO CL7INCH-RAD                 
056400     MOVE CL7INCH-TEXT-BELOW-BARCODE TO CL7INCH-RAD                       
056500     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                  
056600                         ALT-PCB PRT-AFTER-1 CL7INCH-RAD                  
056700                                                                          
056800*POSTEN                                                                   
056900*     MOVE CL7INCH-TEXT-SHIPPER-CDC-POST TO CL7INCH-RAD                   
057000      MOVE CL7INCH-TEXT-SHIPPER-CDC TO CL7INCH-RAD                        
057100      CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                 
057200                          ALT-PCB PRT-AFTER-1 CL7INCH-RAD                 
057300                                                                          
057400*POSTEN-A5-CL7                                                            
057500     MOVE KOLLI-IDTRPTNR         TO TRP01-IDTRP                           
057600     IF TRP01-TRP-MED-POSTEN                                              
057700       MOVE CL7INCH-TEXT-PRODUKT TO CL7INCH-RAD                           
057800       CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                
057900                           ALT-PCB PRT-AFTER-1 CL7INCH-RAD                
058000       IF WS-KDORDKL = '1'                                                
058100*48 HIT                                                                   
058200         MOVE CL7INCH-TEXT-48      TO CL7INCH-RAD                         
058300         CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL              
058400                             ALT-PCB PRT-AFTER-1 CL7INCH-RAD              
058500         MOVE CL7INCH-TEXT-HIT     TO CL7INCH-RAD                         
058600         CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL              
058700                             ALT-PCB PRT-AFTER-1 CL7INCH-RAD              
058800       ELSE                                                               
058900         IF (WS-KDORDKL = '3' OR '4')                                     
059000*69 PAK                                                                   
059100           MOVE CL7INCH-TEXT-69      TO CL7INCH-RAD                       
059200           CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL            
059300                               ALT-PCB PRT-AFTER-1 CL7INCH-RAD            
059400           MOVE CL7INCH-TEXT-PAK     TO CL7INCH-RAD                       
059500           CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL            
059600                               ALT-PCB PRT-AFTER-1 CL7INCH-RAD            
059700         END-IF                                                           
059800       END-IF                                                             
059900                                                                          
060000       MOVE CL7INCH-SORTERINGSKOD TO CL7INCH-RAD                          
060100       CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                
060200                           ALT-PCB PRT-AFTER-1 CL7INCH-RAD                
060300                                                                          
060400*POSTENS KOLLINR                                                          
060500       MOVE WS-IDKLIID   TO CL7INCH-POSTEN1                               
060600                            CL7INCH-POSTEN1-TEXT                          
060700                                                                          
060800       MOVE CL7INCH-BARCODE-POSTEN TO CL7INCH-RAD                         
060900       CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                
061000                           ALT-PCB PRT-AFTER-1 CL7INCH-RAD                
061100                                                                          
061200       MOVE CL7INCH-TEXT-POSTEN  TO CL7INCH-RAD                           
061300       CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                
061400                           ALT-PCB PRT-AFTER-1 CL7INCH-RAD                
061500     END-IF                                                               
061600                                                                          
061700     MOVE CL7INCH-STYR-91 TO CL7INCH-RAD                                  
061800     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                  
061900                         ALT-PCB PRT-AFTER-1 CL7INCH-RAD                  
062000                                                                          
062100     IF WS-IDKOLLI-PRT = WS-IDKOLLI-TOM                                   
062200       CALL W006PRS1 USING PRT-SPOOL-OVR PRT-CLOSE LISTVAL                
062300                           ALT-PCB DUMMY-AREA DUMMY-AREA                  
062400     END-IF                                                               
062500     .                                                                    
062600     EJECT                                                                
062700 S03-MARKP-A6-NL-CDC-SDC        SECTION.                                  
062800                                                                          
062900     MOVE 0000000          TO A6-ORDNR-TXT-S                              
063000                                                                          
063100     MOVE LISTA-IDDISTR    TO A6-IDDISTR-NL                               
063200     MOVE LISTA-IDDISTR    TO A6-IDDISTR-SE                               
063300                              TEST-IDDISTR                                
063400                              DIST05-IDDISTR                              
063500                              DIST29-IDDISTR                              
063600                              A6-DISTR-L                                  
063700                              A6-DISTR-S                                  
063800                              A6-DISTR-TXT-L                              
063900                              A6-DISTR-TXT-S                              
064000                              A6-DISTR-S-ENG                              
064100                              A6-DISTR-TXT-S-ENG                          
064200     MOVE LISTA-IDKUNDNR   TO A6-IDKUNDNR-NL                              
064300                              A6-KUNDNR-L                                 
064400                              A6-KUNDNR-TXT-L                             
064500                              A6-KUNDNR-S-ENG                             
064600                              A6-KUNDNR-TXT-S-ENG                         
064700                              A6-KUNDNR-S                                 
064800                              A6-KUNDNR-TXT-S                             
064900     MOVE LISTA-IDORDNR    TO A6-IDORDNR-NL                               
065000                              A6-ORDNR-L                                  
065100                              A6-ORDNR-S                                  
065200                              A6-ORDNR-TXT-L                              
065300                              A6-ORDNR-TXT-S                              
065400                              A6-ORDNR-S-ENG                              
065500                              A6-ORDNR-TXT-S-ENG                          
065600     MOVE LISTA-IDKOLLI    TO A6-IDKOLLI-NL                               
065700                              A6-KOLLI-L                                  
065800                              A6-KOLLI-S                                  
065900                              A6-KOLLI-TXT-L                              
066000                              A6-KOLLI-TXT-S                              
066100                              A6-KOLLI-S-ENG                              
066200                              A6-KOLLI-TXT-S-ENG                          
066300     MOVE LISTA-KDFRAKT    TO A6-KDFRAKT-CDC                              
066400     MOVE LISTA-KDFRAKT    TO A6-KDFRAKT-NL                               
066500                              WS-WRITE-ZONA                               
066600     MOVE LISTA-TIRFS      TO A6-RFS                                      
066700                              A6-RFS-SE                                   
066800     MOVE LISTA-ADFLGEO    TO A6-8700-ADFLGEO-SORT                        
066900     MOVE LISTA-IDBILREG   TO A6-IDBILREG-S03                             
067000                                                                          
067100     IF (OHUV-IDSYSTEM = 'LDC' OR 'TACD')                                 
067200     AND (DIST13-SVERIGE OR DIST05-NORGE)                                 
067300     AND GMT-FLLDCKND = JA                                                
067400     AND OHUV-IDGROSS > ZERO                                              
067500                                                                          
067600*LISTA-IDKUNDNR IS CHANGED IN F-SECTION                                   
067700       MOVE WS-IDKUNDNR    TO A6-KUNDNR-L                                 
067800                              A6-KUNDNR-TXT-L                             
067900     END-IF                                                               
068000                                                                          
068100     IF CDC-SE                                                            
068200       IF GMT-FLLDCKND = JA                                               
068300         MOVE WS-OHUV-IDDEPT TO A6-IDDEPT-S03                             
068400       END-IF                                                             
068500     END-IF                                                               
068600         MOVE WS-KDORDKL TO A6-KDORDKL                                    
068700                                                                          
068800     MOVE LISTA-ADFLGEO    TO A6-FLGEO                                    
068900     MOVE LISTA-ADFLGEO    TO A6-ADFLGEO-SE                               
069000     MOVE LISTA-ADFLGEO    TO A6-ADFLGEO-SE2                              
069100                                                                          
069200     MOVE LISTA-ADFLOMR    TO A6-ADFLOMR-SE                               
069300     MOVE LISTA-ADRUTNIV   TO A6-ADRUTNIV-SE                              
069400                                                                          
069500     IF DIST83-HIT-FI                                                     
069600        MOVE 4504-ADFLGEO  TO A6-ADFLGEO-FI                               
069700        MOVE SPACE         TO A6-ADFLGEO-SE                               
069800        MOVE SPACE         TO A6-ADFLGEO-NO                               
069900     END-IF                                                               
070000                                                                          
070100     IF DIST83-HIT-NO                                                     
070200     AND VORD-KDFRAKT NOT = +31                                           
070300        MOVE 4504-ADFLGEO  TO A6-ADFLGEO-NO                               
070400        MOVE SPACE         TO A6-ADFLGEO-SE                               
070500        MOVE SPACE         TO A6-ADFLGEO-FI                               
070600     END-IF                                                               
070700                                                                          
070800     MOVE LISTA-ADFLOMR    TO A6-FLOMR                                    
070900     MOVE LISTA-ADRUTNIV   TO A6-RUTNIV                                   
071000                                                                          
071100     IF DIST-KUND-LDC                                                     
071200     OR DIST-KUND-SDC-NL                                                  
071300     OR DIST-KUND-SDC-IT                                                  
071400     OR (DIST-KUND-LDC-GB-3A AND (WS-KDORDKL = 0 OR 1))                   
071500       PERFORM S05-GET-INFO-FR-WDE420                                     
071600     END-IF                                                               
071700*S03                                                                      
071800     MOVE WS-IDDISTR     TO TEST-IDDISTR                                  
071900     IF  CDC-SE                                                           
072000       AND DIST03-SVERIGE                                                 
072100       AND NOT DIST-KUND-LDC-SE                                           
072200       IF LISTA-ADRESS-1 > SPACE                                          
072300         MOVE LISTA-ADRESS-1 TO A6-ADRESS-1-SWE                           
072400         MOVE LISTA-ADRESS-2 TO A6-ADRESS-2-SWE                           
072500         MOVE LISTA-ADRESS-3 TO A6-ADRESS-3-SWE                           
072600         MOVE LISTA-ADRESS-4 TO A6-ADRESS-4-SWE                           
072700         MOVE WS-LISTA-ADRESS-5 TO A6-ADRESS-5-SWE                        
072800       ELSE                                                               
072900         IF LISTA-ADRESS-2 > SPACE                                        
073000           MOVE LISTA-ADRESS-2 TO A6-ADRESS-1-SWE                         
073100           MOVE LISTA-ADRESS-3 TO A6-ADRESS-3-SWE                         
073200           MOVE LISTA-ADRESS-4 TO A6-ADRESS-4-SWE                         
073300           MOVE WS-LISTA-ADRESS-5 TO A6-ADRESS-5-SWE                      
073400         ELSE                                                             
073500           IF LISTA-ADRESS-3 > SPACE                                      
073600             MOVE LISTA-ADRESS-3 TO A6-ADRESS-1-SWE                       
073700             MOVE LISTA-ADRESS-4 TO A6-ADRESS-4-SWE                       
073800             MOVE WS-LISTA-ADRESS-5 TO A6-ADRESS-5-SWE                    
073900           ELSE                                                           
074000             IF LISTA-ADRESS-4 > SPACE                                    
074100               MOVE LISTA-ADRESS-4 TO A6-ADRESS-1-SWE                     
074200               MOVE WS-LISTA-ADRESS-5 TO A6-ADRESS-5-SWE                  
074300             ELSE                                                         
074400               MOVE LISTA-ADRESS-1 TO A6-ADRESS-1-SWE                     
074500               MOVE LISTA-ADRESS-2 TO A6-ADRESS-2-SWE                     
074600               MOVE LISTA-ADRESS-3 TO A6-ADRESS-3-SWE                     
074700               MOVE LISTA-ADRESS-4 TO A6-ADRESS-4-SWE                     
074800               MOVE WS-LISTA-ADRESS-5 TO A6-ADRESS-5-SWE                  
074900             END-IF                                                       
075000           END-IF                                                         
075100         END-IF                                                           
075200       END-IF                                                             
075300     ELSE                                                                 
075400       MOVE LISTA-ADRESS-1 TO A6-ADRESS-1-NL                              
075500       MOVE LISTA-ADRESS-2 TO A6-ADRESS-2-NL                              
075600       MOVE LISTA-ADRESS-3 TO A6-ADRESS-3-NL                              
075700       MOVE LISTA-ADRESS-4 TO A6-ADRESS-4-NL                              
075800                                                                          
075900*BERADREF                                                                 
076000*BEKUNDRF                                                                 
076100       MOVE WS-IDDISTR       TO DIST83-IDDISTR                            
076200       IF (CDC-SE AND DIST30-FRANCE)                                      
076300         PERFORM S05-GET-INFO-FR-WDE420                                   
076400         MOVE WS-ORAD-BERADREF    TO A6-BERADREF                          
076500         MOVE WS-OHUV-BEKUNDRF    TO A6-BEKUNDRF                          
076600       END-IF                                                             
076700                                                                          
076800       IF DIST-KUND-LDC                                                   
076900       OR DIST-KUND-SDC-NL                                                
077000       OR DIST-KUND-SDC-IT                                                
077100       OR (DIST-KUND-LDC-GB-3A AND (WS-KDORDKL = 0 OR 1))                 
077200       OR (DIST05-NORGE AND (WS-OHUV-IDSYSTEM = 'LDC' OR 'TACD'))         
077300         MOVE WS-ORAD-BERADREF TO A6-ADRESS-5-NL                          
077400                                  A6-ADRESS-5-N                           
077500       ELSE                                                               
077600         MOVE WS-LISTA-ADRESS-5 TO A6-ADRESS-5-NL                         
077700                                   A6-ADRESS-5-N                          
077800       END-IF                                                             
077900     END-IF                                                               
078000                                                                          
078100     IF WS-KILO < 10                                                      
078200     AND (WS-KILO > ZERO OR WS-HEKTO > ZERO)                              
078300       MOVE WS-KILO      TO A6-KILO-NL                                    
078400       MOVE WS-HEKTO     TO A6-HEKTO-NL                                   
078500     ELSE                                                                 
078600       MOVE WS-VKORDBTO TO A6-VKORDBTO-NL                                 
078700     END-IF                                                               
078800                                                                          
078900     IF WS-IDKOLLI-PRT = WS-IDKOLLI                                       
079000       CALL W006PRS1 USING PRT-SPOOL-OVR PRT-OPEN LISTVAL                 
079100                           ALT-PCB DUMMY-AREA DUMMY-AREA                  
079200     END-IF                                                               
079300                                                                          
079400     MOVE SPACE                     TO A6-RAD                             
079500                                                                          
079600     MOVE A6-STYR-01                TO A6-RAD                             
079700     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                  
079800                         ALT-PCB PRT-NYSIDA-RAD1 A6-RAD                   
079900                                                                          
080000     MOVE A6-STYR-01                TO A6-RAD                             
080100     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                  
080200                         ALT-PCB PRT-AFTER-1 A6-RAD                       
080300                                                                          
080400     MOVE A6-STYR-42      TO A6-RAD                                       
080500     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                  
080600                         ALT-PCB PRT-AFTER-1 A6-RAD                       
080700                                                                          
080800     MOVE A6-RUB-DISTRICT-NL        TO A6-RAD                             
080900     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                  
081000                         ALT-PCB PRT-AFTER-1 A6-RAD                       
081100                                                                          
081200     MOVE A6-RUB-CUSTOMER-NL   TO A6-RAD                                  
081300     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                  
081400                         ALT-PCB PRT-AFTER-1 A6-RAD                       
081500                                                                          
081600     MOVE A6-RUB-ORDERNUMBER-NL     TO A6-RAD                             
081700     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                  
081800                         ALT-PCB PRT-AFTER-1 A6-RAD                       
081900                                                                          
082000     MOVE A6-RUB-FREIGHTCODE-NL     TO A6-RAD                             
082100     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                  
082200                         ALT-PCB PRT-AFTER-1 A6-RAD                       
082300                                                                          
082400     IF CDC-SE                                                            
082500       MOVE A6-RUB-RFS-GBG          TO A6-RAD                             
082600     END-IF                                                               
082700     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                  
082800                         ALT-PCB PRT-AFTER-1 A6-RAD                       
082900                                                                          
083000     MOVE A6-RUB-ADDRESS-NL         TO A6-RAD                             
083100     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                  
083200                         ALT-PCB PRT-AFTER-1 A6-RAD                       
083300                                                                          
083400     MOVE A6-RUB-CASE-NL            TO A6-RAD                             
083500     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                  
083600                         ALT-PCB PRT-AFTER-1 A6-RAD                       
083700                                                                          
083800     IF CDC-SE                                                            
083900       MOVE A6-DATA-IDDISTR-SE      TO A6-RAD                             
084000     ELSE                                                                 
084100       MOVE A6-DATA-IDDISTR-NL      TO A6-RAD                             
084200     END-IF                                                               
084300     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                  
084400                         ALT-PCB PRT-AFTER-1 A6-RAD                       
084500                                                                          
084600     MOVE A6-DATA-IDKUNDNR-NL       TO A6-RAD                             
084700     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                  
084800                         ALT-PCB PRT-AFTER-1 A6-RAD                       
084900                                                                          
085000     MOVE A6-DATA-IDORDNR-NL        TO A6-RAD                             
085100     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                  
085200                         ALT-PCB PRT-AFTER-1 A6-RAD                       
085300                                                                          
085400     MOVE A6-DATA-IDKOLLI-NL        TO A6-RAD                             
085500     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                  
085600                         ALT-PCB PRT-AFTER-1 A6-RAD                       
085700                                                                          
085800     IF A6-ADRESS-1-SWE     > SPACE                                       
085900       MOVE A6-DATA-ADRESS1-SWE     TO A6-RAD                             
086000       CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                
086100                           ALT-PCB PRT-AFTER-1 A6-RAD                     
086200                                                                          
086300       MOVE A6-DATA-ADRESS2-SWE     TO A6-RAD                             
086400       CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                
086500                           ALT-PCB PRT-AFTER-1 A6-RAD                     
086600                                                                          
086700       MOVE A6-DATA-ADRESS3-SWE     TO A6-RAD                             
086800       CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                
086900                           ALT-PCB PRT-AFTER-1 A6-RAD                     
087000                                                                          
087100       MOVE A6-DATA-ADRESS4-SWE     TO A6-RAD                             
087200       CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                
087300                           ALT-PCB PRT-AFTER-1 A6-RAD                     
087400                                                                          
087500       MOVE A6-DATA-ADRESS5-SWE     TO A6-RAD                             
087600       CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                
087700                           ALT-PCB PRT-AFTER-1 A6-RAD                     
087800     ELSE                                                                 
087900       MOVE A6-DATA-ADRESS1-NL        TO A6-RAD                           
088000       CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                
088100                           ALT-PCB PRT-AFTER-1 A6-RAD                     
088200                                                                          
088300       MOVE A6-DATA-ADRESS2-NL        TO A6-RAD                           
088400       CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                
088500                           ALT-PCB PRT-AFTER-1 A6-RAD                     
088600                                                                          
088700       MOVE A6-DATA-ADRESS3-NL        TO A6-RAD                           
088800       CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                
088900                           ALT-PCB PRT-AFTER-1 A6-RAD                     
089000                                                                          
089100       MOVE A6-DATA-ADRESS4-NL        TO A6-RAD                           
089200       CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                
089300                           ALT-PCB PRT-AFTER-1 A6-RAD                     
089400                                                                          
089500       MOVE OHUV-IDDISTR  TO DIST03-IDDISTR                               
089600                                                                          
089700       IF DIST03-DANMARK-900                                              
089800                                                                          
089900         MOVE A6-RUB-REPDAT-S03    TO A6-RAD                              
090000         CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL              
090100                             ALT-PCB PRT-AFTER-1 A6-RAD                   
090200                                                                          
090300         MOVE WS-OHUV-TIREPDAT          TO A6-TIREPDAT                    
090400         INSPECT A6-TIREPDAT REPLACING LEADING ZERO BY SPACE              
090500         MOVE A6-DATA-TIREPDAT-S03      TO  A6-RAD                        
090600         CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL              
090700                             ALT-PCB PRT-AFTER-1 A6-RAD                   
090800                                                                          
090900         MOVE A6-RUB-CAR-REG-S03  TO A6-RAD                               
091000         CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL              
091100                             ALT-PCB PRT-AFTER-1 A6-RAD                   
091200                                                                          
091300         MOVE A6-DATA-IDBILREG-S03      TO A6-RAD                         
091400         CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL              
091500                             ALT-PCB PRT-AFTER-1 A6-RAD                   
091600       END-IF                                                             
091700                                                                          
091800*BERADREF                                                                 
091900*BEKUNDRF                                                                 
092000       MOVE WS-IDDISTR       TO DIST83-IDDISTR                            
092100       IF (CDC-SE AND DIST30-FRANCE)                                      
092200                                                                          
092300         MOVE A6-DATA-BERADREF          TO A6-RAD                         
092400         CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL              
092500                             ALT-PCB PRT-AFTER-1 A6-RAD                   
092600                                                                          
092700         MOVE A6-DATA-BEKUNDRF          TO A6-RAD                         
092800         CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL              
092900                             ALT-PCB PRT-AFTER-1 A6-RAD                   
093000                                                                          
093100       END-IF                                                             
093200                                                                          
093300       IF GMT-FLLDCKND = JA                                               
093400       OR (CDC-SE AND DIST-KUND-LDC)                                      
093500*HIT-FI START                                                             
093600*POSTEN 1090                                                              
093700         MOVE WS-IDDISTR           TO TEST-IDDISTR                        
093800         IF DIST83-HIT-FI OR DIST83-HIT-NO                                
093900*SMALL     FONT                                                           
094000             MOVE A6-DATA-ADRESS5-N  TO A6-RAD                            
094100         ELSE                                                             
094200*LARGE     FONT                                                           
094300             MOVE A6-DATA-ADRESS5-NL TO A6-RAD                            
094400         END-IF                                                           
094500         CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL              
094600                             ALT-PCB PRT-AFTER-1 A6-RAD                   
094700       ELSE                                                               
094800         MOVE A6-DATA-ADRESS5-N       TO A6-RAD                           
094900         CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL              
095000                             ALT-PCB PRT-AFTER-1 A6-RAD                   
095100       END-IF                                                             
095200     END-IF                                                               
095300                                                                          
095400*BETEXT                                                                   
095500     IF WS-GMT-BETEXT-INFO > SPACE                                        
095600       MOVE WS-GMT-BETEXT-INFO      TO A6-BETEXT-INFO-S03                 
095700       MOVE A6-DATA-BETEXT-S03      TO A6-RAD                             
095800       CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                
095900                           ALT-PCB PRT-AFTER-1 A6-RAD                     
096000     END-IF                                                               
096100                                                                          
096200     IF CDC-SE                                                            
096300       MOVE A6-DATA-KDFRAKT-CDC     TO A6-RAD                             
096400     ELSE                                                                 
096500       MOVE A6-DATA-KDFRAKT-NL      TO A6-RAD                             
096600     END-IF                                                               
096700     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                  
096800                         ALT-PCB PRT-AFTER-1 A6-RAD                       
096900                                                                          
097000     IF CDC-SE                                                            
097100         MOVE A6-RUB-KDORDKL        TO A6-RAD                             
097200         CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL              
097300                             ALT-PCB PRT-AFTER-1 A6-RAD                   
097400                                                                          
097500         MOVE A6-DATA-KDORDKL       TO A6-RAD                             
097600         CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL              
097700                             ALT-PCB PRT-AFTER-1 A6-RAD                   
097800       IF GMT-FLLDCKND = JA                                               
097900                                                                          
098000         IF WS-OHUV-IDDEPT > ZERO                                         
098100           MOVE A6-RUB-IDDEPT-S03   TO A6-RAD                             
098200           CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL            
098300                             ALT-PCB PRT-AFTER-1 A6-RAD                   
098400                                                                          
098500           MOVE A6-DATA-IDDEPT-S03  TO A6-RAD                             
098600           CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL            
098700                             ALT-PCB PRT-AFTER-1 A6-RAD                   
098800         END-IF                                                           
098900                                                                          
099000*THERE IS NO MOVE TO A6-RESTORDER.                                        
099100*STAR MARKED TO BE USED IF NEEDED.                                        
099200*        IF RO-JA                                                         
099300*          MOVE A6-DATA-RESTORDER   TO A6-RAD                             
099400*          CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL            
099500*                             ALT-PCB PRT-AFTER-1 A6-RAD                  
099600*        END-IF                                                           
099700       END-IF                                                             
099800     END-IF                                                               
099900*HIT-FI START                                                             
100000*POSTEN 1090                                                              
100100     MOVE WS-IDDISTR               TO TEST-IDDISTR                        
100200     IF DIST83-HIT-FI OR DIST83-HIT-NO                                    
100300       MOVE A6-TEXT-PRODUKT-1090   TO A6-RAD                              
100400       CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                
100500                           ALT-PCB PRT-AFTER-1 A6-RAD                     
100600       MOVE A6-TEXT-48-1090        TO A6-RAD                              
100700       CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                
100800                           ALT-PCB PRT-AFTER-1 A6-RAD                     
100900       MOVE A6-TEXT-HIT-1090       TO A6-RAD                              
101000       CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                
101100                           ALT-PCB PRT-AFTER-1 A6-RAD                     
101200*      IF WS-KDORDKL = '1'                                                
101300*49   HIT                                                                 
101400*        MOVE A6-TEXT-49-1090      TO A6-RAD                              
101500*        CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL              
101600*                            ALT-PCB PRT-AFTER-1 A6-RAD                   
101700*        MOVE A6-TEXT-HIT-1090     TO A6-RAD                              
101800*        CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL              
101900*                            ALT-PCB PRT-AFTER-1 A6-RAD                   
102000*      ELSE                                                               
102100*        IF (WS-KDORDKL = '3' OR '4')                                     
102200*54   PAK                                                                 
102300*          MOVE A6-TEXT-54-1090      TO A6-RAD                            
102400*          CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL            
102500*                              ALT-PCB PRT-AFTER-1 A6-RAD                 
102600*          MOVE A6-TEXT-PAK-1090     TO A6-RAD                            
102700*          CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL            
102800*                              ALT-PCB PRT-AFTER-1 A6-RAD                 
102900*        END-IF                                                           
103000*      END-IF                                                             
103100                                                                          
103200       MOVE A6-8700-TEXT-SORTCODE     TO A6-RAD                           
103300       CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                
103400                           ALT-PCB PRT-AFTER-1 A6-RAD                     
103500                                                                          
103600       MOVE WS-IDKLIID     TO A6-POSTEN1                                  
103700                              A6-POSTEN1-1090                             
103800                              A6-POSTEN1-TEXT                             
103900                              A6-POSTEN1-TEXT-1090                        
104000                                                                          
104100       MOVE A6-BARCODE-POSTEN-1090  TO A6-RAD                             
104200       CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                
104300                           ALT-PCB PRT-AFTER-1 A6-RAD                     
104400                                                                          
104500       MOVE A6-TEXT-POSTEN-1090     TO A6-RAD                             
104600       CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                
104700                           ALT-PCB PRT-AFTER-1 A6-RAD                     
104800                                                                          
104900     END-IF                                                               
105000                                                                          
105100*DHL-FI 1090 END                                                          
105200                                                                          
105300     IF CDC-SE                                                            
105400       IF GMT-FLLDCKND = JA                                               
105500         MOVE A6-DATA-RFS-SE        TO A6-RAD                             
105600       ELSE                                                               
105700         MOVE A6-DATA-RFS           TO A6-RAD                             
105800       END-IF                                                             
105900       CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                
106000                           ALT-PCB PRT-AFTER-1 A6-RAD                     
106100                                                                          
106200       IF DIST13-SVERIGE                                                  
106300         MOVE A6-DATA-ADFLGEO-SE    TO A6-RAD                             
106400         CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL              
106500                             ALT-PCB PRT-AFTER-1 A6-RAD                   
106600                                                                          
106700       ELSE                                                               
106800         MOVE A6-DATA-ADFLGEO       TO A6-RAD                             
106900                                                                          
107000         IF DIST83-HIT-FI                                                 
107100           MOVE SPACE               TO A6-RAD                             
107200           MOVE 4504-ADFLGEO        TO A6-ADFLGEO-FI                      
107300           MOVE A6-DATA-ADFLGEO-FI  TO A6-RAD                             
107400         END-IF                                                           
107500         IF DIST83-HIT-NO                                                 
107600         AND VORD-KDFRAKT NOT = +31                                       
107700           MOVE SPACE               TO A6-RAD                             
107800           MOVE 4504-ADFLGEO        TO A6-ADFLGEO-NO                      
107900           MOVE A6-DATA-ADFLGEO-NO  TO A6-RAD                             
108000         END-IF                                                           
108100         CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL              
108200                             ALT-PCB PRT-AFTER-1 A6-RAD                   
108300                                                                          
108400         MOVE LISTA-IDDISTR         TO TEST-IDDISTR                       
108500         IF DIST05-NORGE                                                  
108600           MOVE A6-DATA-ADFLGEO-SE2 TO A6-RAD                             
108700           CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL            
108800                               ALT-PCB PRT-AFTER-1 A6-RAD                 
108900         END-IF                                                           
109000                                                                          
109100         IF LISTA-IDDISTR = ' 878'                                        
109200         AND LISTA-IDKUNDNR = '  1117'                                    
109300         AND A6-FLGEO = 'FLY'                                             
109400           CONTINUE                                                       
109500         ELSE                                                             
109600           IF DIST13-SVERIGE                                              
109700           OR DIST05-NORGE                                                
109800                                                                          
109900             MOVE A6-DATA-ADFLOMR-SE TO A6-RAD                            
110000             CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL          
110100                                 ALT-PCB PRT-AFTER-1 A6-RAD               
110200           ELSE                                                           
110300             MOVE A6-DATA-ADFLOMR   TO A6-RAD                             
110400             CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL          
110500                                 ALT-PCB PRT-AFTER-1 A6-RAD               
110600           END-IF                                                         
110700         END-IF                                                           
110800                                                                          
110900       END-IF                                                             
111000     END-IF                                                               
111100                                                                          
111200     MOVE A6-RUB-WEIGHT-KG-NL       TO A6-RAD                             
111300     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                  
111400                         ALT-PCB PRT-AFTER-1 A6-RAD                       
111500                                                                          
111600     IF WS-KILO < 10                                                      
111700     AND (WS-KILO > ZERO OR WS-HEKTO > ZERO)                              
111800       MOVE A6-DATA-WEIGHT-KILO-HEKTO-NL TO A6-RAD                        
111900     ELSE                                                                 
112000       MOVE A6-DATA-WEIGHT-NL       TO A6-RAD                             
112100     END-IF                                                               
112200     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                  
112300                       ALT-PCB PRT-AFTER-1 A6-RAD                         
112400                                                                          
112500     EVALUATE TRUE                                                        
112600       WHEN CDC-SE                                                        
112700                                                                          
112800         MOVE A6-BARCODE-ORDNR-LONG        TO A6-RAD                      
112900         CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL              
113000                             ALT-PCB PRT-AFTER-1 A6-RAD                   
113100                                                                          
113200         MOVE A6-BARCODE-TXT-ORDNR-LONG    TO A6-RAD                      
113300         CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL              
113400                             ALT-PCB PRT-AFTER-1 A6-RAD                   
113500       WHEN OTHER                                                         
113600                                                                          
113700         MOVE A6-BARCODE-ORDNR-SHORT       TO A6-RAD                      
113800         CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL              
113900                             ALT-PCB PRT-AFTER-1 A6-RAD                   
114000                                                                          
114100         MOVE A6-BARCODE-TXT-ORDNR-SHORT   TO A6-RAD                      
114200         CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL              
114300                             ALT-PCB PRT-AFTER-1 A6-RAD                   
114400     END-EVALUATE                                                         
114500                                                                          
114600     IF CDC-SE                                                            
114700        MOVE A6-TEXT-SHIPPER-CDC           TO A6-RAD                      
114800        CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL               
114900                            ALT-PCB PRT-AFTER-1 A6-RAD                    
115000     END-IF                                                               
115100                                                                          
115200     MOVE A6-STYR-91 TO A6-RAD                                            
115300     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                  
115400                         ALT-PCB PRT-AFTER-1 A6-RAD                       
115500                                                                          
115600     IF WS-IDKOLLI-PRT = WS-IDKOLLI-TOM                                   
115700       CALL W006PRS1 USING PRT-SPOOL-OVR PRT-CLOSE LISTVAL                
115800                           ALT-PCB DUMMY-AREA DUMMY-AREA                  
115900     END-IF                                                               
116000     .                                                                    
116100     EJECT                                                                
116200 S10-EV-START-W4339     SECTION.                                          
116300     MOVE 'S10-EV-START-W4339'     TO PGM-POS                             
116400                                                                          
116500     MOVE WS-IDDISTR               TO DIST83-IDDISTR                      
116600                                                                          
116700     IF (DIST83-DHL    AND WS-KDORDKL = '0' AND WS-KDFRAKT = +17)         
116800     OR (DIST83-DHL-GB AND WS-KDORDKL = '1' AND WS-KDFRAKT = +16)         
116900     OR (DIST83-DHL-GB AND WS-KDORDKL = '3' AND WS-KDFRAKT = +16)         
117000     OR (DIST83-DHL-IT AND WS-KDORDKL = '3' AND WS-KDFRAKT = +16)         
117100     OR (DIST83-DHL-IE AND WS-KDORDKL = '0' AND WS-KDFRAKT = +18)         
117200                                                                          
117300        PERFORM IMS-GU-WDGX4545                                           
117400        IF SEGMENT-FINNS                                                  
117500                                                                          
117600           MOVE WS-IDPRODNR        TO W-KY4546-IDPRODNR                   
117700           MOVE WS-IDPRODNR        TO 4546-IDPRODNR                       
117800           MOVE WS-IDKOLLI         TO W-KY4546-IDKOLLI                    
117900           MOVE WS-IDKOLLI         TO 4546-IDKOLLI                        
118000                                                                          
118100           PERFORM IMS-GHNP-WDGX4546                                      
118200           IF SEGMENT-FINNS                                               
118300             MOVE WS-VKORDBTO        TO 4546-VKORDBTO                     
118400             MOVE KOLLI-VLORDBTO-KOLLI                                    
118500                                     TO 4546-VLORDBTO                     
118600             MOVE WS-KOLLI-DIKOLLIL  TO 4546-DIKOLLIL                     
118700             MOVE WS-KOLLI-DIKOLLIB  TO 4546-DIKOLLIB                     
118800             MOVE WS-KOLLI-DIKOLLIH  TO 4546-DIKOLLIH                     
118900                                                                          
119000             PERFORM IMS-REPL-WDGX4546                                    
119100           ELSE                                                           
119200*DHL                                                                      
119300             MOVE WS-IDKUNDNR        TO 4546-IDKUNDNR                     
119400             MOVE WS-IDORDNR         TO 4546-IDORDNR5                     
119500                                                                          
119600             MOVE OHUV-BEGMT-RAD1 TO 4546-BEGMT-RAD1                      
119700             MOVE OHUV-BEGMT-RAD2 TO 4546-BEGMT-RAD2                      
119800                                                                          
119900             MOVE OHUV-ADGMT-GATA TO 4546-ADGMT-GATA                      
120000             MOVE OHUV-ADGMT-PADR TO 4546-ADGMT-PADR                      
120100             MOVE OHUV-ADGMT-LAND TO 4546-ADGMT-LAND                      
120200                                                                          
120300             MOVE ZERO               TO 4546-IDKONTO                      
120400             MOVE WS-KDORDKL         TO 4546-KDORDKL                      
120500                                                                          
120600             MOVE WS-VKORDBTO        TO 4546-VKORDBTO                     
120700             MOVE KOLLI-VLORDBTO-KOLLI                                    
120800                                     TO 4546-VLORDBTO                     
120900             MOVE WS-KOLLI-DIKOLLIL  TO 4546-DIKOLLIL                     
121000             MOVE WS-KOLLI-DIKOLLIB  TO 4546-DIKOLLIB                     
121100             MOVE WS-KOLLI-DIKOLLIH  TO 4546-DIKOLLIH                     
121200                                                                          
121300             MOVE SPACE              TO 4546-IDCITY                       
121400             MOVE ZERO               TO 4546-IDAWB                        
121500             MOVE ZERO               TO 4546-REKSIFFR-AWB                 
121600             MOVE ZERO               TO 4546-REKSIFFR-AWB                 
121700                                                                          
121800             PERFORM IMS-ISRT-WDGX4546                                    
121900           END-IF                                                         
122000                                                                          
122100           MOVE WS-IDDISTR         TO 4339-MID-IDDISTR                    
122200           MOVE WS-IDPRODNR        TO 4339-MID-IDPRODNR                   
122300           MOVE WS-IDKOLLI         TO 4339-MID-IDKOLLI                    
122400                                                                          
122500           COMPUTE 4339-KVLL  =                                           
122600                   LENGTH OF 4339-MID-W4I33901 + 17                       
122700                                                                          
122800           MOVE LOW-VALUE          TO 4339-Z1                             
122900           MOVE LOW-VALUE          TO 4339-Z2                             
123000           MOVE MFS-KDMFSFOR       TO 4339-KDMFSFOR                       
123100           PERFORM IMS-PURGE-ALT4339-MSG                                  
123200        END-IF                                                            
123300     END-IF                                                               
123400     .                                                                    
123500     EJECT                                                                
123600*POSTNORD                                                                 
123700 S09-MARKP-A6-NL-GBG-POSTNORD      SECTION.                               
123800                                                                          
123900     MOVE 0000000          TO A6-ORDNR-TXT-S                              
124000                                                                          
124100     MOVE LISTA-IDDISTR    TO A6-IDDISTR-SE                               
124200                              TEST-IDDISTR                                
124300                              A6-DISTR-L                                  
124400                              A6-DISTR-S                                  
124500                              A6-DISTR-TXT-L                              
124600                              A6-DISTR-TXT-S                              
124700                              A6-DISTR-S-ENG                              
124800                              A6-DISTR-TXT-S-ENG                          
124900     MOVE LISTA-IDKUNDNR   TO A6-IDKUNDNR-NL                              
125000                              A6-KUNDNR-L                                 
125100                              A6-KUNDNR-S                                 
125200                              A6-KUNDNR-TXT-L                             
125300                              A6-KUNDNR-TXT-S                             
125400                              A6-KUNDNR-S-ENG                             
125500                              A6-KUNDNR-TXT-S-ENG                         
125600     MOVE LISTA-IDORDNR    TO A6-IDORDNR-NL                               
125700                              A6-ORDNR-L                                  
125800                              A6-ORDNR-S                                  
125900                              A6-ORDNR-TXT-L                              
126000                              A6-ORDNR-TXT-S                              
126100                              A6-ORDNR-S-ENG                              
126200                              A6-ORDNR-TXT-S-ENG                          
126300     MOVE LISTA-IDKOLLI    TO A6-IDKOLLI-NL                               
126400                              A6-KOLLI-L                                  
126500                              A6-KOLLI-S                                  
126600                              A6-KOLLI-TXT-L                              
126700                              A6-KOLLI-TXT-S                              
126800                              A6-KOLLI-S-ENG                              
126900                              A6-KOLLI-TXT-S-ENG                          
127000     MOVE LISTA-KDFRAKT    TO A6-KDFRAKT-CDC                              
127100                              WS-WRITE-ZONA                               
127200     MOVE LISTA-TIRFS      TO A6-RFS                                      
127300                              A6-RFS-SE                                   
127400     MOVE LISTA-IDBILREG   TO A6-IDBILREG-S09                             
127500                                                                          
127600                                                                          
127700     IF CDC-SE OR LDC-SE                                                  
127800       IF GMT-FLLDCKND = JA                                               
127900         MOVE WS-OHUV-IDDEPT TO A6-IDDEPT                                 
128000       END-IF                                                             
128100     END-IF                                                               
128200     MOVE WS-KDORDKL TO A6-KDORDKL                                        
128300                                                                          
128400     MOVE LISTA-ADFLGEO    TO A6-FLGEO                                    
128500                              A6-ADFLGEO-SE                               
128600                              A6-ADFLGEO-SORT                             
128700                              A6-REFILL-Q-ADFLGEO-SORT                    
128800     MOVE ZERO             TO A6-ADFLOMR-SE                               
128900                              A6-ADRUTNIV-SE                              
129000                                                                          
129100     IF DIST13-SVERIGE                                                    
129200       MOVE KOLLI-IDTRPTNR TO TRP01-IDTRP                                 
129300       IF TRP01-TRP-MED-POSTEN                                            
129400         MOVE 4504-ADFLGEO TO A6-ADFLGEO-SORT                             
129500       END-IF                                                             
129600     END-IF                                                               
129700                                                                          
129800     IF DIST83-HIT-FI                                                     
129900       MOVE 4504-ADFLGEO   TO A6-ADFLGEO-FI                               
130000       MOVE SPACE          TO A6-ADFLGEO-SE                               
130100       MOVE SPACE          TO A6-ADFLGEO-NO                               
130200     END-IF                                                               
130300                                                                          
130400     IF DIST83-HIT-NO                                                     
130500     AND VORD-KDFRAKT NOT = +31                                           
130600       MOVE 4504-ADFLGEO   TO A6-ADFLGEO-NO                               
130700       MOVE SPACE          TO A6-ADFLGEO-SE                               
130800       MOVE SPACE          TO A6-ADFLGEO-FI                               
130900     END-IF                                                               
131000                                                                          
131100     MOVE LISTA-ADFLOMR    TO A6-FLOMR                                    
131200     MOVE LISTA-ADRUTNIV   TO A6-RUTNIV                                   
131300                                                                          
131400     IF GMT-FLLDCKND = JA                                                 
131500       PERFORM S05-GET-INFO-FR-WDE420                                     
131600     END-IF                                                               
131700*S09                                                                      
131800     MOVE WS-IDDISTR     TO TEST-IDDISTR                                  
131900     IF  CDC-SE                                                           
132000       AND GMT-FLLDCKND = NEJ                                             
132100       IF LISTA-ADRESS-1 > SPACE                                          
132200         MOVE LISTA-ADRESS-1 TO A6-ADRESS-1-SWE-S09                       
132300         MOVE LISTA-ADRESS-2 TO A6-ADRESS-2-SWE-S09                       
132400         MOVE LISTA-ADRESS-3 TO A6-ADRESS-3-SWE-S09                       
132500         MOVE LISTA-ADRESS-4 TO A6-ADRESS-4-SWE-S09                       
132600         MOVE WS-LISTA-ADRESS-5 TO A6-ADRESS-5-SWE-S09                    
132700       ELSE                                                               
132800         IF LISTA-ADRESS-2 > SPACE                                        
132900           MOVE LISTA-ADRESS-2 TO A6-ADRESS-1-SWE-S09                     
133000           MOVE LISTA-ADRESS-3 TO A6-ADRESS-3-SWE-S09                     
133100           MOVE LISTA-ADRESS-4 TO A6-ADRESS-4-SWE-S09                     
133200           MOVE WS-LISTA-ADRESS-5 TO A6-ADRESS-5-SWE-S09                  
133300         ELSE                                                             
133400           IF LISTA-ADRESS-3 > SPACE                                      
133500             MOVE LISTA-ADRESS-3 TO A6-ADRESS-1-SWE-S09                   
133600             MOVE LISTA-ADRESS-4 TO A6-ADRESS-4-SWE-S09                   
133700             MOVE WS-LISTA-ADRESS-5 TO A6-ADRESS-5-SWE-S09                
133800           ELSE                                                           
133900             IF LISTA-ADRESS-4 > SPACE                                    
134000               MOVE LISTA-ADRESS-4 TO A6-ADRESS-1-SWE-S09                 
134100               MOVE WS-LISTA-ADRESS-5 TO A6-ADRESS-5-SWE-S09              
134200             ELSE                                                         
134300               MOVE LISTA-ADRESS-1 TO A6-ADRESS-1-SWE-S09                 
134400               MOVE LISTA-ADRESS-2 TO A6-ADRESS-2-SWE-S09                 
134500               MOVE LISTA-ADRESS-3 TO A6-ADRESS-3-SWE-S09                 
134600               MOVE LISTA-ADRESS-4 TO A6-ADRESS-4-SWE-S09                 
134700               MOVE WS-LISTA-ADRESS-5 TO A6-ADRESS-5-SWE-S09              
134800             END-IF                                                       
134900           END-IF                                                         
135000         END-IF                                                           
135100       END-IF                                                             
135200     ELSE                                                                 
135300       MOVE LISTA-ADRESS-1 TO A6-ADRESS-1-NL-S09                          
135400       MOVE LISTA-ADRESS-2 TO A6-ADRESS-2-NL-S09                          
135500       MOVE LISTA-ADRESS-3 TO A6-ADRESS-3-NL-S09                          
135600       MOVE LISTA-ADRESS-4 TO A6-ADRESS-4-NL-S09                          
135700       IF GMT-FLLDCKND = JA                                               
135800         MOVE WS-ORAD-BERADREF TO A6-ADRESS-5-SWE                         
135900                                  A6-ADRESS-5-NL                          
136000                                  A6-ADRESS-5-N                           
136100                                  A6-ADRESS-5-POSTEN                      
136200         MOVE LISTA-IDBILREG   TO A6-IDBILREG-S09                         
136300       ELSE                                                               
136400         MOVE WS-LISTA-ADRESS-5 TO A6-ADRESS-5-NL-S09                     
136500                                   A6-ADRESS-5-N-S09                      
136600       END-IF                                                             
136700     END-IF                                                               
136800                                                                          
136900     IF WS-KILO < 10                                                      
137000     AND (WS-KILO > ZERO OR WS-HEKTO > ZERO)                              
137100       MOVE WS-KILO      TO A6-KILO-NL                                    
137200       MOVE WS-HEKTO     TO A6-HEKTO-NL                                   
137300     ELSE                                                                 
137400       MOVE WS-VKORDBTO TO A6-VKORDBTO-NL                                 
137500     END-IF                                                               
137600                                                                          
137700     IF WS-IDKOLLI-PRT = WS-IDKOLLI                                       
137800       CALL W006PRS1 USING PRT-SPOOL-OVR PRT-OPEN LISTVAL                 
137900                           ALT-PCB DUMMY-AREA DUMMY-AREA                  
138000     END-IF                                                               
138100                                                                          
138200     MOVE SPACE                     TO A6-RAD                             
138300                                                                          
138400     MOVE A6-STYR-01                TO A6-RAD                             
138500     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                  
138600                         ALT-PCB PRT-NYSIDA-RAD1 A6-RAD                   
138700                                                                          
138800     MOVE A6-STYR-01                TO A6-RAD                             
138900     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                  
139000                         ALT-PCB PRT-AFTER-1 A6-RAD                       
139100                                                                          
139200     MOVE A6-STYR-42      TO A6-RAD                                       
139300     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                  
139400                         ALT-PCB PRT-AFTER-1 A6-RAD                       
139500                                                                          
139600     MOVE A6-RUB-DISTRICT-NL        TO A6-RAD                             
139700     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                  
139800                         ALT-PCB PRT-AFTER-1 A6-RAD                       
139900                                                                          
140000     MOVE A6-RUB-CUSTOMER-NL   TO A6-RAD                                  
140100     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                  
140200                         ALT-PCB PRT-AFTER-1 A6-RAD                       
140300                                                                          
140400     MOVE A6-RUB-ORDERNUMBER-NL     TO A6-RAD                             
140500     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                  
140600                         ALT-PCB PRT-AFTER-1 A6-RAD                       
140700                                                                          
140800     MOVE A6-RUB-FREIGHTCODE-NL     TO A6-RAD                             
140900     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                  
141000                         ALT-PCB PRT-AFTER-1 A6-RAD                       
141100                                                                          
141200     IF CDC-SE                                                            
141300       MOVE A6-RUB-RFS-GBG          TO A6-RAD                             
141400       CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                
141500                           ALT-PCB PRT-AFTER-1 A6-RAD                     
141600     END-IF                                                               
141700                                                                          
141800     MOVE A6-RUB-ADDRESS-NL         TO A6-RAD                             
141900     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                  
142000                         ALT-PCB PRT-AFTER-1 A6-RAD                       
142100                                                                          
142200     MOVE A6-RUB-CASE-NL            TO A6-RAD                             
142300     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                  
142400                         ALT-PCB PRT-AFTER-1 A6-RAD                       
142500                                                                          
142600     MOVE A6-DATA-IDDISTR-SE        TO A6-RAD                             
142700     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                  
142800                         ALT-PCB PRT-AFTER-1 A6-RAD                       
142900                                                                          
143000     MOVE A6-DATA-IDKUNDNR-NL       TO A6-RAD                             
143100     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                  
143200                         ALT-PCB PRT-AFTER-1 A6-RAD                       
143300                                                                          
143400     MOVE A6-DATA-IDORDNR-NL        TO A6-RAD                             
143500     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                  
143600                         ALT-PCB PRT-AFTER-1 A6-RAD                       
143700                                                                          
143800     MOVE A6-DATA-IDKOLLI-NL        TO A6-RAD                             
143900     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                  
144000                         ALT-PCB PRT-AFTER-1 A6-RAD                       
144100                                                                          
144200*                                                                         
144300     MOVE OHUV-IDDISTR  TO DIST03-IDDISTR                                 
144400                                                                          
144500     IF DIST03-DANMARK-900                                                
144600                                                                          
144700       MOVE A6-RUB-REPDAT-S09      TO A6-RAD                              
144800       CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                
144900                           ALT-PCB PRT-AFTER-1 A6-RAD                     
145000                                                                          
145100       MOVE WS-OHUV-TIREPDAT       TO A6-TIREPDAT-S09                     
145200       INSPECT A6-TIREPDAT-S09 REPLACING LEADING ZERO BY SPACE            
145300       MOVE A6-DATA-TIREPDAT-S09   TO A6-RAD                              
145400       CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                
145500                           ALT-PCB PRT-AFTER-1 A6-RAD                     
145600                                                                          
145700       MOVE A6-RUB-CAR-REG-S09     TO A6-RAD                              
145800       CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                
145900                           ALT-PCB PRT-AFTER-1 A6-RAD                     
146000                                                                          
146100       MOVE A6-DATA-IDBILREG-S09   TO A6-RAD                              
146200       CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                
146300                           ALT-PCB PRT-AFTER-1  A6-RAD                    
146400     END-IF                                                               
146500*S09                                                                      
146600     IF A6-ADRESS-1-SWE-S09 > SPACE                                       
146700       MOVE A6-DATA-ADRESS1-SWE-S09 TO A6-RAD                             
146800       CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                
146900                           ALT-PCB PRT-AFTER-1 A6-RAD                     
147000                                                                          
147100       MOVE A6-DATA-ADRESS2-SWE-S09 TO A6-RAD                             
147200       CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                
147300                           ALT-PCB PRT-AFTER-1 A6-RAD                     
147400                                                                          
147500       MOVE A6-DATA-ADRESS3-SWE-S09 TO A6-RAD                             
147600       CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                
147700                           ALT-PCB PRT-AFTER-1 A6-RAD                     
147800                                                                          
147900       MOVE A6-DATA-ADRESS4-SWE-S09 TO A6-RAD                             
148000       CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                
148100                           ALT-PCB PRT-AFTER-1 A6-RAD                     
148200                                                                          
148300       IF WS-GMT-BETEXT-INFO = SPACE                                      
148400         MOVE WS-GMT-BETEXT-INFO    TO A6-BETEXT-INFO-S09                 
148500         MOVE A6-DATA-BETEXT-S09    TO A6-RAD                             
148600         CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL              
148700                             ALT-PCB PRT-AFTER-1 A6-RAD                   
148800       END-IF                                                             
148900     ELSE                                                                 
149000       MOVE A6-DATA-ADRESS1-NL-S09  TO A6-RAD                             
149100       CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                
149200                           ALT-PCB PRT-AFTER-1 A6-RAD                     
149300                                                                          
149400       MOVE A6-DATA-ADRESS2-NL-S09  TO A6-RAD                             
149500       CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                
149600                           ALT-PCB PRT-AFTER-1 A6-RAD                     
149700                                                                          
149800       MOVE A6-DATA-ADRESS3-NL-S09  TO A6-RAD                             
149900       CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                
150000                           ALT-PCB PRT-AFTER-1 A6-RAD                     
150100                                                                          
150200       MOVE A6-DATA-ADRESS4-NL-S09  TO A6-RAD                             
150300       CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                
150400                           ALT-PCB PRT-AFTER-1 A6-RAD                     
150500                                                                          
150600       IF WS-GMT-BETEXT-INFO = SPACE                                      
150700         IF GMT-FLLDCKND = JA                                             
150800*LARGE     FONT                                                           
150900           MOVE A6-DATA-ADRESS5-POSTEN TO A6-RAD                          
151000         ELSE                                                             
151100*SMALL     FONT                                                           
151200           MOVE A6-DATA-ADRESS5-N    TO A6-RAD                            
151300         END-IF                                                           
151400         CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL              
151500                             ALT-PCB PRT-AFTER-1 A6-RAD                   
151600       END-IF                                                             
151700     END-IF                                                               
151800                                                                          
151900*BETEXT                                                                   
152000     IF WS-GMT-BETEXT-INFO > SPACE                                        
152100       MOVE WS-GMT-BETEXT-INFO      TO A6-BETEXT-INFO-S09                 
152200       MOVE A6-DATA-BETEXT-S09      TO A6-RAD                             
152300       CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                
152400                           ALT-PCB PRT-AFTER-1 A6-RAD                     
152500     END-IF                                                               
152600                                                                          
152700     MOVE A6-DATA-KDFRAKT-CDC       TO A6-RAD                             
152800     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                  
152900                         ALT-PCB PRT-AFTER-1 A6-RAD                       
153000     IF CDC-SE                                                            
153100     OR KUND12-LDC-AKUT                                                   
153200       IF GMT-FLLDCKND = JA                                               
153300         IF WS-OHUV-IDDEPT > ZERO                                         
153400           MOVE A6-RUB-IDDEPT TO A6-RAD                                   
153500           CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL            
153600                             ALT-PCB PRT-AFTER-1 A6-RAD                   
153700                                                                          
153800           MOVE A6-DATA-IDDEPT TO A6-RAD                                  
153900           CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL            
154000                             ALT-PCB PRT-AFTER-1 A6-RAD                   
154100         END-IF                                                           
154200                                                                          
154300         IF RO-JA                                                         
154400           MOVE A6-DATA-RESTORDER   TO A6-RAD                             
154500           CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL            
154600                              ALT-PCB PRT-AFTER-1 A6-RAD                  
154700         END-IF                                                           
154800       END-IF                                                             
154900     END-IF                                                               
155000                                                                          
155100     MOVE A6-DATA-KDORDKL           TO A6-RAD                             
155200     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                  
155300                         ALT-PCB PRT-AFTER-1 A6-RAD                       
155400                                                                          
155500     IF CDC-SE                                                            
155600       IF GMT-FLLDCKND = JA                                               
155700         MOVE A6-DATA-RFS-SE        TO A6-RAD                             
155800       ELSE                                                               
155900         MOVE A6-DATA-RFS           TO A6-RAD                             
156000       END-IF                                                             
156100       CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                
156200                           ALT-PCB PRT-AFTER-1 A6-RAD                     
156300                                                                          
156400       IF DIST13-SVERIGE                                                  
156500         MOVE A6-DATA-ADFLGEO-SE    TO A6-RAD                             
156600         CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL              
156700                             ALT-PCB PRT-AFTER-1 A6-RAD                   
156800                                                                          
156900       ELSE                                                               
157000         IF DIST83-HIT-FI                                                 
157100           MOVE 4504-ADFLGEO        TO A6-ADFLGEO-FI                      
157200           MOVE A6-DATA-ADFLGEO-FI  TO A6-RAD                             
157300           CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL            
157400                               ALT-PCB PRT-AFTER-1 A6-RAD                 
157500         ELSE                                                             
157600           IF DIST83-HIT-NO                                               
157700           AND VORD-KDFRAKT NOT = +31                                     
157800             MOVE 4504-ADFLGEO      TO A6-ADFLGEO-NO                      
157900             MOVE A6-DATA-ADFLGEO-NO TO A6-RAD                            
158000             CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL          
158100                                 ALT-PCB PRT-AFTER-1 A6-RAD               
158200           END-IF                                                         
158300         END-IF                                                           
158400       END-IF                                                             
158500     END-IF                                                               
158600                                                                          
158700     MOVE A6-RUB-WEIGHT-KG-NL       TO A6-RAD                             
158800     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                  
158900                         ALT-PCB PRT-AFTER-1 A6-RAD                       
159000                                                                          
159100     IF WS-KILO < 10                                                      
159200     AND (WS-KILO > ZERO OR WS-HEKTO > ZERO)                              
159300       MOVE A6-DATA-WEIGHT-KILO-HEKTO-NL TO A6-RAD                        
159400     ELSE                                                                 
159500       MOVE A6-DATA-WEIGHT-NL       TO A6-RAD                             
159600     END-IF                                                               
159700     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                  
159800                       ALT-PCB PRT-AFTER-1 A6-RAD                         
159900                                                                          
160000     EVALUATE TRUE                                                        
160100       WHEN CDC-SE                                                        
160200                                                                          
160300         MOVE A6-BARCODE-ORDNR-LONG        TO A6-RAD                      
160400         CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL              
160500                             ALT-PCB PRT-AFTER-1 A6-RAD                   
160600                                                                          
160700         MOVE A6-BARCODE-TXT-ORDNR-LONG    TO A6-RAD                      
160800         CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL              
160900                             ALT-PCB PRT-AFTER-1 A6-RAD                   
161000       WHEN OTHER                                                         
161100                                                                          
161200         MOVE A6-BARCODE-ORDNR-SHORT       TO A6-RAD                      
161300         CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL              
161400                             ALT-PCB PRT-AFTER-1 A6-RAD                   
161500                                                                          
161600         MOVE A6-BARCODE-TXT-ORDNR-SHORT   TO A6-RAD                      
161700         CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL              
161800                             ALT-PCB PRT-AFTER-1 A6-RAD                   
161900     END-EVALUATE                                                         
162000                                                                          
162100     IF CDC-SE                                                            
162200        MOVE A6-TEXT-SHIPPER-CDC           TO A6-RAD                      
162300        CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL               
162400                            ALT-PCB PRT-AFTER-1 A6-RAD                    
162500     END-IF                                                               
162600                                                                          
162700*POSTEN                                                                   
162800     MOVE KOLLI-IDTRPTNR         TO TRP01-IDTRP                           
162900     IF TRP01-TRP-MED-POSTEN                                              
163000       MOVE A6-TEXT-PRODUKT      TO A6-RAD                                
163100       CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                
163200                           ALT-PCB PRT-AFTER-1 A6-RAD                     
163300       IF WS-KDORDKL = '1'                                                
163400*48   HIT                                                                 
163500         MOVE A6-TEXT-48           TO A6-RAD                              
163600         CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL              
163700                             ALT-PCB PRT-AFTER-1 A6-RAD                   
163800         MOVE A6-TEXT-HIT          TO A6-RAD                              
163900         CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL              
164000                             ALT-PCB PRT-AFTER-1 A6-RAD                   
164100       ELSE                                                               
164200         IF (WS-KDORDKL = '3' OR '4')                                     
164300*69   PAK                                                                 
164400           MOVE A6-TEXT-69           TO A6-RAD                            
164500           CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL            
164600                               ALT-PCB PRT-AFTER-1 A6-RAD                 
164700           MOVE A6-TEXT-PAK          TO A6-RAD                            
164800           CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL            
164900                               ALT-PCB PRT-AFTER-1 A6-RAD                 
165000         END-IF                                                           
165100       END-IF                                                             
165200                                                                          
165300       MOVE A6-TEXT-SORTCODE     TO A6-RAD                                
165400       CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                
165500                           ALT-PCB PRT-AFTER-1 A6-RAD                     
165600                                                                          
165700       MOVE A6-SORTERINGSKOD     TO A6-RAD                                
165800       CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                
165900                           ALT-PCB PRT-AFTER-1 A6-RAD                     
166000                                                                          
166100       MOVE WS-IDKLIID   TO A6-POSTEN1                                    
166200                            A6-POSTEN1-TEXT                               
166300                                                                          
166400       MOVE A6-BARCODE-POSTEN TO A6-RAD                                   
166500       CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                
166600                           ALT-PCB PRT-AFTER-1 A6-RAD                     
166700                                                                          
166800       MOVE A6-TEXT-POSTEN       TO A6-RAD                                
166900       CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                
167000                           ALT-PCB PRT-AFTER-1 A6-RAD                     
167100     END-IF                                                               
167200*************                                                             
167300     MOVE A6-STYR-91 TO A6-RAD                                            
167400     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE LISTVAL                  
167500                         ALT-PCB PRT-AFTER-1 A6-RAD                       
167600                                                                          
167700     MOVE WS-IDDISTR     TO DIST29-IDDISTR                                
167800                                                                          
167900     IF WS-IDKOLLI-PRT = WS-IDKOLLI-TOM                                   
168000       CALL W006PRS1 USING PRT-SPOOL-OVR PRT-CLOSE LISTVAL                
168100                           ALT-PCB DUMMY-AREA DUMMY-AREA                  
168200     END-IF                                                               
168300     .                                                                    
168400     EJECT                                                                
168500 S05-GET-INFO-FR-WDE420   SECTION.                                        
168600     MOVE 'STA S05- SEC  '    TO PGM-POS                                  
168700                                                                          
168800     PERFORM  IMS-GU-WDE601                                               
168900     IF SEGMENT-FINNS                                                     
169000       MOVE VORD-IDPRODNR     TO W-IDPRODNR-WDE421                        
169100       MOVE KOLLI-IDKOLLI     TO W-IDKOLLI-WDE421                         
169200       PERFORM IMS-GU-WDE411                                              
169300       IF SEGMENT-FINNS                                                   
169400         MOVE ORAD-IDARTNR       TO W-IDARTNR                             
169500                                                                          
169600         IF GMT-FLLDCKND = JA                                             
169700           IF ORAD-IDKUNDRF-WIP > SPACE                                   
169800             MOVE ORAD-IDKUNDRF-WIP TO WS-ORAD-BERADREF                   
169900           ELSE                                                           
170000             MOVE ORAD-BERADREF     TO WS-ORAD-BERADREF                   
170100           END-IF                                                         
170200* FÖR TACDIS ORDER MÖRKAS JOBB NR. I BERADREF                             
170300           MOVE WS-IDDISTR          TO DIST05-IDDISTR                     
170400           IF (OHUV-IDSYSTEM = 'LDC' OR 'TACD')                           
170500           AND (DIST13-SVERIGE OR DIST05-NORGE)                           
170600             MOVE SPACE             TO WS-ORAD-BERADREF(9:2)              
170700           END-IF                                                         
170800         ELSE                                                             
170900           MOVE ORAD-BERADREF       TO WS-ORAD-BERADREF                   
171000         END-IF                                                           
171100*                                                                         
171200         MOVE WS-IDDISTR            TO TEST-IDDISTR                       
171300         MOVE WS-IDDISTR            TO DIST83-IDDISTR                     
171400         IF DIST83-DHL-IT                                                 
171500           IF WS-OHUV-BEKUNDRF > SPACE                                    
171600             MOVE WS-OHUV-BEKUNDRF  TO WS-ORAD-BERADREF                   
171700           ELSE                                                           
171800             IF ORAD-BERADREF > SPACE                                     
171900               MOVE ORAD-BERADREF   TO WS-ORAD-BERADREF                   
172000             ELSE                                                         
172100               IF ORAD-BEVOLREF > SPACE                                   
172200                 MOVE ORAD-BEVOLREF TO WS-ORAD-BERADREF                   
172300               END-IF                                                     
172400             END-IF                                                       
172500           END-IF                                                         
172600         END-IF                                                           
172700                                                                          
172800         IF ORAD-IDKUNDRF-RO > ZERO                                       
172900           MOVE JA  TO WS-RESTORDER                                       
173000         ELSE                                                             
173100           MOVE NEJ TO WS-RESTORDER                                       
173200         END-IF                                                           
173300         IF SEGMENT-FINNS                                                 
173400           PERFORM IMS-GNP-WDE421                                         
173500           MOVE KKOLLI-KVLEVART        TO WS-ORAD-KVLEVART                
173600         END-IF                                                           
173700       END-IF                                                             
173800     END-IF                                                               
173900     .                                                                    
174000     SKIP2                                                                
174100 S06-DIST-KUND-LDC SECTION.                                               
174200     MOVE NEJ               TO WS-KDFRAKT-TEST                            
174300*                                                                         
174400*    SECTION ÄR KOMMENTERAD NÄR GMT-KDFRAKT TAGITS BORT FRÅN              
174500*    WDB201.                                                              
174600*    WS-KDFRAKT-TEST KOMMER ALDRIG ATT BLI JA                             
174700*    DÄRFÖR SÄTTER VI DEN BARA TILL NEJ.                                  
174800*    DIST-KUND-WS-IDDC ANVÄNDS BARA I DENNA SECTIONEN                     
174900*                                                                         
175000*    MOVE ZERO                TO  DIST-KUND-WS-IDDC                       
175100*    MOVE OHUV-IDDISTR        TO  W-IDDISTR                               
175200*    MOVE OHUV-IDKUNDNR       TO  W-IDKUNDNR                              
175300*                                                                         
175400*    PERFORM IMS-GU-GMTA01-WDB201                                         
175500*                                                                         
175600*    IF SEGMENT-FINNS                                                     
175700*                                                                         
175800*       IF GMT-FLLDCKND = JA                                              
175900*          MOVE GMT-IDDC-DAY(1) TO DIST-KUND-WS-IDDC                      
176000*       END-IF                                                            
176100*       MOVE +1                TO FK-INDX                                 
176200*       MOVE NEJ               TO WS-KDFRAKT-TEST                         
176300*       PERFORM UNTIL (FK-INDX > MAX-FK-INDX)                             
176400*          IF GMT-KDFRAKT-KONS (FK-INDX) = WS-KDFRAKT                     
176500*             MOVE JA            TO WS-KDFRAKT-TEST                       
176600*          END-IF                                                         
176700*          ADD +1              TO FK-INDX                                 
176800*       END-PERFORM                                                       
176900*    ELSE                                                                 
177000*       MOVE NEJ               TO GMT-FLLDCKND                            
177100*    END-IF                                                               
177200     .                                                                    
177300     EJECT                                                                
177400                                                                          
177500* IMS SEKTIONER                                                           
177600     SKIP3                                                                
177700 IMS-GET-MSG               SECTION.                                       
177800                                                                          
177900     MOVE '  QC' TO GODK-STATUSKODER                                      
178000     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
178100     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
178200     PERFORM IMS-STATUSKONTROLL                                           
178300     .                                                                    
178400     SKIP3                                                                
178500 IMS-ISRT-MSG              SECTION.                                       
178600                                                                          
178700     IF NOT ENGLISH-TEXT                                                  
178800       MOVE '0' TO MFS-KDHUVOMR                                           
178900     END-IF                                                               
179000     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
179100     MOVE SPACE TO GODK-STATUSKODER                                       
179200     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
179300     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
179400     PERFORM IMS-STATUSKONTROLL                                           
179500     .                                                                    
179600     EJECT                                                                
179700 IMS-PURGE-ALT4339-MSG SECTION.                                           
179800     MOVE 'IMS-PURGE-ALT4339-MSG'    TO IMS-POS                           
179900                                                                          
180000     MOVE '    ' TO GODK-STATUSKODER                                      
180100     CALL CBLTDLI USING PURG                                              
180200                        ALT4339-PCB                                       
180300                        4339-MSG-IO-AREA                                  
180400     MOVE ALT4339-STATUS-CODE TO STATUS-WS                                
180500     PERFORM IMS-STATUSKONTROLL                                           
180600     .                                                                    
180700     SKIP3                                                                
180800 IMS-GU-WDGX4545  SECTION.                                                
180900     MOVE 'IMS-GU-WDGX4545'          TO IMS-POS                           
181000                                                                          
181100     STRING 'WDR401  (WDGXKEY  =' W-4545-WDGXKEY-X ')'                    
181200         DELIMITED BY SIZE INTO SSA1                                      
181300     MOVE '  GE' TO GODK-STATUSKODER                                      
181400     CALL CBLTDLI USING GU 4545-PCB DLI-IO-GX01 SSA1                      
181500     MOVE 4545-STATUS-CODE TO STATUS-WS                                   
181600     PERFORM IMS-STATUSKONTROLL                                           
181700     .                                                                    
181800     SKIP3                                                                
181900 IMS-GHNP-WDGX4546  SECTION.                                              
182000     MOVE 'IMS-GHNP-WDGX4546'         TO IMS-POS                          
182100                                                                          
182200     STRING 'WDGX4546(KY4546   =' W-4546-WDGXKEY-X ')'                    
182300         DELIMITED BY SIZE INTO SSA1                                      
182400     MOVE '  GE' TO GODK-STATUSKODER                                      
182500     CALL CBLTDLI USING GHNP 4545-PCB DLI-IO-4546 SSA1                    
182600     MOVE 4545-STATUS-CODE TO STATUS-WS                                   
182700     PERFORM IMS-STATUSKONTROLL                                           
182800     .                                                                    
182900     SKIP3                                                                
183000 IMS-REPL-WDGX4546        SECTION.                                        
183100     MOVE 'IMS-REPL-WDGX4546'        TO IMS-POS                           
183200                                                                          
183300     MOVE '    ' TO GODK-STATUSKODER                                      
183400     CALL CBLTDLI USING REPL 4545-PCB DLI-IO-4546                         
183500     MOVE 4545-STATUS-CODE TO STATUS-WS                                   
183600     PERFORM IMS-STATUSKONTROLL                                           
183700     .                                                                    
183800     SKIP3                                                                
183900                                                                          
184000 IMS-ISRT-WDGX4546          SECTION.                                      
184100     MOVE 'IMS-ISRT-WDGX4546'        TO IMS-POS                           
184200                                                                          
184300     STRING 'WDR401  (WDGXKEY  =' W-4545-WDGXKEY-X ')'                    
184400         DELIMITED BY SIZE INTO SSA1                                      
184500     MOVE 'WDGX4546'         TO SSA2                                      
184600     MOVE '  II' TO GODK-STATUSKODER                                      
184700     CALL CBLTDLI USING ISRT 4545-PCB DLI-IO-4546 SSA1 SSA2               
184800     MOVE 4545-STATUS-CODE TO STATUS-WS                                   
184900     PERFORM IMS-STATUSKONTROLL                                           
185000     .                                                                    
185100     SKIP3                                                                
185200                                                                          
185300 IMS-GU-WDI201      SECTION.                                              
185400     STRING 'WDI201  (IDGMTREF =' W-IDGMTREF-X ')'                        
185500            DELIMITED BY SIZE INTO SSA1                                   
185600     MOVE '  GE' TO GODK-STATUSKODER                                      
185700     CALL CBLTDLI USING GU WDI2-PCB DLI-IO-WDI2 SSA1                      
185800     MOVE WDI2-STATUS-CODE TO STATUS-WS                                   
185900     PERFORM IMS-STATUSKONTROLL                                           
186000     SKIP3                                                                
186100     .                                                                    
186200                                                                          
186300 IMS-GU-GMTA01-WDB201      SECTION.                                       
186400     STRING 'WLGMTA01(IDGMT    =' W-IDGMT-X ')'                           
186500            DELIMITED BY SIZE INTO SSA1                                   
186600     MOVE '    ' TO GODK-STATUSKODER                                      
186700     CALL CBLTDLI USING GU GMTA-PCB GMT-WDB201 SSA1                       
186800     MOVE GMTA-STATUS-CODE TO STATUS-WS                                   
186900     PERFORM IMS-STATUSKONTROLL                                           
187000     SKIP3                                                                
187100     .                                                                    
187200                                                                          
187300 IMS-GU-BETC01-WDB101      SECTION.                                       
187400     STRING 'WLBETC01(WDB101KY =' W-WDB101KY-X ')'                        
187500            DELIMITED BY SIZE INTO SSA1                                   
187600     MOVE '  GE' TO GODK-STATUSKODER                                      
187700     CALL CBLTDLI USING GU BETC-PCB BET-WDB101 SSA1                       
187800     MOVE BETC-STATUS-CODE TO STATUS-WS                                   
187900     PERFORM IMS-STATUSKONTROLL                                           
188000     SKIP3                                                                
188100     .                                                                    
188200 IMS-GU-GMTC01-WDB501      SECTION.                                       
188300     STRING 'WLGMTC01(WDB501KY =' W-WDB501KY-X                            
188400                    '!WDB501KY =' W-WDB501KY-DEFAULT-X ')'                
188500            DELIMITED BY SIZE INTO SSA1                                   
188600     MOVE '  GE' TO GODK-STATUSKODER                                      
188700     CALL CBLTDLI USING GU GMTC-PCB FK-WDB501 SSA1                        
188800     MOVE GMTC-STATUS-CODE TO STATUS-WS                                   
188900     PERFORM IMS-STATUSKONTROLL                                           
189000     SKIP3                                                                
189100     .                                                                    
189200 IMS-GU-WDE401-ASEQ        SECTION.                                       
189300                                                                          
189400     STRING 'WDE401  (WDE4ASEQ =' W-KUNDORDER-SEK-X ')'                   
189500                      DELIMITED BY SIZE INTO SSA1                         
189600     MOVE '  GE' TO GODK-STATUSKODER                                      
189700     CALL CBLTDLI USING GU WDE4A-PCB DLI-IO-AREA SSA1                     
189800     MOVE WDE4A-STATUS-CODE TO STATUS-WS                                  
189900                               STATUS-KUNDORDER-SEK-WS                    
190000     PERFORM IMS-STATUSKONTROLL                                           
190100     .                                                                    
190200     SKIP3                                                                
190300 IMS-GU-WDE411             SECTION.                                       
190400                                                                          
190500     STRING 'WDE411  (WDE4FSEQ =' W-WDE421KY-X ')'                        
190600                      DELIMITED BY SIZE INTO SSA1                         
190700     MOVE '      ' TO GODK-STATUSKODER                                    
190800     CALL CBLTDLI USING GU WDE4-PCB DLI-IOAREA-WDE411 SSA1                
190900     MOVE WDE4-STATUS-CODE TO STATUS-WS                                   
191000                               STATUS-KUNDORDER-SEK-WS                    
191100     PERFORM IMS-STATUSKONTROLL                                           
191200     .                                                                    
191300     SKIP3                                                                
191400 IMS-GNP-WDE421             SECTION.                                      
191500                                                                          
191600     STRING 'WDE421  (WDE421KY =' W-WDE421KY-X ')'                        
191700                      DELIMITED BY SIZE INTO SSA1                         
191800     MOVE '  GE' TO GODK-STATUSKODER                                      
191900     CALL CBLTDLI USING GNP WDE4-PCB DLI-IOAREA-WDE421 SSA1               
192000     MOVE WDE4-STATUS-CODE TO STATUS-WS                                   
192100                               STATUS-KUNDORDER-SEK-WS                    
192200     PERFORM IMS-STATUSKONTROLL                                           
192300     .                                                                    
192400     SKIP3                                                                
192500 IMS-GN-WDE401             SECTION.                                       
192600                                                                          
192700     STRING 'WDE401  (WDE4ASEQ =' W-KUNDORDER-SEK-X ')'                   
192800                      DELIMITED BY SIZE INTO SSA1                         
192900     MOVE '  GEGB' TO GODK-STATUSKODER                                    
193000     CALL CBLTDLI USING GN WDE4A-PCB DLI-IO-AREA SSA1                     
193100     MOVE WDE4A-STATUS-CODE TO STATUS-WS                                  
193200                               STATUS-KUNDORDER-SEK-WS                    
193300     PERFORM IMS-STATUSKONTROLL                                           
193400     .                                                                    
193500     EJECT                                                                
193600 IMS-GU-WDE401             SECTION.                                       
193700                                                                          
193800     STRING 'WDE401  (WDE401KY =' W-KUNDORDER-X ')'                       
193900                      DELIMITED BY SIZE INTO SSA1                         
194000     MOVE '    ' TO GODK-STATUSKODER                                      
194100     CALL CBLTDLI USING GU WDE42-PCB DLI-IO-AREA SSA1                     
194200     MOVE WDE42-STATUS-CODE TO STATUS-WS                                  
194300     PERFORM IMS-STATUSKONTROLL                                           
194400     .                                                                    
194500     SKIP3                                                                
194600 IMS-GU-WDE611 SECTION.                                                   
194700                                                                          
194800     STRING 'WDE601  (IDPRODNR =' W-IDPRODNR-WDE611-X ')'                 
194900          DELIMITED BY SIZE INTO SSA1                                     
195000     STRING 'WDE611  (IDKOLLI  =' W-IDKOLLI-WDE611-X ')'                  
195100          DELIMITED BY SIZE INTO SSA2                                     
195200     MOVE '  GE' TO GODK-STATUSKODER                                      
195300     CALL CBLTDLI USING GU ORDD-PCB DLI-IOAREA-WDE611 SSA1 SSA2           
195400     MOVE ORDD-STATUS-CODE TO STATUS-WS                                   
195500     PERFORM IMS-STATUSKONTROLL                                           
195600     .                                                                    
195700     EJECT                                                                
195800* LAESN. FÖR KONTROLL OM LAASSEGMENT SKALL TAS BORT                       
195900 IMS-GET-WDE4E SECTION.                                                   
196000     STRING 'WDE4E1  (WDE4E1KY>=' W-WDE4E1KY-MIN-X                        
196100                    '&WDE4E1KY<=' W-WDE4E1KY-MAX-X ')'                    
196200            DELIMITED BY SIZE INTO SSA1                                   
196300     MOVE '  GEGB' TO GODK-STATUSKODER                                    
196400     CALL CBLTDLI USING GN WDE4E-PCB DLI-IOAREA-WDE4E1 SSA1               
196500     MOVE WDE4E-STATUS-CODE TO STATUS-WS                                  
196600     PERFORM IMS-STATUSKONTROLL                                           
196700     .                                                                    
196800     EJECT                                                                
196900 IMS-GU-WDE601           SECTION.                                         
197000                                                                          
197100     STRING 'WDE601  (IDPRODNR =' W-IDPRODNR-X ')'                        
197200                      DELIMITED BY SIZE INTO SSA1                         
197300     MOVE '  GE' TO GODK-STATUSKODER                                      
197400     CALL CBLTDLI USING GU WDE6-PCB DLI-IOAREA-WDE601 SSA1                
197500     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
197600     PERFORM IMS-STATUSKONTROLL                                           
197700     .                                                                    
197800     SKIP3                                                                
197900 IMS-GET-WLORQI01          SECTION.                                       
198000                                                                          
198100     STRING 'WLORQI01(IDORDER  =' W-IDORDER-X ')'                         
198200                      DELIMITED BY SIZE INTO SSA1                         
198300     MOVE '  ' TO GODK-STATUSKODER                                        
198400     CALL CBLTDLI USING GU ORQI-PCB DLI-IO-Q201 SSA1                      
198500     MOVE ORQI-STATUS-CODE TO STATUS-WS                                   
198600     PERFORM IMS-STATUSKONTROLL                                           
198700     .                                                                    
198800     SKIP3                                                                
198900 IMS-GNP-WLORQI12          SECTION.                                       
199000                                                                          
199100     STRING 'WLORQI12(IDDC     =' W-IDDC-X ')'                            
199200                      DELIMITED BY SIZE INTO SSA1                         
199300     MOVE '  GE' TO GODK-STATUSKODER                                      
199400     CALL CBLTDLI USING GNP ORQI-PCB DLI-IO-Q212 SSA1                     
199500     MOVE ORQI-STATUS-CODE TO STATUS-WS                                   
199600     PERFORM IMS-STATUSKONTROLL                                           
199700     .                                                                    
199800     SKIP2                                                                
199900 IMS-GU-WLORQA01          SECTION.                                        
200000                                                                          
200100     STRING 'WLORQA01(WDQ301KY =' W-WDQ301KY-X ')'                        
200200                      DELIMITED BY SIZE INTO SSA1                         
200300     MOVE '  ' TO GODK-STATUSKODER                                        
200400     CALL CBLTDLI USING GU ORQA-PCB DLI-IO-AREA3 SSA1                     
200500     MOVE ORQA-STATUS-CODE TO STATUS-WS                                   
200600     PERFORM IMS-STATUSKONTROLL                                           
200700     .                                                                    
200800     SKIP3                                                                
200900 IMS-GU-ARTS01 SECTION.                                                   
201000                                                                          
201100     STRING 'WLARTS01(IDARTNR  =' W-IDARTNR-X ')'                         
201200          DELIMITED BY SIZE INTO SSA1                                     
201300     MOVE '  GE' TO GODK-STATUSKODER                                      
201400     CALL CBLTDLI USING GU ARTS-PCB SART-WDK701 SSA1                      
201500     MOVE ARTS-STATUS-CODE TO STATUS-WS                                   
201600     PERFORM IMS-STATUSKONTROLL                                           
201700     .                                                                    
201800     SKIP3                                                                
201900 IMS-GNP-ARTS11 SECTION.                                                  
202000                                                                          
202100     STRING 'WLARTS11(IDDC     =' W-IDDC-X ')'                            
202200          DELIMITED BY SIZE INTO SSA1                                     
202300     MOVE '  GE' TO GODK-STATUSKODER                                      
202400     CALL CBLTDLI USING GNP ARTS-PCB SLAG-WDK711 SSA1                     
202500     MOVE ARTS-STATUS-CODE TO STATUS-WS                                   
202600     PERFORM IMS-STATUSKONTROLL                                           
202700     .                                                                    
202800     SKIP3                                                                
202900 IMS-GU-ORDD-WDE601 SECTION.                                              
203000     STRING 'WDE601  (IDPRODNR =' W-IDPRODNR-X ')'                        
203100            DELIMITED BY SIZE INTO SSA1                                   
203200     MOVE '  GE' TO GODK-STATUSKODER                                      
203300     CALL CBLTDLI USING GU ORDD-PCB DLI-IOAREA-WDE601 SSA1                
203400     MOVE ORDD-STATUS-CODE TO STATUS-WS                                   
203500     PERFORM IMS-STATUSKONTROLL                                           
203600     .                                                                    
203700     SKIP3                                                                
203800*POSTEN                                                                   
203900 IMS-GHU-WDGX4523-24       SECTION.                                       
204000                                                                          
204100     STRING 'WDR101  (WDGXKEY  =' W-WDGXKEY-4523-X ')'                    
204200         DELIMITED BY SIZE INTO SSA1                                      
204300     STRING 'WDGX4524(KDSEGKEY =' W-WDGXKEY-4524-X ')'                    
204400         DELIMITED BY SIZE INTO SSA2                                      
204500     MOVE '    ' TO GODK-STATUSKODER                                      
204600     CALL CBLTDLI USING GHU 4523-PCB 4524-WDGX4524 SSA1 SSA2              
204700     MOVE 4523-STATUS-CODE TO STATUS-WS                                   
204800     PERFORM IMS-STATUSKONTROLL                                           
204900     .                                                                    
205000     SKIP3                                                                
205100                                                                          
205200 IMS-REPL-WDGX4524        SECTION.                                        
205300     MOVE '    ' TO GODK-STATUSKODER                                      
205400     CALL CBLTDLI USING REPL 4523-PCB 4524-WDGX4524                       
205500     MOVE 4523-STATUS-CODE TO STATUS-WS                                   
205600     PERFORM IMS-STATUSKONTROLL                                           
205700     .                                                                    
205800     SKIP3                                                                
205900                                                                          
206000 IMS-GHU-WDGX4503-04       SECTION.                                       
206100                                                                          
206200     STRING 'WDR401  (WDGXKEY  =' W-4503-WDGXKEY-X ')'                    
206300         DELIMITED BY SIZE INTO SSA1                                      
206400     STRING 'WDGX4504(KY4504   =' W-4504-WDGXKEY-X ')'                    
206500         DELIMITED BY SIZE INTO SSA2                                      
206600     MOVE '  GE' TO GODK-STATUSKODER                                      
206700     CALL CBLTDLI USING GHU 4503-PCB 4504-WDGX4504 SSA1 SSA2              
206800     MOVE 4503-STATUS-CODE TO STATUS-WS                                   
206900     PERFORM IMS-STATUSKONTROLL                                           
207000     .                                                                    
207100     SKIP3                                                                
207200                                                                          
207300 IMS-ISRT-WDGX4503-04       SECTION.                                      
207400                                                                          
207500     STRING 'WDR401  (WDGXKEY  =' W-4503-WDGXKEY-X ')'                    
207600         DELIMITED BY SIZE INTO SSA1                                      
207700     MOVE 'WDGX4504'       TO   SSA2                                      
207800     MOVE '  GE' TO GODK-STATUSKODER                                      
207900     CALL CBLTDLI USING ISRT 4503-PCB 4504-WDGX4504 SSA1 SSA2             
208000     MOVE 4503-STATUS-CODE TO STATUS-WS                                   
208100     PERFORM IMS-STATUSKONTROLL                                           
208200     .                                                                    
208300     SKIP3                                                                
208400                                                                          
208500 IMS-REPL-WDGX4503-04       SECTION.                                      
208600                                                                          
208700*    STRING 'WDR401  (WDGXKEY  =' W-4503-WDGXKEY-X ')'                    
208800*        DELIMITED BY SIZE INTO SSA1                                      
208900*    STRING 'WDGX4504(KY4504   =' W-4504-WDGXKEY-X ')'                    
209000*        DELIMITED BY SIZE INTO SSA2                                      
209100     MOVE '    ' TO GODK-STATUSKODER                                      
209200     CALL CBLTDLI USING REPL 4503-PCB 4504-WDGX4504                       
209300     MOVE 4503-STATUS-CODE TO STATUS-WS                                   
209400     PERFORM IMS-STATUSKONTROLL                                           
209500     .                                                                    
209600     SKIP3                                                                
209700                                                                          
209800 IMS-GU-WDB601    SECTION.                                                
209900     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
210000          DELIMITED BY SIZE INTO SSA1                                     
210100     MOVE '  GE'           TO GODK-STATUSKODER                            
210200     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
210300     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
210400     PERFORM IMS-STATUSKONTROLL                                           
210500     .                                                                    
210600                                                                          
210700 IMS-STATUSKONTROLL        SECTION.                                       
210800                                                                          
210900     SET STATUS-INDX TO 1                                                 
211000     SEARCH GODK-STATUS                                                   
211100       AT END                                                             
211200         CALL FELLOG                                                      
211300       WHEN GODK-STATUS (STATUS-INDX) = STATUS-WS                         
211400         CONTINUE                                                         
212000     END-SEARCH                                                           
220000     .                                                                    
