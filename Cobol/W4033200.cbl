000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W4033200.                                                
000400 AUTHOR.         CAP GEMINI / BOH                                         
000500     DATE-WRITTEN.   FEB   86.                                            
000600                                                                          
000700     REMARKS.                                                             
000800*                                                                         
000900*    FUNKTION.                                                            
001000*    PROGRAMMET ÄR ETT FRÅGE- OCH UPPDATERINGS-PROGRAM                    
001100*    DET UPPDATERAR KOLLIUPPGIFTER SOM TIDIGARE LAGTS                     
001200*    UPP MED W4033100 (OCH W4033700 FÖR BRUTTOVIKTEN).                    
001300*    I DET HÄR PROGRAMMET MÅSTE MAN FÖRST FRÅGA UPP DET                   
001400*    SOM MAN TÄNKER ÄNDRA PÅ BILDEN.                                      
001500*    EFTER ATT PF4 TRYCKTS SKICKAS EN TRANS TILL W4033300,                
001600*    VILKET SKRIVER UT EN 'KOLLIFLAGGA'.                                  
001700*                                                                         
001800*    VAL 'U' VID UTSKRIFT AV KOLLIFLAGGA ELLER FÖLJESEDEL                 
001900*    SKALL INTE GE NÅGON UTSKRIFT, MED ÄR ETT GODKÄNT VAL.                
002000*                                                                         
002100*    INDATA.                                                              
002200*        TRANSAKTION: W4T332                                              
002300*                     W4T332U                                             
002400*        MID:         W4I33201                                            
002500*                                                                         
002600*    UTDATA.                                                              
002700*        TRANSAKTION: W4T333                                              
002800*        MOD:         W4O33201                                            
002900*        WMS DIGITALISATION 2667214 - ADJUST CASE INFO                    
003000     EJECT                                                                
003100 ENVIRONMENT DIVISION.                                                    
003200     SKIP3                                                                
003300 DATA DIVISION.                                                           
003400     EJECT                                                                
003500 WORKING-STORAGE SECTION.                                                 
003600*    -- CHECKED BY WY2000                                                 
003700     SKIP3                                                                
003800 77   PROGRAM-NAMN           VALUE 'W4033200'                             
003900                                 PIC X(8).                                
004000 77  JA                          PIC X(1)    VALUE 'J'.                   
004100 77  FEL                         PIC X(1)    VALUE 'F'.                   
004200 77  FELTEXT                     PIC X(16).                               
004300 77  NEJ                         PIC X(1)    VALUE 'N'.                   
004400 77  RAETT                       PIC X(1)    VALUE 'R'.                   
004500 77  SAKNAS                      PIC X(1)    VALUE 'S'.                   
004600                                                                          
004700 77  INDX                        PIC S9(4)   COMP SYNC.                   
004800 77  WS-MFS-KDMFSFOR             PIC 9       VALUE ZERO.                  
004900 77  WS-IDDC                     PIC X(2)    VALUE SPACE.                 
005000 77  WS-IDDISTR                  PIC X(4)    VALUE SPACE.                 
005100 77  WS-IDKUNDNR                 PIC X(6)    VALUE SPACE.                 
005200 77  WS-IDKOLLI                  PIC X(5)    VALUE SPACE.                 
005300 77  WS-KDKOLLI                  PIC X(8)    VALUE SPACE.                 
005400 77  WS-KDKOLLID                 PIC X       VALUE SPACE.                 
005500 77  WS-KDEMBTYP                 PIC S9      COMP-3 VALUE ZERO.           
005600 77  WS-DIKOLLIL                 PIC S9(5)   COMP-3 VALUE ZERO.           
005700 77  WS-DIKOLLIB                 PIC S9(3)   COMP-3 VALUE ZERO.           
005800 77  WS-DIKOLLIH                 PIC S9(3)   COMP-3 VALUE ZERO.           
005900 77  WS-VKORDBTO-KOLLI           PIC S9(6)V9(1) COMP-3 VALUE ZERO.        
006000 77  WS-VKORDBTO-MID             PIC S9(6)V9(3) COMP-3 VALUE ZERO.        
006100 77  WS-VKARTNTO-KG              PIC S9(6)V9(3).                          
006200 77  WS-VKORDBTO-KG              PIC 9(5).999 VALUE ZERO.                 
006300 77  WS-SPAR-DIKOLLIL            PIC S9(5)   COMP-3 VALUE ZERO.           
006400 77  WS-SPAR-DIKOLLIB            PIC  9(3)          VALUE ZERO.           
006500 77  WS-SPAR-DIKOLLIH            PIC  9(3)          VALUE ZERO.           
006600 77  WS-SPAR-VKORDBTO            PIC  9(6)V9(3) VALUE ZERO.               
006700 77  WS-DEC-IDEDITDATA           PIC  9(11)V9(4) VALUE ZERO.              
006800 77  WS-GAMMAL-VLORDBTO          PIC S9(4)V9(3) COMP-3 VALUE ZERO.        
006900 77  WS-NY-VLORDBTO              PIC S9(4)V9(3) COMP-3 VALUE ZERO.        
007000 77  FILLER                      PIC X(8)    VALUE 'AAAAAAAA'.            
007100 77  WS-IDPRODNR                 PIC X(7)    VALUE SPACE.                 
007200 77  WS-VORD-FLDIRLEV            PIC X(1)    VALUE SPACE.                 
007300 77  WS-KDPRTVAL                 PIC XX      VALUE SPACE.                 
007400 77  WS-PRT-KDSVAR               PIC X       VALUE SPACE.                 
007500 77  WS-KDFRAKT                  PIC S9(3)   COMP-3.                      
007600 77  WS-KDORDKL                  PIC S9      COMP-3.                      
007700 77  WS-SPAR-KVORDRAD-LEVPL      PIC S9(5)   COMP-3.                      
007800 77  PF11-TEST                   PIC X       VALUE 'N'.                   
007900 77  MOD-LAENGD                  PIC S9(4)   VALUE +210 COMP SYNC.        
008000 77  MSG-IX                      PIC S9(4)   COMP SYNC.                   
008100 77  API-SW                      PIC X      VALUE 'N'.                    
008200 77  KDRC-DISPLAY                PIC Z(5).                                
008300 77  ERROR-TEXT                  PIC X(80) VALUE SPACE.                   
008400 77  RKOD-ABEND-MED-DUMP         PIC S9(4)  VALUE +100  COMP SYNC.        
008500 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
008600                                                                          
008700 77  WEIGHT-ISSUE              PIC X(50)                                  
008800       VALUE 'WEIGHT CANNOT BE LESS THAN                   '.             
008900                                                                          
009000 01  SUB-DATA                    PIC X(4000000).                          
009100                                                                          
009200 01  ALL-PLUS.                                                            
009300     03  FILLER                  PIC X(80)  VALUE ALL '+'.                
009400                                                                          
009500 01  WS-IDPRTLST.                                                         
009600     03 WS-SYSTDEL               PIC X(1).                                
009700     03 WS-LISTTYP               PIC X(2).                                
009800     03 WS-DC                    PIC X(2).                                
009900     03 WS-KDPRT                 PIC X(3).                                
010000 01  WS-IDPRTJAP REDEFINES WS-IDPRTLST.                                   
010100     03 WS-LASER-BLANKETT        PIC X(6).                                
010200     03 WS-NDC-JAP-KDPRT         PIC X(3).                                
010300                                                                          
010400 01  WS-KDMATT                   PIC X.                                   
010500     88 US-MEASUREMENT           VALUE 'U'.                               
010600     88 SIS-MEASUREMENT          VALUE 'S'.                               
010700*                                                                         
010800 77  WS-SLINGA-KLAR              PIC X(01).                               
010900     88  SLINGA-KLAR                         VALUE 'J'.                   
011000 77  WS-IDTRANS                  PIC X(4).                                
011100     88  WS-GODKAEND-BILD                    VALUE '4331' '4332'          
011200                                                   '4333' '4334'          
011300                                                   '4335' '4336'          
011400                                                   '4338'.                
011500     EJECT                                                                
011600 01  SPAR-AREOR.                                                          
011700     03  SPAR-IDTRPTNR           PIC S9(3) COMP-3 VALUE ZERO.             
011800     03  SPAR-ADCLGEO            PIC X(5)         VALUE SPACE.            
011900     03  SPAR-ADFLOMR            PIC S9(3) COMP-3 VALUE ZERO.             
012000     03  SPAR-ADRUTNIV           PIC S9(3) COMP-3 VALUE ZERO.             
012100     03  SPAR-DIHMODUL           PIC S9(3) COMP-3 VALUE ZERO.             
012200     03  SPAR-DIDMODUL           PIC S9(3) COMP-3 VALUE ZERO.             
012300     03  SPAR-ADVMODUL           PIC S9(3) COMP-3 VALUE ZERO.             
012400     03  SPAR-ADHMODUL           PIC S9(3) COMP-3 VALUE ZERO.             
012500     03  SPAR-FLUTLAST           PIC X            VALUE SPACE.            
012600     03  SPAR-IDDC-CROSS         PIC X(2)         VALUE SPACE.            
012700     SKIP2                                                                
012800 01  DYNAMISKA-SUBPROGRAM.                                                
012900     03  CBLTDLI                 PIC X(8)  VALUE 'CBLTDLI '.              
013000     03  FELLOG                  PIC X(8)  VALUE 'FELLOG  '.              
013100     03  WDECEDIT                PIC X(8)  VALUE 'WDECEDIT'.              
013200     03  W006PRT                 PIC X(8)  VALUE 'W006PRT '.              
013300     03  W005INIT                PIC X(8)  VALUE 'W005INIT'.              
013400     03  W403PLAT                PIC X(8)  VALUE 'W403PLAT'.              
013500     03  W403TMS1                PIC X(8)  VALUE 'W403TMS1'.              
013600     03  WWOMVAND                PIC X(8)  VALUE 'WWOMVAND'.              
013700     03  WZ01AUTH                PIC X(8)  VALUE 'WZ01AUTH'.              
013800     03  WMSGCONV                PIC X(8)  VALUE 'WMSGCONV'.              
013900     03  WZ01SUB                 PIC X(8)  VALUE 'WZ01SUB '.              
014000     SKIP2                                                                
014100*    --- AREOR TILL DYNAMISKA SUBPROGRAM                                  
014200*                                                                         
014300 01  FILLER                      PIC X(16)  VALUE 'W005INIT '.            
014400*01 -COPY WMSGINIT                                                        
014500     SKIP2                                                                
014600 01  FILLER                      PIC X(16)  VALUE 'W006PRT  '.            
014700*   -COPY W006PRT                                                         
014800     SKIP2                                                                
014900 01  FILLER                      PIC X(16)  VALUE 'W403PLAT '.            
015000*01 -COPY W403PLAT                                                        
015100     SKIP2                                                                
063500*TMS PACKNING INFO                                                        
063600 01  FILLER                      PIC X(16)  VALUE 'W403TMS1 '.            
063700*   -COPY W403TMS1                                                        
063800     SKIP2                                                                
063900 01  FILLER                      PIC X(16)  VALUE 'WWOMVAND '.            
064000*   -COPY WWOMVAND                                                        
064100     SKIP2                                                                
064200*                                                                         
064300 01  FILLER                      PIC X(16)   VALUE 'WZ01AUTH   '.         
064400*01  -COPY WZ01AUTH                                                       
064500*                                                                         
064600 01  FILLER                      PIC X(16)   VALUE 'WMSGCONV'.            
064700*01  -COPY WMSGCONV                                                       
064800*                                                                         
064900     SKIP3                                                                
065000*01  -COPY WZ01SUB                                                        
065100     EJECT                                                                
065200 01  FILLER                      PIC X(16)   VALUE 'REQU-AREA'.           
065300     SKIP3                                                                
065400 01  REQU-AREA.                                                           
065500*    03  -COPY WZ01REQ2                                                   
065600*    03  -COPY W40332I1                                                   
065700     EJECT                                                                
065800 01  FILLER                      PIC X(16)   VALUE 'RESP-AREA'.           
065900     SKIP3                                                                
066000 01  RESP-AREA.                                                           
066100*    03  -COPY WZ01RES2                                                   
066200*    03  -COPY W40332O1                                                   
066300     EJECT                                                                
066400*                                                                         
066500 01  WS-IDKUNDRF.                                                         
066600                                                                          
066700     03  WS-IDORDNR              PIC X(5).                                
066800     03  FILLER                  PIC X(5)  VALUE SPACE.                   
066900                                                                          
067000                                                                          
067100 01  WS-KEYS-TEST                PIC X     VALUE 'N'.                     
067200     88  KEYS-OK                           VALUE 'J'.                     
067300     88  KEYS-WRONG                        VALUE 'N'.                     
067400                                                                          
067500 01  WS-FEL-FUNNET               PIC X     VALUE 'N'.                     
067600                                                                          
067700     88  FEL-FUNNET                        VALUE 'J'.                     
067800     88  FEL-EJ-FUNNET                     VALUE 'N'.                     
067900     SKIP3                                                                
068000                                                                          
068100                                                                          
068200                                                                          
068300 01  WS-PFK                      PIC X     VALUE ' '.                     
068400                                                                          
068500     88  PF4                               VALUE '4'.                     
068600     SKIP3                                                                
068700 01  WS-TEXTER.                                                           
068800                                                                          
068900     03  WS-ADRESS-TEXT         PIC X(7)  VALUE 'ADRESS:'.                
069000     EJECT                                                                
069100 01  NYCKLAR-TILL-DLI.                                                    
069200                                                                          
069300     03  W-K501-KDKOLLI-X.                                                
069400                                                                          
069500         05  W-K501-KDKOLLI      PIC X(8).                                
069600                                                                          
069700     03  W-E4A1-WDE4ASEQ-X.                                               
069800                                                                          
069900         05  W-E4A1-IDDISTR      PIC S9(5)   COMP-3.                      
070000         05  W-E4A1-IDKUNDNR     PIC S9(7)   COMP-3.                      
070100         05  W-E4A1-IDKUNDRF.                                             
070200             07  W-E4A1-IDORDNR  PIC X(5).                                
070300             07  FILLER          PIC X(5)    VALUE SPACE.                 
070400                                                                          
070500     03  W-E401-WDE4KEY-X.                                                
070600                                                                          
070700         05  W-E401-IDDISTR      PIC S9(5)   COMP-3.                      
070800         05  W-E401-IDKUNDNR     PIC S9(7)   COMP-3.                      
070900         05  W-E401-IDKUNDRF.                                             
071000             07  W-E401-IDORDNR  PIC X(5).                                
071100             07  FILLER          PIC X(5)    VALUE SPACE.                 
071200         05  W-E401-IDPRODNR     PIC S9(7)   COMP-3.                      
071300         05  W-E401-IDPLKLST     PIC S9(3)   COMP-3.                      
071400                                                                          
071500     03  W-WDE4FSEQ-X.                                                    
071600         05  W-IDPRODNR-F        PIC S9(7)   VALUE ZERO  COMP-3.          
071700         05  W-IDKOLLI-F         PIC S9(5)   VALUE ZERO  COMP-3.          
071800                                                                          
071900   03    W-WDE421-IDKOLLI-X.                                              
072000     05    W-421-IDPRODNR        PIC S9(7)   VALUE ZERO  COMP-3.          
072100     05    W-421-IDKOLLI         PIC S9(5)   VALUE ZERO  COMP-3.          
072200*                                                                         
072300     03  W-E601-IDPRODNR-X.                                               
072400                                                                          
072500         05  W-E601-IDPRODNR     PIC S9(7)   COMP-3.                      
072600                                                                          
072700     03  W-E611-IDKOLLI-X.                                                
072800                                                                          
072900         05  W-E611-IDKOLLI      PIC S9(5)   COMP-3.                      
073000                                                                          
073100     EJECT                                                                
073200 01  MEDDELANDE.                                                          
073300                                                                          
073400     03  FEL-1.                                                           
073500         05  FILLER              PIC X(40)   VALUE                        
073600             '726 KOLLIKOD SAKNAS                     '.                  
073700         05  FILLER              PIC X(40)   VALUE                        
073800             '726 CASE CODE MISSING                   '.                  
073900     03  FEL-726 REDEFINES FEL-1 OCCURS 2 PIC X(40).                      
074000                                                                          
074100     03  FEL-2.                                                           
074200         05  FILLER              PIC X(40)   VALUE                        
074300             '724 NOLL FÅR EJ ANGES                   '.                  
074400         05  FILLER              PIC X(40)   VALUE                        
074500             '724 ZERO NOT ALLOWED                    '.                  
074600     03  FEL-724 REDEFINES FEL-2 OCCURS 2 PIC X(40).                      
074700                                                                          
074800     03  FEL-3.                                                           
074900         05  FILLER              PIC X(40)   VALUE                        
075000             '748 UPPLYSTA FÄLT FEL                   '.                  
075100         05  FILLER              PIC X(40)   VALUE                        
075200             '748 HIGHLITH FIELDS WRONG               '.                  
075300     03  FEL-748 REDEFINES FEL-3 OCCURS 2 PIC X(40).                      
075400                                                                          
075500     03  FEL-4.                                                           
075600         05  FILLER              PIC X(40)   VALUE                        
075700             '758 KOLLI SAKNAS                        '.                  
075800         05  FILLER              PIC X(40)   VALUE                        
075900             '758 CASE MISSING                        '.                  
076000     03  FEL-758 REDEFINES FEL-4 OCCURS 2 PIC X(40).                      
076100*                                                                         
076200     03  FEL-5.                                                           
076300         05  FILLER              PIC X(40)   VALUE                        
076400             '7621ANGIVET DISTR, KUND, ORDER FINNS EJ '.                  
076500         05  FILLER              PIC X(40)   VALUE                        
076600             '7621WRONG  DISTR, CUST,  ORDER          '.                  
076700     03  FEL-7621 REDEFINES FEL-5 OCCURS 2 PIC X(40).                     
076800*                                                                         
076900     03  FEL-52.                                                          
077000         05  FILLER              PIC X(40)   VALUE                        
077100             '7622ANGIVET DISTR, KUND, ORDER FINNS EJ '.                  
077200         05  FILLER              PIC X(40)   VALUE                        
077300             '7622WRONG  DISTR, CUST,  ORDER          '.                  
077400     03  FEL-7622 REDEFINES FEL-52 OCCURS 2 PIC X(40).                    
077500*                                                                         
077600     03  FEL-6.                                                           
077700         05  FILLER              PIC X(40)   VALUE                        
077800             '764 PLATS FINNS EJ                      '.                  
077900         05  FILLER              PIC X(40)   VALUE                        
078000             '764 LOCATION MISSING                    '.                  
078100     03  FEL-764 REDEFINES FEL-6 OCCURS 2 PIC X(40).                      
078200                                                                          
078300     03  FEL-7.                                                           
078400         05  FILLER              PIC X(40)   VALUE                        
078500             '749 FEL NYCKEL                          '.                  
078600         05  FILLER              PIC X(40)   VALUE                        
078700             '749 WRONG KEY                           '.                  
078800     03  FEL-749 REDEFINES FEL-7 OCCURS 2 PIC X(40).                      
078900                                                                          
079000     03  FEL-8.                                                           
079100         05  FILLER              PIC X(40)   VALUE                        
079200             '788 FRÅGA SKALL STÄLLAS INNAN PF4/PF11  '.                  
079300         05  FILLER              PIC X(40)   VALUE                        
079400             '788 PF4/PF11 AND NEW KEYS NOT ALLOWED   '.                  
079500     03  FEL-788 REDEFINES FEL-8 OCCURS 2 PIC X(40).                      
079600                                                                          
079700     03  FEL-9.                                                           
079800         05  FILLER              PIC X(40)   VALUE                        
079900             '789 INGET ÄNDRAT, UPPDATERING EJ UTFÖRD '.                  
080000         05  FILLER              PIC X(40)   VALUE                        
080100             '789 NOTHING CHANGED, NO UPDATING MADE   '.                  
080200     03  FEL-789 REDEFINES FEL-9 OCCURS 2 PIC X(40).                      
080300                                                                          
080400     03  FEL-10.                                                          
080500         05  FILLER              PIC X(40)   VALUE                        
080600             '772 FELAKTIG PRINTER                    '.                  
080700         05  FILLER              PIC X(40)   VALUE                        
080800             '772 WRONG PRINTER                       '.                  
080900     03  FEL-772 REDEFINES FEL-10 OCCURS 2 PIC X(40).                     
081000                                                                          
081100     03  FEL-11.                                                          
081200         05  FILLER              PIC X(40)   VALUE                        
081300             '717 KOLLI EJ TIDIGARE RAPPORTERAT       '.                  
081400         05  FILLER              PIC X(40)   VALUE                        
081500             '717 CASE NOT PACKED                     '.                  
081600     03  FEL-717 REDEFINES FEL-11 OCCURS 2 PIC X(40).                     
081700                                                                          
081800     03  FEL-12.                                                          
081900         05  FILLER              PIC X(40)   VALUE                        
082000             '812 TRYCK PF11 FÖR UPPDATERING          '.                  
082100         05  FILLER              PIC X(40)   VALUE                        
082200             '812 PRESS PF11 FOR UPDATE               '.                  
082300     03  FEL-812 REDEFINES FEL-12 OCCURS 2 PIC X(40).                     
082400                                                                          
082500     03  FEL-13.                                                          
082600         05  FILLER              PIC X(40)   VALUE                        
082700             '838 VIKT SKALL EJ ANGES FÖR SVERIGE     '.                  
082800         05  FILLER              PIC X(40)   VALUE                        
082900             '838 WEIGHT NOT ALLOWED FOR SWEDISH DIST.'.                  
083000     03  FEL-838 REDEFINES FEL-13 OCCURS 2 PIC X(40).                     
083100                                                                          
083200     03  FEL-14.                                                          
083300         05  FILLER              PIC X(40)   VALUE                        
083400             '721 KOLLIT REDAN RELEASAT               '.                  
083500         05  FILLER              PIC X(40)   VALUE                        
083600             '721 CASE ALREADY RELEASED               '.                  
083700     03  FEL-721 REDEFINES FEL-14 OCCURS 2 PIC X(40).                     
083800                                                                          
083900     03  FEL-15.                                                          
084000         05  FILLER              PIC X(40)   VALUE                        
084100             '839 EJ KOLLIFLAGGA FÖR SVERIGE          '.                  
084200         05  FILLER              PIC X(40)   VALUE                        
084300             '839 NO CASE LABEL FOR SWEDISH DISTRICTS '.                  
084400     03  FEL-839 REDEFINES FEL-15 OCCURS 2 PIC X(40).                     
084500                                                                          
084600     03  FEL-16.                                                          
084700         05  FILLER              PIC X(40)   VALUE                        
084800             '773 FELAKTIG PRINTER                    '.                  
084900         05  FILLER              PIC X(40)   VALUE                        
085000             '773 WRONG PRINTER                       '.                  
085100     03  FEL-773 REDEFINES FEL-16 OCCURS 2 PIC X(40).                     
085200                                                                          
085300     03  MED-1.                                                           
085400         05  FILLER              PIC X(40)   VALUE                        
085500             'UPPDATERING UTFÖRD                      '.                  
085600         05  FILLER              PIC X(40)   VALUE                        
085700             'UPDATING OK                             '.                  
085800     03  MED-756 REDEFINES MED-1 OCCURS 2 PIC X(40).                      
085900                                                                          
086000     03  MED-2.                                                           
086100         05  FILLER              PIC X(40)   VALUE                        
086200             'KOLLIFLAGGA UTSKRIVEN                   '.                  
086300         05  FILLER              PIC X(40)   VALUE                        
086400             'CASE LABEL PRINTED                      '.                  
086500     03  MED-780 REDEFINES MED-2 OCCURS 2 PIC X(40).                      
086600                                                                          
086700     03 ERR-UNAUTHORIZED         PIC X(3)    VALUE '00A'.                 
086800     03 BAD-REQUEST              PIC X(3)    VALUE '400'.                 
086900     03 SYS-ERROR                PIC X(3)    VALUE '099'.                 
087000     03 INP-ERROR                PIC X(3)    VALUE '100'.                 
087100                                                                          
087200     EJECT                                                                
087300 01  TEST-IDDISTR        PIC S9(5)  COMP-3.                               
087400                                                                          
087500*01  FILLER      -COPY WWDIST03    -RED TEST-IDDISTR.                     
087600     EJECT                                                                
087700*01  -COPY WDECAREA                                                       
087800     EJECT                                                                
087900 01    FILLER              PIC X(16)   VALUE 'MFS-WS'.                    
088000                                                                          
088100                                                                          
088200                                                                          
088300 01    FILLER              PIC X(16)   VALUE 'MID W4I332 MID'.            
088400                                                                          
088500                                                                          
088600*01  MID -COPY W4I33201.                                                  
088700     EJECT                                                                
088800*01  -COPY WMSGAREA                                                       
088900     EJECT                                                                
089000*    03  MOD -COPY W4O33201  -RED MSG-AREA.                               
089100     EJECT                                                                
089200 01  4333-MID-IO-AREA.                                                    
089300                                                                          
089400     03  4333-MID-LL             PIC S9(4)   COMP SYNC.                   
089500     03  4333-MID-Z1             PIC X.                                   
089600     03  4333-MID-Z2             PIC X.                                   
089700     03  4333-MID-TRANSKOD       PIC X(8)    VALUE 'W4T333  '.            
089800     03  4333-MID-IDTRANS        PIC X(4)    VALUE '433B'.                
089900     03  4333-MID-KDMFSFOR       PIC X.                                   
090000*    03  MID -COPY W4I33301    -PRE 4333-.                                
090100     EJECT                                                                
090200*01  -COPY WMFSAREA                                                       
090300     EJECT                                                                
090400 01  IMS-WS.                                                              
090500     03  FILLER                  PIC X(16)   VALUE 'IMS-WS     '.         
090600     SKIP3                                                                
090700*                        **** STATUS-KOD FRÅN IMS                         
090800     03  KUNDORDER-SEK-WS        PIC X(2).                                
090900         88  KUNDORDER-SEK-FINNS             VALUE '  '.                  
091000         88  KUNDORDER-SEK-SAKNAS            VALUE 'GE' 'GB'.             
091100     03  STATUS-WS               PIC X(2).                                
091200         88  SEGMENT-FINNS                   VALUE '  '.                  
091300         88  SEGMENT-SAKNAS                  VALUE 'GE'.                  
091400     SKIP3                                                                
091500     03  GODK-STATUSKODER.                                                
091600         05  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC X(2).          
091700     SKIP3                                                                
091800 01  SSA1                        PIC X(198).                              
091900 01  SSA2                        PIC X(64).                               
092000 01  SSA3                        PIC X(64).                               
092100     EJECT                                                                
092200*                            IMS FUNKTIONSKODER                           
092300*01  -COPY W0003                                                          
092400     EJECT                                                                
092500*                            DLI INPUT-OUTPUT AREA                        
092600 01  FILLER                      PIC X(16) VALUE 'DLI-IO-AREA2'.          
092700 01  DLI-IO-AREA2.                                                        
092800     03  IO-AREA2                PIC X(144)  VALUE SPACE.                 
092900                                                                          
093000*    03  WDE401   -COPY WDE401     -RED IO-AREA2.                         
093100     SKIP2                                                                
093200 01  FILLER                      PIC X(16) VALUE 'DLI-IO-AREA'.           
093300 01  DLI-IO-AREA.                                                         
093400     03  IO-AREA                 PIC X(300)  VALUE SPACE.                 
093500*    03  WLEMBB01 -COPY WDK501     -RED IO-AREA.                          
093600     SKIP2                                                                
093700 01  FILLER                      PIC X(16) VALUE 'WDE601-AREA'.           
093800*01  -COPY WDE601                                                         
093900     SKIP2                                                                
094000 01  FILLER                      PIC X(16) VALUE 'WDE611-AREA'.           
094100*01  -COPY WDE611                                                         
094200 01    FILLER                    PIC X(16) VALUE 'DLI-IO-E421'.           
094300 01  DLI-IO-WDE411-21.                                                    
094400     SKIP2                                                                
094500   03    -COPY WDE411                                                     
094600   03    -COPY WDE421                                                     
094700                                                                          
094800 01  FILLER               PIC X(16)   VALUE 'DLI-IO-WDR501'.              
094900 01  DLI-IO-WDR501.                                                       
095000*    03   -COPY WDGX01                                                    
095100 01  FILLER               PIC X(16)   VALUE 'DLI-IO-WDGX0102'.            
095200 01  DLI-IO-WDGX0102.                                                     
095300*    03   -COPY WDGX0102                                                  
095400     SKIP2                                                                
095500 LINKAGE SECTION.                                                         
095600*01  -COPY W0009     -PRE MSG-                                            
095700     SKIP2                                                                
095800*01  -COPY W0009     -PRE ALT-                                            
095900     SKIP2                                                                
096000*                                                                         
096001 01  TMS-CRE-PCB                 PIC X.                                   
096002 01  TMS-DEL-PCB                 PIC X.                                   
096003 01  ATAB-PCB                    PIC X.                                   
096004*                                                                         
096005*01  -COPY W0008     -PRE USEA-                                           
096006     05  FILLER                  PIC X.                                   
096007     SKIP2                                                                
096008*01  -COPY W0008     -PRE WDE4-                                           
096009     05  FILLER                  PIC X.                                   
096010     SKIP2                                                                
096020*01  -COPY W0008     -PRE WDE4A-                                          
096030     05  FILLER                  PIC X.                                   
096040     SKIP2                                                                
096050*01  -COPY W0008     -PRE WDE44-                                          
096060     05  FILLER                  PIC X.                                   
096070     SKIP2                                                                
096080*01  -COPY W0008     -PRE WDE6-                                           
096090     05  FILLER                  PIC X.                                   
096100     SKIP2                                                                
096200*01  -COPY W0008     -PRE EMBB-                                           
096300     05  FILLER                  PIC X.                                   
096400     SKIP2                                                                
096500*01  -COPY W0008     -PRE PLATS-DM-                                       
096600     05  FILLER                  PIC X.                                   
096700     SKIP2                                                                
096800*01  -COPY W0008     -PRE PLATS-DN-                                       
096900     05  FILLER                  PIC X.                                   
097000     SKIP2                                                                
097100*01  -COPY W0008     -PRE PLATS-DP-                                       
097200     05  FILLER                  PIC X.                                   
097300     SKIP2                                                                
097400*01  -COPY W0008     -PRE PLATS-DO-                                       
097500     05  FILLER                  PIC X.                                   
097600     SKIP2                                                                
097700*01  -COPY W0008     -PRE PLATS-WDE6C-                                    
097800     05  FILLER                  PIC X.                                   
097900     SKIP2                                                                
098000*01  -COPY W0008     -PRE PLATS-GMTC-                                     
098100     05  FILLER                  PIC X.                                   
098200     SKIP2                                                                
098300*01  -COPY W0008     -PRE PLATS-WDB6-                                     
098400     05  FILLER                  PIC X.                                   
098500     SKIP2                                                                
098501*                                                                         
098502 01  TMS-1165-PCB                PIC X.                                   
098503 01  TMS-4141-PCB                PIC X.                                   
098504 01  TMS-WDB2-PCB                PIC X.                                   
098505 01  TMS-WDB6-PCB                PIC X.                                   
098506 01  TMS-WDD3-PCB                PIC X.                                   
098507 01  TMS-WDB1-PCB                PIC X.                                   
098508 01  TMS-WDE4A-PCB               PIC X.                                   
098509 01  TMS-WDE4F-PCB               PIC X.                                   
098510 01  TMS-WDQ2-PCB                PIC X.                                   
098511 01  TMS-WDQ3-PCB                PIC X.                                   
098512 01  TMS-WDK6-PCB                PIC X.                                   
098513 01  TMS-WDE6-PCB                PIC X.                                   
098514 01  TMS-WDK5-PCB                PIC X.                                   
098515 01  TMS-WDQ2C-PCB               PIC X.                                   
098516     EJECT                                                                
098517                                                                          
098518 PROCEDURE DIVISION USING MSG-PCB ALT-PCB TMS-CRE-PCB TMS-DEL-PCB         
098519                          ATAB-PCB USEA-PCB                               
098520                          WDE4-PCB WDE4A-PCB WDE44-PCB                    
098530                          WDE6-PCB EMBB-PCB                               
098540                          PLATS-DM-PCB PLATS-DN-PCB                       
098550                          PLATS-DP-PCB PLATS-DO-PCB                       
098560                          PLATS-WDE6C-PCB PLATS-GMTC-PCB                  
098570                          PLATS-WDB6-PCB                                  
098571                          TMS-1165-PCB TMS-4141-PCB TMS-WDB2-PCB          
098572                          TMS-WDB6-PCB TMS-WDD3-PCB TMS-WDB1-PCB          
098573                          TMS-WDE4A-PCB TMS-WDE4F-PCB                     
098574                          TMS-WDQ2-PCB TMS-WDQ3-PCB TMS-WDK6-PCB          
098575                          TMS-WDE6-PCB TMS-WDK5-PCB TMS-WDQ2C-PCB.        
098576                                                                          
098577     ENTRY 'DLITCBL' USING MSG-PCB ALT-PCB TMS-CRE-PCB                    
098578                           TMS-DEL-PCB ATAB-PCB USEA-PCB                  
098579                           WDE4-PCB WDE4A-PCB WDE44-PCB                   
098580                           WDE6-PCB EMBB-PCB                              
098590                           PLATS-DM-PCB PLATS-DN-PCB                      
098600                           PLATS-DP-PCB PLATS-DO-PCB                      
098700                           PLATS-WDE6C-PCB PLATS-GMTC-PCB                 
098800                           PLATS-WDB6-PCB                                 
098801                           TMS-1165-PCB TMS-4141-PCB TMS-WDB2-PCB         
098802                           TMS-WDB6-PCB TMS-WDD3-PCB TMS-WDB1-PCB         
098803                           TMS-WDE4A-PCB TMS-WDE4F-PCB                    
098804                           TMS-WDQ2-PCB TMS-WDQ3-PCB TMS-WDK6-PCB         
098805                          TMS-WDE6-PCB TMS-WDK5-PCB TMS-WDQ2C-PCB.        
098806                                                                          
098807     PERFORM S18-FETCH-REQUEST-ARGUMENT                                   
098808     IF SUB-KDRC = 0                                                      
098809                                                                          
098810         MOVE NEJ       TO WS-FEL-FUNNET                                  
098820         PERFORM A-INITIERA                                               
098830         IF WS-GODKAEND-BILD                                              
098840                                                                          
098850          IF  WS-IDDISTR  NUMERIC                                         
098860          AND WS-IDKUNDNR NUMERIC                                         
098870          AND WS-IDORDNR  NUMERIC                                         
098880          AND WS-IDKOLLI  NUMERIC                                         
098890          AND KEYS-OK                                                     
098900           IF MFS-UPDATE                                                  
099000           OR PF4                                                         
099100           OR REQU-UPDATE                                                 
099200                                                                          
099300             IF (MID-IDDISTR-IN  = ALL '+'                                
099400             AND MID-IDKUNDNR-IN = ALL '+'                                
099500             AND MID-IDORDNR-IN  = ALL '+'                                
099600             AND MID-IDKOLLI-IN  = ALL '+'                                
099700             AND SUB-KDTRANS(1:6) = 'W4T332')                             
099800             OR (SUB-KDTRANS(1:6) = 'W4A332')                             
099900                                                                          
100000                 IF MFS-UPDATE OR                                         
100100                    REQU-UPDATE                                           
100200                     PERFORM B-UPPDATERING                                
100300                 ELSE                                                     
100400                     MOVE WS-IDDISTR TO TEST-IDDISTR                      
100500                     IF DIST03-SVERIGE                                    
100600                       IF FEL-EJ-FUNNET                                   
100700                         MOVE '839'         TO RESP-IDMSG-ERROR           
100800                         MOVE FEL-839(INDX) TO MOD-TEMFSFEL               
100900                                             RESP-IDMSG-ERROR-LINE        
101000                         MOVE JA            TO WS-FEL-FUNNET              
101100                         MOVE BAD-REQUEST   TO RESP-IDMFSINF              
101200                         PERFORM S02-SAETT-ROER-EJ-FAELT                  
101300                       END-IF                                             
101400                     ELSE                                                 
101500                       PERFORM C-PRINTA-KOLLIFLAGGA                       
101600                     END-IF                                               
101700                 END-IF                                                   
101800             ELSE                                                         
101900                 IF FEL-EJ-FUNNET                                         
102000                     MOVE FEL-788(INDX) TO MOD-TEMFSFEL                   
102100                     MOVE JA            TO WS-FEL-FUNNET                  
102200                 END-IF                                                   
102300             END-IF                                                       
102400           ELSE                                                           
102500             IF MFS-IDTRANS = '4332'                                      
102600               PERFORM E-TEST-OM-MAN-MENAT-PF11                           
102700               IF PF11-TEST = NEJ                                         
102800                 PERFORM D-FRAAGA-STAELLD                                 
102900               ELSE                                                       
103000                 IF FEL-EJ-FUNNET                                         
103100                   MOVE FEL-812(INDX) TO MOD-TEMFSFEL                     
103200                   MOVE JA            TO WS-FEL-FUNNET                    
103300                   PERFORM S02-SAETT-ROER-EJ-FAELT                        
103400                 END-IF                                                   
103500               END-IF                                                     
103600             ELSE                                                         
103700               PERFORM D-FRAAGA-STAELLD                                   
103800             END-IF                                                       
103900           END-IF                                                         
104000          ELSE                                                            
104100              IF FEL-EJ-FUNNET                                            
104200                  MOVE FEL-749(INDX)   TO MOD-TEMFSFEL                    
104300                  MOVE JA              TO WS-FEL-FUNNET                   
104400                  PERFORM S02-SAETT-ROER-EJ-FAELT                         
104500                  PERFORM S03-RENSA-UTFAELT                               
104600              END-IF                                                      
104700          END-IF                                                          
104800         ELSE                                                             
104900           PERFORM F-RENSA-NYCKLAR                                        
105000         END-IF                                                           
105100                                                                          
105200         IF API-SW ='Y'                                                   
105300           PERFORM S20-MSG-CONV                                           
105400*          MOVE RESP-IDMSG-ERROR      TO RESP-IDMSG   (1)                 
105500*          MOVE RESP-IDMSG-ERROR-LINE TO RESP-MESSAGE (1)                 
105600           PERFORM S19-RETURN-RESPONSE                                    
105700         ELSE                                                             
105800           PERFORM   IMS-INSERT-MSG                                       
105900         END-IF                                                           
106000                                                                          
106100     END-IF                                                               
106200                                                                          
106300     MOVE ZERO TO RETURN-CODE                                             
106400                                                                          
106500     GOBACK                                                               
106600     .                                                                    
106700     EJECT                                                                
106800 A-INITIERA SECTION.                                                      
106900                                                                          
107000     INITIALIZE TMS-W403TMS1                                              
107100                                                                          
107200     MOVE ALL '+'                TO RESP-W40332O1                         
107300     MOVE SPACE                  TO RESP-IDMSG-ERROR                      
107400                                    RESP-IDMSG-INFO                       
107500                                    RESP-IDELMT-ERROR                     
107600                                    RESP-IDMSG-ERROR-LINE                 
107700                                                                          
107800*  IF CALL IS FROM CLASSIC SCREEN                                         
107900     IF SUB-KDTRANS(1:6) = 'W4T332'                                       
108000        MOVE SUB-KDTRANS                TO MSG-KDTRANS-1                  
108100        MOVE SUB-DATA                   TO MSG-AREA(9:1925)               
108200        IF MSG-DUBBLA-TRANSKODER                                          
108300            MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W4I33201            
108400            MOVE MSG-IDTRANS-2 TO MFS-IDTRANS                             
108500            MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR WS-MFS-KDMFSFOR           
108600            MOVE MSG-KDTRTYP          TO MFS-KDTRTYP                      
108700            MOVE MSG-IDPFK TO MFS-IDPFK WS-PFK                            
108800        ELSE                                                              
108900            MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W4I33201             
109000            MOVE MSG-IDTRANS-1 TO MFS-IDTRANS                             
109100            MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR WS-MFS-KDMFSFOR           
109200            MOVE ' ' TO MFS-KDTRTYP        MFS-IDPFK WS-PFK               
109300        END-IF                                                            
109400        MOVE MFS-IDTRANS TO WS-IDTRANS                                    
109500        MOVE ALL '+'             TO MSGI-WMSGINIT                         
109600        MOVE '001'               TO MSGI-KDCALL                           
109700        MOVE MSG-SIGNON-USERID   TO MSGI-IDUSER                           
109800        MOVE '4332'              TO MSGI-IDTRANS                          
109900        MOVE MSG-LTERM-NAME      TO MSGI-IDLTERM-USER                     
110000        CALL W005INIT         USING MSGI-WMSGINIT                         
110100                                    USEA-PCB                              
110200        MOVE MSGI-KDMATT         TO WS-KDMATT                             
110300        MOVE MSGI-IDDC           TO WS-IDDC                               
110400        IF MSGI-IDLAND-SPR = 'GB'                                         
110500          MOVE +2                TO INDX                                  
110600        ELSE                                                              
110700          MOVE +1                TO INDX                                  
110800        END-IF                                                            
110900     ELSE                                                                 
111000*  IF CALL IS FROM API                                                    
111100        MOVE SUB-DATA(1:SUB-KVDLEN)    TO REQU-AREA                       
111200        MOVE 001                       TO AUTH-KDCALL                     
111300        CALL WZ01AUTH               USING AUTH-WZ01AUTH                   
111400                                          REQU-WZ01REQ2                   
111500        IF AUTH-KDRC = 0                                                  
111600            MOVE REQU-IDDISTR-KEY      TO MID-IDDISTR-IN                  
111700            MOVE REQU-IDKUNDNR-KEY     TO MID-IDKUNDNR-IN                 
111800            MOVE REQU-IDORDNR-KEY      TO MID-IDORDNR-IN                  
111900            MOVE REQU-IDKOLLI-KEY      TO MID-IDKOLLI-IN                  
112000            MOVE ALL '+'               TO MID-KDPRTVAL-IN                 
112100            MOVE FUNCTION UPPER-CASE (REQU-KDPGMACT)                      
112200                                       TO REQU-KDPGMACT                   
112300            MOVE FUNCTION UPPER-CASE (REQU-IDDC-KEY)                      
112400                                       TO WS-IDDC                         
112500            MOVE FUNCTION UPPER-CASE (REQU-KDMATT)                        
112600                                       TO WS-KDMATT                       
112700            MOVE FUNCTION UPPER-CASE (REQU-KDKOLLI)                       
112800                                       TO REQU-KDKOLLI                    
112900            MOVE 'Y'                   TO API-SW                          
113000            MOVE '4332'                TO WS-IDTRANS                      
113100            MOVE +2                    TO INDX                            
113200        ELSE                                                              
113300          IF AUTH-KDRC = 4                                                
113400             MOVE BAD-REQUEST TO RESP-IDMSG-ERROR                         
113500                                                                          
113600             MOVE AUTH-KDRC TO KDRC-DISPLAY                               
113700             STRING 'WZ01AUTH GETARG ERROR RC=' KDRC-DISPLAY              
113800             DELIMITED BY SIZE INTO ERROR-TEXT                            
113900             CALL FELLOG USING RKOD-ABEND-WITH-DUMP                       
114000          END-IF                                                          
114100        END-IF                                                            
114200     END-IF                                                               
114300                                                                          
114400     MOVE NEJ               TO WS-VORD-FLDIRLEV                           
114500                                                                          
114600     MOVE JA                              TO WS-KEYS-TEST                 
114700                                                                          
114800     IF MID-IDDISTR-IN = ALL '+'                                          
114900         MOVE MID-IDDISTR-UT TO WS-IDDISTR                                
115000     ELSE                                                                 
115100         MOVE MID-IDDISTR-IN TO WS-IDDISTR                                
115200     END-IF                                                               
115300     INSPECT WS-IDDISTR REPLACING ALL  SPACE BY ZERO                      
115400                                                                          
115500     IF MID-IDKUNDNR-IN = ALL '+'                                         
115600         MOVE MID-IDKUNDNR-UT TO WS-IDKUNDNR                              
115700     ELSE                                                                 
115800         MOVE MID-IDKUNDNR-IN TO WS-IDKUNDNR                              
115900     END-IF                                                               
116000     INSPECT WS-IDKUNDNR REPLACING ALL  SPACE BY ZERO                     
116100                                                                          
116200     IF MID-IDORDNR-IN = ALL '+'                                          
116300         MOVE MID-IDORDNR-UT TO WS-IDORDNR                                
116400         INSPECT WS-IDORDNR REPLACING ALL  SPACE BY ZERO                  
116500     ELSE                                                                 
116600         MOVE MID-IDORDNR-IN TO WS-IDORDNR                                
116700     END-IF                                                               
116800     INSPECT WS-IDORDNR REPLACING ALL  SPACE BY ZERO                      
116900                                                                          
117000     IF MID-IDKOLLI-IN = ALL '+'                                          
117100         MOVE MID-IDKOLLI-UT TO WS-IDKOLLI                                
117200     ELSE                                                                 
117300         MOVE MID-IDKOLLI-IN TO WS-IDKOLLI                                
117400     END-IF                                                               
117500     INSPECT WS-IDKOLLI REPLACING ALL  SPACE BY ZERO                      
117600                                                                          
117700     IF MID-KDPRTVAL-IN = ALL '+'                                         
117800         MOVE MID-KDPRTVAL-UT TO MOD-KDPRTVAL-UT                          
117900                                 WS-KDPRTVAL                              
118000     ELSE                                                                 
118100         MOVE MID-KDPRTVAL-IN TO MOD-KDPRTVAL-UT                          
118200                                 WS-KDPRTVAL                              
118300     END-IF                                                               
118400                                                                          
118500     IF WS-IDDC = SPACE                                                   
118600       MOVE NEJ                           TO WS-KEYS-TEST                 
118700     END-IF                                                               
118800                                                                          
118900     MOVE LOW-VALUE           TO MSG-AREA                                 
119000     MOVE 'W4O332N1'          TO MFS-IDMOD                                
119100     MOVE '4332'              TO MOD-IDTRANS                              
119200     MOVE MOD-LAENGD          TO MSG-KVLL                                 
119300                                                                          
119400     MOVE WS-IDDISTR TO MOD-IDDISTR-UT                                    
119500     INSPECT MOD-IDDISTR-UT REPLACING LEADING ZERO BY SPACE               
119600                                                                          
119700     MOVE WS-IDKUNDNR TO MOD-IDKUNDNR-UT                                  
119800     INSPECT MOD-IDKUNDNR-UT REPLACING LEADING ZERO BY SPACE              
119900                                                                          
120000     MOVE WS-IDORDNR TO MOD-IDORDNR-UT                                    
120100     INSPECT MOD-IDORDNR-UT REPLACING LEADING ZERO BY SPACE               
120200                                                                          
120300     MOVE WS-IDKOLLI TO MOD-IDKOLLI-UT                                    
120400     INSPECT MOD-IDKOLLI-UT REPLACING LEADING ZERO BY SPACE               
120500                                                                          
120600     IF API-SW = 'Y'                                                      
120700        MOVE MOD-IDDISTR-UT      TO RESP-IDDISTR                          
120800        MOVE MOD-IDKUNDNR-UT     TO RESP-IDKUNDNR                         
120900        MOVE MOD-IDORDNR-UT      TO RESP-IDORDNR                          
121000        MOVE MOD-IDKOLLI-UT      TO RESP-IDKOLLI                          
121100     END-IF                                                               
121200                                                                          
121300     MOVE WS-IDDC                      TO MOD-IDDC-UT                     
121400                                          RESP-IDDC                       
121500                                                                          
121600     MOVE MFS-RENSA-FAELT              TO MOD-IDDISTR-IN                  
121700                                          MOD-IDKUNDNR-IN                 
121800                                          MOD-IDORDNR-IN                  
121900                                          MOD-IDKOLLI-IN                  
122000                                          MOD-KDPRTVAL-IN                 
122100                                          MOD-KDKOLLI-IN                  
122200                                          MOD-IDDC-IN                     
122300                                          MOD-KDEMBTYP-IN                 
122400                                          MOD-DIKOLLIL-IN                 
122500                                          MOD-DIKOLLIB-IN                 
122600                                          MOD-DIKOLLIH-IN                 
122700                                          MOD-VKORDBTO-IN                 
122800                                          MOD-TEMFSFEL                    
122900                                          MOD-TEMFSINF                    
123000     IF API-SW = 'N'                                                      
123100        MOVE MFS-ROER-EJ-FAELT         TO MOD-KDKOLLI-UT                  
123200                                          MOD-KDEMBTYP-UT                 
123300                                          MOD-DIKOLLIL-UT                 
123400                                          MOD-DIKOLLIB-UT                 
123500                                          MOD-DIKOLLIH-UT                 
123600                                          MOD-VKORDBTO-UT                 
123700                                          MOD-ADRESS-TEXT                 
123800                                          MOD-ADFLGEO                     
123900                                          MOD-ADFLOMR                     
124000                                          MOD-ADRUTNIV                    
124100                                          MOD-ADVMODUL                    
124200     ELSE                                                                 
124300        MOVE MFS-ROER-EJ-FAELT         TO RESP-KDKOLLI                    
124400                                          RESP-KDEMBTYP                   
124500                                          RESP-VKORDBTO                   
124600                                          RESP-DIKOLLIL                   
124700                                          RESP-DIKOLLIB                   
124800                                          RESP-DIKOLLIH                   
124900                                          RESP-ADRESS-TEXT                
125000                                          RESP-ADFLGEO                    
125100                                          RESP-ADFLOMR                    
125200                                          RESP-ADRUTNIV                   
125300                                          RESP-ADVMODUL                   
125400     END-IF                                                               
125500     MOVE ZERO                         TO WS-DIKOLLIL                     
125600                                          WS-DIKOLLIB                     
125700                                          WS-DIKOLLIH                     
125800                                          WS-SPAR-DIKOLLIL                
125900                                          WS-SPAR-DIKOLLIB                
126000                                          WS-SPAR-DIKOLLIH                
126100     .                                                                    
126200     EJECT                                                                
126300 B-UPPDATERING SECTION.                                                   
126400                                                                          
126500     PERFORM BA-FORMELLA-KONTROLLER                                       
126600     IF FEL-EJ-FUNNET                                                     
126700         PERFORM BB-LAES-K501                                             
126800         IF FEL-EJ-FUNNET                                                 
126900             PERFORM S01-LAES-E611-VIA-E4                                 
127000             PERFORM BD-KOLLA-VKORDBTO                                    
127100             IF FEL-EJ-FUNNET                                             
127200                 EVALUATE TRUE                                            
127300                 WHEN KOLLI-KDKOLSTA > 1                                  
127400                     MOVE '721'         TO RESP-IDMSG-ERROR               
127500                     MOVE FEL-721(INDX)     TO MOD-TEMFSFEL               
127600                                           RESP-IDMSG-ERROR-LINE          
127700                     MOVE JA                TO WS-FEL-FUNNET              
127800                     MOVE BAD-REQUEST       TO RESP-IDMFSINF              
127900                     PERFORM S02-SAETT-ROER-EJ-FAELT                      
128000*   CHECK FOR ZERO KOLLI-DIKOLLI IS ADDED AS REPLACEMENT OF               
128100*   OF KOLLI-KDKOLSTA WHICH IS NOT 1 EVEN AFTER PACKING IN API            
128200                 WHEN KOLLI-DIKOLLIL = ZERO                               
128300                     MOVE '717'         TO RESP-IDMSG-ERROR               
128400                     MOVE FEL-717(INDX)     TO MOD-TEMFSFEL               
128500                                           RESP-IDMSG-ERROR-LINE          
128600                     MOVE JA                TO WS-FEL-FUNNET              
128700                     MOVE BAD-REQUEST       TO RESP-IDMFSINF              
128800                     PERFORM S02-SAETT-ROER-EJ-FAELT                      
128900                 WHEN OTHER                                               
129000                     PERFORM BC-UPPDATERA-E611                            
129100                 END-EVALUATE                                             
129200             END-IF                                                       
129300         END-IF                                                           
129400     END-IF                                                               
129500     .                                                                    
129600     EJECT                                                                
129700 BA-FORMELLA-KONTROLLER SECTION.                                          
129800                                                                          
129900     IF API-SW = 'Y'                                                      
130000        PERFORM BAA-UPD-INPUTS-API                                        
130100     END-IF                                                               
130200                                                                          
130300     IF MID-KDKOLLI-IN = ALL '+'                                          
130400         MOVE MID-KDKOLLI-UT            TO WS-KDKOLLI                     
130500     ELSE                                                                 
130600         MOVE MID-KDKOLLI-IN            TO WS-KDKOLLI                     
130700     END-IF                                                               
130800     IF WS-KDKOLLI = SPACE                                                
130900         IF FEL-EJ-FUNNET                                                 
131000             MOVE '726'                 TO RESP-IDMSG-ERROR               
131100             MOVE FEL-726(INDX)         TO MOD-TEMFSFEL                   
131200                                           RESP-IDMSG-ERROR-LINE          
131300             MOVE JA                    TO WS-FEL-FUNNET                  
131400             MOVE BAD-REQUEST           TO RESP-IDMFSINF                  
131500             PERFORM S02-SAETT-ROER-EJ-FAELT                              
131600         END-IF                                                           
131700         MOVE MFS-ALFA-FAELT-FEL        TO MOD-KDKOLLI-IN-ATTR            
131800     END-IF                                                               
131900                                                                          
132000     IF MID-KDEMBTYP-IN NOT = ALL '+'                                     
132100         IF MID-KDEMBTYP-IN NUMERIC                                       
132200             IF MID-KDEMBTYP-IN = ZERO                                    
132300                 IF FEL-EJ-FUNNET                                         
132400                     MOVE '724'         TO RESP-IDMSG-ERROR               
132500                     MOVE FEL-724(INDX) TO MOD-TEMFSFEL                   
132600                                           RESP-IDMSG-ERROR-LINE          
132700                     MOVE JA           TO WS-FEL-FUNNET                   
132800                     MOVE BAD-REQUEST   TO RESP-IDMFSINF                  
132900                     PERFORM S02-SAETT-ROER-EJ-FAELT                      
133000                 END-IF                                                   
133100                 MOVE MFS-NUM-FAELT-FEL TO MOD-KDEMBTYP-IN-ATTR           
133200             END-IF                                                       
133300         ELSE                                                             
133400             IF FEL-EJ-FUNNET                                             
133500                     MOVE '748'         TO RESP-IDMSG-ERROR               
133600                 MOVE FEL-748(INDX)     TO MOD-TEMFSFEL                   
133700                                           RESP-IDMSG-ERROR-LINE          
133800                 MOVE JA                TO WS-FEL-FUNNET                  
133900                 MOVE BAD-REQUEST       TO RESP-IDMFSINF                  
134000                 PERFORM S02-SAETT-ROER-EJ-FAELT                          
134100             END-IF                                                       
134200             MOVE MFS-NUM-FAELT-FEL     TO MOD-KDEMBTYP-IN-ATTR           
134300         END-IF                                                           
134400     END-IF                                                               
134500                                                                          
134600     IF MID-DIKOLLIL-IN NOT = ALL '+'                                     
134700         IF MID-DIKOLLIL-IN NUMERIC                                       
134800             IF MID-DIKOLLIL-IN = ZERO                                    
134900                 IF FEL-EJ-FUNNET                                         
135000                     MOVE '724'         TO RESP-IDMSG-ERROR               
135100                     MOVE FEL-724(INDX) TO MOD-TEMFSFEL                   
135200                                           RESP-IDMSG-ERROR-LINE          
135300                     MOVE JA            TO WS-FEL-FUNNET                  
135400                     MOVE BAD-REQUEST   TO RESP-IDMFSINF                  
135500                     PERFORM S02-SAETT-ROER-EJ-FAELT                      
135600                 END-IF                                                   
135700                 MOVE MFS-NUM-FAELT-FEL TO MOD-DIKOLLIL-IN-ATTR           
135800             END-IF                                                       
135900         ELSE                                                             
136000             IF FEL-EJ-FUNNET                                             
136100                 MOVE '748'         TO RESP-IDMSG-ERROR                   
136200                 MOVE FEL-748(INDX)     TO MOD-TEMFSFEL                   
136300                                           RESP-IDMSG-ERROR-LINE          
136400                 MOVE JA                TO WS-FEL-FUNNET                  
136500                 MOVE BAD-REQUEST       TO RESP-IDMFSINF                  
136600                 PERFORM S02-SAETT-ROER-EJ-FAELT                          
136700             END-IF                                                       
136800             MOVE MFS-NUM-FAELT-FEL     TO MOD-DIKOLLIL-IN-ATTR           
136900         END-IF                                                           
137000     END-IF                                                               
137100                                                                          
137200     IF MID-DIKOLLIB-IN NOT = ALL '+'                                     
137300         IF MID-DIKOLLIB-IN NUMERIC                                       
137400             IF MID-DIKOLLIB-IN = ZERO                                    
137500                 IF FEL-EJ-FUNNET                                         
137600                     MOVE '724'         TO RESP-IDMSG-ERROR               
137700                     MOVE FEL-724(INDX) TO MOD-TEMFSFEL                   
137800                                           RESP-IDMSG-ERROR-LINE          
137900                     MOVE JA            TO WS-FEL-FUNNET                  
138000                     MOVE BAD-REQUEST   TO RESP-IDMFSINF                  
138100                     PERFORM S02-SAETT-ROER-EJ-FAELT                      
138200                 END-IF                                                   
138300                 MOVE MFS-NUM-FAELT-FEL TO MOD-DIKOLLIB-IN-ATTR           
138400             END-IF                                                       
138500         ELSE                                                             
138600             IF FEL-EJ-FUNNET                                             
138700                 MOVE '748'         TO RESP-IDMSG-ERROR                   
138800                 MOVE FEL-748(INDX)     TO MOD-TEMFSFEL                   
138900                                           RESP-IDMSG-ERROR-LINE          
139000                 MOVE JA                TO WS-FEL-FUNNET                  
139100                 MOVE BAD-REQUEST       TO RESP-IDMFSINF                  
139200                 PERFORM S02-SAETT-ROER-EJ-FAELT                          
139300             END-IF                                                       
139400             MOVE MFS-NUM-FAELT-FEL     TO MOD-DIKOLLIB-IN-ATTR           
139500         END-IF                                                           
139600     END-IF                                                               
139700                                                                          
139800     IF MID-DIKOLLIH-IN NOT = ALL '+'                                     
139900         IF MID-DIKOLLIH-IN NUMERIC                                       
140000             IF MID-DIKOLLIH-IN = ZERO                                    
140100                 IF FEL-EJ-FUNNET                                         
140200                     MOVE '724'         TO RESP-IDMSG-ERROR               
140300                     MOVE FEL-724(INDX) TO MOD-TEMFSFEL                   
140400                                           RESP-IDMSG-ERROR-LINE          
140500                     MOVE JA            TO WS-FEL-FUNNET                  
140600                     MOVE BAD-REQUEST       TO RESP-IDMFSINF              
140700                     PERFORM S02-SAETT-ROER-EJ-FAELT                      
140800                 END-IF                                                   
140900                 MOVE MFS-NUM-FAELT-FEL TO MOD-DIKOLLIH-IN-ATTR           
141000             END-IF                                                       
141100         ELSE                                                             
141200             IF FEL-EJ-FUNNET                                             
141300                 MOVE '748'             TO RESP-IDMSG-ERROR               
141400                 MOVE FEL-748(INDX)     TO MOD-TEMFSFEL                   
141500                                           RESP-IDMSG-ERROR-LINE          
141600                 MOVE JA                TO WS-FEL-FUNNET                  
141700                 MOVE BAD-REQUEST       TO RESP-IDMFSINF                  
141800                 PERFORM S02-SAETT-ROER-EJ-FAELT                          
141900             END-IF                                                       
142000             MOVE MFS-NUM-FAELT-FEL     TO MOD-DIKOLLIH-IN-ATTR           
142100         END-IF                                                           
142200     END-IF                                                               
142300                                                                          
142400     IF MID-VKORDBTO-IN NOT = ALL '+'                                     
142500       MOVE WS-IDDISTR TO TEST-IDDISTR                                    
142600       MOVE MID-VKORDBTO-IN               TO DEC-IDFRIDATA                
142700       MOVE +6                            TO DEC-KVHELTAL                 
142800       MOVE +1                            TO DEC-KVDECIMAL                
142900       CALL WDECEDIT USING DEC-WDECAREA                                   
143000       IF DEC-KDSVAR-FEL                                                  
143100         IF FEL-EJ-FUNNET                                                 
143200             MOVE '748'                   TO RESP-IDMSG-ERROR             
143300             MOVE FEL-748(INDX)           TO MOD-TEMFSFEL                 
143400                                             RESP-IDMSG-ERROR-LINE        
143500             MOVE JA                      TO WS-FEL-FUNNET                
143600             MOVE BAD-REQUEST             TO RESP-IDMFSINF                
143700             PERFORM S02-SAETT-ROER-EJ-FAELT                              
143800         END-IF                                                           
143900         MOVE MFS-NUM-FAELT-FEL           TO MOD-VKORDBTO-IN-ATTR         
144000       ELSE                                                               
144100         IF DEC-IDEDITDATA = ZERO                                         
144200           IF FEL-EJ-FUNNET                                               
144300               MOVE '724'         TO RESP-IDMSG-ERROR                     
144400               MOVE FEL-724(INDX) TO MOD-TEMFSFEL                         
144500                                     RESP-IDMSG-ERROR-LINE                
144600               MOVE JA                    TO WS-FEL-FUNNET                
144700               MOVE BAD-REQUEST           TO RESP-IDMFSINF                
144800               PERFORM S02-SAETT-ROER-EJ-FAELT                            
144900           END-IF                                                         
145000           MOVE MFS-NUM-FAELT-FEL TO MOD-VKORDBTO-IN-ATTR                 
145100         ELSE                                                             
145200           IF US-MEASUREMENT                                              
145300              MOVE DEC-IDEDITDATA       TO WS-DEC-IDEDITDATA              
145400                                                                          
145500              PERFORM S17-CONVERT-VKORDBTO-TO-KG                          
145600              MOVE WS-SPAR-VKORDBTO     TO WS-VKORDBTO-MID                
145700           ELSE                                                           
145800              MOVE DEC-IDEDITDATA       TO WS-VKORDBTO-MID                
145900                                                                          
146000           END-IF                                                         
146100         END-IF                                                           
146200       END-IF                                                             
146300     END-IF                                                               
146400     .                                                                    
146500     EJECT                                                                
146600 BAA-UPD-INPUTS-API SECTION.                                              
146700                                                                          
146800     MOVE REQU-KDKOLLI               TO MID-KDKOLLI-IN                    
146900     MOVE REQU-KDEMBTYP              TO MID-KDEMBTYP-IN                   
147000     MOVE REQU-DIKOLLIL              TO MID-DIKOLLIL-IN                   
147100     MOVE REQU-DIKOLLIB              TO MID-DIKOLLIB-IN                   
147200     MOVE REQU-DIKOLLIH              TO MID-DIKOLLIH-IN                   
147300     MOVE REQU-VKORDBTO              TO MID-VKORDBTO-IN                   
147400                                                                          
147500     IF REQU-KDEMBTYP = ALL '+' OR LOW-VALUES                             
147600        MOVE ALL-PLUS                TO MID-KDEMBTYP-IN                   
147700     END-IF                                                               
147800     IF REQU-DIKOLLIL = ALL '+' OR LOW-VALUES                             
147900        MOVE ALL-PLUS                TO MID-DIKOLLIL-IN                   
148000     END-IF                                                               
148100     IF REQU-DIKOLLIB = ALL '+' OR LOW-VALUES                             
148200        MOVE ALL-PLUS                TO MID-DIKOLLIB-IN                   
148300     END-IF                                                               
148400     IF REQU-DIKOLLIH = ALL '+' OR LOW-VALUES                             
148500        MOVE ALL-PLUS                TO MID-DIKOLLIH-IN                   
148600     END-IF                                                               
148700     IF REQU-VKORDBTO = ALL '+' OR LOW-VALUES                             
148800        MOVE ALL-PLUS                TO MID-VKORDBTO-IN                   
148900     END-IF                                                               
149000     .                                                                    
149100     EJECT                                                                
149200 BB-LAES-K501 SECTION.                                                    
149300                                                                          
149400                                                                          
149500     IF MID-KDKOLLI-IN  NOT = ALL '+'                                     
149600     OR MID-DIKOLLIL-IN NOT = ALL '+'                                     
149700     OR MID-DIKOLLIB-IN NOT = ALL '+'                                     
149800     OR MID-DIKOLLIH-IN NOT = ALL '+'                                     
149900         MOVE WS-KDKOLLI                TO W-K501-KDKOLLI                 
150000         PERFORM IMS-GU-K501-KVAL                                         
150100         IF SEGMENT-SAKNAS                                                
150200             IF FEL-EJ-FUNNET                                             
150300                 MOVE '726'             TO RESP-IDMSG-ERROR               
150400                 MOVE FEL-726(INDX)     TO MOD-TEMFSFEL                   
150500                                           RESP-IDMSG-ERROR-LINE          
150600                 MOVE JA                TO WS-FEL-FUNNET                  
150700                 MOVE BAD-REQUEST       TO RESP-IDMFSINF                  
150800                 PERFORM S02-SAETT-ROER-EJ-FAELT                          
150900             END-IF                                                       
151000             MOVE MFS-ALFA-FAELT-FEL    TO MOD-KDKOLLI-IN-ATTR            
151100         ELSE                                                             
151200             MOVE EMB-KDKOLLID          TO WS-KDKOLLID                    
151300             MOVE EMB-KDEMBTYP          TO WS-KDEMBTYP                    
151400             MOVE EMB-DIKOLLIL          TO WS-DIKOLLIL                    
151500             MOVE EMB-DIKOLLIB          TO WS-DIKOLLIB                    
151600             MOVE EMB-DIKOLLIH          TO WS-DIKOLLIH                    
151700         END-IF                                                           
151800     END-IF                                                               
151900     .                                                                    
152000     EJECT                                                                
152100 BC-UPPDATERA-E611 SECTION.                                               
152200                                                                          
152300     IF MID-KDKOLLI-IN  NOT = ALL '+'                                     
152400     OR MID-KDEMBTYP-IN NOT = ALL '+'                                     
152500     OR MID-DIKOLLIL-IN NOT = ALL '+'                                     
152600     OR MID-DIKOLLIB-IN NOT = ALL '+'                                     
152700     OR MID-DIKOLLIH-IN NOT = ALL '+'                                     
152800     OR MID-VKORDBTO-IN NOT = ALL '+'                                     
152900                                                                          
153000         IF MID-KDKOLLI-IN  NOT = ALL '+'                                 
153100         OR MID-DIKOLLIL-IN NOT = ALL '+'                                 
153200         OR MID-DIKOLLIB-IN NOT = ALL '+'                                 
153300         OR MID-DIKOLLIH-IN NOT = ALL '+'                                 
153400             IF (KOLLI-ADFLOMR > 499 AND < 900) OR                        
153500                (KOLLI-ADFLOMR = 999)                                     
153600                 PERFORM S04-NOLLA-PLATSAREAN                             
153700                 PERFORM BCA-INPUT-TILL-PLATSSOEKNING                     
153800                 CALL W403PLAT USING PLATS-W403PLAT                       
153900                                     PLATS-DM-PCB                         
154000                                     PLATS-DN-PCB                         
154100                                     PLATS-DP-PCB                         
154200                                     PLATS-DO-PCB                         
154300                                     PLATS-WDE6C-PCB                      
154400                                     PLATS-GMTC-PCB                       
154500                                     PLATS-WDB6-PCB                       
154600                                                                          
154700                 IF PLATS-KDSVAR = FEL                                    
154800                     IF FEL-EJ-FUNNET                                     
154900                         MOVE '764'         TO RESP-IDMSG-ERROR           
155000                         MOVE FEL-764(INDX) TO MOD-TEMFSFEL               
155100                                            RESP-IDMSG-ERROR-LINE         
155200                         MOVE JA            TO WS-FEL-FUNNET              
155300                         MOVE BAD-REQUEST   TO RESP-IDMFSINF              
155400                         PERFORM S02-SAETT-ROER-EJ-FAELT                  
155500                     END-IF                                               
155600                 ELSE                                                     
155700                     MOVE PLATS-IDTRPTNR        TO SPAR-IDTRPTNR          
155800                     MOVE PLATS-ADCLGEO         TO SPAR-ADCLGEO           
155900                     MOVE PLATS-ADFLOMR         TO SPAR-ADFLOMR           
156000                     MOVE PLATS-ADRUTNIV        TO SPAR-ADRUTNIV          
156100                     MOVE PLATS-DIHMODUL        TO SPAR-DIHMODUL          
156200                     MOVE PLATS-DIDMODUL        TO SPAR-DIDMODUL          
156300                     MOVE PLATS-ADVMODUL        TO SPAR-ADVMODUL          
156400                     MOVE PLATS-ADHMODUL        TO SPAR-ADHMODUL          
156500                     MOVE PLATS-FLUTLAST        TO SPAR-FLUTLAST          
156600                     MOVE PLATS-IDDC-CROSS      TO SPAR-IDDC-CROSS        
156700                                                                          
156800                     PERFORM S04-NOLLA-PLATSAREAN                         
156900                     PERFORM BCB-INPUT-TILL-PLATSAVBOKNING                
157000                     CALL W403PLAT USING PLATS-W403PLAT                   
157100                                         PLATS-DM-PCB                     
157200                                         PLATS-DN-PCB                     
157300                                         PLATS-DP-PCB                     
157400                                         PLATS-DO-PCB                     
157500                                         PLATS-WDE6C-PCB                  
157600                                         PLATS-GMTC-PCB                   
157700                                         PLATS-WDB6-PCB                   
157800                                                                          
157900                     IF PLATS-KDSVAR = FEL                                
158000                         IF FEL-EJ-FUNNET                                 
158100                                                                          
158200                             MOVE '764'       TO RESP-IDMSG-ERROR         
158300                             MOVE FEL-764(INDX) TO MOD-TEMFSFEL           
158400                                            RESP-IDMSG-ERROR-LINE         
158500                             MOVE JA            TO WS-FEL-FUNNET          
158600                             MOVE BAD-REQUEST   TO RESP-IDMFSINF          
158700                             PERFORM S02-SAETT-ROER-EJ-FAELT              
158800                         END-IF                                           
158900                     ELSE                                                 
159000                         PERFORM BCD-NY-ADRESS-I-WDE611                   
159100                         PERFORM BCC-UPPDATERA-RESTERANDE-FAELT           
159200                     END-IF                                               
159300                 END-IF                                                   
159400             ELSE                                                         
159500                 PERFORM BCC-UPPDATERA-RESTERANDE-FAELT                   
159600             END-IF                                                       
159700         ELSE                                                             
159800             PERFORM BCC-UPPDATERA-RESTERANDE-FAELT                       
159900         END-IF                                                           
160000     ELSE                                                                 
160100         IF FEL-EJ-FUNNET                                                 
160200             MOVE '789'                 TO RESP-IDMSG-ERROR               
160300             MOVE FEL-789(INDX)         TO MOD-TEMFSFEL                   
160400                                           RESP-IDMSG-ERROR-LINE          
160500             MOVE JA                    TO WS-FEL-FUNNET                  
160600             MOVE BAD-REQUEST           TO RESP-IDMFSINF                  
160700             PERFORM S02-SAETT-ROER-EJ-FAELT                              
160800         END-IF                                                           
160900     END-IF                                                               
161000     .                                                                    
161100     EJECT                                                                
161200 BCA-INPUT-TILL-PLATSSOEKNING SECTION.                                    
161300                                                                          
161400                                                                          
161500     MOVE SPACE                         TO PLATS-ADFLGEO                  
161600                                           PLATS-FLUTLAST                 
161700                                           PLATS-IDDC-CROSS               
161800     MOVE ZERO                          TO PLATS-IDTRPTNR                 
161900                                           PLATS-ADFLOMR                  
162000                                           PLATS-ADRUTNIV                 
162100                                           PLATS-ADVMODUL                 
162200                                           PLATS-ADHMODUL                 
162300                                           PLATS-DIHMODUL                 
162400                                           PLATS-DIDMODUL                 
162500     MOVE WS-IDDC                       TO PLATS-IDDC                     
162600     MOVE WS-IDDISTR                    TO PLATS-IDDISTR                  
162700     MOVE WS-IDKUNDNR                   TO PLATS-IDKUNDNR                 
162800     MOVE WS-KDFRAKT                    TO PLATS-KDFRAKT                  
162900     MOVE WS-KDORDKL                    TO PLATS-KDORDKLX                 
163000     MOVE WS-IDORDNR                    TO PLATS-IDORDNR                  
163100     EVALUATE TRUE                                                        
163200     WHEN MID-DIKOLLIL-IN NOT = ALL '+'                                   
163300         MOVE MID-DIKOLLIL-IN           TO WS-SPAR-DIKOLLIL               
163400         IF US-MEASUREMENT                                                
163500           PERFORM S14-CONVERT-DIKOLLIL-TO-CM                             
163600         END-IF                                                           
163700         MOVE WS-SPAR-DIKOLLIL          TO PLATS-DIKOLLIL                 
163800     WHEN WS-DIKOLLIL NOT = ZERO                                          
163900         MOVE WS-DIKOLLIL               TO PLATS-DIKOLLIL                 
164000     WHEN OTHER                                                           
164100         MOVE KOLLI-DIKOLLIL            TO PLATS-DIKOLLIL                 
164200     END-EVALUATE                                                         
164300                                                                          
164400     EVALUATE TRUE                                                        
164500     WHEN MID-DIKOLLIB-IN NOT = ALL '+'                                   
164600         MOVE MID-DIKOLLIB-IN           TO WS-SPAR-DIKOLLIB               
164700         IF US-MEASUREMENT                                                
164800           PERFORM S15-CONVERT-DIKOLLIB-TO-CM                             
164900         END-IF                                                           
165000         MOVE WS-SPAR-DIKOLLIB              TO PLATS-DIKOLLIB             
165100     WHEN WS-DIKOLLIB NOT = ZERO                                          
165200         MOVE WS-DIKOLLIB               TO PLATS-DIKOLLIB                 
165300     WHEN OTHER                                                           
165400         MOVE KOLLI-DIKOLLIB            TO PLATS-DIKOLLIB                 
165500     END-EVALUATE                                                         
165600                                                                          
165700     EVALUATE TRUE                                                        
165800     WHEN MID-DIKOLLIH-IN NOT = ALL '+'                                   
165900         MOVE MID-DIKOLLIH-IN           TO WS-SPAR-DIKOLLIH               
166000         IF US-MEASUREMENT                                                
166100           PERFORM S16-CONVERT-DIKOLLIH-TO-CM                             
166200         END-IF                                                           
166300         MOVE WS-SPAR-DIKOLLIH          TO PLATS-DIKOLLIH                 
166400     WHEN WS-DIKOLLIH NOT = ZERO                                          
166500         MOVE WS-DIKOLLIH               TO PLATS-DIKOLLIH                 
166600     WHEN OTHER                                                           
166700         MOVE KOLLI-DIKOLLIH            TO PLATS-DIKOLLIH                 
166800     END-EVALUATE                                                         
166900                                                                          
167000     MOVE WS-KDKOLLID                   TO PLATS-KDKOLLID                 
167100     MOVE KOLLI-VKORDNTO-KOLLI          TO PLATS-VKORDNTO-KOLLI           
167200     IF KOLLI-KDFARLIG-KOLLI = +4                                         
167300     OR KOLLI-KDFARLIG-KOLLI = +7                                         
167400       MOVE +6                          TO PLATS-KDCALL                   
167500     ELSE                                                                 
167600       MOVE ZERO                        TO PLATS-KDCALL                   
167700     END-IF                                                               
167800     .                                                                    
167900     EJECT                                                                
168000 BCB-INPUT-TILL-PLATSAVBOKNING SECTION.                                   
168100                                                                          
168200                                                                          
168300     MOVE +3                            TO PLATS-KDCALL                   
168400     MOVE KOLLI-IDTRPTNR                TO PLATS-IDTRPTNR                 
168500     MOVE KOLLI-DARFS (3:10)            TO PLATS-TIRFS                    
168600     MOVE KOLLI-ADCLGEO                 TO PLATS-ADCLGEO                  
168700     MOVE KOLLI-ADFLOMR                 TO PLATS-ADFLOMR                  
168800     MOVE KOLLI-ADRUTNIV                TO PLATS-ADRUTNIV                 
168900     MOVE KOLLI-DIHMODUL                TO PLATS-DIHMODUL                 
169000     MOVE KOLLI-DIDMODUL                TO PLATS-DIDMODUL                 
169100     MOVE KOLLI-ADVMODUL                TO PLATS-ADVMODUL                 
169200     MOVE KOLLI-ADHMODUL                TO PLATS-ADHMODUL                 
169300     .                                                                    
169400     EJECT                                                                
169500 BCC-UPPDATERA-RESTERANDE-FAELT SECTION.                                  
169600                                                                          
169700     MOVE WS-KDKOLLI         TO KOLLI-KDKOLLI                             
169800                                MOD-KDKOLLI-UT                            
169900                                                                          
170000     EVALUATE TRUE                                                        
170100     WHEN MID-KDEMBTYP-IN NOT = ALL '+'                                   
170200         MOVE MID-KDEMBTYP-IN TO KOLLI-KDEMBTYP                           
170300                                 MOD-KDEMBTYP-UT                          
170400     WHEN WS-KDEMBTYP NOT = ZERO                                          
170500         MOVE WS-KDEMBTYP     TO KOLLI-KDEMBTYP                           
170600                                 MOD-KDEMBTYP-UT                          
170700     WHEN OTHER                                                           
170800         MOVE KOLLI-KDEMBTYP  TO MOD-KDEMBTYP-UT                          
170900     END-EVALUATE                                                         
171000                                                                          
171100     EVALUATE TRUE                                                        
171200     WHEN MID-DIKOLLIL-IN NOT = ALL '+'                                   
171300         IF US-MEASUREMENT                                                
171400           MOVE MID-DIKOLLIL-IN        TO WS-SPAR-DIKOLLIL                
171500                                          MOD-DIKOLLIL-UT                 
171600           PERFORM S14-CONVERT-DIKOLLIL-TO-CM                             
171700           MOVE WS-SPAR-DIKOLLIL      TO KOLLI-DIKOLLIL                   
171800         ELSE                                                             
171900           MOVE MID-DIKOLLIL-IN       TO KOLLI-DIKOLLIL                   
172000                                         MOD-DIKOLLIL-UT                  
172100         END-IF                                                           
172200     WHEN WS-DIKOLLIL NOT = ZERO                                          
172300         IF US-MEASUREMENT                                                
172400           MOVE WS-DIKOLLIL           TO KOLLI-DIKOLLIL                   
172500                                         WS-SPAR-DIKOLLIL                 
172600           PERFORM S10-CONVERT-DIKOLLIL-TO-INCH                           
172700           MOVE WS-SPAR-DIKOLLIL      TO MOD-DIKOLLIL-UT                  
172800         ELSE                                                             
172900           MOVE WS-DIKOLLIL           TO KOLLI-DIKOLLIL                   
173000                                         MOD-DIKOLLIL-UT                  
173100         END-IF                                                           
173200     WHEN OTHER                                                           
173300         IF US-MEASUREMENT                                                
173400           MOVE KOLLI-DIKOLLIL        TO WS-SPAR-DIKOLLIL                 
173500           PERFORM S10-CONVERT-DIKOLLIL-TO-INCH                           
173600           MOVE WS-SPAR-DIKOLLIL      TO MOD-DIKOLLIL-UT                  
173700         ELSE                                                             
173800           MOVE KOLLI-DIKOLLIL        TO MOD-DIKOLLIL-UT                  
173900         END-IF                                                           
174000     END-EVALUATE                                                         
174100                                                                          
174200     EVALUATE TRUE                                                        
174300     WHEN MID-DIKOLLIB-IN NOT = ALL '+'                                   
174400         IF US-MEASUREMENT                                                
174500           MOVE MID-DIKOLLIB-IN     TO WS-SPAR-DIKOLLIB                   
174600                                       MOD-DIKOLLIB-UT                    
174700           PERFORM S15-CONVERT-DIKOLLIB-TO-CM                             
174800           MOVE WS-SPAR-DIKOLLIB    TO KOLLI-DIKOLLIB                     
174900         ELSE                                                             
175000           MOVE MID-DIKOLLIB-IN     TO KOLLI-DIKOLLIB                     
175100                                       MOD-DIKOLLIB-UT                    
175200         END-IF                                                           
175300     WHEN WS-DIKOLLIB NOT = ZERO                                          
175400         IF US-MEASUREMENT                                                
175500           MOVE WS-DIKOLLIB         TO KOLLI-DIKOLLIB                     
175600                                       WS-SPAR-DIKOLLIB                   
175700           PERFORM S11-CONVERT-DIKOLLIB-TO-INCH                           
175800           MOVE WS-SPAR-DIKOLLIB    TO MOD-DIKOLLIB-UT                    
175900         ELSE                                                             
176000           MOVE WS-DIKOLLIB         TO KOLLI-DIKOLLIB                     
176100                                       MOD-DIKOLLIB-UT                    
176200         END-IF                                                           
176300     WHEN OTHER                                                           
176400         IF US-MEASUREMENT                                                
176500           MOVE KOLLI-DIKOLLIB      TO WS-SPAR-DIKOLLIB                   
176600           PERFORM S11-CONVERT-DIKOLLIB-TO-INCH                           
176700           MOVE WS-SPAR-DIKOLLIB    TO MOD-DIKOLLIB-UT                    
176800         ELSE                                                             
176900           MOVE KOLLI-DIKOLLIB      TO MOD-DIKOLLIB-UT                    
177000         END-IF                                                           
177100     END-EVALUATE                                                         
177200                                                                          
177300     EVALUATE TRUE                                                        
177400     WHEN MID-DIKOLLIH-IN NOT = ALL '+'                                   
177500         IF US-MEASUREMENT                                                
177600           MOVE MID-DIKOLLIH-IN     TO WS-SPAR-DIKOLLIH                   
177700                                       MOD-DIKOLLIH-UT                    
177800           PERFORM S16-CONVERT-DIKOLLIH-TO-CM                             
177900           MOVE WS-SPAR-DIKOLLIH    TO KOLLI-DIKOLLIH                     
178000         ELSE                                                             
178100           MOVE MID-DIKOLLIH-IN     TO KOLLI-DIKOLLIH                     
178200                                       MOD-DIKOLLIH-UT                    
178300         END-IF                                                           
178400     WHEN WS-DIKOLLIH NOT = ZERO                                          
178500         IF US-MEASUREMENT                                                
178600           MOVE WS-DIKOLLIH         TO KOLLI-DIKOLLIH                     
178700                                       WS-SPAR-DIKOLLIH                   
178800           PERFORM S12-CONVERT-DIKOLLIH-TO-INCH                           
178900           MOVE WS-SPAR-DIKOLLIH    TO MOD-DIKOLLIH-UT                    
179000         ELSE                                                             
179100           MOVE WS-DIKOLLIH         TO KOLLI-DIKOLLIH                     
179200                                       MOD-DIKOLLIH-UT                    
179300         END-IF                                                           
179400     WHEN OTHER                                                           
179500         IF US-MEASUREMENT                                                
179600           MOVE KOLLI-DIKOLLIH      TO WS-SPAR-DIKOLLIH                   
179700           PERFORM S12-CONVERT-DIKOLLIH-TO-INCH                           
179800           MOVE WS-SPAR-DIKOLLIH    TO MOD-DIKOLLIH-UT                    
179900         ELSE                                                             
180000           MOVE KOLLI-DIKOLLIH      TO MOD-DIKOLLIH-UT                    
180100         END-IF                                                           
180200     END-EVALUATE                                                         
180300                                                                          
180400     IF MID-VKORDBTO-IN NOT = ALL '+'                                     
180500       MOVE KOLLI-VKORDBTO-KOLLI   TO WS-VKORDBTO-KOLLI                   
180600       IF US-MEASUREMENT                                                  
180700         MOVE DEC-IDEDITDATA      TO WS-DEC-IDEDITDATA                    
180800                                     MOD-VKORDBTO-UT                      
180900         PERFORM S17-CONVERT-VKORDBTO-TO-KG                               
181000         MOVE WS-SPAR-VKORDBTO     TO KOLLI-VKORDBTO-KOLLI                
181100                                      DEC-IDEDITDATA                      
181200       ELSE                                                               
181300         MOVE DEC-IDEDITDATA       TO KOLLI-VKORDBTO-KOLLI                
181400                                      MOD-VKORDBTO-UT                     
181500       END-IF                                                             
181600       IF KOLLI-VKORDBTO-KOLLI < KOLLI-VKORDNTO-KOLLI                     
181700         MOVE KOLLI-VKORDBTO-KOLLI TO KOLLI-VKORDNTO-KOLLI                
181800       END-IF                                                             
181900     ELSE                                                                 
182000       IF US-MEASUREMENT                                                  
182100         MOVE KOLLI-VKORDBTO-KOLLI TO WS-SPAR-VKORDBTO                    
182200         PERFORM S13-CONVERT-VKORDBTO-TO-LB                               
182300         MOVE WS-SPAR-VKORDBTO     TO MOD-VKORDBTO-UT                     
182400       ELSE                                                               
182500         MOVE KOLLI-VKORDBTO-KOLLI TO MOD-VKORDBTO-UT                     
182600       END-IF                                                             
182700       MOVE ZERO                   TO WS-VKORDBTO-KOLLI                   
182800                                      DEC-IDEDITDATA                      
182900     END-IF                                                               
183000                                                                          
183100     MOVE KOLLI-VLORDBTO-KOLLI TO WS-GAMMAL-VLORDBTO                      
183200     COMPUTE WS-NY-VLORDBTO =                                             
183300             KOLLI-DIKOLLIL * KOLLI-DIKOLLIH * KOLLI-DIKOLLIB             
183400             / 1000000                                                    
183500     MOVE WS-NY-VLORDBTO TO KOLLI-VLORDBTO-KOLLI                          
183600                                                                          
183700     PERFORM IMS-REPLACE-E611                                             
187200                                                                          
187300*TMS PACKING INFO                                                         
           IF KOLLI-KDKOLSTA = 1                                                
187600        MOVE WS-IDDC            TO TMS-IDDC                               
187700        MOVE KORD-IDDISTR       TO TMS-IDDISTR                            
187800        MOVE KORD-IDKUNDNR      TO TMS-IDKUNDNR                           
187900        MOVE KORD-IDORDNR5      TO TMS-IDORDNR7                           
187910        MOVE KOLLI-IDKOLLI      TO TMS-IDKOLLI(1)                         
188000        CALL W403TMS1 USING TMS-W403TMS1                                  
188100                 TMS-CRE-PCB TMS-DEL-PCB ATAB-PCB                         
188200                 TMS-1165-PCB TMS-4141-PCB TMS-WDB2-PCB                   
188300                 TMS-WDB6-PCB TMS-WDD3-PCB TMS-WDB1-PCB                   
188400                 TMS-WDE4A-PCB TMS-WDE4F-PCB TMS-WDQ2-PCB                 
188500                 TMS-WDQ3-PCB TMS-WDK6-PCB TMS-WDE6-PCB                   
188600                 TMS-WDK5-PCB TMS-WDQ2C-PCB                               
           END-IF                                                               
189100                                                                          
189200     PERFORM BCCA-FLYTTA-ADRESS-TILL-MOD                                  
189300                                                                          
189400     PERFORM IMS-GHU-E601                                                 
189500     COMPUTE VORD-VKORDBTO =                                              
189600                             VORD-VKORDBTO                                
189700                           - WS-VKORDBTO-KOLLI                            
189800                           + DEC-IDEDITDATA                               
189900                                                                          
190000     COMPUTE VORD-VLORDBTO =                                              
190100                             VORD-VLORDBTO                                
190200                           - WS-GAMMAL-VLORDBTO                           
190300                           + WS-NY-VLORDBTO                               
190400     PERFORM IMS-REPLACE-E601                                             
190500     .                                                                    
190600     EJECT                                                                
190700 BCCA-FLYTTA-ADRESS-TILL-MOD SECTION.                                     
190800     SKIP2                                                                
190900     MOVE WS-ADRESS-TEXT     TO MOD-ADRESS-TEXT                           
191000     MOVE KOLLI-ADFLGEO      TO MOD-ADFLGEO                               
191100     MOVE KOLLI-ADFLOMR      TO MOD-ADFLOMR                               
191200     MOVE KOLLI-ADRUTNIV     TO MOD-ADRUTNIV                              
191300     MOVE KOLLI-ADVMODUL     TO MOD-ADVMODUL                              
191400     MOVE MED-756(INDX)      TO MOD-TEMFSINF                              
191500     .                                                                    
191600     EJECT                                                                
191700 BCD-NY-ADRESS-I-WDE611 SECTION.                                          
191800     SKIP3                                                                
191900     MOVE SPAR-IDTRPTNR             TO KOLLI-IDTRPTNR                     
192000     MOVE SPAR-ADCLGEO              TO KOLLI-ADCLGEO                      
192100     MOVE SPAR-ADFLOMR              TO KOLLI-ADFLOMR                      
192200     MOVE SPAR-ADRUTNIV             TO KOLLI-ADRUTNIV                     
192300     MOVE SPAR-DIHMODUL             TO KOLLI-DIHMODUL                     
192400     MOVE SPAR-DIDMODUL             TO KOLLI-DIDMODUL                     
192500     MOVE SPAR-ADVMODUL             TO KOLLI-ADVMODUL                     
192600     MOVE SPAR-ADHMODUL             TO KOLLI-ADHMODUL                     
192700     MOVE SPAR-FLUTLAST             TO KOLLI-FLUTLAST                     
192800     .                                                                    
192900     EJECT                                                                
193000 BD-KOLLA-VKORDBTO SECTION.                                               
193100                                                                          
193200*LK GROSS WT MUST BE LESS THAN (PART WT * QUANTITY).                      
193300*LK                            CASE WT IS NOT CONSIDERED                  
193400     IF WS-VKORDBTO-MID > ZERO                                            
193500       IF SEGMENT-FINNS                                                   
193600          MOVE ZERO TO WS-VKARTNTO-KG                                     
193700          PERFORM IMS-GN-WDE411-21-FSEQ                                   
193800          PERFORM UNTIL SEGMENT-SAKNAS                                    
193900              COMPUTE WS-VKARTNTO-KG = WS-VKARTNTO-KG +                   
194000                      ( ORAD-VKART-NTO-KG * KKOLLI-KVLEVART)              
194100              END-COMPUTE                                                 
194200              PERFORM IMS-GN-WDE411-21-FSEQ                               
194300          END-PERFORM                                                     
194400                                                                          
194500         IF WS-VKORDBTO-MID < WS-VKARTNTO-KG                              
194600            COMPUTE WS-VKORDBTO-KG ROUNDED =                              
194700                                       WS-VKARTNTO-KG                     
194800            MOVE INP-ERROR              TO RESP-IDMSG-ERROR               
194900            MOVE WEIGHT-ISSUE           TO MOD-TEMFSINF                   
195000                                      RESP-IDMSG-ERROR-LINE(1:26)         
195100            MOVE JA                     TO WS-FEL-FUNNET                  
195200            MOVE BAD-REQUEST       TO RESP-IDMFSINF                       
195300            IF US-MEASUREMENT                                             
195400               COMPUTE WS-VKORDBTO-KG ROUNDED =                           
195500                       CONV-KG-TO-LB * WS-VKARTNTO-KG                     
195600               MOVE 'LBS'               TO MOD-TEMFSINF(41:3)             
195700                                                                          
195800            ELSE                                                          
195900               MOVE 'KG'                TO MOD-TEMFSINF(41:2)             
196000            END-IF                                                        
196100*RC IS THIS RIGHT? NEED TO CONFIRM                                        
196200            IF API-SW = 'Y'                                               
196300               MOVE MOD-TEMFSINF(41:3)  TO                                
196400                                       RESP-IDMSG-ERROR-LINE(41:3)        
196500            END-IF                                                        
196600            INSPECT WS-VKORDBTO-KG REPLACING LEADING ZERO BY SPACE        
196700            MOVE WS-VKORDBTO-KG         TO MOD-TEMFSINF(29:10)            
196800                                     RESP-IDMSG-ERROR-LINE(29:10)         
196900            PERFORM S02-SAETT-ROER-EJ-FAELT                               
197000         END-IF                                                           
197100       END-IF                                                             
197200     END-IF                                                               
197300     .                                                                    
197400     EJECT                                                                
197500 C-PRINTA-KOLLIFLAGGA SECTION.                                            
197600                                                                          
197700     PERFORM CA-KOLLA-PRINTER                                             
197800                                                                          
197900     MOVE PRT-KDSVAR        TO WS-PRT-KDSVAR                              
198000     IF FEL-EJ-FUNNET AND PRT-KDSVAR = RAETT                              
198100         PERFORM S01-LAES-E611-VIA-E4                                     
198200         IF FEL-EJ-FUNNET                                                 
198300             MOVE WS-IDDISTR         TO 4333-MID-IDDISTR-UT               
198400             MOVE WS-IDKUNDNR        TO 4333-MID-IDKUNDNR-UT              
198500             MOVE WS-IDORDNR         TO 4333-MID-IDORDNR-UT               
198600             MOVE WS-IDKOLLI         TO 4333-MID-IDKOLLI-UT               
198700             MOVE WS-IDDC            TO 4333-MID-IDDC-UT                  
198800             IF WS-VORD-FLDIRLEV = 'J'                                    
198900               MOVE WS-IDPRODNR      TO 4333-MID-IDPRODNR-UT              
199000             ELSE                                                         
199100               MOVE ZERO             TO 4333-MID-IDPRODNR-UT              
199200             END-IF                                                       
199300             MOVE WS-KDPRTVAL        TO 4333-MID-KDPRTVAL-UT              
199400             MOVE ZERO               TO 4333-MID-IDKOLLI-TOM              
199500             MOVE '++++'             TO 4333-MID-IDDISTR-IN               
199600             MOVE '++++++'           TO 4333-MID-IDKUNDNR-IN              
199700             MOVE '+++++'            TO 4333-MID-IDORDNR-IN               
199800                                        4333-MID-IDKOLLI-IN               
199900             MOVE '++'               TO 4333-MID-IDDC-IN                  
200000             MOVE '+++++++'          TO 4333-MID-IDPRODNR-IN              
200100             MOVE '++'               TO 4333-MID-KDPRTVAL-IN              
200200                                                                          
200300             COMPUTE 4333-MID-LL =                                        
200400                              LENGTH OF 4333-MID-W4I33301 + 17            
200500             MOVE 'W4T333  '         TO 4333-MID-TRANSKOD                 
200600             MOVE '433B'             TO 4333-MID-IDTRANS                  
200700             MOVE WS-MFS-KDMFSFOR    TO 4333-MID-KDMFSFOR                 
200800                                                                          
200900             PERFORM IMS-INSERT-ALT-MSG                                   
201000             MOVE MED-780(INDX)      TO MOD-TEMFSINF                      
201100         END-IF                                                           
201200     END-IF                                                               
201300     .                                                                    
201400     EJECT                                                                
201500 CA-KOLLA-PRINTER SECTION.                                                
201600                                                                          
201700     IF WS-KDPRTVAL = 'U '                                                
201800     OR WS-KDPRTVAL = 'UU'                                                
201900       MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDPRTVAL-IN-ATTR                  
202000     ELSE                                                                 
202100*      IF NDC-JP                                                          
202200*        MOVE 'W40301'           TO WS-LASER-BLANKETT                     
202300*        MOVE WS-KDPRTVAL        TO WS-NDC-JAP-KDPRT                      
202400*      ELSE                                                               
202500         MOVE '4'                TO WS-SYSTDEL                            
202600         MOVE 'KF'               TO WS-LISTTYP                            
202700         MOVE WS-IDDC            TO WS-DC                                 
202800         MOVE WS-KDPRTVAL        TO WS-KDPRT                              
202900*      END-IF                                                             
203000                                                                          
203100       MOVE 001                  TO PRT-KDCALL                            
203200       MOVE WS-IDPRTLST          TO PRT-IDPRTLST                          
203300                                                                          
203400       CALL W006PRT USING PRT-W006PRT                                     
203500                                                                          
203600       IF PRT-KDSVAR = RAETT                                              
203700         MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDPRTVAL-IN-ATTR                
203800       ELSE                                                               
203900         IF PRT-KDSVAR = FEL                                              
204000           IF FEL-EJ-FUNNET                                               
204100             MOVE '772'         TO RESP-IDMSG-ERROR                       
204200             MOVE FEL-772(INDX)  TO MOD-TEMFSFEL                          
204300                                    RESP-IDMSG-ERROR-LINE                 
204400             MOVE JA             TO WS-FEL-FUNNET                         
204500             MOVE BAD-REQUEST    TO RESP-IDMFSINF                         
204600             PERFORM S02-SAETT-ROER-EJ-FAELT                              
204700           END-IF                                                         
204800         ELSE                                                             
204900           IF FEL-EJ-FUNNET                                               
205000             MOVE '773'         TO RESP-IDMSG-ERROR                       
205100             MOVE FEL-773(INDX) TO MOD-TEMFSFEL                           
205200                                   RESP-IDMSG-ERROR-LINE                  
205300             MOVE JA           TO WS-FEL-FUNNET                           
205400             MOVE BAD-REQUEST    TO RESP-IDMFSINF                         
205500             PERFORM S02-SAETT-ROER-EJ-FAELT                              
205600           END-IF                                                         
205700         END-IF                                                           
205800         MOVE MFS-ALFA-FAELT-FEL TO MOD-KDPRTVAL-IN-ATTR                  
205900       END-IF                                                             
206000     END-IF                                                               
206100     .                                                                    
206200     EJECT                                                                
206300 D-FRAAGA-STAELLD SECTION.                                                
206400                                                                          
206500     PERFORM S01-LAES-E611-VIA-E4                                         
206600     IF FEL-EJ-FUNNET                                                     
206700         EVALUATE TRUE                                                    
206800         WHEN KOLLI-KDKOLSTA > 1                                          
206900             IF FEL-EJ-FUNNET                                             
207000                 MOVE '721'         TO RESP-IDMSG-ERROR                   
207100                 MOVE FEL-721(INDX)     TO MOD-TEMFSFEL                   
207200                                           RESP-IDMSG-ERROR-LINE          
207300                 MOVE JA                TO WS-FEL-FUNNET                  
207400                 MOVE BAD-REQUEST       TO RESP-IDMFSINF                  
207500                 PERFORM S02-SAETT-ROER-EJ-FAELT                          
207600                 PERFORM S03-RENSA-UTFAELT                                
207700             END-IF                                                       
207800*   CHECK FOR NON ZERO KOLLI-DIKOLLI IS ADDED AS REPLACEMENT OF           
207900*   OF KOLLI-KDKOLSTA WHICH IS NOT 1 EVEN AFTER PACKING IN API            
208000         WHEN KOLLI-DIKOLLIL NOT = ZERO                                   
208100             MOVE KOLLI-KDKOLLI         TO MOD-KDKOLLI-UT                 
208200                                           RESP-KDKOLLI                   
208300             MOVE KOLLI-KDEMBTYP        TO MOD-KDEMBTYP-UT                
208400                                           RESP-KDEMBTYP                  
208500             IF US-MEASUREMENT                                            
208600               PERFORM DA-CONVERSION                                      
208700             ELSE                                                         
208800               MOVE KOLLI-DIKOLLIL      TO MOD-DIKOLLIL-UT                
208900                                           RESP-DIKOLLIL                  
209000               MOVE KOLLI-DIKOLLIB      TO MOD-DIKOLLIB-UT                
209100                                           RESP-DIKOLLIB                  
209200               MOVE KOLLI-DIKOLLIH      TO MOD-DIKOLLIH-UT                
209300                                           RESP-DIKOLLIH                  
209400               MOVE KOLLI-VKORDBTO-KOLLI TO MOD-VKORDBTO-UT               
209500                                            RESP-VKORDBTO                 
209600             END-IF                                                       
209700             MOVE WS-ADRESS-TEXT        TO MOD-ADRESS-TEXT                
209800                                           RESP-ADRESS-TEXT               
209900             MOVE KOLLI-ADFLGEO         TO MOD-ADFLGEO                    
210000                                           RESP-ADFLGEO                   
210100             MOVE KOLLI-ADFLOMR         TO MOD-ADFLOMR                    
210200                                           RESP-ADFLOMR                   
210300             MOVE KOLLI-ADRUTNIV        TO MOD-ADRUTNIV                   
210400                                           RESP-ADRUTNIV                  
210500             MOVE KOLLI-ADVMODUL        TO MOD-ADVMODUL                   
210600                                           RESP-ADVMODUL                  
210700*   CHECK FOR ZERO KOLLI-DIKOLLI IS ADDED AS REPLACEMENT OF               
210800*   OF KOLLI-KDKOLSTA WHICH IS NOT 1 EVEN AFTER PACKING IN API            
210900         WHEN KOLLI-DIKOLLIL = ZERO                                       
211000             IF FEL-EJ-FUNNET                                             
211100                 MOVE '717'         TO RESP-IDMSG-ERROR                   
211200                 MOVE FEL-717(INDX)     TO MOD-TEMFSFEL                   
211300                                           RESP-IDMSG-ERROR-LINE          
211400                 MOVE JA                TO WS-FEL-FUNNET                  
211500                 MOVE BAD-REQUEST       TO RESP-IDMFSINF                  
211600                 PERFORM S02-SAETT-ROER-EJ-FAELT                          
211700                 PERFORM S03-RENSA-UTFAELT                                
211800             END-IF                                                       
211900         END-EVALUATE                                                     
212000     END-IF                                                               
212100     .                                                                    
212200     EJECT                                                                
212300 DA-CONVERSION     SECTION.                                               
212400                                                                          
212500     MOVE KOLLI-DIKOLLIL              TO WS-SPAR-DIKOLLIL                 
212600     PERFORM S10-CONVERT-DIKOLLIL-TO-INCH                                 
212700     MOVE WS-SPAR-DIKOLLIL            TO MOD-DIKOLLIL-UT                  
212800                                                                          
212900     MOVE KOLLI-DIKOLLIB              TO WS-SPAR-DIKOLLIB                 
213000     PERFORM S11-CONVERT-DIKOLLIB-TO-INCH                                 
213100     MOVE WS-SPAR-DIKOLLIB            TO MOD-DIKOLLIB-UT                  
213200                                                                          
213300     MOVE KOLLI-DIKOLLIH              TO WS-SPAR-DIKOLLIH                 
213400     PERFORM S12-CONVERT-DIKOLLIH-TO-INCH                                 
213500     MOVE WS-SPAR-DIKOLLIH            TO MOD-DIKOLLIH-UT                  
213600                                                                          
213700     MOVE KOLLI-VKORDBTO-KOLLI        TO WS-SPAR-VKORDBTO                 
213800     PERFORM S13-CONVERT-VKORDBTO-TO-LB                                   
213900     MOVE WS-SPAR-VKORDBTO            TO MOD-VKORDBTO-UT                  
214000     .                                                                    
214100     SKIP2                                                                
214200 E-TEST-OM-MAN-MENAT-PF11 SECTION.                                        
214300                                                                          
214400     MOVE NEJ TO PF11-TEST                                                
214500                                                                          
214600     IF  MID-IDDISTR-IN  = ALL '+'                                        
214700     AND MID-IDKUNDNR-IN = ALL '+'                                        
214800     AND MID-IDORDNR-IN  = ALL '+'                                        
214900     AND MID-IDKOLLI-IN  = ALL '+'                                        
215000                                                                          
215100       IF MID-KDKOLLI-IN   = ALL '+' AND                                  
215200          MID-KDEMBTYP-IN  = ALL '+' AND                                  
215300          MID-DIKOLLIL-IN  = ALL '+' AND                                  
215400          MID-DIKOLLIB-IN  = ALL '+' AND                                  
215500          MID-DIKOLLIH-IN  = ALL '+' AND                                  
215600          MID-VKORDBTO-IN  = ALL '+'                                      
215700          CONTINUE                                                        
215800       ELSE                                                               
215900         MOVE JA TO PF11-TEST                                             
216000       END-IF                                                             
216100     END-IF                                                               
216200     .                                                                    
216300     EJECT                                                                
216400 F-RENSA-NYCKLAR SECTION.                                                 
216500*RC SHOULD WE HAVE RESP-?                                                 
216600     MOVE MFS-RENSA-FAELT      TO MOD-IDDISTR-UT                          
216700                                  MOD-IDKUNDNR-UT                         
216800                                  MOD-IDORDNR-UT                          
216900                                  MOD-IDKOLLI-UT                          
217000                                  MOD-KDPRTVAL-UT                         
217100     IF API-SW = 'Y'                                                      
217200       MOVE MFS-RENSA-FAELT    TO RESP-IDDISTR                            
217300                                  RESP-IDKUNDNR                           
217400                                  RESP-IDORDNR                            
217500                                  RESP-IDKOLLI                            
217600                                  RESP-IDDC                               
217700                                  RESP-KDPRTVAL                           
217800     END-IF                                                               
217900     .                                                                    
218000     EJECT                                                                
218100 S01-LAES-E611-VIA-E4 SECTION.                                            
218200                                                                          
218300     MOVE 'N'              TO WS-SLINGA-KLAR                              
218400                                                                          
218500     MOVE WS-IDDISTR       TO W-E4A1-IDDISTR                              
218600     MOVE WS-IDKUNDNR      TO W-E4A1-IDKUNDNR                             
218700     MOVE WS-IDORDNR       TO W-E4A1-IDORDNR                              
218800                                                                          
218900     PERFORM IMS-GU-E401-SEK-KVAL                                         
219000     IF KUNDORDER-SEK-FINNS                                               
219100        PERFORM UNTIL KUNDORDER-SEK-SAKNAS OR                             
219200                      SLINGA-KLAR                                         
219300           MOVE KORD-IDDISTR TO W-E401-IDDISTR                            
219400           MOVE KORD-IDKUNDNR TO W-E401-IDKUNDNR                          
219500           MOVE KORD-IDORDNR5 TO W-E401-IDORDNR                           
219600           MOVE KORD-IDPRODNR TO W-E401-IDPRODNR                          
219700           MOVE KORD-IDPLKLST TO W-E401-IDPLKLST                          
219800           PERFORM IMS-GU-E401-KVAL                                       
219900           MOVE KORD-KVORDRAD-LEVPL TO WS-SPAR-KVORDRAD-LEVPL             
220000           MOVE KORD-IDPRODNR TO W-E601-IDPRODNR                          
220100                                                                          
220200           PERFORM IMS-GU-E601                                            
220300                                                                          
220400           IF SEGMENT-FINNS                                               
220500                   IF  VORD-IDDC     = WS-IDDC                            
220600                   AND VORD-FLDIRLEV = NEJ                                
220700                     CONTINUE                                             
220800                   ELSE                                                   
220900                     SET SEGMENT-SAKNAS TO TRUE                           
221000                   END-IF                                                 
221100           END-IF                                                         
221200                                                                          
221300           IF SEGMENT-FINNS                                               
221400              MOVE 'J'       TO WS-SLINGA-KLAR                            
221500              MOVE VORD-IDPRODNR TO W-E601-IDPRODNR                       
221600                                    WS-IDPRODNR                           
221700                                    W-IDPRODNR-F                          
221800                                    W-421-IDPRODNR                        
221900                                                                          
222000              MOVE VORD-KDORDKL  TO WS-KDORDKL                            
222100              MOVE VORD-KDFRAKT  TO WS-KDFRAKT                            
222200              MOVE VORD-FLDIRLEV TO WS-VORD-FLDIRLEV                      
222300              MOVE WS-IDKOLLI    TO W-E611-IDKOLLI                        
222400                                    W-IDKOLLI-F                           
222500                                    W-421-IDKOLLI                         
222600                                                                          
222700              PERFORM IMS-GHNP-E611-KVAL                                  
222800              IF SEGMENT-SAKNAS                                           
222900                 IF FEL-EJ-FUNNET                                         
223000                    MOVE '758'         TO RESP-IDMSG-ERROR                
223100                    MOVE FEL-758(INDX) TO MOD-TEMFSFEL                    
223200                                          RESP-IDMSG-ERROR-LINE           
223300                    MOVE JA           TO WS-FEL-FUNNET                    
223400                    MOVE BAD-REQUEST  TO RESP-IDMFSINF                    
223500                    PERFORM S02-SAETT-ROER-EJ-FAELT                       
223600                    PERFORM S03-RENSA-UTFAELT                             
223700                 END-IF                                                   
223800              END-IF                                                      
223900                                                                          
224000           END-IF                                                         
224100           IF NOT SLINGA-KLAR                                             
224200             PERFORM IMS-GN-E401-SEK-KVAL                                 
224300           END-IF                                                         
224400        END-PERFORM                                                       
224500        IF KUNDORDER-SEK-SAKNAS                                           
224600           IF FEL-EJ-FUNNET                                               
224700             MOVE '7621'        TO RESP-IDMSG-ERROR                       
224800             MOVE FEL-7621(INDX)      TO MOD-TEMFSFEL                     
224900                                         RESP-IDMSG-ERROR-LINE            
225000             MOVE JA                  TO WS-FEL-FUNNET                    
225100             MOVE BAD-REQUEST         TO RESP-IDMFSINF                    
225200             PERFORM S02-SAETT-ROER-EJ-FAELT                              
225300             PERFORM S03-RENSA-UTFAELT                                    
225400          END-IF                                                          
225500        END-IF                                                            
225600     ELSE                                                                 
225700        IF FEL-EJ-FUNNET                                                  
225800           MOVE '7622'        TO RESP-IDMSG-ERROR                         
225900           MOVE FEL-7622(INDX) TO MOD-TEMFSFEL                            
226000                                  RESP-IDMSG-ERROR-LINE                   
226100           MOVE JA            TO WS-FEL-FUNNET                            
226200           MOVE BAD-REQUEST   TO RESP-IDMFSINF                            
226300           PERFORM S02-SAETT-ROER-EJ-FAELT                                
226400           PERFORM S03-RENSA-UTFAELT                                      
226500        END-IF                                                            
226600     END-IF                                                               
226700     .                                                                    
226800     EJECT                                                                
226900 S02-SAETT-ROER-EJ-FAELT SECTION.                                         
227000                                                                          
227100                                                                          
227200     MOVE MFS-ROER-EJ-FAELT     TO MOD-KDKOLLI-IN                         
227300                                   MOD-KDEMBTYP-IN                        
227400                                   MOD-DIKOLLIL-IN                        
227500                                   MOD-DIKOLLIB-IN                        
227600                                   MOD-DIKOLLIH-IN                        
227700                                   MOD-VKORDBTO-IN                        
227800                                   MOD-ADRESS-TEXT                        
227900                                   MOD-ADFLGEO                            
228000                                   MOD-ADFLOMR                            
228100                                   MOD-ADRUTNIV                           
228200                                   MOD-ADVMODUL                           
228300     MOVE MFS-ALFA-FAELT-RAETT  TO MOD-KDKOLLI-IN-ATTR                    
228400     MOVE MFS-NUM-FAELT-RAETT   TO MOD-KDEMBTYP-IN-ATTR                   
228500                                   MOD-DIKOLLIL-IN-ATTR                   
228600                                   MOD-DIKOLLIB-IN-ATTR                   
228700                                   MOD-DIKOLLIH-IN-ATTR                   
228800                                   MOD-VKORDBTO-IN-ATTR                   
228900     .                                                                    
229000     EJECT                                                                
229100 S03-RENSA-UTFAELT SECTION.                                               
229200                                                                          
229300     IF API-SW = 'N'                                                      
229400        MOVE MFS-RENSA-FAELT       TO MOD-KDKOLLI-UT                      
229500                                      MOD-KDEMBTYP-UT                     
229600                                      MOD-DIKOLLIL-UT                     
229700                                      MOD-DIKOLLIB-UT                     
229800                                      MOD-DIKOLLIH-UT                     
229900                                      MOD-VKORDBTO-UT                     
230000                                      MOD-ADRESS-TEXT                     
230100                                      MOD-ADFLGEO                         
230200                                      MOD-ADFLOMR                         
230300                                      MOD-ADRUTNIV                        
230400                                      MOD-ADVMODUL                        
230500     ELSE                                                                 
230600        MOVE MFS-ROER-EJ-FAELT     TO RESP-KDKOLLI                        
230700                                      RESP-KDEMBTYP                       
230800                                      RESP-VKORDBTO                       
230900                                      RESP-DIKOLLIL                       
231000                                      RESP-DIKOLLIB                       
231100                                      RESP-DIKOLLIH                       
231200                                      RESP-ADRESS-TEXT                    
231300                                      RESP-ADFLGEO                        
231400                                      RESP-ADFLOMR                        
231500                                      RESP-ADRUTNIV                       
231600                                      RESP-ADVMODUL                       
231700     END-IF                                                               
231800     .                                                                    
231900     EJECT                                                                
232000 S04-NOLLA-PLATSAREAN SECTION.                                            
232100                                                                          
232200                                                                          
232300     MOVE ZERO                  TO PLATS-KDCALL                           
232400                                   PLATS-IDDISTR                          
232500                                   PLATS-IDKUNDNR                         
232600                                   PLATS-KDFRAKT                          
232700                                   PLATS-IDORDNR                          
232800                                   PLATS-DIKOLLIH                         
232900                                   PLATS-DIKOLLIB                         
233000                                   PLATS-DIKOLLIL                         
233100                                   PLATS-VKORDNTO-KOLLI                   
233200                                   PLATS-IDTRPTNR                         
233300                                   PLATS-ADFLOMR                          
233400                                   PLATS-ADRUTNIV                         
233500                                   PLATS-DIHMODUL                         
233600                                   PLATS-DIDMODUL                         
233700                                   PLATS-ADVMODUL                         
233800                                   PLATS-ADHMODUL                         
233900                                   PLATS-VLRUTNIV                         
234000                                   PLATS-TIRFS                            
234100     MOVE SPACE                 TO PLATS-KDORDKLX                         
234200                                   PLATS-KDKOLLID                         
234300                                   PLATS-IDDC                             
234400                                   PLATS-ADFLGEO                          
234500                                   PLATS-FLUTLAST                         
234600                                   PLATS-TESPAERR                         
234700                                   PLATS-IDDC-CROSS                       
234800     .                                                                    
234900     SKIP2                                                                
235000 S10-CONVERT-DIKOLLIL-TO-INCH      SECTION.                               
235100                                                                          
235200     COMPUTE WS-SPAR-DIKOLLIL ROUNDED =                                   
235300             WS-SPAR-DIKOLLIL * CONV-CM-TO-IN                             
235400     END-COMPUTE                                                          
235500     .                                                                    
235600     SKIP2                                                                
235700 S11-CONVERT-DIKOLLIB-TO-INCH       SECTION.                              
235800                                                                          
235900     COMPUTE WS-SPAR-DIKOLLIB ROUNDED =                                   
236000             WS-SPAR-DIKOLLIB * CONV-CM-TO-IN                             
236100     END-COMPUTE                                                          
236200     .                                                                    
236300     SKIP2                                                                
236400 S12-CONVERT-DIKOLLIH-TO-INCH       SECTION.                              
236500                                                                          
236600     COMPUTE WS-SPAR-DIKOLLIH ROUNDED =                                   
236700             WS-SPAR-DIKOLLIH * CONV-CM-TO-IN                             
236800     END-COMPUTE                                                          
236900     .                                                                    
237000     SKIP2                                                                
237100 S13-CONVERT-VKORDBTO-TO-LB         SECTION.                              
237200                                                                          
237300     COMPUTE WS-SPAR-VKORDBTO  ROUNDED =                                  
237400             WS-SPAR-VKORDBTO * CONV-KG-TO-LB                             
237500     END-COMPUTE                                                          
237600     .                                                                    
237700     SKIP2                                                                
237800 S14-CONVERT-DIKOLLIL-TO-CM        SECTION.                               
237900                                                                          
238000     COMPUTE WS-SPAR-DIKOLLIL ROUNDED =                                   
238100             WS-SPAR-DIKOLLIL * CONV-IN-TO-CM                             
238200     END-COMPUTE                                                          
238300     .                                                                    
238400     SKIP2                                                                
238500 S15-CONVERT-DIKOLLIB-TO-CM         SECTION.                              
238600                                                                          
238700     COMPUTE WS-SPAR-DIKOLLIB ROUNDED =                                   
238800             WS-SPAR-DIKOLLIB * CONV-IN-TO-CM                             
238900     END-COMPUTE                                                          
239000     .                                                                    
239100     SKIP2                                                                
239200 S16-CONVERT-DIKOLLIH-TO-CM         SECTION.                              
239300                                                                          
239400     COMPUTE WS-SPAR-DIKOLLIH ROUNDED =                                   
239500             WS-SPAR-DIKOLLIH * CONV-IN-TO-CM                             
239600     END-COMPUTE                                                          
239700     .                                                                    
239800     SKIP2                                                                
239900 S17-CONVERT-VKORDBTO-TO-KG         SECTION.                              
240000                                                                          
240100     COMPUTE WS-SPAR-VKORDBTO ROUNDED =                                   
240200             WS-DEC-IDEDITDATA * CONV-LB-TO-KG                            
240300     END-COMPUTE                                                          
240400     .                                                                    
240500     SKIP2                                                                
240600 S18-FETCH-REQUEST-ARGUMENT SECTION.                                      
240700     MOVE 'GETARG'               TO SUB-KDFUNC                            
240800                                                                          
240900     MOVE 'CARPARTS.PULS.CORRECTREPCASE'    TO SUB-ADDISPABS              
241000     MOVE SPACE TO SUB-DATA                                               
241100     MOVE LENGTH OF SUB-DATA          TO SUB-KVDLEN                       
241200                                                                          
241300     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN SUB-DATA              
241400                                                                          
241500     IF SUB-KDRC > 0                                                      
241600       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
241700       STRING 'WZ01SUB GETARG ERROR RC=' KDRC-DISPLAY                     
241800       DELIMITED BY SIZE INTO ERROR-TEXT                                  
241900       CALL FELLOG USING RKOD-ABEND-WITH-DUMP                             
242000     END-IF                                                               
242100     .                                                                    
242200     SKIP3                                                                
242300 S19-RETURN-RESPONSE SECTION.                                             
242400                                                                          
242500     MOVE 'RETURN'                   TO SUB-KDFUNC                        
242600     MOVE LENGTH OF RESP-AREA        TO SUB-KVDLEN                        
242700                                                                          
242800     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN RESP-AREA             
242900                                                                          
243000     IF SUB-KDRC > 0                                                      
243100       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
243200       STRING 'WZ01SUB RETURN ERROR RC=' KDRC-DISPLAY                     
243300       DELIMITED BY SIZE INTO ERROR-TEXT                                  
243400       CALL FELLOG USING RKOD-ABEND-WITH-DUMP                             
243500     END-IF                                                               
243600     .                                                                    
243700 S20-MSG-CONV SECTION.                                                    
243800                                                                          
243900     MOVE SPACES                  TO RESP-MESSAGES (1)                    
244000                                     RESP-MESSAGES (2)                    
244100     MOVE 1                       TO MSG-IX                               
244200*    REQUEST OK                                                           
244300     MOVE 200                     TO RESP-KDSTATUS-API                    
244400                                                                          
244500*    BAD REQUEST                                                          
244600     IF RESP-IDMSG-ERROR > SPACE                                          
244700       MOVE 400                   TO RESP-KDSTATUS-API                    
244800       MOVE RESP-IDMSG-ERROR      TO RESP-IDMSG   (MSG-IX)                
244900       MOVE RESP-IDMSG-ERROR-LINE TO RESP-MESSAGE (MSG-IX)                
245000     END-IF                                                               
245100     .                                                                    
245200* IMS SEKTIONER                                                           
245300     SKIP3                                                                
245400 IMS-GET-MSG SECTION.                                                     
245500     MOVE '  QC' TO GODK-STATUSKODER                                      
245600     CALL CBLTDLI USING GU                                                
245700                          MSG-PCB                                         
245800                          MSG-IO-AREA                                     
245900     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
246000     PERFORM IMS-STATUSKONTROLL                                           
246100     SKIP3                                                                
246200     .                                                                    
246300 IMS-INSERT-MSG SECTION.                                                  
246400                                                                          
246500     IF MSGI-IDLAND-SPR NOT = 'GB'                                        
246600       MOVE '0' TO MFS-KDHUVOMR                                           
246700     END-IF                                                               
246800     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
246900     MOVE SPACE TO GODK-STATUSKODER                                       
247000     CALL CBLTDLI USING ISRT                                              
247100                          MSG-PCB                                         
247200                          MSG-IO-AREA                                     
247300                          MFS-IDMOD                                       
247400     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
247500     PERFORM IMS-STATUSKONTROLL                                           
247600     SKIP3                                                                
247700     .                                                                    
247800 IMS-INSERT-ALT-MSG SECTION.                                              
247900     MOVE LOW-VALUE TO 4333-MID-Z1 4333-MID-Z2                            
248000     MOVE SPACE TO GODK-STATUSKODER                                       
248100     CALL CBLTDLI USING ISRT                                              
248200                          ALT-PCB                                         
248300                          4333-MID-IO-AREA                                
248400     MOVE ALT-STATUS-CODE TO STATUS-WS                                    
248500     PERFORM IMS-STATUSKONTROLL                                           
248600     .                                                                    
248700     EJECT                                                                
248800 IMS-GU-K501-KVAL SECTION.                                                
248900     STRING 'WLEMBB01(KDKOLLI  =' W-K501-KDKOLLI-X ')'                    
249000            DELIMITED BY SIZE INTO SSA1                                   
249100     MOVE '  GE' TO GODK-STATUSKODER                                      
249200     CALL CBLTDLI USING GU                                                
249300                          EMBB-PCB                                        
249400                          DLI-IO-AREA                                     
249500                          SSA1                                            
249600     MOVE EMBB-STATUS-CODE TO STATUS-WS                                   
249700     PERFORM IMS-STATUSKONTROLL                                           
249800     SKIP3                                                                
249900     .                                                                    
250000 IMS-GU-E401-KVAL SECTION.                                                
250100     STRING 'WDE401  (WDE401KY =' W-E401-WDE4KEY-X ')'                    
250200            DELIMITED BY SIZE INTO SSA1                                   
250300     MOVE '    ' TO GODK-STATUSKODER                                      
250400     CALL CBLTDLI USING GU                                                
250500                          WDE4-PCB                                        
250600                          DLI-IO-AREA                                     
250700                          SSA1                                            
250800     MOVE WDE4-STATUS-CODE TO STATUS-WS                                   
250900     PERFORM IMS-STATUSKONTROLL                                           
251000     SKIP3                                                                
251100     .                                                                    
251200 IMS-GU-E401-SEK-KVAL SECTION.                                            
251300                                                                          
251400     STRING 'WDE401  (WDE4ASEQ =' W-E4A1-WDE4ASEQ-X ')'                   
251500            DELIMITED BY SIZE INTO SSA1                                   
251600     MOVE '  GE' TO GODK-STATUSKODER                                      
251700     CALL CBLTDLI USING GU                                                
251800                          WDE4A-PCB                                       
251900                          DLI-IO-AREA2                                    
252000                          SSA1                                            
252100     MOVE WDE4A-STATUS-CODE TO STATUS-WS                                  
252200                               KUNDORDER-SEK-WS                           
252300     PERFORM IMS-STATUSKONTROLL                                           
252400     SKIP3                                                                
252500     .                                                                    
252600 IMS-GN-E401-SEK-KVAL SECTION.                                            
252700                                                                          
252800     STRING 'WDE401  (WDE4ASEQ =' W-E4A1-WDE4ASEQ-X ')'                   
252900            DELIMITED BY SIZE INTO SSA1                                   
253000     MOVE '  GE' TO GODK-STATUSKODER                                      
253100     CALL CBLTDLI USING GN                                                
253200                          WDE4A-PCB                                       
253300                          DLI-IO-AREA2                                    
253400                          SSA1                                            
253500     MOVE WDE4A-STATUS-CODE TO STATUS-WS                                  
253600                               KUNDORDER-SEK-WS                           
253700     PERFORM IMS-STATUSKONTROLL                                           
253800     SKIP3                                                                
253900     .                                                                    
254000 IMS-GN-WDE411-21-FSEQ  SECTION.                                          
254100     STRING 'WDE411  *D(WDE4FSEQ =' W-WDE4FSEQ-X ')'                      
254200            DELIMITED BY SIZE INTO SSA1                                   
254300     STRING 'WDE421  (WDE421KY =' W-WDE421-IDKOLLI-X ')'                  
254400            DELIMITED BY SIZE INTO SSA2                                   
254500     MOVE '  GE' TO GODK-STATUSKODER                                      
254600     CALL CBLTDLI USING GN WDE44-PCB DLI-IO-WDE411-21 SSA1 SSA2           
254700     MOVE WDE44-STATUS-CODE TO STATUS-WS                                  
254800     PERFORM IMS-STATUSKONTROLL                                           
254900     .                                                                    
255000     EJECT                                                                
255100 IMS-GHU-E601 SECTION.                                                    
255200     STRING 'WDE601  (IDPRODNR =' W-E601-IDPRODNR-X ')'                   
255300            DELIMITED BY SIZE INTO SSA1                                   
255400     MOVE '  ' TO GODK-STATUSKODER                                        
255500     CALL CBLTDLI USING GHU                                               
255600                          WDE6-PCB                                        
255700                          VORD-WDE601                                     
255800                          SSA1                                            
255900     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
256000     PERFORM IMS-STATUSKONTROLL                                           
256100     SKIP3                                                                
256200     .                                                                    
256300 IMS-GU-E601 SECTION.                                                     
256400     STRING 'WDE601  (IDPRODNR =' W-E601-IDPRODNR-X ')'                   
256500            DELIMITED BY SIZE INTO SSA1                                   
256600     MOVE '  ' TO GODK-STATUSKODER                                        
256700     CALL CBLTDLI USING GHU                                               
256800                          WDE6-PCB                                        
256900                          VORD-WDE601                                     
257000                          SSA1                                            
257100     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
257200     PERFORM IMS-STATUSKONTROLL                                           
257300     SKIP3                                                                
257400     .                                                                    
257500 IMS-GHNP-E611-KVAL SECTION.                                              
257600     STRING 'WDE611  (IDKOLLI  =' W-E611-IDKOLLI-X ')'                    
257700            DELIMITED BY SIZE INTO SSA1                                   
257800     MOVE '  GE' TO GODK-STATUSKODER                                      
257900     CALL CBLTDLI USING GHNP                                              
258000                          WDE6-PCB                                        
258100                          KOLLI-WDE611                                    
258200                          SSA1                                            
258300     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
258400     PERFORM IMS-STATUSKONTROLL                                           
258500     SKIP3                                                                
258600     .                                                                    
258700 IMS-REPLACE-E601 SECTION.                                                
258800     MOVE '  ' TO GODK-STATUSKODER                                        
258900     CALL CBLTDLI USING REPL                                              
259000                          WDE6-PCB                                        
259100                          VORD-WDE601                                     
259200     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
259300     PERFORM IMS-STATUSKONTROLL                                           
259400     SKIP3                                                                
259500     .                                                                    
259600 IMS-REPLACE-E611 SECTION.                                                
259700     MOVE '  ' TO GODK-STATUSKODER                                        
259800     CALL CBLTDLI USING REPL                                              
259900                          WDE6-PCB                                        
260000                          KOLLI-WDE611                                    
260100     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
260200     PERFORM IMS-STATUSKONTROLL                                           
260300     .                                                                    
260400     EJECT                                                                
260500 IMS-STATUSKONTROLL SECTION.                                              
260600     SET STATUS-IX TO 1                                                   
260700     SEARCH GODK-STATUS AT END CALL FELLOG                                
260800       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
260900     END-SEARCH                                                           
261000     .                                                                    
