000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W4033100.                                                
000300 AUTHOR.         CAP GEMINI / BOH                                         
000400 DATE-WRITTEN.   FEB   86.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION.                                                            
000800*    PROGRAMMET ÄR ETT RENT UPPDATERINGSPROGRAM.                          
000900*    DET UPPDATERAR KOLLIUPPGIFTER PÅ OLIKA REGISTER                      
001000*    EFTER INMATNING VID BANDSTATION.                                     
001100*    EFTER DET ATT UPPDATERINGARNA ÄR GJORDA, LÄGGS                       
001200*    W4O33701 -MODEN UT FÖR ATT BRUTTOVIKTEN SKALL                        
001300*    REGISTRERAS.                                                         
001400*    DENNA UPPDATERING GÖRS AV W4033700 SOM I SIN TUR                     
001500*    LÄGGER UT W4O33101-MODEN NÄR BRUTTOVIKTEN UPP-                       
001600*    DATERATS, SAMT SKAPAR TRANS TILL 4333 SOM SKRIVER                    
001700*    ADRESSFLAGGA.                                                        
001800*                                                                         
001900*    VAL 'U' VID UTSKRIFT AV KOLLIFLAGGA ELLER FÖLJESEDEL                 
002000*    SKALL INTE GE NÅGON UTSKRIFT, MED ÄR ETT GODKÄNT VAL.                
002100*                                                                         
002200*    INDATA.                                                              
002300*        TRANSAKTION: W4T331                                              
002400*        MID:         W4I33101                                            
002500*                                                                         
002600*    UTDATA.                                                              
002700*        TRANSAKTION: W4T333                                              
002800*        MOD:         W4O33101                                            
002900*                     W4O33701                                            
003000*                                                                         
003100*    CHANGE LOG                                                           
003200*                                                                         
003300*    DIGAMBAR/021011                                                      
003400*    STRUCTURE OF ACTION TRANSACTION 4487 IS CHANGED TO IMPROVE           
003500*    THE RESPONSE TIME OF THE SCREEN 4312.                                
003600     EJECT                                                                
003700 ENVIRONMENT DIVISION.                                                    
003800     SKIP3                                                                
003900 DATA DIVISION.                                                           
004000     EJECT                                                                
004100 WORKING-STORAGE SECTION.                                                 
004200*    -- CHECKED BY WY2000                                                 
004300     SKIP3                                                                
004400 77  IDPGM                       PIC X(8)    VALUE 'W4033100'.            
004500 77  JA                          PIC X(1)    VALUE 'J'.                   
004600 77  NEJ                         PIC X(1)    VALUE 'N'.                   
004700 77  WS-IDDC                     PIC X(2)    VALUE SPACE.                 
004800 77  FEL                         PIC X(1)    VALUE 'F'.                   
004900 77  RAETT                       PIC X(1)    VALUE 'R'.                   
005000 77  INDX                        PIC S9(4)   COMP SYNC.                   
005100 77  WS-MFS-KDMFSFOR             PIC 9.                                   
005200 77  WS-DAGENS-DATUM             PIC 9(6)   VALUE ZERO.                   
005300 77  WS-TIDPUNKT                 PIC 9(8)   VALUE ZERO.                   
005400 77  WS-TISKPTID                 PIC 9(6)   VALUE ZERO.                   
005500 01  WS-IDDISTR                  PIC X(4).                                
005600 01  WS-IDDISTR-NUM REDEFINES WS-IDDISTR PIC 9(4).                        
005700 77  WS-IDKUNDNR                 PIC X(6).                                
005800 77  WS-IDKUNDNR-7               PIC 9(7).                                
005900 77  WS-IDKOLLI                  PIC X(5).                                
006000 77  WS-EMBPROF                  PIC X.                                   
006100 77  WS-KDKOLLI                  PIC X(8).                                
006200 77  WS-KDKOLLID                 PIC X.                                   
006300 77  WS-IDKOLLI-NUM5             PIC 9(5).                                
006400 77  WS-IDPLKLST                 PIC S9(3)   VALUE ZERO COMP-3.           
006500 77  SPAR-ORDD-KOLLI-KDKOLSTA    PIC S9      COMP-3.                      
006600 77  WS-KDEMBTYP                 PIC S9      COMP-3.                      
006700 77  WS-DIKOLLIL                 PIC S9(5)   COMP-3.                      
006800 77  WS-DIKOLLIB                 PIC S9(3)   COMP-3.                      
006900 77  WS-DIKOLLIH                 PIC S9(3)   COMP-3.                      
007000 77  WS-MOD-DIKOLLIL             PIC S9(5)   COMP-3.                      
007100 77  WS-MOD-DIKOLLIB             PIC S9(3)   COMP-3.                      
007200 77  WS-MOD-DIKOLLIH             PIC S9(3)   COMP-3.                      
007300 77  WS-VLORDBTO                 PIC S9(4)V9(3) COMP-3.                   
007400 77  WS-SUORDV-KOLLI             PIC S9(9)V9(2) COMP-3.                   
007500 77  WS-SUORDV-KOLLI-LOC         PIC S9(9)V9(2) COMP-3.                   
007600 77  WS-SUORDV-KOLLI-LOCPREL     PIC S9(9)V9(2) COMP-3.                   
007700 77  WS-KDFRAKT                  PIC S9(3)   COMP-3.                      
007800 77  WS-KDFRAKT-NUM2             PIC  9(2)   VALUE ZERO.                  
007900 77  WS-IDPRODNR                 PIC  9(7).                               
008000 77  WS-KDORDKL                  PIC S9      COMP-3.                      
008100 77  WS-FLAUTFAK                 PIC X.                                   
008200 77  WS-KDFAKTYP                 PIC X.                                   
008300 77  WS-KVLOCK                   PIC S9(3)   COMP-3.                      
008400 77  WS-SAVE-VORD-KDORDSTA       PIC S9(1)   VALUE ZERO COMP-3.           
008500 77  WS-SAVE-KVORDRAD-LEVPL      PIC S9(5)   VALUE ZERO COMP-3.           
008600 77  4337-MOD-LAENGD             PIC S9(4)   VALUE +200 COMP SYNC.        
008700 77  4331-MOD-LAENGD             PIC S9(4)   VALUE +210 COMP SYNC.        
008800 77  WS-W006-KDSVAR-FOLJEFL      PIC X(1)    VALUE SPACE.                 
008900 77  KDRC-DISP                   PIC 9(4)    VALUE ZERO.                  
009000 77  WS-IDCOM                    PIC S9(9)   VALUE ZERO COMP-3.           
009100 77  W-IDSHIPM                   PIC 9(7)    VALUE ZERO.                  
009200 77  WS-TIORDREG-NUM6            PIC 9(06)   VALUE ZERO.                  
009300                                                                          
009400 77  TMS-IX                      PIC S9(9)  VALUE +0   COMP SYNC.         
009500 77  FELTEXT                     PIC X(64)  VALUE SPACE.                  
009600 77  RKOD-ABEND                  PIC S9(4)  VALUE +33   COMP SYNC.        
009700 77  IX                          PIC S9(9)  VALUE ZERO  COMP SYNC.        
009800 77  IX-MAX                      PIC S9(9)  VALUE +7    COMP SYNC.        
009900 01  WS-TIPACKN-GRP              PIC 9(7).                                
010000 01  FILLER REDEFINES WS-TIPACKN-GRP.                                     
010100     03  FILLER                  PIC 9.                                   
010200     03  WS-TIPACKN              PIC 9(6).                                
010300                                                                          
010400 01  WS-KDMATT                   PIC X.                                   
010500     88 US-MEASUREMENT           VALUE 'U'.                               
010600     88 SIS-MEASUREMENT          VALUE 'S'.                               
010700                                                                          
010800 01  WS-IDPRTLST.                                                         
010900     03 WS-SYSTDEL               PIC X(1).                                
011000     03 WS-LISTTYP               PIC X(2).                                
011100     03 WS-DC                    PIC X(2).                                
011200     03 WS-KDPRT                 PIC X(3).                                
011300 01  WS-IDPRTJAP REDEFINES WS-IDPRTLST.                                   
011400     03 WS-LASER-BLANKETT        PIC X(6).                                
011500     03 WS-NDC-JAP-KDPRT         PIC X(3).                                
011600                                                                          
011700 01  WS-VKTARA                   PIC S9(6)V9(1) VALUE ZERO.               
011800*                                                                         
011900 77  WS-SLINGA-KLAR              PIC X(1).                                
012000     88  SLINGA-KLAR                         VALUE 'J'.                   
012100                                                                          
012200 77  4490-BORTTAG-SW             PIC X(1).                                
012300     88  SEG-4490-BORTTAG                    VALUE 'J'.                   
012400                                                                          
012500 77  FOERSTA-SW                  PIC X(1).                                
012600     88  FOERSTA-PERFORM-UNTIL               VALUE 'J'.                   
012700*                                                                         
012800 77    IDPRODNR-IFYLLT-SW        PIC X(01).                               
012900   88  IDPRODNR-IFYLLT                       VALUE 'J'.                   
013000*                                                                         
013100                                                                          
013200 77  WS-IDTRANS                  PIC X(4)    VALUE '4331'.                
013300     88  GODKAEND-BILD                       VALUE '4331' '4337'.         
013400     88  GODKAEND-BILDFAMILJ                 VALUE '4331' '4332'          
013500                                     '4333' '4334' '4335' '4336'.         
013600     SKIP2                                                                
013700 77  WS-VORD-FARDIGPACKAD        PIC X(1).                                
013800     EJECT                                                                
013900 01  DYNAMISKA-SUBPROGRAM.                                                
014000     03  CBLTDLI                 PIC X(8)  VALUE 'CBLTDLI '.              
014100     03  FELLOG                  PIC X(8)  VALUE 'FELLOG  '.              
014200     03  W005INIT                PIC X(8)  VALUE 'W005INIT'.              
014300     03  W403PLAT                PIC X(8)  VALUE 'W403PLAT'.              
014400     03  W006PRT                 PIC X(8)  VALUE 'W006PRT '.              
014500     03  WWOMVAND                PIC X(8)  VALUE 'WWOMVAND'.              
014600     03  W403TMS1                PIC X(8)  VALUE 'W403TMS1'.              
014700                                                                          
014800*                                                                         
014900*    --- AREOR TILL GEMENSAMMA SUBPROGRAM                                 
015000*                                                                         
015100 01  FILLER                      PIC X(16)  VALUE 'WMSGINIT'.             
015200*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
015300*01 -COPY WMSGINIT                                                        
015400     SKIP2                                                                
015500*                                                                         
015600 01  FILLER                      PIC X(16)  VALUE 'W403PLAT '.            
015700*01 -COPY W403PLAT                                                        
015800     SKIP2                                                                
015900*                                                                         
016000 01  FILLER                      PIC X(16)  VALUE 'W006PRT  '.            
016100*   -COPY W006PRT                                                         
016200     SKIP2                                                                
016300                                                                          
016400 01  FILLER                      PIC X(16)  VALUE 'WWOMVAND '.            
016500*   -COPY WWOMVAND                                                        
016600     SKIP2                                                                
016700                                                                          
016800 01  FILLER                      PIC X(16)  VALUE 'W403TMS1 '.            
016900*   -COPY W403TMS1                                                        
017000     SKIP2                                                                
017100                                                                          
017200 01  WS-IDKUNDRF.                                                         
017300                                                                          
017400     03  WS-IDORDNR              PIC X(5).                                
017500     03  FILLER                  PIC X(5)  VALUE SPACE.                   
017600                                                                          
017700                                                                          
017800 01  WS-TIPACTID-8.                                                       
017900                                                                          
018000     03  WS-TIPACTID-6           PIC 9(6).                                
018100     03  FILLER                  PIC 9(2).                                
018200                                                                          
018300                                                                          
018400                                                                          
018500 01  WS-FEL-FUNNET               PIC X     VALUE 'N'.                     
018600                                                                          
018700     88  FEL-FUNNET                        VALUE 'J'.                     
018800     88  FEL-EJ-FUNNET                     VALUE 'N'.                     
018900                                                                          
019000                                                                          
019100                                                                          
019200 01  WS-DISTRIKT-TYP             PIC X(3)  VALUE SPACE.                   
019300                                                                          
019400     88  EXPORT-DISTRIKT                   VALUE 'EXP'.                   
019500     88  SVERIGE-DISTRIKT                  VALUE 'SVE'.                   
019600     SKIP3                                                                
019700 01  WS-TEXTER.                                                           
019800                                                                          
019900     03  WS-ADRESS-TEXT         PIC X(7)  VALUE 'ADRESS:'.                
020000     03  WS-SLUT-TEXT           PIC X(15) VALUE ' REDAN UPPTAGEN'.        
020100     EJECT                                                                
020200 01  NYCKLAR-TILL-DLI.                                                    
020300                                                                          
020400   03    W-WDE4E-IDPRODNR-X.                                              
020500     05    W-WDE4E-IDPRODNR      PIC S9(7)   VALUE ZERO  COMP-3.          
020600     EJECT                                                                
020700     03  W-K501-KDKOLLI-X.                                                
020800                                                                          
020900         05  W-K501-KDKOLLI      PIC X(8).                                
021000                                                                          
021100     03  W-E4A1-WDE4KEY-X.                                                
021200         05  W-E4A1-IDDISTR      PIC S9(5)   COMP-3.                      
021300         05  W-E4A1-IDKUNDNR     PIC S9(7)   COMP-3.                      
021400         05  W-E4A1-IDKUNDRF.                                             
021500             07  W-E4A1-IDORDNR  PIC X(5).                                
021600             07  FILLER          PIC X(5)    VALUE SPACE.                 
021700                                                                          
021800     03  W-E401-WDE4KEY-X.                                                
021900         05  W-E401-IDDISTR      PIC S9(5)   COMP-3.                      
022000         05  W-E401-IDKUNDNR     PIC S9(7)   COMP-3.                      
022100         05  W-E401-IDKUNDRF.                                             
022200             07  W-E401-IDORDNR  PIC X(5).                                
022300             07  FILLER          PIC X(5)    VALUE SPACE.                 
022400         05  W-E401-IDPRODNR     PIC S9(7)   COMP-3.                      
022500         05  W-E401-IDPLKLST     PIC S9(3)   COMP-3.                      
022600                                                                          
022700     03  W-E601-IDPRODNR-X.                                               
022800                                                                          
022900         05  W-E601-IDPRODNR     PIC S9(7)   COMP-3.                      
023000                                                                          
023100     03  W-E611-IDKOLLI-X.                                                
023200                                                                          
023300         05  W-E611-IDKOLLI      PIC S9(5)   COMP-3.                      
023400                                                                          
023500     03  W-E612-IDPURAD-MIN-X.                                            
023600         05  W-E612-IDPURAD-MIN  PIC  X(3)   VALUE LOW-VALUE.             
023700                                                                          
023800     03  W-E612-IDPURAD-MAX-X.                                            
023900         05  W-E612-IDPURAD-MAX  PIC  X(3)   VALUE HIGH-VALUE.            
024000                                                                          
024100     03  W-WDQ201-X.                                                      
024200         05  W-Q201-IDORDER      PIC S9(7)   COMP-3.                      
024300                                                                          
024400     03  W-IDDC-X.                                                        
024500         05  W-IDDC              PIC  X(2)   VALUE SPACES.                
024600                                                                          
024700     03  W-Q301-KEY-X.                                                    
024800         05  W-Q301-IDORDER      PIC S9(7)   COMP-3.                      
024900         05  W-Q301-IDDC         PIC X(2).                                
025000         05  W-Q301-IDPRODNR     PIC S9(7)   COMP-3.                      
025100         05  W-Q301-IDPLKLST     PIC S9(3)   COMP-3.                      
025200                                                                          
025300     03  W-Q301-KEY-MIN-X.                                                
025400         05  W-Q301-MIN-IDORDER  PIC S9(7)   COMP-3.                      
025500         05  W-Q301-MIN-IDDC     PIC X(2).                                
025600         05  W-Q301-MIN-IDPRODNR PIC S9(7)   COMP-3.                      
025700         05  FILLER              PIC X(2)    VALUE LOW-VALUE.             
025800                                                                          
025900     03  W-Q301-KEY-MAX-X.                                                
026000         05  W-Q301-MAX-IDORDER  PIC S9(7)   COMP-3.                      
026100         05  W-Q301-MAX-IDDC     PIC X(2).                                
026200         05  W-Q301-MAX-IDPRODNR PIC S9(7)   COMP-3.                      
026300         05  FILLER              PIC X(2)    VALUE HIGH-VALUE.            
026400                                                                          
026500     03  W-4301-WDGXKEY-X.                                                
026600         05 W-4301-IDHTYP        PIC X(4)  VALUE '4301'.                  
026700         05 W-4301-IDPRODNR      PIC S9(7) VALUE ZERO COMP-3.             
026800         05 W-4301-NYCKEL-VALFRI PIC X(22) VALUE LOW-VALUE.               
026900                                                                          
027000     03  W-4302-WDGXKEY-X.                                                
027100         05 W-4302-IDKOLLI-X.                                             
027200            07 W-4302-IDKOLLI    PIC S9(5) VALUE ZERO COMP-3.             
027300         05 W-4302-IDPLKLST-X.                                            
027400            07 W-4302-IDPLKLST   PIC S9(3) VALUE ZERO COMP-3.             
027500                                                                          
027600     03  W-4302-IDKOLLI-MIN-X.                                            
027700         05 W-4302-IDKOLLI-MIN   PIC X(3)  VALUE LOW-VALUE.               
027800                                                                          
027900     03  W-4302-IDKOLLI-MAX-X.                                            
028000         05 W-4302-IDKOLLI-MAX   PIC X(3)  VALUE HIGH-VALUE.              
028100                                                                          
028200     03  W-4726-WDGXKEY-ROT-X.                                            
028300                                                                          
028400         05  W-4726-IDHTYP       PIC X(4)    VALUE '4726'.                
028500         05  W-4726-FLBATCH      PIC X(1)    VALUE SPACE.                 
028600         05  W-4726-LOWVALUE     PIC X(25)   VALUE LOW-VALUE.             
028700                                                                          
028800     03  W-4726-WDGXKEY-UNDSEG-X.                                         
028900                                                                          
029000         05  W-4726-IDDISTR      PIC S9(5)   COMP-3.                      
029100         05  W-4726-IDKUNDNR     PIC S9(7)   COMP-3.                      
029200         05  W-4726-IDDC         PIC X(2).                                
029300         05  W-4726-KDFAKTYP     PIC X.                                   
029400*                                                                         
029500     03  W-4447-WDGXKEY-X.                                                
029600         05  FILLER              PIC X(4)  VALUE '4447'.                  
029700         05  W-4447-IDDC         PIC X(2).                                
029800         05  FILLER              PIC X(24) VALUE LOW-VALUE.               
029900*                                                                         
030000     03  W-4448-WDGXKEY-X.                                                
030100         05  W-4448-IDPRC        PIC X(4).                                
030200         05  FILLER              PIC X(1)  VALUE LOW-VALUE.               
030300*                                                                         
030400     03  W-4487-WDGXKEY-X.                                                
030500         05  FILLER              PIC X(4)  VALUE '4487'.                  
030600         05  W-4487-IDDC         PIC X(2).                                
030700         05  FILLER              PIC X(24) VALUE LOW-VALUE.               
030800*                                                                         
030900     03  W-4488-WDGXKEY-X.                                                
031000         05  W-4488-KDPRCGRP     PIC X(5).                                
031100*                                                                         
031200     03  W-4490-WDGXKEY-X.                                                
031300         05  W-4490-DARFS        PIC 9(12).                               
031400         05  W-4490-IDPRODNR     PIC S9(7)  COMP-3.                       
031500         05  W-4490-IDPLKLST     PIC S9(3)  COMP-3.                       
031600                                                                          
031700     03  W-IDDC-B6-X.                                                     
031800         05 W-IDDC-B6                  PIC X(2).                          
031900*                                                                         
032000     03  W-IDARTNR-X.                                                     
032100         05  W-IDARTNR           PIC S9(9) VALUE ZERO COMP-3.             
032200*                                                                         
032300     03  W-KDSEGKEY-X.                                                    
032400         05  W-KDSEGKEY          PIC X(1)  VALUE '1'.                     
032500*                                                                         
032600 SKIP2                                                                    
032700 01  MEDDELANDE.                                                          
032800                                                                          
032900     03  FEL-1.                                                           
033000         05  FILLER              PIC X(40)   VALUE                        
033100             '726 KOLLIKOD SAKNAS                     '.                  
033200         05  FILLER              PIC X(40)   VALUE                        
033300             '726 CASE CODE MISSING                   '.                  
033400     03  FEL-726 REDEFINES FEL-1 OCCURS 2 PIC X(40).                      
033500                                                                          
033600     03  FEL-2.                                                           
033700         05  FILLER              PIC X(40)   VALUE                        
033800             '761 ANGE MÅTT                           '.                  
033900         05  FILLER              PIC X(40)   VALUE                        
034000             '761 ENTRE MEASURES                      '.                  
034100     03  FEL-761 REDEFINES FEL-2 OCCURS 2 PIC X(40).                      
034200                                                                          
034300     03  FEL-3.                                                           
034400         05  FILLER              PIC X(40)   VALUE                        
034500             '762 ANGIVET DISTR, KUND, ORDER FINNS EJ '.                  
034600         05  FILLER              PIC X(40)   VALUE                        
034700             '762 WRONG DISTR CUST ORDER              '.                  
034800     03  FEL-762 REDEFINES FEL-3 OCCURS 2 PIC X(40).                      
034900                                                                          
035000     03  FEL-4.                                                           
035100         05  FILLER              PIC X(40)   VALUE                        
035200             '756 KOLLI SAKNAS                        '.                  
035300         05  FILLER              PIC X(40)   VALUE                        
035400             '756 CASE MISSING                        '.                  
035500     03  FEL-756 REDEFINES FEL-4 OCCURS 2 PIC X(40).                      
035600                                                                          
035700     03  FEL-5.                                                           
035800         05  FILLER              PIC X(40)   VALUE                        
035900             '764 PLATS FINNS EJ                      '.                  
036000         05  FILLER              PIC X(40)   VALUE                        
036100             '764 LOCATION MISSING                    '.                  
036200     03  FEL-764 REDEFINES FEL-5 OCCURS 2 PIC X(40).                      
036300                                                                          
036400     03  FEL-6.                                                           
036500         05  FILLER              PIC X(40)   VALUE                        
036600             '763 MÅTTUPPGIFTER MÅSTE VARA NUMERISKA  '.                  
036700         05  FILLER              PIC X(40)   VALUE                        
036800             '763 MEASURES MUST BE NUMERIC            '.                  
036900     03  FEL-763 REDEFINES FEL-6 OCCURS 2 PIC X(40).                      
037000                                                                          
037100     03  FEL-7.                                                           
037200         05  FILLER              PIC X(40)   VALUE                        
037300             '749 FEL NYCKEL                          '.                  
037400         05  FILLER              PIC X(40)   VALUE                        
037500             '749 WRONG KEY                           '.                  
037600     03  FEL-749 REDEFINES FEL-7 OCCURS 2 PIC X(40).                      
037700                                                                          
037800     03  FEL-8.                                                           
037900         05  FILLER              PIC X(40)   VALUE                        
038000             '772 FELAKTIG PRINTER                    '.                  
038100         05  FILLER              PIC X(40)   VALUE                        
038200             '772 WRONG PRINTER                       '.                  
038300     03  FEL-772 REDEFINES FEL-8 OCCURS 2 PIC X(40).                      
038400                                                                          
038500     03  FEL-9.                                                           
038600         05  FILLER              PIC X(40)   VALUE                        
038700             '721 KOLLIT REDAN RAPPORTERAT            '.                  
038800         05  FILLER              PIC X(40)   VALUE                        
038900             '721 CASE ALREADY REPORTED               '.                  
039000     03  FEL-721 REDEFINES FEL-9 OCCURS 2 PIC X(40).                      
039100                                                                          
039200     03  FEL-10.                                                          
039300         05  FILLER              PIC X(40)   VALUE                        
039400             '765 ANGIVET PRODNR FINNS EJ.            '.                  
039500         05  FILLER              PIC X(40)   VALUE                        
039600             '765 PRODNR DOES NOT EXIST.              '.                  
039700     03  FEL-765 REDEFINES FEL-10 OCCURS 2 PIC X(40).                     
039800                                                                          
039900     03  MED-1.                                                           
040000         05  FILLER              PIC X(61)   VALUE                        
040100             'UPPDATERING UTFÖRD OCH KOLLIFLAGGA UTSKRIVEN    '.          
040200         05  FILLER              PIC X(61)   VALUE                        
040300             'UPDATING OK AND CASE LABEL PRINTED              '.          
040400     03  MED-803 REDEFINES MED-1 OCCURS 2 PIC X(61).                      
040500     EJECT                                                                
040600***********************************************************               
040700 01    FILLER              PIC X(16)  VALUE 'TEST-IDDISTR'.               
040800 01  TEST-IDDISTR          PIC S9(5)  COMP-3.                             
040900                                                                          
041000                                                                          
041100*01    FILLER    -COPY WWDIST03      -RED TEST-IDDISTR.                   
041200     EJECT                                                                
041300*01    FILLER    -COPY WWDIS128      -RED TEST-IDDISTR.                   
041400     EJECT                                                                
041500 01  FILLER                      PIC X(08)  VALUE 'FRAKT1  '.             
041600*   -COPY WWFRAKT1                                                        
041700     EJECT                                                                
041800 01    FILLER              PIC X(16)  VALUE 'EMB-TABELL'.                 
041900 01    EMB-TABELL.                                                        
042000       03  EMB-TAB-X.                                                     
042100           05  KLASS OCCURS 3 INDEXED BY KL-INDX.                         
042200               07  TYP  OCCURS 5                                          
042300                        INDEXED BY TYP-INDX  PIC S9(5) COMP-3.            
042400       03  EMB-TAB REDEFINES EMB-TAB-X.                                   
042500           05  FILLER.                                                    
042600               07  PALLAR   OCCURS 5 PIC S9(5) COMP-3.                    
042700           05  FILLER.                                                    
042800               07  KRAGAR   OCCURS 5 PIC S9(5) COMP-3.                    
042900           05  FILLER.                                                    
043000               07  EMB-LOCK OCCURS 5 PIC S9(5) COMP-3.                    
043100     EJECT                                                                
043200 01    FILLER              PIC X(16)   VALUE 'MFS-WS'.                    
043300                                                                          
043400                                                                          
043500                                                                          
043600 01    FILLER              PIC X(16)   VALUE 'MID W4I331 MID'.            
043700*01  MID -COPY W4I33101.                                                  
043800     EJECT                                                                
043900*01  -COPY WMSGAREA                                                       
044000     EJECT                                                                
044100*    03  MOD -COPY W4O33101  -RED MSG-AREA.                               
044200     EJECT                                                                
044300 01    FILLER              PIC X(16)   VALUE 'MID W4I337 MID'.            
044400*01  MOD -COPY W4O33701  -PRE 4337-.                                      
044500     EJECT                                                                
044600 01    FILLER              PIC X(16)   VALUE 'M0D W40636I1 M0D'.          
044700*01  -COPY W40636I1      -PRE MOD4636-                                    
044800     EJECT                                                                
044900 01  4341-MID-IO-AREA.                                                    
045000                                                                          
045100     03  4341-MID-LL            PIC S9(4)  COMP SYNC.                     
045200     03  4341-MID-Z1            PIC X.                                    
045300     03  4341-MID-Z2            PIC X.                                    
045400     03  4341-MID-TRANSKOD      PIC X(8)   VALUE 'W4T341U'.               
045500     03  4341-MID-IDTRANS       PIC X(4)   VALUE '433A'.                  
045600     03  4341-MID-KDMFSFOR      PIC X.                                    
045700     03  4341-MID-DATA-AREA     PIC X(61).                                
045800*    03  MID  -COPY W4I34101 -RED 4341-MID-DATA-AREA  -PRE 4341-.         
045900     EJECT                                                                
046000*01  -COPY WMFSAREA                                                       
046100     EJECT                                                                
046200 01  IMS-WS.                                                              
046300     03  FILLER                  PIC X(16)   VALUE 'IMS-WS     '.         
046400     SKIP3                                                                
046500*                        **** STATUS-KOD FRÅN IMS                         
046600     03  STATUS-KUNDORDER-SEK-WS PIC X(2).                                
046700         88  KUNDORDER-SEK-FINNS             VALUE '  '.                  
046800         88  KUNDORDER-SEK-SAKNAS            VALUE 'GE' 'GB'.             
046900     03  STATUS-WS               PIC X(2).                                
047000         88  SEGMENT-FINNS                   VALUE '  '.                  
047100         88  SEGMENT-SAKNAS                  VALUE 'GE'.                  
047200         88  END-OF-DATA                     VALUE 'GB'.                  
047300     SKIP3                                                                
047400     03  GODK-STATUSKODER.                                                
047500         05  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC X(2).          
047600     SKIP3                                                                
047700 01  SSA1                        PIC X(160).                              
047800 01  SSA2                        PIC X(160).                              
047900 01  SSA3                        PIC X(160).                              
048000     EJECT                                                                
048100*                            IMS FUNKTIONSKODER                           
048200*01  -COPY W0003                                                          
048300     EJECT                                                                
048400*                            DLI INPUT-OUTPUT AREA                        
048500 01  DLI-IO-AREA.                                                         
048600     03  IO-AREA                 PIC X(352)  VALUE SPACE.                 
048700                                                                          
048800                                                                          
048900*    03  WLEMBB01 -COPY WDK501     -RED IO-AREA.                          
049000     EJECT                                                                
049100*    03  WDE401   -COPY WDE401     -RED IO-AREA.                          
049200     EJECT                                                                
049300*    03  WLXXDV11 -COPY WDGX4726   -RED IO-AREA.                          
049400     EJECT                                                                
049500*    03  WLXXDV21 -COPY WDGX4727   -RED IO-AREA.                          
049600     EJECT                                                                
049700*    03  WLXXKH01 -COPY WDGX4447   -RED IO-AREA.                          
049800     SKIP2                                                                
049900*    03  WLXXKH11 -COPY WDGX4448   -RED IO-AREA.                          
050000     EJECT                                                                
050100*    03  4487-AREA -COPY WDGX4487   -RED IO-AREA.                         
050200     SKIP2                                                                
050300*    03  WDGX4490 -COPY WDGX4490   -RED IO-AREA.                          
050400     EJECT                                                                
050500*    03  -COPY WDGX01     -RED IO-AREA.                                   
050600     EJECT                                                                
050700 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA2'.        
050800 01  DLI-IO-AREA2.                                                        
050900     03  IO-AREA2                PIC X(259)  VALUE SPACE.                 
051000     SKIP2                                                                
051100     03  WDE601   -COPY WDE601   -PRE WDE62- -RED IO-AREA2.               
051200     EJECT                                                                
051300     03  WLORQA01 -COPY WDQ301   -PRE ORQA-  -RED IO-AREA2.               
051400     EJECT                                                                
051500 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-Q212'.         
051900 01  DLI-IO-WDQ212.                                                       
052000     03  -COPY WDQ212                                                     
052100     EJECT                                                                
052200 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA4'.        
052300 01  DLI-IO-AREA4.                                                        
052400     03  WLXXDU11 -COPY WDGX4302                                          
052500     EJECT                                                                
052600 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-WDE4E'.        
052700 01  DLI-IO-WDE4E.                                                        
052800     03  WDE401   -COPY WDE401 -PRE E4E-.                                 
052900     EJECT                                                                
053000 01  FILLER                      PIC X(16)  VALUE 'DLI-IO-WDE601'.        
053100 01  DLI-IO-WDE601.                                                       
053200     03  WDE601   -COPY WDE601.                                           
053300     EJECT                                                                
053400 01  FILLER                      PIC X(16)  VALUE 'DLI-IO-WDE611'.        
053500 01  DLI-IO-WDE611.                                                       
053600     03  WDE611   -COPY WDE611.                                           
053700                                                                          
053800 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
053900 01   DLI-IO-AREA-B601.                                                   
054000*     03  -COPY WDB601                                                    
054100     EJECT                                                                
054200 01  FILLER               PIC X(16)   VALUE 'DLI-IO-WDK601'.              
054300 01    DLI-IO-WDK601.                                                     
054400*      03  -COPY WDK601                                                   
054500     EJECT                                                                
054600 01    FILLER             PIC X(16)   VALUE 'DLI-IO-WDK611'.              
054700 01    DLI-IO-WDK611.                                                     
054800*      03  -COPY WDK611                                                   
054900     EJECT                                                                
055000 01    FILLER             PIC X(16)   VALUE 'DLI-IO-WDE411'.              
055100 01    DLI-IO-WDE411.                                                     
055200*      03  -COPY WDE411                                                   
055300     EJECT                                                                
055400 LINKAGE SECTION.                                                         
055500*01  -COPY W0009     -PRE MSG-                                            
055600     EJECT                                                                
055700*01  -COPY W0009     -PRE 4341-                                           
055800     EJECT                                                                
055900 01  TMS-CRE-PCB                 PIC X.                                   
056000 01  TMS-DEL-PCB                 PIC X.                                   
056100 01  ATAB-PCB                    PIC X.                                   
056200     EJECT                                                                
056300*01  -COPY W0008     -PRE USEA-                                           
056400     05  FILLER                  PIC X.                                   
056500     EJECT                                                                
056600*01  -COPY W0008     -PRE WDE4-                                           
056700     05  FILLER                  PIC X.                                   
056800     EJECT                                                                
056900*01  -COPY W0008     -PRE WDE4A-                                          
057000     05  FILLER                  PIC X.                                   
057100     EJECT                                                                
057200*01  -COPY W0008     -PRE WDE4E-                                          
057300     05  FILLER                  PIC X.                                   
057400     EJECT                                                                
057500*01  -COPY W0008     -PRE WDK6-                                           
057600     05  FILLER                  PIC X.                                   
057700     EJECT                                                                
057800*01  -COPY W0008     -PRE EMBB-                                           
057900     05  FILLER                  PIC X.                                   
058000     EJECT                                                                
058100*01  -COPY W0008     -PRE XXDV-                                           
058200     05  FILLER                  PIC X.                                   
058300     EJECT                                                                
058400*01  -COPY W0008     -PRE PLATS-DM-                                       
058500     05  FILLER                  PIC X.                                   
058600     EJECT                                                                
058700*01  -COPY W0008     -PRE PLATS-DN-                                       
058800     05  FILLER                  PIC X.                                   
058900     EJECT                                                                
059000*01  -COPY W0008     -PRE PLATS-DP-                                       
059100     05  FILLER                  PIC X.                                   
059200     EJECT                                                                
059300*01  -COPY W0008     -PRE PLATS-DO-                                       
059400     05  FILLER                  PIC X.                                   
059500     EJECT                                                                
059600*01  -COPY W0008     -PRE PLATS-WDE6C-                                    
059700     05  FILLER                  PIC X.                                   
059800     EJECT                                                                
059900*01  -COPY W0008     -PRE PLATS-GMTC-                                     
060000     05  FILLER                  PIC X.                                   
060100     EJECT                                                                
060200*01  -COPY W0008     -PRE PLATS-WDB6-                                     
060300     05  FILLER                  PIC X.                                   
060400     EJECT                                                                
060500*01  -COPY W0008     -PRE WDE62-                                          
060600     05  FILLER                  PIC X.                                   
060700     EJECT                                                                
060800*01  -COPY W0008     -PRE WDE6-                                           
060900     05  FILLER                  PIC X.                                   
061000     EJECT                                                                
061100*01  -COPY W0008     -PRE ORQA-                                           
061200     05  FILLER                  PIC X.                                   
061300     EJECT                                                                
061400*01  -COPY W0008     -PRE ORQI-                                           
061500     05  FILLER                  PIC X.                                   
061600     EJECT                                                                
061700*01  -COPY W0008     -PRE XXKH-                                           
061800     05  FILLER                  PIC X.                                   
061900     EJECT                                                                
062000*01  -COPY W0008     -PRE 4487-                                           
062100     05  FILLER                  PIC X.                                   
062200     EJECT                                                                
062300*01  -COPY W0008     -PRE XXDU-                                           
062400     05  FILLER                  PIC X.                                   
062500     EJECT                                                                
062600*01  -COPY W0008     -PRE XXDU2-                                          
062700     05  FILLER                  PIC X.                                   
062800     EJECT                                                                
062900*01  -COPY W0008     -PRE WDB6-                                           
063000     05  FILLER                  PIC X.                                   
063100 01  TMS-1165-PCB                PIC X.                                   
063200 01  TMS-4141-PCB                PIC X.                                   
063300 01  TMS-WDB2-PCB                PIC X.                                   
063400 01  TMS-WDB6-PCB                PIC X.                                   
063500 01  TMS-WDD3-PCB                PIC X.                                   
063600 01  TMS-WDB1-PCB                PIC X.                                   
063700 01  TMS-WDE4A-PCB               PIC X.                                   
063800 01  TMS-WDE4F-PCB               PIC X.                                   
063900 01  TMS-WDQ2-PCB                PIC X.                                   
064000 01  TMS-WDQ3-PCB                PIC X.                                   
064100 01  TMS-WDK6-PCB                PIC X.                                   
064200 01  TMS-WDE6-PCB                PIC X.                                   
064300 01  TMS-WDK5-PCB                PIC X.                                   
064400 01  TMS-WDQ2C-PCB               PIC X.                                   
064500     EJECT                                                                
064600 PROCEDURE DIVISION USING MSG-PCB  4341-PCB                               
064700                          TMS-CRE-PCB TMS-DEL-PCB ATAB-PCB                
064800                          USEA-PCB WDE4-PCB WDE4A-PCB WDE4E-PCB           
064900                          WDK6-PCB                                        
065000                          EMBB-PCB  XXDV-PCB                              
065100                          PLATS-DM-PCB  PLATS-DN-PCB                      
065200                          PLATS-DP-PCB  PLATS-DO-PCB                      
065300                          PLATS-WDE6C-PCB PLATS-GMTC-PCB                  
065400                          PLATS-WDB6-PCB                                  
065500                          WDE62-PCB  WDE6-PCB ORQA-PCB  ORQI-PCB          
065600                          XXKH-PCB  4487-PCB  XXDU-PCB                    
065700                          XXDU2-PCB WDB6-PCB                              
065800                          TMS-1165-PCB TMS-4141-PCB TMS-WDB2-PCB          
065900                          TMS-WDB6-PCB TMS-WDD3-PCB TMS-WDB1-PCB          
066000                          TMS-WDE4A-PCB TMS-WDE4F-PCB                     
066100                          TMS-WDQ2-PCB TMS-WDQ3-PCB TMS-WDK6-PCB          
066200                          TMS-WDE6-PCB TMS-WDK5-PCB TMS-WDQ2C-PCB.        
066300     ENTRY 'DLITCBL' USING MSG-PCB   4341-PCB                             
066400                          TMS-CRE-PCB TMS-DEL-PCB ATAB-PCB                
066500                           USEA-PCB WDE4-PCB WDE4A-PCB WDE4E-PCB          
066600                           WDK6-PCB                                       
066700                           EMBB-PCB  XXDV-PCB                             
066800                           PLATS-DM-PCB  PLATS-DN-PCB                     
066900                           PLATS-DP-PCB  PLATS-DO-PCB                     
067000                           PLATS-WDE6C-PCB PLATS-GMTC-PCB                 
067100                           PLATS-WDB6-PCB                                 
067200                           WDE62-PCB WDE6-PCB ORQA-PCB  ORQI-PCB          
067300                           XXKH-PCB  4487-PCB  XXDU-PCB                   
067400                           XXDU2-PCB WDB6-PCB                             
067500                           TMS-1165-PCB TMS-4141-PCB TMS-WDB2-PCB         
067600                           TMS-WDB6-PCB TMS-WDD3-PCB TMS-WDB1-PCB         
067700                          TMS-WDE4A-PCB TMS-WDE4F-PCB                     
067800                          TMS-WDQ2-PCB TMS-WDQ3-PCB TMS-WDK6-PCB          
067900                          TMS-WDE6-PCB TMS-WDK5-PCB TMS-WDQ2C-PCB.        
068000                                                                          
068100     PERFORM IMS-GET-MSG                                                  
068200                                                                          
068300     IF SEGMENT-FINNS                                                     
068400       MOVE NEJ       TO WS-FEL-FUNNET                                    
068500       PERFORM A-INIT-SPARA-INPUT                                         
068600                                                                          
068700       IF GODKAEND-BILDFAMILJ                                             
068800          IF FEL-EJ-FUNNET                                                
068900             IF GODKAEND-BILD                                             
069000                 PERFORM C-BEHANDLA                                       
069100                                                                          
069200                 IF FEL-EJ-FUNNET                                         
069300                    IF WS-W006-KDSVAR-FOLJEFL = RAETT                     
069400                       PERFORM F-SKAPA-FOLJESEDEL-TRANS                   
069500                    END-IF                                                
069600                    PERFORM D-REDIGERA-W4O33701-MOD                       
069700                 END-IF                                                   
069800              END-IF                                                      
069900           END-IF                                                         
070000       ELSE                                                               
070100         PERFORM E-RENSA-NYCKLAR                                          
070200       END-IF                                                             
070300                                                                          
070400       PERFORM   IMS-INSERT-MSG                                           
070500                                                                          
070600     END-IF                                                               
070700                                                                          
070800     MOVE ZERO TO RETURN-CODE                                             
070900                                                                          
071000     GOBACK                                                               
071100     .                                                                    
071200     EJECT                                                                
071300 A-INIT-SPARA-INPUT SECTION.                                              
071400                                                                          
071500                                                                          
071600     IF MSG-DUBBLA-TRANSKODER                                             
071700         MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W4I33101               
071800         MOVE MSG-IDTRANS-2 TO MFS-IDTRANS WS-IDTRANS                     
071900         MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR WS-MFS-KDMFSFOR              
072000         MOVE MSG-KDTRTYP          TO MFS-KDTRTYP                         
072100         MOVE MSG-IDPFK TO MFS-IDPFK                                      
072200     ELSE                                                                 
072300         MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W4I33101                
072400         MOVE MSG-IDTRANS-1 TO MFS-IDTRANS WS-IDTRANS                     
072500         MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR WS-MFS-KDMFSFOR              
072600         MOVE ' ' TO MFS-KDTRTYP        MFS-IDPFK                         
072700     END-IF                                                               
072800                                                                          
072900     MOVE ALL '+'           TO MSGI-WMSGINIT                              
073000     MOVE '001'             TO MSGI-KDCALL                                
073100     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
073200     MOVE '4331'            TO MSGI-IDTRANS                               
073300     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
073400     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
073500     MOVE MSGI-KDMATT       TO WS-KDMATT                                  
073600     MOVE NEJ               TO IDPRODNR-IFYLLT-SW                         
073700                                                                          
073800     ACCEPT WS-DAGENS-DATUM               FROM DATE                       
073900     ACCEPT WS-TIDPUNKT                   FROM TIME                       
074000     INITIALIZE TMS-W403TMS1                                              
074100     MOVE WS-TIDPUNKT(1:6) TO      WS-TISKPTID                            
074200                                                                          
074300     IF MSGI-IDLAND-SPR = 'GB'                                            
074400       MOVE +2 TO INDX                                                    
074500     ELSE                                                                 
074600       MOVE +1 TO INDX                                                    
074700     END-IF                                                               
074800                                                                          
074900     IF MID-IDDISTR-IN = ALL '+'                                          
075000         MOVE MID-IDDISTR-UT TO WS-IDDISTR                                
075100         INSPECT WS-IDDISTR REPLACING ALL  SPACE BY ZERO                  
075200     ELSE                                                                 
075300         MOVE MID-IDDISTR-IN TO WS-IDDISTR                                
075400     END-IF                                                               
075500                                                                          
075600     IF MID-IDKUNDNR-IN = ALL '+'                                         
075700         MOVE MID-IDKUNDNR-UT TO WS-IDKUNDNR                              
075800         INSPECT WS-IDKUNDNR REPLACING ALL  SPACE BY ZERO                 
075900     ELSE                                                                 
076000         MOVE MID-IDKUNDNR-IN TO WS-IDKUNDNR                              
076100     END-IF                                                               
076200                                                                          
076300     IF MID-IDORDNR-IN = ALL '+'                                          
076400         MOVE MID-IDORDNR-UT TO WS-IDORDNR                                
076500         INSPECT WS-IDORDNR REPLACING ALL  SPACE BY ZERO                  
076600     ELSE                                                                 
076700         MOVE MID-IDORDNR-IN TO WS-IDORDNR                                
076800     END-IF                                                               
076900                                                                          
077000     IF MID-IDKOLLI-IN = ALL '+'                                          
077100         MOVE MID-IDKOLLI-UT TO WS-IDKOLLI WS-IDKOLLI-NUM5                
077200         INSPECT WS-IDKOLLI REPLACING ALL  SPACE BY ZERO                  
077300     ELSE                                                                 
077400         MOVE MID-IDKOLLI-IN TO WS-IDKOLLI WS-IDKOLLI-NUM5                
077500     END-IF                                                               
077600                                                                          
077700     IF MID-IDPRODNR-IN = ALL '+'                                         
077800       MOVE MID-IDPRODNR-UT TO WS-IDPRODNR                                
077900       IF MID-IDPRODNR-UT > ZERO                                          
078000         MOVE JA            TO IDPRODNR-IFYLLT-SW                         
078100       END-IF                                                             
078200     ELSE                                                                 
078300       MOVE MID-IDPRODNR-IN TO WS-IDPRODNR                                
078400       MOVE JA              TO IDPRODNR-IFYLLT-SW                         
078500     END-IF                                                               
078600                                                                          
078700     MOVE MSGI-IDDC           TO WS-IDDC                                  
078800                                                                          
078900     MOVE LOW-VALUE           TO MSG-AREA                                 
079000     MOVE 'W4O331N1'          TO MFS-IDMOD                                
079100     MOVE '4331'              TO MOD-IDTRANS                              
079200     MOVE 4331-MOD-LAENGD     TO MSG-KVLL                                 
079300                                                                          
079400     MOVE WS-IDDISTR TO MOD-IDDISTR-UT                                    
079500                        4337-MOD-IDDISTR-UT                               
079600     INSPECT MOD-IDDISTR-UT REPLACING LEADING ZERO BY SPACE               
079700     INSPECT 4337-MOD-IDDISTR-UT REPLACING LEADING ZERO BY SPACE          
079800                                                                          
079900     MOVE WS-IDKUNDNR TO MOD-IDKUNDNR-UT                                  
080000                         4337-MOD-IDKUNDNR-UT                             
080100     INSPECT MOD-IDKUNDNR-UT REPLACING LEADING ZERO BY SPACE              
080200     INSPECT 4337-MOD-IDKUNDNR-UT REPLACING LEADING ZERO BY SPACE         
080300                                                                          
080400     MOVE WS-IDORDNR TO MOD-IDORDNR-UT                                    
080500                        4337-MOD-IDORDNR-UT                               
080600     INSPECT MOD-IDORDNR-UT REPLACING LEADING ZERO BY SPACE               
080700     INSPECT 4337-MOD-IDORDNR-UT REPLACING LEADING ZERO BY SPACE          
080800                                                                          
080900     MOVE WS-IDKOLLI TO MOD-IDKOLLI-UT                                    
081000                        4337-MOD-IDKOLLI-UT                               
081100     INSPECT MOD-IDKOLLI-UT REPLACING LEADING ZERO BY SPACE               
081200     INSPECT 4337-MOD-IDKOLLI-UT REPLACING LEADING ZERO BY SPACE          
081300                                                                          
081400     MOVE WS-IDPRODNR TO MOD-IDPRODNR-UT                                  
081500     INSPECT MOD-IDPRODNR-UT REPLACING LEADING ZERO BY SPACE              
081600                                                                          
081700     MOVE WS-IDDC    TO MOD-IDDC-UT                                       
081800                        4337-MOD-IDDC-UT                                  
081900     INSPECT 4337-MOD-IDDC-UT REPLACING LEADING ZERO BY SPACE             
082000     MOVE MFS-RENSA-FAELT TO MOD-IDDISTR-IN                               
082100                             MOD-IDKUNDNR-IN                              
082200                             MOD-IDORDNR-IN                               
082300                             MOD-IDKOLLI-IN                               
082400                             MOD-IDDC-IN                                  
082500                             MOD-IDPRODNR-IN                              
082600                             MOD-KDPRTVAL-AF-IN                           
082700                             MOD-KDPRTVAL-FS-IN                           
082800                             MOD-DIKOLLIL                                 
082900                             MOD-DIKOLLIB                                 
083000                             MOD-DIKOLLIH                                 
083100                             MOD-ADRESS-TEXT                              
083200                             MOD-ADFLGEO                                  
083300                             MOD-ADFLOMR                                  
083400                             MOD-ADRUTNIV                                 
083500                             MOD-ADVMODUL                                 
083600                             MOD-SLUT-TEXT                                
083700                             MOD-TEMFSFEL                                 
083800                             MOD-TEMFSINF                                 
083900                             4337-MOD-IDDISTR-IN                          
084000                             4337-MOD-IDKUNDNR-IN                         
084100                             4337-MOD-IDORDNR-IN                          
084200                             4337-MOD-IDKOLLI-IN                          
084300                             4337-MOD-IDDC-IN                             
084400                             4337-MOD-KDPRTVAL-AF-IN                      
084500                             4337-MOD-VKORDBTO                            
084600                             4337-MOD-IDPRODNR                            
084700                             4337-MOD-ADRESS-TEXT                         
084800                             4337-MOD-ADFLGEO                             
084900                             4337-MOD-ADFLOMR                             
085000                             4337-MOD-ADRUTNIV                            
085100                             4337-MOD-ADVMODUL                            
085200                             4337-MOD-TEMFSFEL                            
085300                             4337-MOD-TEMFSINF                            
085400     .                                                                    
085500     EJECT                                                                
085600 C-BEHANDLA SECTION.                                                      
085700                                                                          
085800     IF WS-IDDISTR  NUMERIC AND                                           
085900        WS-IDKUNDNR NUMERIC AND                                           
086000        WS-IDORDNR  NUMERIC AND                                           
086100        WS-IDKOLLI  NUMERIC AND                                           
086200        WS-IDDC > SPACE                                                   
086300                                                                          
086400        PERFORM CN-KOLLA-PRINTER                                          
086500        IF FEL-EJ-FUNNET                                                  
086600           MOVE MID-KDKOLLI         TO W-K501-KDKOLLI                     
086700           PERFORM IMS-GU-K501-KVAL                                       
086800                                                                          
086900           IF SEGMENT-SAKNAS                                              
087000              IF FEL-EJ-FUNNET                                            
087100                 MOVE FEL-726(INDX)  TO MOD-TEMFSFEL                      
087200                 MOVE JA             TO WS-FEL-FUNNET                     
087300                 PERFORM S01-ROER-EJ-MODFAELT                             
087400                 PERFORM S02-SAETT-LAES-IGEN-ATTRIBUT                     
087500              END-IF                                                      
087600              MOVE MFS-ALFA-FAELT-FEL TO MOD-KDKOLLI-ATTR                 
087700           ELSE                                                           
087800              PERFORM CA-HAEMTA-DATA-I-WDK501                             
087900              PERFORM CB-FORMELL-KONTROLL                                 
088000                                                                          
088100              IF FEL-EJ-FUNNET                                            
088200                 PERFORM CK-HAEMTA-STARTNYCKEL                            
088300                                                                          
088400                 IF FEL-EJ-FUNNET                                         
088500                    PERFORM CC-HAEMTA-DATA-I-WDE601                       
088600                    MOVE WS-IDPRODNR      TO W-E601-IDPRODNR              
088700                    MOVE WS-IDKOLLI       TO W-E611-IDKOLLI               
088800                    PERFORM IMS-GU-E611                                   
088900                                                                          
089000                    IF SEGMENT-SAKNAS                                     
089100                       MOVE FEL-756(INDX)   TO MOD-TEMFSFEL               
089200                       MOVE JA              TO WS-FEL-FUNNET              
089300                       PERFORM S01-ROER-EJ-MODFAELT                       
089400                       PERFORM S02-SAETT-LAES-IGEN-ATTRIBUT               
089500                    ELSE                                                  
089600                                                                          
089700                       IF KOLLI-KDKOLSTA = ZERO AND                       
089800                                           KOLLI-FLBANDST = JA            
089900                          PERFORM CD-INPUT-TILL-PLATSSOEKNING             
090000                          MOVE KOLLI-SUORDV-KOLLI                         
090100                                               TO WS-SUORDV-KOLLI         
090200                          MOVE KOLLI-SUORDV-LOC                           
090300                                       TO WS-SUORDV-KOLLI-LOC             
090400                          MOVE KOLLI-SUORDV-LOCPREL                       
090500                                       TO WS-SUORDV-KOLLI-LOCPREL         
090600                          CALL W403PLAT USING PLATS-W403PLAT              
090700                                              PLATS-DM-PCB                
090800                                              PLATS-DN-PCB                
090900                                              PLATS-DP-PCB                
091000                                              PLATS-DO-PCB                
091100                                              PLATS-WDE6C-PCB             
091200                                              PLATS-GMTC-PCB              
091300                                              PLATS-WDB6-PCB              
091400                                                                          
091500                          IF PLATS-KDSVAR = FEL                           
091600                             MOVE FEL-764(INDX) TO MOD-TEMFSFEL           
091700                             MOVE JA            TO WS-FEL-FUNNET          
091800                             PERFORM S01-ROER-EJ-MODFAELT                 
091900                             PERFORM S02-SAETT-LAES-IGEN-ATTRIBUT         
092000                          ELSE                                            
092100                             MOVE WS-IDPRODNR   TO W-E601-IDPRODNR        
092200                             PERFORM IMS-GHU-FIRST-E601-KVAL              
092300                             PERFORM CE-REDIGERA-WDE601                   
092400                             PERFORM IMS-REPLACE-WDE601                   
092500                             MOVE WS-IDPRODNR   TO W-E601-IDPRODNR        
092600                             MOVE WS-IDKOLLI    TO W-E611-IDKOLLI         
092700                             PERFORM IMS-GHU-E601                         
092800                             PERFORM CF-REDIGERA-WDE611                   
092900                             PERFORM IMS-REPLACE-WDE611                   
093000                             PERFORM CG-SEND-TMSINFO                      
093100                             PERFORM CL-UPPDAT-KDORDSTA                   
093200                             PERFORM CM-EV-BORTTAG-HTYP-4302-4490         
093300                                                                          
093400                             IF WS-FLAUTFAK = JA                          
093500                                IF WS-SAVE-VORD-KDORDSTA = 3              
093600                                   PERFORM CH-UPPDAT-4726-4727            
093700                                END-IF                                    
093800                             END-IF                                       
093900                             MOVE MED-803(INDX) TO MOD-TEMFSINF           
094000                          END-IF                                          
094100                       ELSE                                               
094200                          MOVE FEL-721(INDX)  TO MOD-TEMFSFEL             
094300                          MOVE JA             TO WS-FEL-FUNNET            
094400                          PERFORM S01-ROER-EJ-MODFAELT                    
094500                          PERFORM S02-SAETT-LAES-IGEN-ATTRIBUT            
094600                       END-IF                                             
094700                    END-IF                                                
094800                 END-IF                                                   
094900              END-IF                                                      
095000           END-IF                                                         
095100        END-IF                                                            
095200     ELSE                                                                 
095300        MOVE FEL-749(INDX)       TO MOD-TEMFSFEL                          
095400        MOVE JA                  TO WS-FEL-FUNNET                         
095500        PERFORM S01-ROER-EJ-MODFAELT                                      
095600        PERFORM S02-SAETT-LAES-IGEN-ATTRIBUT                              
095700     END-IF                                                               
095800     .                                                                    
095900     EJECT                                                                
096000 CA-HAEMTA-DATA-I-WDK501 SECTION.                                         
096100                                                                          
096200                                                                          
096300     MOVE EMB-EMBPROF    TO WS-EMBPROF                                    
096400     MOVE EMB-KDKOLLI    TO WS-KDKOLLI                                    
096500     MOVE EMB-KDKOLLID   TO WS-KDKOLLID                                   
096600     MOVE EMB-KVLOCK     TO WS-KVLOCK                                     
096700     MOVE EMB-VKTARA     TO WS-VKTARA                                     
096800     .                                                                    
096900     EJECT                                                                
097000 CB-FORMELL-KONTROLL SECTION.                                             
097100                                                                          
097200                                                                          
097300     IF MID-KDEMBTYP = ALL '+'                                            
097400         IF EMB-KDEMBTYP = ZERO                                           
097500             IF FEL-EJ-FUNNET                                             
097600                 MOVE FEL-761(INDX)    TO MOD-TEMFSFEL                    
097700                 MOVE JA               TO WS-FEL-FUNNET                   
097800                 PERFORM S01-ROER-EJ-MODFAELT                             
097900                 PERFORM S02-SAETT-LAES-IGEN-ATTRIBUT                     
098000             END-IF                                                       
098100             MOVE MFS-NUM-FAELT-FEL    TO MOD-KDEMBTYP-ATTR               
098200         ELSE                                                             
098300             MOVE EMB-KDEMBTYP         TO MOD-KDEMBTYP                    
098400                                          WS-KDEMBTYP                     
098500         END-IF                                                           
098600     ELSE                                                                 
098700         INSPECT MID-KDEMBTYP REPLACING LEADING SPACE BY ZERO             
098800         IF MID-KDEMBTYP NUMERIC                                          
098900             IF MID-KDEMBTYP = ZERO                                       
099000                 IF EMB-KDEMBTYP = ZERO                                   
099100                     IF FEL-EJ-FUNNET                                     
099200                         MOVE FEL-761(INDX) TO MOD-TEMFSFEL               
099300                         MOVE JA            TO WS-FEL-FUNNET              
099400                         PERFORM S01-ROER-EJ-MODFAELT                     
099500                         PERFORM S02-SAETT-LAES-IGEN-ATTRIBUT             
099600                     END-IF                                               
099700                     MOVE MFS-NUM-FAELT-FEL TO MOD-KDEMBTYP-ATTR          
099800                 ELSE                                                     
099900                     MOVE EMB-KDEMBTYP      TO MOD-KDEMBTYP               
100000                                               WS-KDEMBTYP                
100100                 END-IF                                                   
100200             ELSE                                                         
100300                 MOVE MID-KDEMBTYP          TO WS-KDEMBTYP                
100400             END-IF                                                       
100500         ELSE                                                             
100600             IF FEL-EJ-FUNNET                                             
100700                 MOVE FEL-763(INDX)         TO MOD-TEMFSFEL               
100800                 MOVE JA                    TO WS-FEL-FUNNET              
100900                 PERFORM S01-ROER-EJ-MODFAELT                             
101000                 PERFORM S02-SAETT-LAES-IGEN-ATTRIBUT                     
101100             END-IF                                                       
101200             MOVE MFS-NUM-FAELT-FEL         TO MOD-KDEMBTYP-ATTR          
101300         END-IF                                                           
101400     END-IF                                                               
101500                                                                          
101600     IF MID-DIKOLLIL = ALL '+'                                            
101700         IF EMB-DIKOLLIL = ZERO                                           
101800             IF FEL-EJ-FUNNET                                             
101900                 MOVE FEL-761(INDX)    TO MOD-TEMFSFEL                    
102000                 MOVE JA               TO WS-FEL-FUNNET                   
102100                 PERFORM S01-ROER-EJ-MODFAELT                             
102200                 PERFORM S02-SAETT-LAES-IGEN-ATTRIBUT                     
102300             END-IF                                                       
102400             MOVE MFS-NUM-FAELT-FEL    TO MOD-DIKOLLIL-ATTR               
102500         ELSE                                                             
102600           MOVE EMB-DIKOLLIL         TO WS-MOD-DIKOLLIL                   
102700                                        WS-DIKOLLIL                       
102800           IF US-MEASUREMENT                                              
102900             COMPUTE WS-MOD-DIKOLLIL =                                    
103000                     WS-MOD-DIKOLLIL * CONV-CM-TO-IN                      
103100             END-COMPUTE                                                  
103200             MOVE WS-MOD-DIKOLLIL    TO MOD-DIKOLLIL                      
103300           ELSE                                                           
103400             MOVE WS-DIKOLLIL        TO MOD-DIKOLLIL                      
103500           END-IF                                                         
103600         END-IF                                                           
103700     ELSE                                                                 
103800         INSPECT MID-DIKOLLIL REPLACING LEADING SPACE BY ZERO             
103900         IF MID-DIKOLLIL NUMERIC                                          
104000           IF MID-DIKOLLIL = ZERO                                         
104100             IF EMB-DIKOLLIL = ZERO                                       
104200               IF FEL-EJ-FUNNET                                           
104300                   MOVE FEL-761(INDX) TO MOD-TEMFSFEL                     
104400                   MOVE JA                  TO WS-FEL-FUNNET              
104500                   PERFORM S01-ROER-EJ-MODFAELT                           
104600                   PERFORM S02-SAETT-LAES-IGEN-ATTRIBUT                   
104700               END-IF                                                     
104800               MOVE MFS-NUM-FAELT-FEL TO MOD-DIKOLLIL-ATTR                
104900             ELSE                                                         
105000               MOVE EMB-DIKOLLIL TO WS-DIKOLLIL                           
105100                                    WS-MOD-DIKOLLIL                       
105200               IF US-MEASUREMENT                                          
105300                 COMPUTE WS-MOD-DIKOLLIL =                                
105400                         WS-MOD-DIKOLLIL * CONV-CM-TO-IN                  
105500                 END-COMPUTE                                              
105600                 MOVE WS-MOD-DIKOLLIL  TO MOD-DIKOLLIL                    
105700               ELSE                                                       
105800                 MOVE WS-DIKOLLIL      TO MOD-DIKOLLIL                    
105900               END-IF                                                     
106000             END-IF                                                       
106100           ELSE                                                           
106200             MOVE MID-DIKOLLIL              TO WS-DIKOLLIL                
106300           END-IF                                                         
106400         ELSE                                                             
106500             IF FEL-EJ-FUNNET                                             
106600                 MOVE FEL-763(INDX)         TO MOD-TEMFSFEL               
106700                 MOVE JA                    TO WS-FEL-FUNNET              
106800                 PERFORM S01-ROER-EJ-MODFAELT                             
106900                 PERFORM S02-SAETT-LAES-IGEN-ATTRIBUT                     
107000             END-IF                                                       
107100             MOVE MFS-NUM-FAELT-FEL         TO MOD-DIKOLLIL-ATTR          
107200         END-IF                                                           
107300     END-IF                                                               
107400                                                                          
107500     IF MID-DIKOLLIB = ALL '+'                                            
107600         IF EMB-DIKOLLIB = ZERO                                           
107700             IF FEL-EJ-FUNNET                                             
107800                 MOVE FEL-761(INDX)    TO MOD-TEMFSFEL                    
107900                 MOVE JA               TO WS-FEL-FUNNET                   
108000                 PERFORM S01-ROER-EJ-MODFAELT                             
108100                 PERFORM S02-SAETT-LAES-IGEN-ATTRIBUT                     
108200             END-IF                                                       
108300             MOVE MFS-NUM-FAELT-FEL    TO MOD-DIKOLLIB-ATTR               
108400         ELSE                                                             
108500             MOVE EMB-DIKOLLIB         TO WS-DIKOLLIB                     
108600                                          WS-MOD-DIKOLLIB                 
108700             IF US-MEASUREMENT                                            
108800               COMPUTE WS-MOD-DIKOLLIB =                                  
108900                       WS-MOD-DIKOLLIB * CONV-CM-TO-IN                    
109000               END-COMPUTE                                                
109100               MOVE WS-MOD-DIKOLLIB      TO MOD-DIKOLLIB                  
109200             ELSE                                                         
109300               MOVE WS-DIKOLLIB          TO MOD-DIKOLLIB                  
109400             END-IF                                                       
109500         END-IF                                                           
109600     ELSE                                                                 
109700         INSPECT MID-DIKOLLIB REPLACING LEADING SPACE BY ZERO             
109800         IF MID-DIKOLLIB NUMERIC                                          
109900           IF MID-DIKOLLIB = ZERO                                         
110000             IF EMB-DIKOLLIB = ZERO                                       
110100               IF FEL-EJ-FUNNET                                           
110200                   MOVE FEL-761(INDX) TO MOD-TEMFSFEL                     
110300                   MOVE JA                  TO WS-FEL-FUNNET              
110400                   PERFORM S01-ROER-EJ-MODFAELT                           
110500                   PERFORM S02-SAETT-LAES-IGEN-ATTRIBUT                   
110600               END-IF                                                     
110700               MOVE MFS-NUM-FAELT-FEL TO MOD-DIKOLLIB-ATTR                
110800             ELSE                                                         
110900               MOVE EMB-DIKOLLIB            TO WS-DIKOLLIB                
111000                                               WS-MOD-DIKOLLIB            
111100               IF US-MEASUREMENT                                          
111200                 COMPUTE WS-MOD-DIKOLLIB =                                
111300                         WS-MOD-DIKOLLIB * CONV-CM-TO-IN                  
111400                 END-COMPUTE                                              
111500                 MOVE WS-MOD-DIKOLLIB            TO MOD-DIKOLLIB          
111600               ELSE                                                       
111700                 MOVE WS-DIKOLLIB                TO MOD-DIKOLLIB          
111800               END-IF                                                     
111900             END-IF                                                       
112000           ELSE                                                           
112100             MOVE MID-DIKOLLIB              TO WS-DIKOLLIB                
112200           END-IF                                                         
112300         ELSE                                                             
112400             IF FEL-EJ-FUNNET                                             
112500                 MOVE FEL-763(INDX)         TO MOD-TEMFSFEL               
112600                 MOVE JA                    TO WS-FEL-FUNNET              
112700                 PERFORM S01-ROER-EJ-MODFAELT                             
112800                 PERFORM S02-SAETT-LAES-IGEN-ATTRIBUT                     
112900             END-IF                                                       
113000             MOVE MFS-NUM-FAELT-FEL         TO MOD-DIKOLLIB-ATTR          
113100         END-IF                                                           
113200     END-IF                                                               
113300                                                                          
113400     IF MID-DIKOLLIH = ALL '+'                                            
113500         IF EMB-DIKOLLIH = ZERO                                           
113600             IF FEL-EJ-FUNNET                                             
113700                 MOVE FEL-761(INDX)    TO MOD-TEMFSFEL                    
113800                 MOVE JA               TO WS-FEL-FUNNET                   
113900                 PERFORM S01-ROER-EJ-MODFAELT                             
114000                 PERFORM S02-SAETT-LAES-IGEN-ATTRIBUT                     
114100             END-IF                                                       
114200             MOVE MFS-NUM-FAELT-FEL    TO MOD-DIKOLLIH-ATTR               
114300         ELSE                                                             
114400             MOVE EMB-DIKOLLIH         TO WS-DIKOLLIH                     
114500                                          WS-MOD-DIKOLLIH                 
114600             IF US-MEASUREMENT                                            
114700               COMPUTE WS-MOD-DIKOLLIH =                                  
114800                       WS-MOD-DIKOLLIH * CONV-CM-TO-IN                    
114900               END-COMPUTE                                                
115000               MOVE WS-MOD-DIKOLLIH    TO MOD-DIKOLLIH                    
115100             ELSE                                                         
115200               MOVE WS-DIKOLLIH        TO MOD-DIKOLLIH                    
115300             END-IF                                                       
115400         END-IF                                                           
115500     ELSE                                                                 
115600         INSPECT MID-DIKOLLIH REPLACING LEADING SPACE BY ZERO             
115700         IF MID-DIKOLLIH NUMERIC                                          
115800             IF MID-DIKOLLIH = ZERO                                       
115900               IF EMB-DIKOLLIH = ZERO                                     
116000                 IF FEL-EJ-FUNNET                                         
116100                     MOVE FEL-761(INDX) TO MOD-TEMFSFEL                   
116200                     MOVE JA                TO WS-FEL-FUNNET              
116300                     PERFORM S01-ROER-EJ-MODFAELT                         
116400                     PERFORM S02-SAETT-LAES-IGEN-ATTRIBUT                 
116500                 END-IF                                                   
116600                 MOVE MFS-NUM-FAELT-FEL TO MOD-DIKOLLIH-ATTR              
116700               ELSE                                                       
116800                 MOVE EMB-DIKOLLIH     TO WS-DIKOLLIH                     
116900                                          WS-MOD-DIKOLLIH                 
117000                 IF US-MEASUREMENT                                        
117100                 COMPUTE WS-MOD-DIKOLLIH =                                
117200                         WS-MOD-DIKOLLIH * CONV-CM-TO-IN                  
117300                   END-COMPUTE                                            
117400                   MOVE WS-MOD-DIKOLLIH TO MOD-DIKOLLIH                   
117500                 ELSE                                                     
117600                   MOVE WS-DIKOLLIH    TO MOD-DIKOLLIH                    
117700                 END-IF                                                   
117800               END-IF                                                     
117900             ELSE                                                         
118000               MOVE MID-DIKOLLIH            TO WS-DIKOLLIH                
118100             END-IF                                                       
118200         ELSE                                                             
118300             IF FEL-EJ-FUNNET                                             
118400                 MOVE FEL-763(INDX)         TO MOD-TEMFSFEL               
118500                 MOVE JA                    TO WS-FEL-FUNNET              
118600                 PERFORM S01-ROER-EJ-MODFAELT                             
118700                 PERFORM S02-SAETT-LAES-IGEN-ATTRIBUT                     
118800             END-IF                                                       
118900             MOVE MFS-NUM-FAELT-FEL         TO MOD-DIKOLLIH-ATTR          
119000         END-IF                                                           
119100     END-IF                                                               
119200     .                                                                    
119300     EJECT                                                                
119400 CC-HAEMTA-DATA-I-WDE601 SECTION.                                         
119500                                                                          
119600                                                                          
119700     MOVE VORD-IDDC       TO WS-IDDC                                      
119800     MOVE VORD-IDPRODNR   TO WS-IDPRODNR                                  
119900     MOVE VORD-KDFRAKT    TO WS-KDFRAKT                                   
120000     MOVE VORD-KDORDKL    TO WS-KDORDKL                                   
120100     MOVE VORD-FLAUTFAK   TO WS-FLAUTFAK                                  
120200     MOVE VORD-KDFAKTYP   TO WS-KDFAKTYP                                  
120300                                                                          
120400     MOVE VORD-IDDISTR    TO TEST-IDDISTR                                 
120500     IF DIST03-SVERIGE                                                    
120600         MOVE 'SVE'       TO WS-DISTRIKT-TYP                              
120700     ELSE                                                                 
120800         MOVE 'EXP'       TO WS-DISTRIKT-TYP                              
120900     END-IF                                                               
121000     .                                                                    
121100     EJECT                                                                
121200 CD-INPUT-TILL-PLATSSOEKNING SECTION.                                     
121300                                                                          
121400                                                                          
121500     MOVE SPACE       TO PLATS-ADFLGEO                                    
121600                         PLATS-FLUTLAST                                   
121700                         PLATS-IDDC-CROSS                                 
121800     MOVE ZERO        TO PLATS-IDTRPTNR                                   
121900                         PLATS-ADFLOMR                                    
122000                         PLATS-ADRUTNIV                                   
122100                         PLATS-ADVMODUL                                   
122200                         PLATS-ADHMODUL                                   
122300                         PLATS-DIHMODUL                                   
122400                         PLATS-DIDMODUL                                   
122500     MOVE WS-IDDC     TO PLATS-IDDC                                       
122600     MOVE WS-IDDISTR  TO PLATS-IDDISTR                                    
122700     MOVE WS-IDKUNDNR TO PLATS-IDKUNDNR                                   
122800     MOVE WS-KDFRAKT  TO PLATS-KDFRAKT                                    
122900     MOVE WS-KDORDKL  TO PLATS-KDORDKLX                                   
123000     MOVE WS-IDORDNR  TO PLATS-IDORDNR                                    
123100     MOVE WS-DIKOLLIL TO PLATS-DIKOLLIL                                   
123200     MOVE WS-DIKOLLIB TO PLATS-DIKOLLIB                                   
123300     MOVE WS-DIKOLLIH TO PLATS-DIKOLLIH                                   
123400     MOVE WS-KDKOLLID TO PLATS-KDKOLLID                                   
123500     MOVE KOLLI-VKORDNTO-KOLLI  TO PLATS-VKORDNTO-KOLLI                   
123600     IF KOLLI-KDFARLIG-KOLLI = +4                                         
123700     OR KOLLI-KDFARLIG-KOLLI = +7                                         
123800       MOVE +6        TO PLATS-KDCALL                                     
123900     ELSE                                                                 
124000       MOVE ZERO      TO PLATS-KDCALL                                     
124100     END-IF                                                               
124200     .                                                                    
124300     EJECT                                                                
124400 CE-REDIGERA-WDE601        SECTION.                                       
124500                                                                          
124600     ADD 1                TO VORD-KVKOLLI                                 
124700                                                                          
124800     PERFORM CEB-EV-FRAKTSEDEL-SVERIGE                                    
124900     IF VORD-KVORDRAD-PACK = VORD-KVORDRAD                                
125000         IF VORD-KVKOLLI   = VORD-KVKOLPAC                                
125100         AND VORD-KDORDSTA NOT > 2                                        
125200           PERFORM CEA-KTRL-VORD-FARDIGPACKAD                             
125300           IF  WS-VORD-FARDIGPACKAD = JA                                  
125400             MOVE 3       TO VORD-KDORDSTA                                
125500             MOVE 3       TO WS-SAVE-VORD-KDORDSTA                        
125600           END-IF                                                         
125700         END-IF                                                           
125800     END-IF                                                               
125900     ACCEPT VORD-TIPACKN-SK FROM DATE                                     
126000                                                                          
126100     COMPUTE WS-VLORDBTO =                                                
126200     WS-DIKOLLIL * WS-DIKOLLIH * WS-DIKOLLIB / 1000000                    
126300     END-COMPUTE                                                          
126400                                                                          
126500     COMPUTE VORD-VLORDBTO =                                              
126600     VORD-VLORDBTO + WS-VLORDBTO                                          
126700     END-COMPUTE                                                          
126800                                                                          
126900     ADD WS-SUORDV-KOLLI TO VORD-SUORDV-PACK                              
127000     ADD WS-SUORDV-KOLLI-LOC     TO VORD-SUORDV-PACK-LOC                  
127100     ADD WS-SUORDV-KOLLI-LOCPREL TO VORD-SUORDV-PACK-LOCPREL              
127200     .                                                                    
127300 CEA-KTRL-VORD-FARDIGPACKAD SECTION.                                      
127400                                                                          
127500     MOVE JA                 TO WS-VORD-FARDIGPACKAD                      
127600                                                                          
127700     MOVE VORD-IDPRODNR      TO W-E601-IDPRODNR                           
127800     PERFORM IMS-GU-WDE62-VORD                                            
127900                                                                          
128000     MOVE WDE62-VORD-IDDC    TO W-Q301-MIN-IDDC                           
128100                                W-Q301-MAX-IDDC                           
128200     MOVE WDE62-VORD-IDPRODNR TO W-Q301-MIN-IDPRODNR                      
128300                                W-Q301-MAX-IDPRODNR                       
128400                                W-WDE4E-IDPRODNR                          
128500                                                                          
128600     PERFORM IMS-GN-WDE401-ESEQ                                           
128700                                                                          
128800     IF  SEGMENT-FINNS                                                    
128900*      * IDORDER SPARAS FRÅN 1:A KORD FÖR NYCKEL TILL WDQ3                
129000       MOVE E4E-KORD-IDORDER   TO W-Q301-MIN-IDORDER                      
129100                                  W-Q301-MAX-IDORDER                      
129200                                  W-Q201-IDORDER                          
129300                                                                          
129400       PERFORM UNTIL ((NOT SEGMENT-FINNS)                                 
129500               OR     WS-VORD-FARDIGPACKAD = NEJ)                         
129600                                                                          
129700         IF E4E-KORD-KVORDRAD-PACK <                                      
129800            (E4E-KORD-KVORDRAD + E4E-KORD-KVORDRAD-LEVPL)                 
129900                                                                          
130000           MOVE NEJ            TO WS-VORD-FARDIGPACKAD                    
130100         ELSE                                                             
130200           PERFORM IMS-GN-WDE401-ESEQ                                     
130300         END-IF                                                           
130400       END-PERFORM                                                        
130500                                                                          
130600       IF  WS-VORD-FARDIGPACKAD = JA                                      
130700                                                                          
130800         PERFORM IMS-GU-ORQA-ODEL                                         
130900                                                                          
131000         PERFORM UNTIL ((NOT SEGMENT-FINNS)                               
131100                 OR   WS-VORD-FARDIGPACKAD = NEJ)                         
131200                                                                          
131300           IF  ORQA-ODEL-KDODELSTA = 'R'                                  
131400             MOVE NEJ        TO WS-VORD-FARDIGPACKAD                      
131500           ELSE                                                           
131600             PERFORM IMS-GN-ORQA-ODEL                                     
131700           END-IF                                                         
131800         END-PERFORM                                                      
131900       END-IF                                                             
132000     ELSE                                                                 
132100       MOVE NEJ            TO WS-VORD-FARDIGPACKAD                        
132200     END-IF                                                               
132300     .                                                                    
132400     EJECT                                                                
132500 CEB-EV-FRAKTSEDEL-SVERIGE  SECTION.                                      
132600                                                                          
132700     IF WS-IDDC NOT = W-IDDC-B6                                           
132800        MOVE WS-IDDC TO W-IDDC-B6                                         
132900        PERFORM IMS-GU-WDB601                                             
133000     END-IF                                                               
133100     MOVE VORD-IDDISTR TO TEST-IDDISTR                                    
133200                                                                          
133300     IF  VORD-KDORDSTA < +3                                               
133400     AND VORD-KVKOLLI > +0                                                
133500     AND DIST03-SVERIGE                                                   
133600     AND DCS-CDC                                                          
133700                                                                          
133800       MOVE VORD-KDFRAKT TO WS-KDFRAKT-NUM2                               
133900       MOVE WS-KDFRAKT-NUM2  TO FRAK01-KDFRAKT                            
134000       IF  FRAK01-SVERIGE2                                                
134100       OR  FRAK01-NORDEN                                                  
134200       OR  FRAK01-KDFRAKT21                                               
134300       OR  FRAK01-KDFRAKT62                                               
134400       OR (WS-IDKOLLI-NUM5 > 349 AND WS-IDKOLLI-NUM5 < 400)               
134500         IF   VORD-FLDIRLEV = NEJ                                         
134600         AND  VORD-KDFRAKT NOT = +17                                      
134700           IF NOT DIS128-FRAKTS                                           
134800             MOVE JA TO VORD-FLFRAKTS                                     
134900           END-IF                                                         
135000         END-IF                                                           
135100       END-IF                                                             
135200     END-IF                                                               
135300     .                                                                    
135400     SKIP2                                                                
135500 CF-REDIGERA-WDE611 SECTION.                                              
135600                                                                          
135700     IF WS-IDDC NOT = W-IDDC-B6                                           
135800        MOVE WS-IDDC TO W-IDDC-B6                                         
135900        PERFORM IMS-GU-WDB601                                             
136000     END-IF                                                               
136100                                                                          
136200     MOVE ZERO               TO KOLLI-IDKOLLI-FLER                        
136300     MOVE PLATS-IDTRPTNR     TO KOLLI-IDTRPTNR                            
136400     MOVE PLATS-ADCLGEO      TO KOLLI-ADCLGEO                             
136500     MOVE PLATS-ADFLOMR      TO KOLLI-ADFLOMR                             
136600     MOVE PLATS-ADRUTNIV     TO KOLLI-ADRUTNIV                            
136700     MOVE PLATS-ADVMODUL     TO KOLLI-ADVMODUL                            
136800     MOVE PLATS-ADHMODUL     TO KOLLI-ADHMODUL                            
136900     MOVE PLATS-DIHMODUL     TO KOLLI-DIHMODUL                            
137000     MOVE PLATS-DIDMODUL     TO KOLLI-DIDMODUL                            
137100     MOVE PLATS-FLUTLAST     TO KOLLI-FLUTLAST                            
137300     IF KOLLI-FLAUTFAK = JA AND KOLLI-FLUTLAST = JA                       
137400       AND WS-SAVE-VORD-KDORDSTA = 3                                      
137500       MOVE NEJ              TO KOLLI-FLUTLAST                            
137600     END-IF                                                               
137700     MOVE 1                  TO KOLLI-KDKOLSTA                            
137800     ACCEPT KOLLI-TIPACKN FROM DATE                                       
137900     ACCEPT WS-TIPACTID-8 FROM TIME                                       
138000                                                                          
138100     IF DCS-NDC OR                                                        
138200       (DCS-SDC AND DCS-IDLANDX2 = 'GB')                                  
138300       MOVE ALL '+'           TO MSGI-WMSGINIT                            
138400       MOVE '011'             TO MSGI-KDCALL                              
138500       MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                              
138600                                 MSGI-IDLTERM-USER                        
138700       MOVE KOLLI-TIPACKN TO WS-TIPACKN-GRP                               
138800       MOVE WS-TIPACKN         TO MSGI-TILOKDAT                           
138900       MOVE WS-TIPACTID-6(1:4) TO MSGI-TILOKTID                           
139000       CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                         
139100       MOVE MSGI-TILOKDAT     TO WS-TIPACKN                               
139200       MOVE WS-TIPACKN        TO KOLLI-TIPACKN                            
139300       MOVE MSGI-TILOKTID     TO WS-TIPACTID-6 (1:4)                      
139400     END-IF                                                               
139500     MOVE WS-TIPACTID-6 TO KOLLI-TIPACTID                                 
139600     MOVE WS-DIKOLLIL        TO KOLLI-DIKOLLIL                            
139700     MOVE WS-DIKOLLIB        TO KOLLI-DIKOLLIB                            
139800     MOVE WS-DIKOLLIH        TO KOLLI-DIKOLLIH                            
139900     MOVE WS-KDEMBTYP        TO KOLLI-KDEMBTYP                            
140000     MOVE WS-KDKOLLI         TO KOLLI-KDKOLLI                             
140100     MOVE WS-VLORDBTO        TO KOLLI-VLORDBTO-KOLLI                      
140200     .                                                                    
140300     EJECT                                                                
140400 CH-UPPDAT-4726-4727 SECTION.                                             
140500                                                                          
140600                                                                          
140700       MOVE '4726'              TO W-4726-IDHTYP                          
140800*      IF SVERIGE-DISTRIKT                                                
140900*          MOVE JA              TO W-4726-FLBATCH                         
141000*      ELSE                                                               
141100           MOVE NEJ             TO W-4726-FLBATCH                         
141200*      END-IF                                                             
141300       MOVE LOW-VALUE           TO W-4726-LOWVALUE                        
141400                                                                          
141500       PERFORM IMS-GU-4726-ROT-KVAL                                       
141600                                                                          
141700       MOVE WS-IDDISTR          TO W-4726-IDDISTR                         
141800       MOVE WS-IDKUNDNR         TO W-4726-IDKUNDNR                        
141900       MOVE WS-IDDC             TO W-4726-IDDC                            
142000       MOVE WS-KDFAKTYP         TO W-4726-KDFAKTYP                        
142100                                                                          
142200       PERFORM IMS-GNP-4726-UNDSEG-KVAL                                   
142300                                                                          
142400       IF SEGMENT-SAKNAS                                                  
142500           MOVE WS-IDDISTR      TO AUTFAKT-IDDISTR                        
142600           MOVE WS-IDKUNDNR     TO AUTFAKT-IDKUNDNR                       
142700           MOVE WS-IDDC         TO AUTFAKT-IDDC                           
142800           MOVE WS-KDFAKTYP     TO AUTFAKT-KDFAKTYP                       
142900                                                                          
143000           PERFORM IMS-INSERT-4726-UNDSEG                                 
143100                                                                          
143200       END-IF                                                             
143300       MOVE WS-IDPRODNR         TO AUTFAKT-IDPRODNR                       
143400       MOVE ZERO                TO AUTFAKT-IDSKEPPN                       
143500                                   AUTFAKT-PRFRAKT                        
143600                                                                          
143700       IF SVERIGE-DISTRIKT                                                
143800           MOVE NEJ             TO AUTFAKT-FLLASTA                        
143900       ELSE                                                               
144000           MOVE JA              TO AUTFAKT-FLLASTA                        
144100       END-IF                                                             
144200                                                                          
144300       PERFORM IMS-INSERT-4727                                            
144400                                                                          
144500     .                                                                    
144600     EJECT                                                                
144700 CK-HAEMTA-STARTNYCKEL       SECTION.                                     
144800                                                                          
144900     MOVE 'N'               TO WS-SLINGA-KLAR                             
145000                                                                          
145100     IF IDPRODNR-IFYLLT                                                   
145200                                                                          
145300       MOVE WS-IDPRODNR              TO W-E601-IDPRODNR                   
145400       MOVE WS-IDPRODNR              TO W-WDE4E-IDPRODNR                  
145500       PERFORM IMS-GU-WDE62-VORD-GODK                                     
145600       IF SEGMENT-FINNS                                                   
145700         MOVE WDE62-VORD-IDDISTR          TO WS-IDDISTR-NUM               
145800         MOVE WS-IDDISTR-NUM              TO 4337-MOD-IDDISTR-UT          
145900         MOVE WDE62-VORD-IDKUNDNR         TO WS-IDKUNDNR-7                
146000         MOVE WS-IDKUNDNR-7(2:6)          TO WS-IDKUNDNR                  
146100         MOVE WS-IDKUNDNR                 TO 4337-MOD-IDKUNDNR-UT         
146200         PERFORM IMS-GN-WDE401-ESEQ                                       
146300         IF SEGMENT-FINNS                                                 
146400           MOVE E4E-KORD-IDORDNR5    TO WS-IDORDNR                        
146500           MOVE WS-IDORDNR           TO 4337-MOD-IDORDNR-UT               
146600                                                                          
146700         END-IF                                                           
146800       ELSE                                                               
146900         MOVE SPACE                  TO WS-IDDISTR                        
147000                                        WS-IDKUNDNR                       
147100                                        WS-IDKUNDRF                       
147200       END-IF                                                             
147300     END-IF                                                               
147400*                                                                         
147500       MOVE WS-IDDISTR      TO W-E4A1-IDDISTR                             
147600       MOVE WS-IDKUNDNR     TO W-E4A1-IDKUNDNR                            
147700       MOVE SPACE           TO W-E4A1-IDKUNDRF                            
147800       MOVE WS-IDKUNDRF     TO W-E4A1-IDKUNDRF                            
147900       PERFORM IMS-GU-E4A1-SEK-KVAL                                       
148000*          STRING '*' STATUS-WS                                           
148100*                 '*' WS-IDDISTR                                          
148200*                 '*' WS-IDKUNDNR                                         
148300*                 '*' WS-IDKUNDRF                                         
148400*                 '*' WS-IDPRODNR                                         
148500*                 '*' IDPRODNR-IFYLLT-SW                                  
148600*          DELIMITED BY SIZE INTO MOD-TEMFSINF                            
148700*                                                                         
148800     IF KUNDORDER-SEK-FINNS                                               
148900        PERFORM UNTIL KUNDORDER-SEK-SAKNAS OR                             
149000                      SLINGA-KLAR                                         
149100          MOVE KORD-IDDISTR   TO W-E401-IDDISTR                           
149200          MOVE KORD-IDKUNDNR  TO W-E401-IDKUNDNR                          
149300          MOVE KORD-IDKUNDRF  TO W-E401-IDKUNDRF                          
149400          MOVE KORD-IDPRODNR  TO W-E401-IDPRODNR                          
149500                                 W-Q301-IDPRODNR                          
149600          MOVE KORD-IDPLKLST  TO W-E401-IDPLKLST                          
149700                                                                          
149800          PERFORM IMS-GU-E401-KVAL                                        
149900          MOVE KORD-KVORDRAD-LEVPL   TO WS-SAVE-KVORDRAD-LEVPL            
150000          MOVE KORD-IDORDER          TO W-Q201-IDORDER                    
150100                                        W-Q301-IDORDER                    
150200          MOVE KORD-IDPRODNR  TO W-E601-IDPRODNR                          
150300          PERFORM IMS-GU-E601                                             
150400          IF SEGMENT-FINNS                                                
150500             IF VORD-IDDC = WS-IDDC AND                                   
150600                WS-SAVE-KVORDRAD-LEVPL = ZERO                             
150700                CONTINUE                                                  
150800             ELSE                                                         
150900                SET SEGMENT-SAKNAS TO TRUE                                
151000             END-IF                                                       
151100          END-IF                                                          
151200                                                                          
151300          IF SEGMENT-FINNS                                                
151400             MOVE 'J' TO WS-SLINGA-KLAR                                   
151500          ELSE                                                            
151600             PERFORM IMS-GN-E4A1-SEK-KVAL                                 
151700          END-IF                                                          
151800        END-PERFORM                                                       
151900     END-IF                                                               
152000*                                                                         
152100     IF NOT SLINGA-KLAR                                                   
152200        IF IDPRODNR-IFYLLT                                                
152300          MOVE FEL-765(INDX)  TO MOD-TEMFSFEL                             
152400        ELSE                                                              
152500          MOVE FEL-762(INDX)  TO MOD-TEMFSFEL                             
152600        END-IF                                                            
152700        MOVE JA               TO WS-FEL-FUNNET                            
152800        PERFORM S01-ROER-EJ-MODFAELT                                      
152900        PERFORM S02-SAETT-LAES-IGEN-ATTRIBUT                              
153000     END-IF                                                               
153100     .                                                                    
153200     EJECT                                                                
153300 CL-UPPDAT-KDORDSTA SECTION.                                              
153400                                                                          
153500     MOVE WS-IDDC               TO W-IDDC                                 
153600     PERFORM IMS-GHU-ORQI12                                               
153800     IF ARB-KDORDSTA = 'U '                                               
153900        MOVE 'U*'               TO ARB-KDORDSTA                           
153910        PERFORM IMS-REPL-ORQI12                                           
154000     END-IF                                                               
154300     .                                                                    
154400     EJECT                                                                
154500 CM-EV-BORTTAG-HTYP-4302-4490     SECTION.                                
154600                                                                          
154700     MOVE WS-IDPRODNR          TO W-4301-IDPRODNR                         
154800     MOVE JA                   TO 4490-BORTTAG-SW                         
154900                                  FOERSTA-SW                              
155000                                                                          
155100     PERFORM IMS-GU-XXDU01                                                
155200     IF SEGMENT-FINNS                                                     
155300       MOVE WS-IDKOLLI         TO W-4302-IDKOLLI                          
155400       PERFORM IMS-GNP-XXDU11                                             
155500                                                                          
155600       IF SEGMENT-FINNS                                                   
155700         MOVE WS-IDDISTR         TO W-E401-IDDISTR                        
155800         MOVE WS-IDKUNDNR        TO W-E401-IDKUNDNR                       
155900         MOVE WS-IDORDNR         TO W-E401-IDORDNR                        
156000         MOVE WS-IDPRODNR        TO W-E401-IDPRODNR                       
156100                                                                          
156200         PERFORM UNTIL SEGMENT-SAKNAS OR END-OF-DATA                      
156300           MOVE 4302-IDPLKLST    TO W-E401-IDPLKLST                       
156400                                    W-4302-IDPLKLST                       
156500                                    WS-IDPLKLST                           
156600                                                                          
156700           PERFORM IMS-GU-E401-KVAL                                       
156800           IF SEGMENT-FINNS                                               
156900             MOVE WS-IDKOLLI     TO W-4302-IDKOLLI                        
157000             PERFORM CMA-BORTTAG-HTYP-4302                                
157100             IF KORD-KVORDRAD    NOT = KORD-KVORDRAD-PACK                 
157200               MOVE NEJ          TO 4490-BORTTAG-SW                       
157300             END-IF                                                       
157400           END-IF                                                         
157500                                                                          
157600           PERFORM IMS-GU-XXDU01-XXDU2                                    
157700           PERFORM IMS-GNP-XXDU11-IDPLKLST-XXDU2                          
157800*          LÄS MED IDPLKLST FÖR ATT FÅ EV. ANDRA IDKOLLIN                 
157900           IF SEGMENT-SAKNAS AND SEG-4490-BORTTAG                         
158000             PERFORM CMB-BORTTAG-4490                                     
158100           END-IF                                                         
158200           MOVE JA                TO 4490-BORTTAG-SW                      
158300           PERFORM IMS-GNP-XXDU11-IDKOLLI                                 
158400*          LÄS MED SAMMA IDKOLLI FÖR ATT FÅ EV. ANDRA IDPLKLSTOR          
158500         END-PERFORM                                                      
158600                                                                          
158700         PERFORM IMS-GNP-XXDU11-FIRST                                     
158800         IF SEGMENT-SAKNAS                                                
158900           PERFORM IMS-GHU-XXDU01                                         
159000           PERFORM IMS-DLET-XXDU                                          
159100         END-IF                                                           
159200       END-IF                                                             
159300     END-IF                                                               
159400     .                                                                    
159500     SKIP2                                                                
159600 CMA-BORTTAG-HTYP-4302       SECTION.                                     
159700                                                                          
159800     PERFORM IMS-GHU-XXDU01                                               
159900     PERFORM IMS-GHNP-XXDU11-KVAL                                         
160000     PERFORM IMS-DLET-XXDU                                                
160100     .                                                                    
160200     SKIP2                                                                
160300 CMB-BORTTAG-4490      SECTION.                                           
160400                                                                          
160500     MOVE W-Q201-IDORDER       TO W-Q301-IDORDER                          
160600     MOVE WS-IDDC              TO W-Q301-IDDC                             
160700     MOVE WS-IDPRODNR          TO W-Q301-IDPRODNR                         
160800     MOVE WS-IDPLKLST          TO W-Q301-IDPLKLST                         
160900     PERFORM IMS-GU-ORQA01                                                
161000                                                                          
161100     MOVE ORQA-ODEL-IDDC       TO W-4447-IDDC                             
161200     MOVE ORQA-ODEL-IDPRC      TO W-4448-IDPRC                            
161300     PERFORM IMS-GU-XXKH11                                                
161400                                                                          
161500     MOVE ORQA-ODEL-IDDC       TO W-4487-IDDC                             
161600     MOVE 4448-KDPRCGRP        TO W-4488-KDPRCGRP                         
161700     MOVE ORQA-ODEL-DARFS      TO W-4490-DARFS                            
161800     MOVE ORQA-ODEL-IDPRODNR   TO W-4490-IDPRODNR                         
161900     MOVE ORQA-ODEL-IDPLKLST   TO W-4490-IDPLKLST                         
162000     PERFORM IMS-GHU-WDGX4490                                             
162100                                                                          
162200     IF SEGMENT-FINNS                                                     
162300        PERFORM IMS-DLET-WDGX4490                                         
162400     END-IF                                                               
162500     .                                                                    
162600     EJECT                                                                
162700 CN-KOLLA-PRINTER SECTION.                                                
162800                                                                          
162900*KOLLIFLAGGA                                                              
163000                                                                          
163100     IF MID-KDPRTVAL-AF-IN = ALL '+'                                      
163200       IF MID-KDPRTVAL-AF-UT = 'U '                                       
163300       OR MID-KDPRTVAL-AF-UT = 'UU'                                       
163400         MOVE 'UU'                 TO MOD-KDPRTVAL-AF-UT                  
163500                                      4341-MID-KDPRTVAL-UT                
163600       ELSE                                                               
163700                                                                          
163800         MOVE '4'                  TO WS-SYSTDEL                          
163900         MOVE 'KF'                 TO WS-LISTTYP                          
164000         MOVE WS-IDDC              TO WS-DC                               
164100         MOVE MID-KDPRTVAL-AF-UT TO WS-KDPRT                              
164200         MOVE 001                  TO PRT-KDCALL                          
164300         MOVE WS-IDPRTLST          TO PRT-IDPRTLST                        
164400         CALL W006PRT USING PRT-W006PRT                                   
164500                                                                          
164600         IF PRT-KDSVAR = RAETT                                            
164700           MOVE MID-KDPRTVAL-AF-UT TO MOD-KDPRTVAL-AF-UT                  
164800                                      4337-MOD-KDPRTVAL-AF-UT             
164900         ELSE                                                             
165000           IF FEL-EJ-FUNNET                                               
165100             MOVE FEL-772(INDX) TO MOD-TEMFSFEL                           
165200             MOVE JA           TO WS-FEL-FUNNET                           
165300             PERFORM S01-ROER-EJ-MODFAELT                                 
165400             PERFORM S02-SAETT-LAES-IGEN-ATTRIBUT                         
165500           END-IF                                                         
165600           MOVE MFS-ALFA-FAELT-FEL TO MOD-KDPRTVAL-AF-IN-ATTR             
165700           MOVE MID-KDPRTVAL-AF-UT TO MOD-KDPRTVAL-AF-UT                  
165800         END-IF                                                           
165900       END-IF                                                             
166000     ELSE                                                                 
166100       IF MID-KDPRTVAL-AF-IN = 'U '                                       
166200       OR MID-KDPRTVAL-AF-IN = 'UU'                                       
166300         MOVE 'UU'                 TO MOD-KDPRTVAL-AF-UT                  
166400                                      4337-MOD-KDPRTVAL-AF-UT             
166500       ELSE                                                               
166600         MOVE '4'                  TO WS-SYSTDEL                          
166700         MOVE 'KF'                 TO WS-LISTTYP                          
166800         MOVE WS-IDDC              TO WS-DC                               
166900         MOVE MID-KDPRTVAL-AF-IN TO WS-KDPRT                              
167000         MOVE 001                  TO PRT-KDCALL                          
167100         MOVE WS-IDPRTLST          TO PRT-IDPRTLST                        
167200         CALL W006PRT USING PRT-W006PRT                                   
167300                                                                          
167400         IF PRT-KDSVAR = RAETT                                            
167500           MOVE MID-KDPRTVAL-AF-IN TO MOD-KDPRTVAL-AF-UT                  
167600                                      4337-MOD-KDPRTVAL-AF-UT             
167700         ELSE                                                             
167800           IF FEL-EJ-FUNNET                                               
167900             MOVE FEL-772(INDX) TO MOD-TEMFSFEL                           
168000             MOVE JA           TO WS-FEL-FUNNET                           
168100             PERFORM S01-ROER-EJ-MODFAELT                                 
168200             PERFORM S02-SAETT-LAES-IGEN-ATTRIBUT                         
168300           END-IF                                                         
168400           MOVE MFS-ALFA-FAELT-FEL TO MOD-KDPRTVAL-AF-IN-ATTR             
168500           MOVE MID-KDPRTVAL-AF-IN TO MOD-KDPRTVAL-AF-IN                  
168600           MOVE MID-KDPRTVAL-AF-UT TO MOD-KDPRTVAL-AF-UT                  
168700         END-IF                                                           
168800       END-IF                                                             
168900     END-IF                                                               
169000                                                                          
169100* FOLJESEDEL                                                              
169200                                                                          
169300     IF MID-KDPRTVAL-FS-IN = ALL '+'                                      
169400                                                                          
169500       IF MID-KDPRTVAL-FS-UT = 'U '                                       
169600       OR MID-KDPRTVAL-FS-UT = 'UU'                                       
169700         MOVE 'UU'                TO MOD-KDPRTVAL-FS-UT                   
169800                                     4341-MID-KDPRTVAL-UT                 
169900                                     4337-MOD-KDPRTVAL-FS                 
170000       ELSE                                                               
170100         MOVE MID-KDPRTVAL-FS-UT  TO WS-KDPRT                             
170200         MOVE '4'                 TO WS-SYSTDEL                           
170300         MOVE 'FS'                TO WS-LISTTYP                           
170400         MOVE WS-IDDC             TO WS-DC                                
170500                                                                          
170600         MOVE 001                 TO PRT-KDCALL                           
170700         MOVE WS-IDPRTLST         TO PRT-IDPRTLST                         
170800         CALL W006PRT USING PRT-W006PRT                                   
170900                                                                          
171000         IF PRT-KDSVAR = RAETT                                            
171100            MOVE PRT-KDSVAR       TO WS-W006-KDSVAR-FOLJEFL               
171200            MOVE MID-KDPRTVAL-FS-UT TO MOD-KDPRTVAL-FS-UT                 
171300                                       4341-MID-KDPRTVAL-UT               
171400                                       4337-MOD-KDPRTVAL-FS               
171500         ELSE                                                             
171600            IF FEL-EJ-FUNNET                                              
171700               MOVE FEL-772(INDX) TO MOD-TEMFSFEL                         
171800               MOVE JA           TO WS-FEL-FUNNET                         
171900               PERFORM S01-ROER-EJ-MODFAELT                               
172000               PERFORM S02-SAETT-LAES-IGEN-ATTRIBUT                       
172100            END-IF                                                        
172200           MOVE MFS-ALFA-FAELT-FEL TO MOD-KDPRTVAL-FS-IN-ATTR             
172300           MOVE MID-KDPRTVAL-FS-UT TO MOD-KDPRTVAL-FS-UT                  
172400         END-IF                                                           
172500       END-IF                                                             
172600     ELSE                                                                 
172700                                                                          
172800       IF MID-KDPRTVAL-FS-IN = 'U '                                       
172900       OR MID-KDPRTVAL-FS-IN = 'UU'                                       
173000         MOVE 'UU'                TO MOD-KDPRTVAL-FS-UT                   
173100                                    4341-MID-KDPRTVAL-UT                  
173200                                    4337-MOD-KDPRTVAL-FS                  
173300       ELSE                                                               
173400         MOVE MID-KDPRTVAL-FS-IN  TO WS-KDPRT                             
173500         MOVE '4'                 TO WS-SYSTDEL                           
173600         MOVE 'FS'                TO WS-LISTTYP                           
173700         MOVE WS-IDDC             TO WS-DC                                
173800                                                                          
173900         MOVE 001                 TO PRT-KDCALL                           
174000         MOVE WS-IDPRTLST         TO PRT-IDPRTLST                         
174100         CALL W006PRT USING PRT-W006PRT                                   
174200                                                                          
174300         IF PRT-KDSVAR = RAETT                                            
174400           MOVE PRT-KDSVAR       TO WS-W006-KDSVAR-FOLJEFL                
174500           MOVE MID-KDPRTVAL-FS-IN TO MOD-KDPRTVAL-FS-UT                  
174600                                      4341-MID-KDPRTVAL-UT                
174700                                      4337-MOD-KDPRTVAL-FS                
174800         ELSE                                                             
174900           IF FEL-EJ-FUNNET                                               
175000             MOVE FEL-772(INDX) TO MOD-TEMFSFEL                           
175100             MOVE JA           TO WS-FEL-FUNNET                           
175200             PERFORM S01-ROER-EJ-MODFAELT                                 
175300             PERFORM S02-SAETT-LAES-IGEN-ATTRIBUT                         
175400             MOVE MID-KDPRTVAL-FS-IN TO MOD-KDPRTVAL-FS-IN                
175500             MOVE MFS-ALFA-FAELT-FEL TO MOD-KDPRTVAL-FS-IN-ATTR           
175600           END-IF                                                         
175700         END-IF                                                           
175800       END-IF                                                             
175900     END-IF                                                               
176000     .                                                                    
176100     EJECT                                                                
176200 D-REDIGERA-W4O33701-MOD SECTION.                                         
176300                                                                          
176400     MOVE LOW-VALUE             TO MSG-AREA                               
176500     MOVE 4337-MOD-LAENGD       TO MSG-KVLL                               
176600     MOVE 'W4O33701'            TO MFS-IDMOD                              
176700     MOVE '4337'                TO 4337-MOD-IDTRANS                       
176800     MOVE WS-IDPRODNR           TO 4337-MOD-IDPRODNR                      
176900     MOVE PLATS-ADFLGEO         TO 4337-MOD-ADFLGEO                       
177000     MOVE PLATS-ADFLOMR         TO 4337-MOD-ADFLOMR                       
177100     MOVE PLATS-ADRUTNIV        TO 4337-MOD-ADRUTNIV                      
177200     MOVE PLATS-ADVMODUL        TO 4337-MOD-ADVMODUL                      
177300     MOVE WS-ADRESS-TEXT        TO 4337-MOD-ADRESS-TEXT                   
177400     MOVE 4337-MOD-W4O33701     TO MSG-AREA                               
177500     .                                                                    
177600     EJECT                                                                
177700 E-RENSA-NYCKLAR SECTION.                                                 
177800     MOVE MFS-RENSA-FAELT    TO MOD-IDDISTR-UT                            
177900                                MOD-IDKUNDNR-UT                           
178000                                MOD-IDORDNR-UT                            
178100                                MOD-IDKOLLI-UT                            
178200                                MOD-IDPRODNR-UT                           
178300                                MOD-KDPRTVAL-AF-UT                        
178400                                MOD-KDPRTVAL-FS-UT                        
178500     .                                                                    
178600     EJECT                                                                
178700 F-SKAPA-FOLJESEDEL-TRANS SECTION.                                        
178800                                                                          
178900     MOVE '++++'            TO  4341-MID-IDDISTR-IN                       
179000     MOVE WS-IDDISTR        TO  4341-MID-IDDISTR-UT                       
179100     MOVE '++++++'          TO  4341-MID-IDKUNDNR-IN                      
179200     MOVE WS-IDKUNDNR       TO  4341-MID-IDKUNDNR-UT                      
179300     MOVE '+++++'           TO  4341-MID-IDORDNR-IN                       
179400     MOVE WS-IDORDNR        TO  4341-MID-IDORDNR-UT                       
179500     MOVE '+++++'           TO  4341-MID-IDKOLLI-IN                       
179600     MOVE WS-IDKOLLI        TO  4341-MID-IDKOLLI-UT                       
179700     MOVE '+++++'           TO  4341-MID-IDKOLLI-TOM-IN                   
179800     MOVE ZERO              TO  4341-MID-IDKOLLI-TOM-UT                   
179900     MOVE '++'              TO  4341-MID-IDDC-IN                          
180000     MOVE WS-IDDC           TO  4341-MID-IDDC-UT                          
180100     MOVE '++'              TO  4341-MID-KDPRTVAL-IN                      
180200     MOVE '++'              TO  4341-MID-KDPRTVAL-IN                      
180300     MOVE 'N'               TO  4341-MID-FL-SVENSK-FSEDEL                 
180400                                                                          
180500     MOVE WS-MFS-KDMFSFOR   TO  4341-MID-KDMFSFOR                         
180600     MOVE +80               TO  4341-MID-LL                               
180700                                                                          
180800     MOVE 4341-MID         TO MSG-INDATA-MINUS-1-TRANSKOD                 
180900     PERFORM IMS-INSERT-4341                                              
181000     .                                                                    
181100     EJECT                                                                
181200 CG-SEND-TMSINFO SECTION.                                                 
181300                                                                          
181400     MOVE KORD-IDDC              TO TMS-IDDC                              
181500     MOVE KORD-IDDISTR           TO TMS-IDDISTR                           
181600     MOVE KORD-IDKUNDNR          TO TMS-IDKUNDNR                          
181700     MOVE KORD-IDORDNR5          TO TMS-IDORDNR7                          
181800     MOVE KOLLI-IDKOLLI          TO TMS-IDKOLLI(1)                        
181900       CALL W403TMS1 USING TMS-W403TMS1                                   
182000               TMS-CRE-PCB TMS-DEL-PCB ATAB-PCB                           
182100               TMS-1165-PCB TMS-4141-PCB TMS-WDB2-PCB                     
182200               TMS-WDB6-PCB TMS-WDD3-PCB TMS-WDB1-PCB                     
182300               TMS-WDE4A-PCB TMS-WDE4F-PCB TMS-WDQ2-PCB                   
182400               TMS-WDQ3-PCB TMS-WDK6-PCB TMS-WDE6-PCB                     
182500               TMS-WDK5-PCB TMS-WDQ2C-PCB                                 
182600     .                                                                    
182700     EJECT                                                                
182800 S01-ROER-EJ-MODFAELT SECTION.                                            
182900                                                                          
183000                                                                          
183100     MOVE MFS-ROER-EJ-FAELT  TO MOD-KDPRTVAL-AF-IN                        
183200                                MOD-KDPRTVAL-AF-UT                        
183300                                MOD-KDPRTVAL-FS-IN                        
183400                                MOD-KDPRTVAL-FS-UT                        
183500                                MOD-KDKOLLI                               
183600                                MOD-KDEMBTYP                              
183700                                MOD-DIKOLLIL                              
183800                                MOD-DIKOLLIB                              
183900                                MOD-DIKOLLIH                              
184000     .                                                                    
184100     EJECT                                                                
184200 S02-SAETT-LAES-IGEN-ATTRIBUT SECTION.                                    
184300                                                                          
184400                                                                          
184500     MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KDPRTVAL-AF-IN-ATTR                
184600                                   MOD-KDPRTVAL-FS-IN-ATTR                
184700                                   MOD-KDKOLLI-ATTR                       
184800                                   MOD-KDEMBTYP-ATTR                      
184900                                   MOD-DIKOLLIL-ATTR                      
185000                                   MOD-DIKOLLIB-ATTR                      
185100                                   MOD-DIKOLLIH-ATTR                      
185200     SKIP2                                                                
185300                                                                          
185400     .                                                                    
185500     EJECT                                                                
185600* IMS SEKTIONER                                                           
185700     SKIP3                                                                
185800 IMS-GET-MSG SECTION.                                                     
185900     MOVE '  QC' TO GODK-STATUSKODER                                      
186000     CALL CBLTDLI USING GU                                                
186100                          MSG-PCB                                         
186200                          MSG-IO-AREA                                     
186300     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
186400     PERFORM IMS-STATUSKONTROLL                                           
186500     SKIP3                                                                
186600     .                                                                    
186700 IMS-INSERT-MSG SECTION.                                                  
186800                                                                          
186900     IF MSGI-IDLAND-SPR NOT = 'GB'                                        
187000       MOVE '0' TO MFS-KDHUVOMR                                           
187100     END-IF                                                               
187200     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
187300     MOVE SPACE TO GODK-STATUSKODER                                       
187400     CALL CBLTDLI USING ISRT                                              
187500                          MSG-PCB                                         
187600                          MSG-IO-AREA                                     
187700                          MFS-IDMOD                                       
187800     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
187900     PERFORM IMS-STATUSKONTROLL                                           
188000     .                                                                    
188100     EJECT                                                                
188200 IMS-INSERT-4341 SECTION.                                                 
188300                                                                          
188400     MOVE LOW-VALUE TO 4341-MID-Z1 4341-MID-Z2                            
188500     MOVE SPACE TO GODK-STATUSKODER                                       
188600     CALL CBLTDLI USING ISRT 4341-PCB 4341-MID-IO-AREA                    
188700     MOVE 4341-STATUS-CODE TO STATUS-WS                                   
188800     PERFORM IMS-STATUSKONTROLL                                           
188900     .                                                                    
189000     EJECT                                                                
189100 IMS-GU-K501-KVAL SECTION.                                                
189200     STRING 'WLEMBB01(KDKOLLI  =' W-K501-KDKOLLI-X ')'                    
189300            DELIMITED BY SIZE INTO SSA1                                   
189400     MOVE '  GE' TO GODK-STATUSKODER                                      
189500     CALL CBLTDLI USING GU                                                
189600                          EMBB-PCB                                        
189700                          DLI-IO-AREA                                     
189800                          SSA1                                            
189900     MOVE EMBB-STATUS-CODE TO STATUS-WS                                   
190000     PERFORM IMS-STATUSKONTROLL                                           
190100     .                                                                    
190200     EJECT                                                                
190300 IMS-GU-E4A1-SEK-KVAL SECTION.                                            
190400                                                                          
190500     STRING 'WDE401  (WDE4ASEQ =' W-E4A1-WDE4KEY-X ')'                    
190600            DELIMITED BY SIZE INTO SSA1                                   
190700     MOVE '  GE' TO GODK-STATUSKODER                                      
190800     CALL CBLTDLI USING GU                                                
190900                          WDE4A-PCB                                       
191000                          DLI-IO-AREA                                     
191100                          SSA1                                            
191200     MOVE WDE4A-STATUS-CODE TO STATUS-WS                                  
191300                               STATUS-KUNDORDER-SEK-WS                    
191400     PERFORM IMS-STATUSKONTROLL                                           
191500     SKIP3                                                                
191600     .                                                                    
191700 IMS-GN-E4A1-SEK-KVAL SECTION.                                            
191800                                                                          
191900     STRING 'WDE401  (WDE4ASEQ =' W-E4A1-WDE4KEY-X ')'                    
192000            DELIMITED BY SIZE INTO SSA1                                   
192100     MOVE '  GE' TO GODK-STATUSKODER                                      
192200     CALL CBLTDLI USING GN                                                
192300                          WDE4A-PCB                                       
192400                          DLI-IO-AREA                                     
192500                          SSA1                                            
192600     MOVE WDE4A-STATUS-CODE TO STATUS-WS                                  
192700                               STATUS-KUNDORDER-SEK-WS                    
192800     PERFORM IMS-STATUSKONTROLL                                           
192900     .                                                                    
193000     EJECT                                                                
193100 IMS-GU-E401-KVAL SECTION.                                                
193200                                                                          
193300     STRING 'WDE401  (WDE401KY =' W-E401-WDE4KEY-X ')'                    
193400            DELIMITED BY SIZE INTO SSA1                                   
193500     MOVE '    ' TO GODK-STATUSKODER                                      
193600     CALL CBLTDLI USING GU WDE4-PCB                                       
193700                           DLI-IO-AREA                                    
193800                           SSA1                                           
193900     MOVE WDE4-STATUS-CODE TO STATUS-WS                                   
194000     PERFORM IMS-STATUSKONTROLL                                           
194100     SKIP3                                                                
194200     .                                                                    
194300 IMS-GNP-WDE411 SECTION.                                                  
194400     MOVE 'WDE411  ' TO SSA1                                              
194500     MOVE '  GE' TO GODK-STATUSKODER                                      
194600     CALL CBLTDLI USING GNP WDE4-PCB                                      
194700                            DLI-IO-WDE411                                 
194800                            SSA1                                          
194900     MOVE WDE4-STATUS-CODE TO STATUS-WS                                   
195000     PERFORM IMS-STATUSKONTROLL                                           
195100     SKIP3                                                                
195200     .                                                                    
195300 IMS-GU-E601        SECTION.                                              
195400                                                                          
195500     STRING 'WDE601  (IDPRODNR =' W-E601-IDPRODNR-X ')'                   
195600            DELIMITED BY SIZE INTO SSA1                                   
195700     MOVE '  GE' TO GODK-STATUSKODER                                      
195800     CALL CBLTDLI USING GU                                                
195900                          WDE6-PCB                                        
196000                          DLI-IO-WDE601                                   
196100                          SSA1                                            
196200     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
196300     PERFORM IMS-STATUSKONTROLL                                           
196400     SKIP3                                                                
196500     .                                                                    
196600 IMS-GU-E611 SECTION.                                                     
196700     STRING 'WDE601  (IDPRODNR =' W-E601-IDPRODNR-X ')'                   
196800            DELIMITED BY SIZE INTO SSA1                                   
196900     STRING 'WDE611  (IDKOLLI  =' W-E611-IDKOLLI-X ')'                    
197000            DELIMITED BY SIZE INTO SSA2                                   
197100     MOVE '  GE' TO GODK-STATUSKODER                                      
197200     CALL CBLTDLI USING GU                                                
197300                          WDE6-PCB                                        
197400                          DLI-IO-WDE611                                   
197500                          SSA1                                            
197600                          SSA2                                            
197700     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
197800     PERFORM IMS-STATUSKONTROLL                                           
197900     SKIP3                                                                
198000     .                                                                    
198100 IMS-GHU-FIRST-E601-KVAL SECTION.                                         
198200     STRING 'WDE601  (IDPRODNR =' W-E601-IDPRODNR-X ')'                   
198300            DELIMITED BY SIZE INTO SSA1                                   
198400     MOVE '  ' TO GODK-STATUSKODER                                        
198500     CALL CBLTDLI USING GHU                                               
198600                          WDE6-PCB                                        
198700                          DLI-IO-WDE601                                   
198800                          SSA1                                            
198900     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
199000     PERFORM IMS-STATUSKONTROLL                                           
199100     .                                                                    
199200     EJECT                                                                
199300 IMS-REPLACE-WDE601 SECTION.                                              
199400     MOVE '  ' TO GODK-STATUSKODER                                        
199500     CALL CBLTDLI USING REPL                                              
199600                          WDE6-PCB                                        
199700                          DLI-IO-WDE601                                   
199800     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
199900     PERFORM IMS-STATUSKONTROLL                                           
200000     SKIP3                                                                
200100     .                                                                    
200200 IMS-REPLACE-WDE611 SECTION.                                              
200300     MOVE '  ' TO GODK-STATUSKODER                                        
200400     CALL CBLTDLI USING REPL                                              
200500                          WDE6-PCB                                        
200600                          DLI-IO-WDE611                                   
200700     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
200800     PERFORM IMS-STATUSKONTROLL                                           
200900     SKIP3                                                                
201000     .                                                                    
201100 IMS-GHU-E601     SECTION.                                                
201200     STRING 'WDE601  (IDPRODNR =' W-E601-IDPRODNR-X ')'                   
201300            DELIMITED BY SIZE INTO SSA1                                   
201400     STRING 'WDE611  (IDKOLLI  =' W-E611-IDKOLLI-X ')'                    
201500            DELIMITED BY SIZE INTO SSA2                                   
201600     MOVE '  ' TO GODK-STATUSKODER                                        
201700     CALL CBLTDLI USING GHU                                               
201800                          WDE6-PCB                                        
201900                          DLI-IO-WDE611                                   
202000                          SSA1                                            
202100                          SSA2                                            
202200     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
202300     PERFORM IMS-STATUSKONTROLL                                           
202400     SKIP3                                                                
202500     .                                                                    
202600 IMS-GU-WDK601  SECTION.                                                  
202700     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
202800          DELIMITED BY SIZE INTO SSA1                                     
202900     MOVE '  GE' TO GODK-STATUSKODER                                      
203000     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-WDK601 SSA1                    
203100     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
203200     PERFORM IMS-STATUSKONTROLL                                           
203300     .                                                                    
203400     SKIP3                                                                
203500 IMS-GNP-WDK611  SECTION.                                                 
203600     STRING 'WDK611  (KDSEGKEY =' W-KDSEGKEY-X ')'                        
203700          DELIMITED BY SIZE INTO SSA1                                     
203800     MOVE '  GE' TO GODK-STATUSKODER                                      
203900     CALL CBLTDLI USING GNP WDK6-PCB DLI-IO-WDK611 SSA1                   
204000     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
204100     PERFORM IMS-STATUSKONTROLL                                           
204200     .                                                                    
204300     SKIP3                                                                
204400 IMS-GU-4726-ROT-KVAL SECTION.                                            
204500     STRING 'WLXXDV01(WDGXKEY  =' W-4726-WDGXKEY-ROT-X ')'                
204600            DELIMITED BY SIZE INTO SSA1                                   
204700     MOVE '  ' TO GODK-STATUSKODER                                        
204800     CALL CBLTDLI USING GU                                                
204900                          XXDV-PCB                                        
205000                          DLI-IO-AREA                                     
205100                          SSA1                                            
205200     MOVE XXDV-STATUS-CODE TO STATUS-WS                                   
205300     PERFORM IMS-STATUSKONTROLL                                           
205400     SKIP3                                                                
205500     .                                                                    
205600 IMS-GNP-4726-UNDSEG-KVAL SECTION.                                        
205700     STRING 'WLXXDV11(WDGXKEY  =' W-4726-WDGXKEY-UNDSEG-X ')'             
205800            DELIMITED BY SIZE INTO SSA1                                   
205900     MOVE '  GE' TO GODK-STATUSKODER                                      
206000     CALL CBLTDLI USING GNP                                               
206100                          XXDV-PCB                                        
206200                          DLI-IO-AREA                                     
206300                          SSA1                                            
206400     MOVE XXDV-STATUS-CODE TO STATUS-WS                                   
206500     PERFORM IMS-STATUSKONTROLL                                           
206600     .                                                                    
206700     EJECT                                                                
206800 IMS-INSERT-4726-UNDSEG SECTION.                                          
206900     STRING 'WLXXDV01(WDGXKEY  =' W-4726-WDGXKEY-ROT-X ')'                
207000            DELIMITED BY SIZE INTO SSA1                                   
207100     MOVE   'WLXXDV11 '         TO SSA2                                   
207200     MOVE '  II' TO GODK-STATUSKODER                                      
207300     CALL CBLTDLI USING ISRT                                              
207400                          XXDV-PCB                                        
207500                          DLI-IO-AREA                                     
207600                          SSA1                                            
207700                          SSA2                                            
207800     MOVE XXDV-STATUS-CODE TO STATUS-WS                                   
207900     PERFORM IMS-STATUSKONTROLL                                           
208000     SKIP3                                                                
208100     .                                                                    
208200 IMS-INSERT-4727 SECTION.                                                 
208300     STRING 'WLXXDV01(WDGXKEY  =' W-4726-WDGXKEY-ROT-X ')'                
208400            DELIMITED BY SIZE INTO SSA1                                   
208500     STRING 'WLXXDV11(WDGXKEY  =' W-4726-WDGXKEY-UNDSEG-X ')'             
208600            DELIMITED BY SIZE INTO SSA2                                   
208700     MOVE   'WLXXDV21 '         TO SSA3                                   
208800     MOVE '  II' TO GODK-STATUSKODER                                      
208900     CALL CBLTDLI USING ISRT                                              
209000                          XXDV-PCB                                        
209100                          DLI-IO-AREA                                     
209200                          SSA1                                            
209300                          SSA2                                            
209400                          SSA3                                            
209500     MOVE XXDV-STATUS-CODE TO STATUS-WS                                   
209600     PERFORM IMS-STATUSKONTROLL                                           
209700     .                                                                    
209800 IMS-GU-WDE62-VORD SECTION.                                               
209900                                                                          
210000     STRING 'WDE601  (IDPRODNR =' W-E601-IDPRODNR-X ')'                   
210100            DELIMITED BY SIZE INTO SSA1                                   
210200     MOVE '  ' TO GODK-STATUSKODER                                        
210300     CALL CBLTDLI USING GU WDE62-PCB DLI-IO-AREA2 SSA1                    
210400     MOVE WDE62-STATUS-CODE TO STATUS-WS                                  
210500     PERFORM IMS-STATUSKONTROLL                                           
210600     .                                                                    
210700     SKIP3                                                                
210800 IMS-GU-WDE62-VORD-GODK     SECTION.                                      
210900                                                                          
211000     STRING 'WDE601  (IDPRODNR =' W-E601-IDPRODNR-X ')'                   
211100            DELIMITED BY SIZE INTO SSA1                                   
211200     MOVE '  GE' TO GODK-STATUSKODER                                      
211300     CALL CBLTDLI USING GU WDE62-PCB DLI-IO-AREA2 SSA1                    
211400     MOVE WDE62-STATUS-CODE TO STATUS-WS                                  
211500     PERFORM IMS-STATUSKONTROLL                                           
211600     .                                                                    
211700     SKIP3                                                                
211800 IMS-GN-WDE401-ESEQ SECTION.                                              
211900                                                                          
212000     STRING 'WDE401  (WDE4ESEQ =' W-WDE4E-IDPRODNR-X ')'                  
212100            DELIMITED BY SIZE INTO SSA1                                   
212200     MOVE '  GEGB' TO GODK-STATUSKODER                                    
212300     CALL CBLTDLI USING GN WDE4E-PCB DLI-IO-WDE4E SSA1                    
212400     MOVE WDE4E-STATUS-CODE TO STATUS-WS                                  
212500     PERFORM IMS-STATUSKONTROLL                                           
212600     .                                                                    
212700     EJECT                                                                
212800 IMS-GU-ORQA-ODEL SECTION.                                                
212900                                                                          
213000     STRING 'WLORQA01(WDQ301KY >' W-Q301-KEY-MIN-X                        
213100                    '&WDQ301KY <' W-Q301-KEY-MAX-X ')'                    
213200            DELIMITED BY SIZE INTO SSA1                                   
213300     MOVE '  GE' TO GODK-STATUSKODER                                      
213400     CALL CBLTDLI USING GU ORQA-PCB DLI-IO-AREA2 SSA1                     
213500     MOVE ORQA-STATUS-CODE TO STATUS-WS                                   
213600     PERFORM IMS-STATUSKONTROLL                                           
213700     .                                                                    
213800     SKIP3                                                                
213900 IMS-GU-ORQA01    SECTION.                                                
214000                                                                          
214100     STRING 'WLORQA01(WDQ301KY =' W-Q301-KEY-X ')'                        
214200            DELIMITED BY SIZE INTO SSA1                                   
214300     MOVE '  GE' TO GODK-STATUSKODER                                      
214400     CALL CBLTDLI USING GU ORQA-PCB DLI-IO-AREA2 SSA1                     
214500     MOVE ORQA-STATUS-CODE TO STATUS-WS                                   
214600     PERFORM IMS-STATUSKONTROLL                                           
214700     .                                                                    
214800     SKIP3                                                                
214900 IMS-GN-ORQA-ODEL SECTION.                                                
215000                                                                          
215100     STRING 'WLORQA01(WDQ301KY >' W-Q301-KEY-MIN-X                        
215200                    '&WDQ301KY <' W-Q301-KEY-MAX-X ')'                    
215300            DELIMITED BY SIZE INTO SSA1                                   
215400     MOVE '  GEGB' TO GODK-STATUSKODER                                    
215500     CALL CBLTDLI USING GN ORQA-PCB DLI-IO-AREA2 SSA1                     
215600     MOVE ORQA-STATUS-CODE TO STATUS-WS                                   
215700     PERFORM IMS-STATUSKONTROLL                                           
215800     .                                                                    
215900     EJECT                                                                
216000 IMS-GHU-ORQI12    SECTION.                                               
216100                                                                          
216200     STRING 'WLORQI01(IDORDER  =' W-WDQ201-X ')'                          
216300            DELIMITED BY SIZE INTO SSA1                                   
216400     STRING 'WLORQI12(IDDC     =' W-IDDC-X   ')'                          
216500            DELIMITED BY SIZE INTO SSA2                                   
216600     MOVE '    ' TO GODK-STATUSKODER                                      
216700     CALL CBLTDLI USING GHU  ORQI-PCB DLI-IO-WDQ212 SSA1 SSA2             
216800     MOVE ORQI-STATUS-CODE TO STATUS-WS                                   
216900     PERFORM IMS-STATUSKONTROLL                                           
217000     .                                                                    
217100 IMS-REPL-ORQI12      SECTION.                                            
217200                                                                          
217300     MOVE '  ' TO GODK-STATUSKODER                                        
217400     CALL CBLTDLI USING REPL ORQI-PCB DLI-IO-WDQ212                       
217500     MOVE ORQI-STATUS-CODE TO STATUS-WS                                   
217600     PERFORM IMS-STATUSKONTROLL                                           
217700     .                                                                    
217800     EJECT                                                                
217900 IMS-GU-XXKH11      SECTION.                                              
218000                                                                          
218100     STRING 'WLXXKH01(WDGXKEY  =' W-4447-WDGXKEY-X ')'                    
218200            DELIMITED BY SIZE INTO SSA1                                   
218300     STRING 'WLXXKH11(WDGXKEY  =' W-4448-WDGXKEY-X ')'                    
218400            DELIMITED BY SIZE INTO SSA2                                   
218500     MOVE '  ' TO GODK-STATUSKODER                                        
218600     CALL CBLTDLI USING GU   XXKH-PCB DLI-IO-AREA  SSA1 SSA2              
218700     MOVE XXKH-STATUS-CODE TO STATUS-WS                                   
218800     PERFORM IMS-STATUSKONTROLL                                           
218900     .                                                                    
219000 IMS-GHU-WDGX4490    SECTION.                                             
219100                                                                          
219200     STRING 'WDR401  (WDGXKEY  =' W-4487-WDGXKEY-X ')'                    
219300            DELIMITED BY SIZE INTO SSA1                                   
219400     STRING 'WDGX4488(KDPRCGRP =' W-4488-WDGXKEY-X ')'                    
219500            DELIMITED BY SIZE INTO SSA2                                   
219600     STRING 'WDGX4490(KY4490   =' W-4490-WDGXKEY-X ')'                    
219700            DELIMITED BY SIZE INTO SSA3                                   
219800     MOVE '  GE' TO GODK-STATUSKODER                                      
219900     CALL CBLTDLI USING GHU  4487-PCB DLI-IO-AREA  SSA1 SSA2 SSA3         
220000     MOVE 4487-STATUS-CODE TO STATUS-WS                                   
220100     PERFORM IMS-STATUSKONTROLL                                           
220200     .                                                                    
220300 IMS-DLET-WDGX4490    SECTION.                                            
220400                                                                          
220500     MOVE '  ' TO GODK-STATUSKODER                                        
220600     CALL CBLTDLI USING DLET 4487-PCB DLI-IO-AREA                         
220700     MOVE 4487-STATUS-CODE TO STATUS-WS                                   
220800     PERFORM IMS-STATUSKONTROLL                                           
220900     .                                                                    
221000 IMS-GHU-XXDU01      SECTION.                                             
221100                                                                          
221200     STRING 'WLXXDU01(WDGXKEY  =' W-4301-WDGXKEY-X ')'                    
221300            DELIMITED BY SIZE INTO SSA1                                   
221400     MOVE '  GE' TO GODK-STATUSKODER                                      
221500     CALL CBLTDLI USING GHU XXDU-PCB DLI-IO-AREA4 SSA1                    
221600     MOVE XXDU-STATUS-CODE TO STATUS-WS                                   
221700     PERFORM IMS-STATUSKONTROLL                                           
221800     .                                                                    
221900 IMS-GU-XXDU01      SECTION.                                              
222000                                                                          
222100     STRING 'WLXXDU01(WDGXKEY  =' W-4301-WDGXKEY-X ')'                    
222200            DELIMITED BY SIZE INTO SSA1                                   
222300     MOVE '  GE' TO GODK-STATUSKODER                                      
222400     CALL CBLTDLI USING GU XXDU-PCB DLI-IO-AREA4 SSA1                     
222500     MOVE XXDU-STATUS-CODE TO STATUS-WS                                   
222600     PERFORM IMS-STATUSKONTROLL                                           
222700     .                                                                    
222800 IMS-GU-XXDU01-XXDU2    SECTION.                                          
222900                                                                          
223000     STRING 'WLXXDU01(WDGXKEY  =' W-4301-WDGXKEY-X ')'                    
223100            DELIMITED BY SIZE INTO SSA1                                   
223200     MOVE '  GE' TO GODK-STATUSKODER                                      
223300     CALL CBLTDLI USING GU XXDU2-PCB DLI-IO-AREA4 SSA1                    
223400     MOVE XXDU2-STATUS-CODE TO STATUS-WS                                  
223500     PERFORM IMS-STATUSKONTROLL                                           
223600     .                                                                    
223700 IMS-GNP-XXDU11    SECTION.                                               
223800                                                                          
223900     STRING 'WLXXDU11(IDKOLLI  =' W-4302-IDKOLLI-X ')'                    
224000            DELIMITED BY SIZE INTO SSA1                                   
224100     MOVE '  GE' TO GODK-STATUSKODER                                      
224200     CALL CBLTDLI USING GNP XXDU-PCB DLI-IO-AREA4 SSA1                    
224300     MOVE XXDU-STATUS-CODE TO STATUS-WS                                   
224400     PERFORM IMS-STATUSKONTROLL                                           
224500     .                                                                    
224600 IMS-GNP-XXDU11-IDKOLLI         SECTION.                                  
224700*    LÄS MED SAMMA IDKOLLI OCH NÄSTA PLOCKLISTA                           
224800*                                                                         
224900     STRING 'WLXXDU11(IDKOLLI  =' W-4302-IDKOLLI-X ')'                    
225000            DELIMITED BY SIZE INTO SSA1                                   
225100     MOVE '  GE' TO GODK-STATUSKODER                                      
225200     CALL CBLTDLI USING GNP XXDU-PCB DLI-IO-AREA4 SSA1                    
225300     MOVE XXDU-STATUS-CODE TO STATUS-WS                                   
225400     PERFORM IMS-STATUSKONTROLL                                           
225500     .                                                                    
225600 IMS-GNP-XXDU11-IDPLKLST-XXDU2   SECTION.                                 
225700*    LÄS NÄSTA 4302 MED SAMMA PLOCKLISTA                                  
225800*                                                                         
225900     STRING 'WLXXDU11(IDPLKLST =' W-4302-IDPLKLST-X ')'                   
226000            DELIMITED BY SIZE INTO SSA1                                   
226100     MOVE '  GE' TO GODK-STATUSKODER                                      
226200     CALL CBLTDLI USING GNP XXDU2-PCB DLI-IO-AREA4 SSA1                   
226300     MOVE XXDU2-STATUS-CODE TO STATUS-WS                                  
226400     PERFORM IMS-STATUSKONTROLL                                           
226500     .                                                                    
226600 IMS-GHNP-XXDU11-KVAL    SECTION.                                         
226700                                                                          
226800     STRING 'WLXXDU11(WDGXKEY  =' W-4302-WDGXKEY-X ')'                    
226900            DELIMITED BY SIZE INTO SSA1                                   
227000     MOVE '  GE' TO GODK-STATUSKODER                                      
227100     CALL CBLTDLI USING GHNP XXDU-PCB DLI-IO-AREA4 SSA1                   
227200     MOVE XXDU-STATUS-CODE TO STATUS-WS                                   
227300     PERFORM IMS-STATUSKONTROLL                                           
227400     .                                                                    
227500 IMS-GNP-XXDU11-FIRST SECTION.                                            
227600                                                                          
227700     MOVE 'WLXXDU11*F '  TO SSA1                                          
227800     MOVE '  GE' TO GODK-STATUSKODER                                      
227900     CALL CBLTDLI USING GNP XXDU-PCB DLI-IO-AREA4 SSA1                    
228000     MOVE XXDU-STATUS-CODE TO STATUS-WS                                   
228100     PERFORM IMS-STATUSKONTROLL                                           
228200     .                                                                    
228300                                                                          
228400 IMS-DLET-XXDU       SECTION.                                             
228500                                                                          
228600     MOVE '  ' TO GODK-STATUSKODER                                        
228700     CALL CBLTDLI USING DLET XXDU-PCB DLI-IO-AREA4                        
228800     MOVE XXDU-STATUS-CODE TO STATUS-WS                                   
228900     PERFORM IMS-STATUSKONTROLL                                           
229000     .                                                                    
229100     EJECT                                                                
229200 IMS-GU-WDB601    SECTION.                                                
229300                                                                          
229400     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
229500     DELIMITED BY SIZE INTO SSA1                                          
229600     MOVE '  GE' TO GODK-STATUSKODER                                      
229700     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
229800     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
229900     PERFORM IMS-STATUSKONTROLL                                           
230000     IF SEGMENT-SAKNAS                                                    
230100        MOVE SPACE TO DCS-KDDC                                            
230200     END-IF                                                               
230300     .                                                                    
230400     EJECT                                                                
230500 IMS-STATUSKONTROLL SECTION.                                              
230600     SET STATUS-IX TO 1                                                   
230700     SEARCH GODK-STATUS AT END CALL FELLOG                                
230800       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
230900     END-SEARCH                                                           
231000     .                                                                    
