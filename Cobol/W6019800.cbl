001000 ID DIVISION.                                                             
001100     SKIP2                                                                
001200 PROGRAM-ID.     W6019800.                                                
001300*AUTHOR.         KATARINA KYMMER.                                         
001400*DATE-WRITTEN.   92/06/09.                                                
001500                                                                          
001600*    REMARKS.                                                             
001700*                                                                         
001900*        FUNKTION:                                                        
002000*        BAKGRUNDS-MPP SOM SKRIVER UT LISTA FÖR VAGNEN.                   
002100*        PROGRAMMET STARTAS UPP FRÅN BILD 6131 MED PF4.                   
002110*                                                                         
002300*        PROGRAMMET LÄSER     W6INLA (W6D1)                               
002301*        PROGRAMMET LÄSER     W6G110                                      
002310*                                                                         
002400*        INDATA.                                                          
002500*        TRANSAKTION: W6T198                                              
002600*        MID:         W6I13101                                            
002610*                                                                         
002700*        UTDATA.                                                          
002800*        MOD:         W6O19801                                            
002900*                                                                         
003010*        PROGRAMMET LÄSER      W6INLA (W6D1)                              
003100*                                                                         
003900     SKIP3                                                                
004000 ENVIRONMENT DIVISION.                                                    
004100     EJECT                                                                
004200 DATA DIVISION.                                                           
004300 WORKING-STORAGE SECTION.                                                 
004301                                                                          
004310*    -- CHECKED BY WY2000                                                 
004400 77  IDPGM                       PIC X(08)   VALUE 'W6019800'.            
004500                                                                          
004600*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
004700 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
004800                                                                          
004900 77  JA                          PIC X       VALUE 'J'.                   
005000 77  NEJ                         PIC X       VALUE 'N'.                   
005100                                                                          
005300 77  IX                          PIC S9(4)  VALUE +0    COMP SYNC.        
005301 77  INDX                        PIC S9(4)  VALUE +0    COMP SYNC.        
005302 77  KVAVIS-IX                   PIC S9(4)  VALUE +0    COMP SYNC.        
005303 77  SPRAK-IX                    PIC S9(9)  VALUE +0    COMP SYNC.        
005310 77  RAD-IX                      PIC S9(3)  VALUE +34   COMP-3.           
005320 77  MAX-RADER                   PIC S9(3)  VALUE +33   COMP-3.           
005330 77  IDSID-RAKN                  PIC S9(3)  VALUE ZERO.                   
005600 77  MAX-MOD-LAENGD              PIC S9(4)  VALUE +155 COMP SYNC.         
005601                                                                          
005605 77  NYA-NYCKLAR                 PIC X       VALUE 'N'.                   
005606 77  FOERSTA-RADEN               PIC X       VALUE 'N'.                   
005607 77  FLER-21RADER                PIC X       VALUE 'N'.                   
005608 77  WS-IDINLVGN                 PIC X(3)    VALUE SPACE.                 
005609 77  WS-IDDC                     PIC X(2)    VALUE SPACE.                 
005610 77  W-IDLOPNRM                  PIC S9(9)   VALUE ZERO COMP-3.           
005611 77  W-IDRADNR                   PIC S9(5)    COMP-3.                     
005620 77  SPAR-IDLEVNR                PIC  X(5)   VALUE SPACE.                 
005622 77  WS-PRIM                     PIC 9(6).                                
005623 77  W-PRIM                      PIC X(6).                                
005624 77  WS-SEK                      PIC 9(6).                                
005625 77  W-SEK                       PIC X(6).                                
005630 77  W-KVAVIS                    PIC 9(6).                                
005640 77  W-KVAVIS-KIT                PIC 9(6).                                
005650 77  NYTT-IDTRPNR                PIC X(1)    VALUE 'J'.                   
005700                                                                          
005800*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
006000                                                                          
006310 77  PRINTER-OK                  PIC X       VALUE 'J'.                   
006320 77  UTSKRIFT-OK                 PIC X       VALUE 'N'.                   
006330 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
006400     88  NYCKLAR-OK                          VALUE 'J'.                   
006500     88  NYCKLAR-FEL                         VALUE 'N'.                   
006510 77  DAGENS-DATUM                PIC X(6)    VALUE SPACE.                 
006600                                                                          
006700 77  ALLT-SW                     PIC X       VALUE 'J'.                   
006800     88  ALLT-OK                             VALUE 'J'.                   
006900                                                                          
007000 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
007100     88  EGEN-MID                            VALUE '6198'.                
007200     88  GODK-MID                            VALUE '6191' '6192'          
007300                                                   '6193' '6194'          
007400                                                   '6195' '6196'          
007500                                                   '6197' '6198'          
007600                                                   '6199'.                
007700     88  HELP-MID                            VALUE '0551'.                
007800     EJECT                                                                
007840                                                                          
007900*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
008000 01  GENERELLA-SUBPROGRAM.                                                
008100     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
008200     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
008300     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
008400     03  W006PRS1                PIC X(8)    VALUE 'W006PRS1'.            
008410     03  W611ADR                 PIC X(8)    VALUE 'W611ADR'.             
008420     03  W006PRT                 PIC X(8)    VALUE 'W006PRT '.            
008500     EJECT                                                                
008600*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
008700*01 -COPY WMEDAREA                                                        
008800     SKIP3                                                                
008810*    ---AREA FÖR SUBPGM W006PRS1                                          
008820 01 FILLER                      PIC X(16)   VALUE 'W006PRS1'.             
008830                                                                          
008840*01 -COPY W006PRAR                                                        
008850                                                                          
008860 01 WS-PRINTER-PARM.                                                      
008870     03 WS-LIST-PRINTER          PIC X(8).                                
008880     03 WS-RAD.                                                           
008890       05 WS-FILLER              PIC X(1).                                
008891       05 WS-LISTRAD             PIC X(120).                              
008892     03 WS-DUMMY                 PIC X(1).                                
008894                                                                          
008900 01  MESSAGE-CODES.                                                       
009201     03  INF-FIRST-PAGE          PIC X(3)    VALUE '006'.                 
009202     03  INF-LAST-PAGE           PIC X(3)    VALUE '106'.                 
009203     03  INF-MORE-INFO-EXISTS    PIC X(3)    VALUE '105'.                 
009204     03  INF-TRYCK-PF11          PIC X(3)    VALUE '003'.                 
009205     03  INF-UDAT-UTFORD         PIC X(3)    VALUE '101'.                 
009206     03  INF-PRINT-START         PIC X(3)    VALUE '202'.                 
009207     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
009208     03  ERR-OTILL-UPPD          PIC X(3)    VALUE '007'.                 
009209     03  ERR-FINNS-EJ            PIC X(3)    VALUE '010'.                 
009210     03  ERR-PF11-INGENDATA      PIC X(3)    VALUE '011'.                 
009211     03  ERR-FEL-NYCKEL          PIC X(3)    VALUE '401'.                 
009220     03  ERR-KORR-UPPL           PIC X(3)    VALUE '001'.                 
009301     03  ERR-FEL-PRINTER         PIC X(3)    VALUE '772'.                 
009302     EJECT                                                                
009307*01  -COPY W006PRT                                                        
009308     EJECT                                                                
009320*01  -COPY W611ADR                                                        
009400     EJECT                                                                
009500*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
009600*                                                                         
009700 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
009800     SKIP3                                                                
009900*01  MID -COPY W6I13101                                                   
010000     EJECT                                                                
010100 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
010200     SKIP3                                                                
010300*01  -COPY WMSGAREA                                                       
010400     EJECT                                                                
010500     03  MOD REDEFINES MSG-AREA.                                          
010600*      05  -COPY W6O19801                                                 
010700     EJECT                                                                
010800 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
010900     SKIP3                                                                
011000*01  -COPY WMFSAREA                                                       
011100     EJECT                                                                
011200*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
011300*                                                                         
011400     EJECT                                                                
011500 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
011600     SKIP3                                                                
011700 01  NYCKLAR-TILL-DLI.                                                    
011809                                                                          
011810     03  MIN-W6D1F1KY-X.                                                  
011812         05  MIN-IDINLVGN          PIC 9(3).                              
011813         05  FILLER                PIC X(25)    VALUE LOW-VALUE.          
011814                                                                          
011815     03  MAX-W6D1F1KY-X.                                                  
011817         05  MAX-IDINLVGN          PIC 9(3).                              
011818         05  FILLER                PIC X(25)    VALUE HIGH-VALUE.         
011831                                                                          
011880     03  W-IDDC-X.                                                        
011890         05  W-IDDC              PIC X(2).                                
011891                                                                          
011892     03  W-IDINLVGN-X.                                                    
011893         05  W-VAGN              PIC 9(3).                                
011894                                                                          
011895     03  W-W6D101KY-X.                                                    
011896         05 W-IDDC-101KY         PIC  X(2).                               
011897         05 W-IDLEVNR            PIC  X(5).                               
011898         05 W-IDFS               PIC X(8).                                
011899         05 W-TIAVIDAT           PIC S9(7)    COMP-3.                     
011900                                                                          
011901     03  W-IDRADNR-INL-X.                                                 
011902         05  W-IDRADNR-INL       PIC S9(5)    COMP-3.                     
011903                                                                          
011904     03  W-W6GXKEY-6017-X.                                                
011905         05  W-6017-IDHTYP       PIC X(4)    VALUE '6017'.                
011906         05  FILLER              PIC X(26)   VALUE LOW-VALUE.             
011907*                                                                         
011908     03  W-W6GXKEY-6018-X.                                                
011909         05  W-6018-KDSEGKEY     PIC X(1)    VALUE '1'.                   
011910                                                                          
011920     SKIP2                                                                
012000*    --- STATUS-KOD FRÅN IMS                                              
012100 01  STATUS-WS                   PIC XX.                                  
012200     88  SEGMENT-FINNS                       VALUE '  '.                  
012300     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
012400     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
012410     88  SEGMENT-SLUT                        VALUE 'GB'.                  
012500     SKIP2                                                                
012600 01  GODK-STATUSKODER.                                                    
012700     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
012800     SKIP3                                                                
012900 01  SSA1                        PIC X(300).                              
013000 01  SSA2                        PIC X(300).                              
013100     EJECT                                                                
013110***********************************************                           
013120*  PRINTRADER                                 *                           
013130***********************************************                           
013140                                                                          
013150                                                                          
013160 01  RAD1.                                                                
013170     03 FILLER          PIC X(4)  VALUE 'VCCS'.                           
013180     03 FILLER          PIC X(10) VALUE SPACE.                            
013181     03 FILLER          PIC X(8)  VALUE 'TRP-ID: '.                       
013182     03 RAD1-IDTRPTNR   PIC 9(5).                                         
013183     03 FILLER          PIC X(14) VALUE SPACE.                            
013190     03 FILLER          PIC X(10) VALUE 'W60198-001'.                     
013191     03 FILLER          PIC X(10) VALUE SPACE.                            
013192     03 FILLER          PIC X(10) VALUE 'VAGNSLISTA'.                     
013193     03 FILLER          PIC X(19) VALUE SPACE.                            
013194     03 FILLER          PIC X(5)  VALUE 'DATE '.                          
013195     03 RAD1-DATUM      PIC X(6).                                         
013196     03 FILLER          PIC X(8)  VALUE '   SID  '.                       
013197     03 RAD1-IDSID           PIC ZZ9.                                     
013198                                                                          
013199 01  RAD1-ENG.                                                            
013200     03 FILLER          PIC X(4)  VALUE 'VCCS'.                           
013201     03 FILLER          PIC X(10) VALUE SPACE.                            
013202     03 FILLER          PIC X(8)  VALUE 'TRP-ID: '.                       
013203     03 RAD1-IDTRPTNR-ENG PIC 9(5).                                       
013204     03 FILLER          PIC X(14) VALUE SPACE.                            
013205     03 FILLER          PIC X(10) VALUE 'W60198-001'.                     
013206     03 FILLER          PIC X(10) VALUE SPACE.                            
013207     03 FILLER          PIC X(12) VALUE 'TROLLEY LIST'.                   
013208     03 FILLER          PIC X(17) VALUE SPACE.                            
013209     03 FILLER          PIC X(5)  VALUE 'DATE '.                          
013210     03 RAD1-DATUM-ENG  PIC X(6).                                         
013211     03 FILLER          PIC X(8)  VALUE '  PAGE  '.                       
013212     03 RAD1-IDSID-ENG       PIC ZZ9.                                     
013213                                                                          
013214 01  RAD2.                                                                
013215     03 FILLER               PIC X(121) VALUE SPACE.                      
013216                                                                          
013217 01  RAD3.                                                                
013218     03 FILLER               PIC X(121) VALUE SPACE.                      
013219                                                                          
013220 01  RAD4.                                                                
013221     03 FILLER               PIC X(7)   VALUE ' VAGN  '.                  
013222     03 RAD4-IDINLVGN        PIC X(3).                                    
013223     03 FILLER               PIC X(11)  VALUE '     PLAC  '.              
013224     03 RAD4-ADINLOMR        PIC X(4).                                    
013225                                                                          
013226 01  RAD4-ENG.                                                            
013227     03 FILLER               PIC X(9)   VALUE ' TROLLEY '.                
013228     03 RAD4-IDINLVGN-ENG    PIC X(3).                                    
013229     03 FILLER               PIC X(09)  VALUE '    LOC  '.                
013230     03 RAD4-ADINLOMR-ENG    PIC X(4).                                    
013231                                                                          
013232 01  RAD5.                                                                
013233     03 FILLER               PIC X(121) VALUE SPACE.                      
013234                                                                          
013235 01  RAD6.                                                                
013236     03 FILLER               PIC X(121) VALUE SPACE.                      
013237                                                                          
013238 01  RAD7.                                                                
013239     03 FILLER               PIC X(4)   VALUE '    '.                     
013240     03 FILLER               PIC X(5)   VALUE 'ARTNR'.                    
013241     03 FILLER               PIC X(2)   VALUE '  '.                       
013242     03 FILLER               PIC X(5)   VALUE 'ANTAL'.                    
013243     03 FILLER               PIC X(1)   VALUE ' '.                        
013244     03 FILLER               PIC X(9)   VALUE 'BENÄMNING'.                
013245     03 FILLER               PIC X(3)   VALUE '   '.                      
013246     03 FILLER               PIC X(3)   VALUE '   '.                      
013247     03 FILLER               PIC X(5)   VALUE 'LEVNR'.                    
013248     03 FILLER               PIC X(2)   VALUE '  '.                       
013249     03 FILLER               PIC X(3)   VALUE '   '.                      
013250     03 FILLER               PIC X(7)   VALUE 'KOLLINR'.                  
013251     03 FILLER               PIC X(1)   VALUE ' '.                        
013252     03 FILLER               PIC X(8)   VALUE 'ANT PRIO'.                 
013253     03 FILLER               PIC X(1)   VALUE ' '.                        
013254     03 FILLER               PIC X(1)   VALUE 'P'.                        
013255     03 FILLER               PIC X(1)   VALUE ' '.                        
013256     03 FILLER               PIC X(2)   VALUE 'FT'.                       
013257     03 FILLER               PIC X(1)   VALUE ' '.                        
013258     03 FILLER               PIC X(4)   VALUE 'AKON'.                     
013259     03 FILLER               PIC X(1)   VALUE ' '.                        
013260     03 FILLER               PIC X(8)   VALUE 'ANT PRIM'.                 
013261     03 FILLER               PIC X(1)   VALUE ' '.                        
013262     03 FILLER               PIC X(7)   VALUE 'ANT SEK'.                  
013263     03 FILLER               PIC X(1)   VALUE ' '.                        
013264     03 FILLER               PIC X(6)   VALUE 'SATS  '.                   
013265     03 FILLER               PIC X(1)   VALUE ' '.                        
013266     03 FILLER               PIC X(4)   VALUE 'DEST'.                     
013267     03 FILLER               PIC X(1)   VALUE ' '.                        
013268     03 FILLER               PIC X(3)   VALUE 'EMB'.                      
013269     03 FILLER               PIC X(2)   VALUE '  '.                       
013270     03 FILLER               PIC X(2)   VALUE 'FG'.                       
013271     03 FILLER               PIC X(2)   VALUE '  '.                       
013272     03 FILLER               PIC X(9)   VALUE 'NÄSTA ADR'.                
013273                                                                          
013274 01  RAD7-ENG.                                                            
013275     03 FILLER               PIC X(3)   VALUE '   '.                      
013276     03 FILLER               PIC X(6)   VALUE 'PARTNO'.                   
013277     03 FILLER               PIC X(2)   VALUE '  '.                       
013278     03 FILLER               PIC X(5)   VALUE 'QTY  '.                    
013279     03 FILLER               PIC X(1)   VALUE ' '.                        
013280     03 FILLER               PIC X(11)  VALUE 'DESCRIPTION'.              
013281     03 FILLER               PIC X(1)   VALUE ' '.                        
013282     03 FILLER               PIC X(3)   VALUE '   '.                      
013283     03 FILLER               PIC X(5)   VALUE 'SUPPL'.                    
013284     03 FILLER               PIC X(2)   VALUE '  '.                       
013285     03 FILLER               PIC X(3)   VALUE '   '.                      
013286     03 FILLER               PIC X(7)   VALUE 'CASE NO'.                  
013287     03 FILLER               PIC X(1)   VALUE ' '.                        
013288     03 FILLER               PIC X(8)   VALUE 'QTY PRIO'.                 
013289     03 FILLER               PIC X(1)   VALUE ' '.                        
013290     03 FILLER               PIC X(1)   VALUE 'P'.                        
013291     03 FILLER               PIC X(1)   VALUE ' '.                        
013292     03 FILLER               PIC X(2)   VALUE 'PT'.                       
013293     03 FILLER               PIC X(1)   VALUE ' '.                        
013294     03 FILLER               PIC X(4)   VALUE 'QTYC'.                     
013295     03 FILLER               PIC X(1)   VALUE ' '.                        
013296     03 FILLER               PIC X(8)   VALUE 'QTY PRIM'.                 
013297     03 FILLER               PIC X(1)   VALUE ' '.                        
013298     03 FILLER               PIC X(7)   VALUE 'QTY SEK'.                  
013299     03 FILLER               PIC X(1)   VALUE ' '.                        
013300     03 FILLER               PIC X(6)   VALUE 'TO KIT'.                   
013301     03 FILLER               PIC X(1)   VALUE ' '.                        
013302     03 FILLER               PIC X(4)   VALUE 'DEST'.                     
013303     03 FILLER               PIC X(1)   VALUE ' '.                        
013304     03 FILLER               PIC X(3)   VALUE 'T.P'.                      
013305     03 FILLER               PIC X(2)   VALUE '  '.                       
013306     03 FILLER               PIC X(2)   VALUE 'PG'.                       
013307     03 FILLER               PIC X(2)   VALUE '  '.                       
013308     03 FILLER               PIC X(9)   VALUE 'NEXT ADDR'.                
013309                                                                          
013310 01  RAD8.                                                                
013311     03 RAD8-IDARTNR         PIC Z(8)9.                                   
013312     03 FILLER               PIC X(1)   VALUE SPACE.                      
013313     03 RAD8-KVINLART        PIC Z(5)9.                                   
013314     03 FILLER               PIC X(1)   VALUE SPACE.                      
013315     03 RAD8-BEART           PIC X(14).                                   
013316     03 FILLER               PIC X(1)   VALUE SPACE.                      
013317     03 RAD8-IDLEVNR-KOLLI   PIC X(5)   VALUE SPACE.                      
013318     03 FILLER               PIC X(3)   VALUE SPACE.                      
013319     03 RAD8-IDOKOLLI        PIC Z(8)9.                                   
013320     03 FILLER               PIC X(3)   VALUE SPACE.                      
013321     03 RAD8-KVAVIS-PRIO     PIC Z(5)9.                                   
013322     03 FILLER               PIC X(1)   VALUE SPACE.                      
013323     03 RAD8-KDINLPRIO       PIC X(1).                                    
013324     03 FILLER               PIC X(1)   VALUE SPACE.                      
013325     03 RAD8-BEFT            PIC 9(2).                                    
013326     03 FILLER               PIC X(3)   VALUE SPACE.                      
013327     03 RAD8-FLKVAANT-TOT    PIC X(1).                                    
013328     03 FILLER               PIC X(4)   VALUE SPACE.                      
013329     03 RAD8-KVKVAPRIM       PIC Z(5)9.                                   
013330     03 FILLER               PIC X(2)   VALUE SPACE.                      
013331     03 RAD8-KVKVASEK        PIC Z(5)9.                                   
013332     03 FILLER               PIC X(1)   VALUE SPACE.                      
013333     03 RAD8-KVAVIS-KIT      PIC Z(5)9.                                   
013334     03 FILLER               PIC X(1)   VALUE SPACE.                      
013335     03 RAD8-ADTRDEST-KIT    PIC X(3).                                    
013336     03 FILLER               PIC X(1)   VALUE SPACE.                      
013337     03 RAD8-KDLAGEMB        PIC X(4).                                    
013338     03 FILLER               PIC X(1)   VALUE SPACE.                      
013339     03 RAD8-KDFARLIG        PIC X(3).                                    
013340     03 FILLER               PIC X(1)   VALUE SPACE.                      
013341     03 RAD8-ADINLOMR-NXT1   PIC X(4).                                    
013342     03 FILLER               PIC X(1)   VALUE SPACE.                      
013343     03 RAD8-ADINLOMR-NXT2   PIC X(4).                                    
013344     03 FILLER               PIC X(1)   VALUE SPACE.                      
013345     03 RAD8-ADINLOMR-NXT3   PIC X(4).                                    
013346                                                                          
013347 01  RAD9.                                                                
013348     03 FILLER               PIC X(10)  VALUE SPACE.                      
013349     03 RAD9-KVINLART        PIC Z(5)9.                                   
013350     03 FILLER               PIC X(16) VALUE SPACE.                       
013351     03 RAD9-IDLEVNR-KOLLI   PIC X(5)   VALUE SPACE.                      
013352     03 FILLER               PIC X(3)   VALUE SPACE.                      
013353     03 RAD9-IDOKOLLI        PIC Z(8)9.                                   
013354     03 FILLER               PIC X(10)  VALUE SPACE.                      
013355     03 RAD9-KDINLPRIO       PIC X(1).                                    
013356     03 FILLER               PIC X(71) VALUE SPACE.                       
013357                                                                          
013360*    --- IMS FUNKTIONSKODER                                               
013400*01  -COPY W0003                                                          
013500     EJECT                                                                
013600*    ---  DLI INPUT-OUTPUT AREA                                           
013700 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
013800     SKIP3                                                                
013900 01  DLI-IO-AREA.                                                         
014000     03  IO-AREA                 PIC X(150)  VALUE SPACE.                 
014101     SKIP3                                                                
014108 01  DLI-IO-AREA-INL01.                                                   
014109     03  W6INLA01.                                                        
014110*        05  -COPY W6D101                                                 
014111     SKIP3                                                                
014112 01  DLI-IO-AREA-INL11.                                                   
014113     03  W6INLA11.                                                        
014114*        05  -COPY W6D111                                                 
014115     SKIP3                                                                
014116 01  DLI-IO-AREA-INL21.                                                   
014117     03  W6INLA21.                                                        
014120*        05  -COPY W6D121                                                 
014121     SKIP3                                                                
014122 01  DLI-IO-AREA-INLG.                                                    
014130     03  W6INLG01.                                                        
014140*        05  -COPY W6D1F1                                                 
014400     EJECT                                                                
014410 01  FILLER                      PIC X(16)  VALUE 'DLI-IO-W6G1'.          
014420 01  DLI-IO-W6G1.                                                         
014430     03  IO-W6G1                PIC X(83)  VALUE SPACE.                   
014440     03  W6G110 REDEFINES IO-W6G1.                                        
014450*        05  -COPY W6GX6018                                               
014460     EJECT                                                                
014500 LINKAGE SECTION.                                                         
014600                                                                          
014700*01  -COPY W0009   -PRE MSG-                                              
014801     EJECT                                                                
014802*01  -COPY W0009   -PRE ALT-                                              
014803     EJECT                                                                
014807*01  -COPY W0008  -PRE INLG-                                              
014810     05  FILLER                  PIC X.                                   
014900     EJECT                                                                
015000*01  -COPY W0008  -PRE INLA-                                              
015001     05  FILLER                  PIC X.                                   
015002     EJECT                                                                
015003*01  -COPY W0008  -PRE W6G1-                                              
015004     05  FILLER                  PIC X.                                   
015005     EJECT                                                                
015006*01  -COPY W0008  -PRE ADR-INLA-                                          
015007     05  FILLER                  PIC X.                                   
015008     EJECT                                                                
015009*01  -COPY W0008  -PRE ADR-INLC-                                          
015010     05  FILLER                  PIC X.                                   
015011     EJECT                                                                
015012*01  -COPY W0008  -PRE ADR-PLAA-                                          
015013     05  FILLER                  PIC X.                                   
015014     EJECT                                                                
015015*01  -COPY W0008  -PRE ADR-WDK6-                                          
015016     05  FILLER                  PIC X.                                   
015017     EJECT                                                                
015018*01  -COPY W0008  -PRE ADR-STYR-HANA-                                     
015019     05  FILLER                  PIC X.                                   
015020     EJECT                                                                
015021*01  -COPY W0008  -PRE ADR-STYR-PLAA-                                     
015022     05  FILLER                  PIC X.                                   
015023     EJECT                                                                
015024 PROCEDURE DIVISION  USING MSG-PCB                                        
015025                           ALT-PCB                                        
015026                           INLG-PCB                                       
015027                           INLA-PCB                                       
015028                           W6G1-PCB                                       
015029                           ADR-INLA-PCB                                   
015030                           ADR-INLC-PCB                                   
015031                           ADR-PLAA-PCB                                   
015032                           ADR-WDK6-PCB                                   
015033                           ADR-STYR-HANA-PCB                              
015034                           ADR-STYR-PLAA-PCB.                             
015035     ENTRY 'DLITCBL' USING MSG-PCB                                        
015040                           ALT-PCB                                        
015050                           INLG-PCB                                       
015060                           INLA-PCB                                       
015070                           W6G1-PCB                                       
015080                           ADR-INLA-PCB                                   
015090                           ADR-INLC-PCB                                   
015091                           ADR-PLAA-PCB                                   
015092                           ADR-WDK6-PCB                                   
015093                           ADR-STYR-HANA-PCB                              
015094                           ADR-STYR-PLAA-PCB.                             
015220                                                                          
015300     PERFORM IMS-GET-MSG                                                  
015400     IF SEGMENT-FINNS                                                     
015500       PERFORM A-INIT                                                     
015600       PERFORM B-KOLLA-NYCKLAR                                            
015700       IF NYCKLAR-OK                                                      
016100         PERFORM C-KOLLA-PRINTER                                          
016200         IF PRINTER-OK = JA                                               
016300           PERFORM D-UTSKRIFT                                             
016310         END-IF                                                           
016320       END-IF                                                             
016500       MOVE MAX-MOD-LAENGD TO MSG-KVLL                                    
016600       PERFORM IMS-INSERT-MSG                                             
016700     END-IF                                                               
016900                                                                          
017000     MOVE ZERO TO RETURN-CODE                                             
017100     GOBACK                                                               
017200     .                                                                    
017300     EJECT                                                                
017400 A-INIT SECTION.                                                          
017500                                                                          
017510     ACCEPT DAGENS-DATUM FROM DATE                                        
017520                                                                          
017600     IF MSG-DUBBLA-TRANSKODER                                             
017700       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W6I13101                 
017800       MOVE MSG-IDTRANS-2 TO MFS-IDTRANS                                  
017900       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
018000     ELSE                                                                 
018100       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W6I13101                  
018200       MOVE MSG-IDTRANS-1 TO MFS-IDTRANS                                  
018300       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
018400     END-IF                                                               
018500                                                                          
018600     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
018700     MOVE MSG-IDPFK TO MFS-IDPFK                                          
018800     MOVE MFS-IDTRANS TO W-IDTRANS                                        
018900                                                                          
019000     MOVE LOW-VALUE TO MSG-AREA                                           
019100     MOVE 'W6O13102' TO MFS-IDMOD                                         
019200     MOVE '6131' TO MOD-IDTRANS                                           
019300     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                    
019400                                                                          
019500     IF EGEN-MID OR HELP-MID                                              
019600       CONTINUE                                                           
019700     ELSE                                                                 
019800       MOVE SPACE TO MFS-KDTRTYP                                          
019900       MOVE '7' TO MFS-IDPFK                                              
020000     END-IF                                                               
020100                                                                          
020200     IF ENGLISH-TEXT                                                      
020300       MOVE +2 TO SPRAK-IX                                                
020400       MOVE 'GB ' TO MED-IDSKYLT                                          
020500     ELSE                                                                 
020600       MOVE +1 TO SPRAK-IX                                                
020700       MOVE 'S  ' TO MED-IDSKYLT                                          
020800     END-IF                                                               
021100     .                                                                    
021200     EJECT                                                                
021300 B-KOLLA-NYCKLAR SECTION.                                                 
021400                                                                          
021410     MOVE MFS-RENSA-FAELT TO MOD-IDINLVGN-IN                              
021411                             MOD-IDDC-IN                                  
021420                                                                          
021430     IF MID-IDINLVGN-IN = ALL '+'                                         
021440       MOVE MID-IDINLVGN-UT TO WS-IDINLVGN                                
021450       INSPECT WS-IDINLVGN REPLACING LEADING SPACE BY ZERO                
021460     ELSE                                                                 
021491       MOVE NEJ             TO NYCKLAR-SW                                 
021492     END-IF                                                               
021493     MOVE MFS-ROER-EJ-FAELT TO MOD-ADINLOMR-PRT                           
021494                                                                          
021495     IF MID-IDDC-IN     = ALL '+'                                         
021496       MOVE MID-IDDC-UT     TO WS-IDDC                                    
021497       INSPECT WS-IDDC     REPLACING LEADING SPACE BY ZERO                
021498     ELSE                                                                 
021499       MOVE NEJ             TO NYCKLAR-SW                                 
021500     END-IF                                                               
021520                                                                          
021600                                                                          
021900     IF NYCKLAR-FEL                                                       
022000       MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                                 
022100       CALL WMEDKONV USING MED-WMEDAREA                                   
022200       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
022210       MOVE MFS-ROER-EJ-FAELT     TO MOD-IDINLVGN-IN                      
022211                                     MOD-IDDC-IN                          
022220                                     MOD-ADINLOMR-PRT                     
022230       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDINLVGN-IN-ATTR                 
022240                                     MOD-IDDC-IN-ATTR                     
022250                                     MOD-ADINLOMR-PRT-ATTR                
022500     END-IF                                                               
022600     .                                                                    
022800     EJECT                                                                
022900                                                                          
023000 C-KOLLA-PRINTER SECTION.                                                 
023001                                                                          
023004     MOVE 001             TO PRT-KDCALL                                   
023005     MOVE '6M'             TO PRT-IDPRTLST(1:2)                           
023006     MOVE MID-ADINLOMR-PRT TO PRT-IDPRTLST(3:6)                           
023008     CALL W006PRT USING PRT-W006PRT                                       
023009     IF PRT-KDSVAR = 'F'                                                  
023010       MOVE MFS-ALFA-FAELT-FEL TO MOD-ADINLOMR-PRT-ATTR                   
023013       MOVE NEJ TO PRINTER-OK                                             
023014       MOVE ERR-FEL-PRINTER TO MED-IDMFSFEL                               
023015       CALL WMEDKONV USING MED-WMEDAREA                                   
023016       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
023017       MOVE MFS-ROER-EJ-FAELT     TO MOD-IDINLVGN-IN                      
023018                                     MOD-IDDC-IN                          
023019                                     MOD-ADINLOMR-PRT                     
023020       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDINLVGN-IN-ATTR                 
023021                                     MOD-IDDC-IN-ATTR                     
023023     ELSE                                                                 
023024       MOVE PRT-BEPRTLST TO MOD-TEMFSFEL                                  
023025       MOVE PRT-IDPRTLST TO WS-LIST-PRINTER                               
023026       MOVE MID-ADINLOMR-PRT TO MOD-ADINLOMR-PRT                          
023027       MOVE JA TO PRINTER-OK                                              
023028     END-IF                                                               
023095                                                                          
023100     .                                                                    
024300     EJECT                                                                
024400 D-UTSKRIFT SECTION.                                                      
025543     PERFORM S01-PRT-OPEN                                                 
025544                                                                          
025545     MOVE WS-IDINLVGN    TO MIN-IDINLVGN                                  
025546                            MAX-IDINLVGN                                  
025547                            W-VAGN                                        
025548     MOVE WS-IDDC        TO W-IDDC-101KY                                  
025550                            W-IDDC                                        
025551     PERFORM IMS-GET-INLG01                                               
025552     IF SEGMENT-FINNS                                                     
025553       MOVE SEQF-IDLEVNR   TO W-IDLEVNR                                   
025554                              SPAR-IDLEVNR                                
025555       MOVE SEQF-IDFS      TO W-IDFS                                      
025556       MOVE SEQF-TIAVIDAT  TO W-TIAVIDAT                                  
025557       MOVE SEQF-IDRADNR-INL TO W-IDRADNR-INL                             
025559       PERFORM IMS-GU-W6INLA11                                            
025560                                                                          
025561                                                                          
025562       PERFORM UNTIL SEGMENT-SAKNAS OR                                    
025563                     SEGMENT-SLUT                                         
025568         IF RAD-IX > 33                                                   
025569           PERFORM DA-SKRIV-RAD1-7                                        
025570           ADD  +1 TO IDSID-RAKN                                          
025571           MOVE +0 TO RAD-IX                                              
025572         ELSE                                                             
025574           IF ART-IDLOPNRM NOT = W-IDLOPNRM OR FLER-21RADER = JA          
025577             PERFORM DB-SKRIV-RAD-8                                       
025578           END-IF                                                         
025579             IF FLER-21RADER = NEJ                                        
025580               PERFORM IMS-GN-INLG01                                      
025581               IF SEGMENT-FINNS                                           
025582                 MOVE SEQF-IDLEVNR   TO W-IDLEVNR                         
025583                                        SPAR-IDLEVNR                      
025584                 MOVE SEQF-IDFS      TO W-IDFS                            
025585                 MOVE SEQF-TIAVIDAT  TO W-TIAVIDAT                        
025586                 MOVE SEQF-IDRADNR-INL TO W-IDRADNR-INL                   
025587                 PERFORM IMS-GU-W6INLA11                                  
025588               END-IF                                                     
025589             END-IF                                                       
025590         END-IF                                                           
025591       END-PERFORM                                                        
025592                                                                          
025597                                                                          
025598       MOVE INF-PRINT-START TO MED-IDMFSINF                               
025599       CALL WMEDKONV USING MED-WMEDAREA                                   
025600       MOVE MED-MFSINF TO MOD-TEMFSINF                                    
025601                                                                          
025603     ELSE                                                                 
025604        MOVE ERR-FINNS-EJ TO MED-IDMFSFEL                                 
025605        CALL WMEDKONV USING MED-WMEDAREA                                  
025606        MOVE MED-MFSFEL TO MOD-TEMFSFEL                                   
025607     END-IF                                                               
025608     PERFORM S03-PRT-CLOSE                                                
025609                                                                          
025610     .                                                                    
025611     EJECT                                                                
025612                                                                          
025613 DA-SKRIV-RAD1-7 SECTION.                                                 
025614     IF ART-IDLOPNRM = W-IDLOPNRM AND FLER-21RADER = JA                   
025615       PERFORM IMS-GNP-FIRST-W6INLA21                                     
025616       PERFORM UNTIL RAD-IDRADNR = W-IDRADNR  OR                          
025617                     SEGMENT-SAKNAS           OR                          
025618                     SEGMENT-SLUT                                         
025619         PERFORM IMS-GNP-W6INLA21                                         
025620       END-PERFORM                                                        
025621     ELSE                                                                 
025622       PERFORM IMS-GNP-FIRST-W6INLA21                                     
025623     END-IF                                                               
025624                                                                          
025625     IF IDSID-RAKN = +0                                                   
025626        MOVE +1 TO IDSID-RAKN                                             
025627     END-IF                                                               
025628                                                                          
025629     MOVE DAGENS-DATUM     TO RAD1-DATUM                                  
025630                              RAD1-DATUM-ENG                              
025631     MOVE IDSID-RAKN       TO RAD1-IDSID                                  
025632                              RAD1-IDSID-ENG                              
025633                                                                          
025634     IF NYTT-IDTRPNR = JA                                                 
025635       PERFORM IMS-GHU-W6G110                                             
025636       ADD 1 TO 6018-IDTRPTNR-INLEV                                       
025637       IF 6018-IDTRPTNR-INLEV > 59999                                     
025638         MOVE 10000 TO 6018-IDTRPTNR-INLEV                                
025640       END-IF                                                             
025643       PERFORM IMS-REPL-W6G110                                            
025644     END-IF                                                               
025645                                                                          
025646     MOVE 6018-IDTRPTNR-INLEV TO RAD1-IDTRPTNR                            
025647                                 RAD1-IDTRPTNR-ENG                        
025648     MOVE NEJ TO NYTT-IDTRPNR                                             
025649                                                                          
025650     IF ENGLISH-TEXT                                                      
025651       MOVE RAD1-ENG         TO WS-LISTRAD                                
025652     ELSE                                                                 
025653       MOVE RAD1             TO WS-LISTRAD                                
025654     END-IF                                                               
025655     MOVE PRT-NYSIDA-RAD4  TO PRT-RADSKIP                                 
025656     PERFORM S02-SKRIV-RAD                                                
025657                                                                          
025658     MOVE RAD2             TO WS-LISTRAD                                  
025659     MOVE PRT-AFTER-1      TO PRT-RADSKIP                                 
025660     PERFORM S02-SKRIV-RAD                                                
025661                                                                          
025662     MOVE RAD3             TO WS-LISTRAD                                  
025663     MOVE PRT-AFTER-1      TO PRT-RADSKIP                                 
025664     PERFORM S02-SKRIV-RAD                                                
025665                                                                          
025666     MOVE RAD-IDINLVGN     TO RAD4-IDINLVGN                               
025667                              RAD4-IDINLVGN-ENG                           
025668     MOVE RAD-ADINLOMR     TO RAD4-ADINLOMR                               
025669                              RAD4-ADINLOMR-ENG                           
025670     IF ENGLISH-TEXT                                                      
025671       MOVE RAD4-ENG         TO WS-LISTRAD                                
025672     ELSE                                                                 
025673       MOVE RAD4             TO WS-LISTRAD                                
025674     END-IF                                                               
025675     MOVE PRT-AFTER-1      TO PRT-RADSKIP                                 
025676     PERFORM S02-SKRIV-RAD                                                
025677                                                                          
025678     MOVE RAD5             TO WS-LISTRAD                                  
025679     MOVE PRT-AFTER-1      TO PRT-RADSKIP                                 
025680     PERFORM S02-SKRIV-RAD                                                
025681                                                                          
025682     MOVE RAD6             TO WS-LISTRAD                                  
025683     MOVE PRT-AFTER-1      TO PRT-RADSKIP                                 
025684     PERFORM S02-SKRIV-RAD                                                
025685                                                                          
025686     IF ENGLISH-TEXT                                                      
025687       MOVE RAD7-ENG         TO WS-LISTRAD                                
025688     ELSE                                                                 
025689       MOVE RAD7             TO WS-LISTRAD                                
025690     END-IF                                                               
025691     MOVE PRT-AFTER-1      TO PRT-RADSKIP                                 
025692     PERFORM S02-SKRIV-RAD                                                
025693     .                                                                    
025694     EJECT                                                                
025695                                                                          
025696 DB-SKRIV-RAD-8 SECTION.                                                  
025697                                                                          
025698     IF RAD-IX = +0                                                       
025699       ADD +1 TO RAD-IX                                                   
025700       MOVE JA TO FOERSTA-RADEN                                           
025701     ELSE                                                                 
025702       MOVE NEJ TO FOERSTA-RADEN                                          
025703     END-IF                                                               
025704                                                                          
025705     IF FLER-21RADER = NEJ                                                
025706        PERFORM DC-KVAVIS-KIT                                             
025707       PERFORM IMS-GNP-FIRST-W6INLA21                                     
025708     END-IF                                                               
025709                                                                          
025710                                                                          
025711     MOVE ART-IDARTNR     TO  RAD8-IDARTNR                                
025712     MOVE ART-KVAVIS-PRIO TO  RAD8-KVAVIS-PRIO                            
025713     MOVE ART-BEFT        TO  RAD8-BEFT                                   
025714     MOVE ART-BEART       TO  RAD8-BEART                                  
025715     MOVE ART-KDLAGEMB    TO  RAD8-KDLAGEMB                               
025716     MOVE ART-IDLOPNRM    TO  W-IDLOPNRM                                  
025717                                                                          
025718                                                                          
025719     MOVE ART-IDLOPNRM TO ADR-IDLOPNRM                                    
025720                                                                          
025721     CALL W611ADR USING ADR-W611ADR ADR-INLA-PCB ADR-INLC-PCB             
025722                        ADR-PLAA-PCB ADR-WDK6-PCB                         
025723                                     ADR-STYR-HANA-PCB                    
025724                                     ADR-STYR-PLAA-PCB                    
025725                                                                          
025726     MOVE ADR-ADINLOMR-NXT1 TO RAD8-ADINLOMR-NXT1                         
025727     MOVE ADR-ADINLOMR-NXT2 TO RAD8-ADINLOMR-NXT2                         
025728     MOVE ADR-ADINLOMR-NXT3 TO RAD8-ADINLOMR-NXT3                         
025729                                                                          
025730                                                                          
025731                                                                          
025732     IF ART-KDKVAANT > ZERO                                               
025733       MOVE 'J' TO  RAD8-FLKVAANT-TOT                                     
025734     ELSE                                                                 
025735       MOVE 'N' TO  RAD8-FLKVAANT-TOT                                     
025736     END-IF                                                               
025737                                                                          
025738                                                                          
025739     COMPUTE WS-PRIM = ART-KVKVAPRIM-BER - ART-KVKVAPRIM-VER              
025740     MOVE WS-PRIM TO W-PRIM                                               
025741     IF W-PRIM < ZERO                                                     
025742        MOVE ZERO TO RAD8-KVKVAPRIM                                       
025743     ELSE                                                                 
025744        MOVE WS-PRIM TO RAD8-KVKVAPRIM                                    
025745     END-IF                                                               
025746                                                                          
025747     COMPUTE WS-SEK = ART-KVKVASEK-BER - ART-KVKVASEK-VER                 
025748     MOVE WS-SEK TO W-SEK                                                 
025749     IF W-SEK < ZERO                                                      
025750        MOVE ZERO TO RAD8-KVKVASEK                                        
025751     ELSE                                                                 
025752        MOVE WS-SEK TO RAD8-KVKVASEK                                      
025753     END-IF                                                               
025754                                                                          
025755     EVALUATE TRUE                                                        
025756       WHEN ART-KDFARLIG = 4                                              
025757         IF ENGLISH-TEXT                                                  
025758           MOVE 'YES' TO RAD8-KDFARLIG                                    
025759         ELSE                                                             
025760           MOVE 'JA' TO RAD8-KDFARLIG                                     
025761         END-IF                                                           
025762       WHEN ART-KDFARLIG = 5                                              
025763         MOVE 'ASB' TO RAD8-KDFARLIG                                      
025764       WHEN ART-KDFARLIG = 6                                              
025765         IF ENGLISH-TEXT                                                  
025766           MOVE 'CHE' TO RAD8-KDFARLIG                                    
025767         ELSE                                                             
025768           MOVE 'KEM' TO RAD8-KDFARLIG                                    
025769         END-IF                                                           
025770       WHEN ART-KDFARLIG = 7                                              
025771         IF ENGLISH-TEXT                                                  
025772           MOVE 'YES' TO RAD8-KDFARLIG                                    
025773         ELSE                                                             
025774           MOVE 'JA' TO RAD8-KDFARLIG                                     
025775         END-IF                                                           
025776       WHEN OTHER                                                         
025777         IF ENGLISH-TEXT                                                  
025778           MOVE 'NO ' TO RAD8-KDFARLIG                                    
025779         ELSE                                                             
025780           MOVE 'NEJ' TO RAD8-KDFARLIG                                    
025781         END-IF                                                           
025782     END-EVALUATE                                                         
025783                                                                          
025784     IF RAD-FLPRIO = JA                                                   
025785       MOVE 'P' TO RAD8-KDINLPRIO                                         
025786     ELSE                                                                 
025787       MOVE ' ' TO RAD8-KDINLPRIO                                         
025788     END-IF                                                               
025789                                                                          
025790     IF RAD-IDOKOLLI > ZERO                                               
025791       MOVE RAD-IDOKOLLI TO RAD8-IDOKOLLI                                 
025792     ELSE                                                                 
025793       MOVE ZERO TO RAD8-IDOKOLLI                                         
025794     END-IF                                                               
025795                                                                          
025796     IF RAD-IDLEVNR-KOLLI NOT = SPACE                                     
025797        MOVE RAD-IDLEVNR-KOLLI TO RAD8-IDLEVNR-KOLLI                      
025798     ELSE                                                                 
025799        MOVE SPAR-IDLEVNR      TO RAD8-IDLEVNR-KOLLI                      
025800     END-IF                                                               
025801                                                                          
025802                                                                          
025803     MOVE RAD-KVINLART TO  RAD8-KVINLART                                  
025804                                                                          
025805     IF FOERSTA-RADEN = JA                                                
025806       MOVE PRT-AFTER-1    TO PRT-RADSKIP                                 
025807     ELSE                                                                 
025808       MOVE PRT-AFTER-3    TO PRT-RADSKIP                                 
025809       ADD +3              TO RAD-IX                                      
025810     END-IF                                                               
025811                                                                          
025820     MOVE RAD8             TO WS-LISTRAD                                  
025900     PERFORM S02-SKRIV-RAD                                                
037101                                                                          
037102     PERFORM IMS-GNP-W6INLA21                                             
037104       PERFORM UNTIL RAD-IX > 33 OR                                       
037105                     SEGMENT-SAKNAS OR                                    
037106                     SEGMENT-SLUT                                         
037107                                                                          
037108         IF RAD-IDOKOLLI > ZERO                                           
037109           MOVE RAD-IDOKOLLI TO RAD9-IDOKOLLI                             
037110         ELSE                                                             
037111           MOVE ZERO TO RAD9-IDOKOLLI                                     
037112         END-IF                                                           
037113         MOVE RAD-KVINLART TO  RAD9-KVINLART                              
037114                                                                          
037115         IF RAD-IDLEVNR-KOLLI NOT = SPACE                                 
037116            MOVE RAD-IDLEVNR-KOLLI TO RAD9-IDLEVNR-KOLLI                  
037117         ELSE                                                             
037118            MOVE SPAR-IDLEVNR      TO RAD9-IDLEVNR-KOLLI                  
037119         END-IF                                                           
037120                                                                          
037121         IF RAD-FLPRIO = JA                                               
037122           MOVE 'P' TO RAD9-KDINLPRIO                                     
037123         ELSE                                                             
037124           MOVE '  ' TO RAD9-KDINLPRIO                                    
037125         END-IF                                                           
037126                                                                          
037127                                                                          
037128         MOVE PRT-AFTER-1      TO PRT-RADSKIP                             
037129         ADD +1 TO RAD-IX                                                 
037130         MOVE RAD9         TO WS-LISTRAD                                  
037131         PERFORM S02-SKRIV-RAD                                            
037132                                                                          
037133         PERFORM IMS-GNP-W6INLA21                                         
037134       END-PERFORM                                                        
037135                                                                          
037136     IF SEGMENT-SAKNAS OR SEGMENT-SLUT                                    
037137       MOVE NEJ TO FLER-21RADER                                           
037138     ELSE                                                                 
037139       MOVE JA TO FLER-21RADER                                            
037140       MOVE RAD-IDRADNR TO W-IDRADNR                                      
037141     END-IF                                                               
037142                                                                          
037143     .                                                                    
037144     EJECT                                                                
037145 DC-KVAVIS-KIT SECTION.                                                   
037146     PERFORM IMS-GNP-W6INLA21                                             
037147     MOVE +0 TO W-KVAVIS                                                  
037148     PERFORM UNTIL SEGMENT-SAKNAS OR SEGMENT-SLUT                         
037149       IF RAD-FLSATS = 'J'                                                
037150          ADD RAD-KVINLART TO W-KVAVIS                                    
037151       END-IF                                                             
037152       PERFORM IMS-GNP-W6INLA21                                           
037153     END-PERFORM                                                          
037154     COMPUTE W-KVAVIS-KIT = ART-KVAVIS-KIT - W-KVAVIS                     
037155     MOVE W-KVAVIS-KIT     TO RAD8-KVAVIS-KIT                             
037156     MOVE ART-ADTRDEST-KIT TO RAD8-ADTRDEST-KIT                           
037157     .                                                                    
037158     EJECT                                                                
037169                                                                          
037172 S01-PRT-OPEN SECTION.                                                    
037173                                                                          
037174     CALL W006PRS1  USING PRT-SPOOL-OVR                                   
037175                          PRT-OPEN                                        
037176                          WS-LIST-PRINTER                                 
037177                          ALT-PCB                                         
037178                          WS-DUMMY                                        
037179                          WS-DUMMY                                        
037180     .                                                                    
037181     EJECT                                                                
037182 S02-SKRIV-RAD SECTION.                                                   
037183                                                                          
037184     CALL W006PRS1  USING PRT-SPOOL-OVR                                   
037185                          PRT-WRITE                                       
037186                          WS-LIST-PRINTER                                 
037187                          ALT-PCB                                         
037188                          PRT-RADSKIP                                     
037189                          WS-RAD                                          
037190     .                                                                    
037191     EJECT                                                                
037192 S03-PRT-CLOSE SECTION.                                                   
037193                                                                          
037194     CALL W006PRS1  USING PRT-SPOOL-OVR                                   
037195                          PRT-CLOSE                                       
037196                          WS-LIST-PRINTER                                 
037197                          ALT-PCB                                         
037198                          WS-DUMMY                                        
037199                          WS-DUMMY                                        
037200     .                                                                    
037201     EJECT                                                                
037202* --- IMS SEKTIONER ---                                                   
037203     SKIP3                                                                
037204 IMS-GET-MSG SECTION.                                                     
037205                                                                          
037206     MOVE '  QC' TO GODK-STATUSKODER                                      
037207     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
037208     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
037209     PERFORM IMS-STATUSKONTROLL                                           
037210     .                                                                    
037211     SKIP3                                                                
037212 IMS-INSERT-MSG SECTION.                                                  
037213                                                                          
037214     IF ENGLISH-TEXT                                                      
037215       MOVE 'N' TO MFS-KDHUVOMR                                           
037216     END-IF                                                               
037217     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
037218     MOVE SPACE TO GODK-STATUSKODER                                       
037219     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
037220     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
037221     PERFORM IMS-STATUSKONTROLL                                           
037222     .                                                                    
037223     EJECT                                                                
037224 IMS-GET-INLG01 SECTION.                                                  
037225     STRING 'W6INLG01(W6D1F1KY=>' MIN-W6D1F1KY-X                          
037226                    '&W6D1F1KY=<' MAX-W6D1F1KY-X                          
037227                    '&IDDC    = ' W-IDDC ')'                              
037228             DELIMITED BY SIZE INTO SSA1                                  
037229     MOVE '  GE' TO GODK-STATUSKODER                                      
037230     CALL CBLTDLI USING GU INLG-PCB DLI-IO-AREA-INLG SSA1                 
037231     MOVE INLG-STATUS-CODE TO STATUS-WS                                   
037232     PERFORM IMS-STATUSKONTROLL                                           
037233     .                                                                    
037234     EJECT                                                                
037235 IMS-GN-INLG01 SECTION.                                                   
037236     STRING 'W6INLG01(W6D1F1KY=>' MIN-W6D1F1KY-X                          
037237                    '&W6D1F1KY=<' MAX-W6D1F1KY-X                          
037238                    '&IDDC    = ' W-IDDC ')'                              
037239             DELIMITED BY SIZE INTO SSA1                                  
037240     MOVE '  GEGB' TO GODK-STATUSKODER                                    
037241     CALL CBLTDLI USING GN INLG-PCB DLI-IO-AREA-INLG SSA1                 
037242     MOVE INLG-STATUS-CODE TO STATUS-WS                                   
037243     PERFORM IMS-STATUSKONTROLL                                           
037244     .                                                                    
037245     EJECT                                                                
037246 IMS-GU-W6INLA11 SECTION.                                                 
037247     STRING 'W6INLA01(W6D101KY =' W-W6D101KY-X ')'                        
037248             DELIMITED BY SIZE INTO SSA1                                  
037249     STRING 'W6INLA11(IDRADNRI =' W-IDRADNR-INL-X ')'                     
037250             DELIMITED BY SIZE INTO SSA2                                  
037251     MOVE '  GE' TO GODK-STATUSKODER                                      
037252     CALL CBLTDLI USING GU INLA-PCB DLI-IO-AREA-INL11 SSA1 SSA2           
037253     MOVE INLA-STATUS-CODE TO STATUS-WS                                   
037254     PERFORM IMS-STATUSKONTROLL                                           
037255     .                                                                    
037256     EJECT                                                                
037257 IMS-GNP-W6INLA21 SECTION.                                                
037258     STRING 'W6INLA21(IDINLVGN =' W-IDINLVGN-X ')'                        
037259             DELIMITED BY SIZE INTO SSA1                                  
037260     MOVE '  GEGB' TO GODK-STATUSKODER                                    
037261     CALL CBLTDLI USING GNP INLA-PCB DLI-IO-AREA-INL21 SSA1               
037262     MOVE INLA-STATUS-CODE TO STATUS-WS                                   
037263     PERFORM IMS-STATUSKONTROLL                                           
037264     .                                                                    
037265     EJECT                                                                
037266 IMS-GNP-FIRST-W6INLA21 SECTION.                                          
037267     STRING 'W6INLA21*F(IDINLVGN =' W-IDINLVGN-X ')'                      
037268             DELIMITED BY SIZE INTO SSA1                                  
037269     MOVE '  GEGB' TO GODK-STATUSKODER                                    
037270     CALL CBLTDLI USING GNP INLA-PCB DLI-IO-AREA-INL21 SSA1               
037271     MOVE INLA-STATUS-CODE TO STATUS-WS                                   
037272     PERFORM IMS-STATUSKONTROLL                                           
037273     .                                                                    
037274     EJECT                                                                
037275 IMS-GHU-W6G110 SECTION.                                                  
037276     STRING 'W6G101  (W6GXKEY  =' W-W6GXKEY-6017-X ')'                    
037277          DELIMITED BY SIZE INTO SSA1                                     
037278     STRING 'W6G110  (KDSEGKEY =' W-W6GXKEY-6018-X ')'                    
037279          DELIMITED BY SIZE INTO SSA2                                     
037280     MOVE '    ' TO GODK-STATUSKODER                                      
037281     CALL CBLTDLI USING GHU W6G1-PCB DLI-IO-W6G1 SSA1 SSA2                
037282     MOVE W6G1-STATUS-CODE TO STATUS-WS                                   
037283     PERFORM IMS-STATUSKONTROLL                                           
037284     .                                                                    
037285     SKIP3                                                                
037286 IMS-REPL-W6G110 SECTION.                                                 
037287     MOVE '    ' TO GODK-STATUSKODER                                      
037288     CALL CBLTDLI USING REPL W6G1-PCB DLI-IO-W6G1                         
037289     MOVE W6G1-STATUS-CODE TO STATUS-WS                                   
037290     PERFORM IMS-STATUSKONTROLL                                           
037291     .                                                                    
037292     SKIP3                                                                
037293 IMS-STATUSKONTROLL SECTION.                                              
037294                                                                          
037295     SET STATUS-IX TO 1                                                   
037296     SEARCH GODK-STATUS                                                   
037297       AT END                                                             
037298         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
037299         DELIMITED BY SIZE INTO FELTEXT                                   
037300         CALL FELLOG                                                      
037301       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
037302         CONTINUE                                                         
037310     END-SEARCH                                                           
037400     .                                                                    
