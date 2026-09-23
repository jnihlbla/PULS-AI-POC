000100 PROCESS DYNAM                                                            
000201 ID DIVISION.                                                             
000301 PROGRAM-ID.     W6030900.                                                
000401 AUTHOR.         BODIL LINDAHL/ TOMMIE JIVARP                             
000501 DATE-WRITTEN.   97/02/09.   /  TILLÄGG 98/03/26                          
000601 DATE-COMPILED.                                                           
000701                                                                          
000801*    FUNKTION:                                                            
000901*        NDC INLÄGGNING                                                   
001001*                                                                         
001101*        ÅTERFUNNA LOST KOLLIN                                            
001201*        WEB VERSIONEN AV PROGRAMMET HETER WL0109.                        
001301*                                                                         
001401*    INDATA.                                                              
001501*        TRANSAKTION: W6T309                                              
001601*        MID:         W6I30901                                            
001701*                                                                         
001801*    UTDATA.                                                              
001901*        MOD:         W6O30901                                            
002001                                                                          
002101     SKIP3                                                                
002201 ENVIRONMENT DIVISION.                                                    
002301     EJECT                                                                
002401 DATA DIVISION.                                                           
002501 WORKING-STORAGE SECTION.                                                 
002601                                                                          
002701                                                                          
002801                                                                          
002901*    -- CHECKED BY WY2000                                                 
003001 77  IDPGM                       PIC X(08)   VALUE 'W6030900'.            
003101                                                                          
003201*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
003301 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
003401 77  ERROR-TEXT                  PIC X(80) VALUE SPACE.                   
003501                                                                          
003601 77  JA                          PIC X       VALUE 'J'.                   
003701 77  NEJ                         PIC X       VALUE 'N'.                   
003801                                                                          
003901*01  -COPY WWDCKONS              PIC X       VALUE 'N'.                   
004001                                                                          
004101 77  REC-WS-IDDC                 PIC X(2)    VALUE SPACE.                 
004201 77  SEND-WS-IDDC                PIC X(2)    VALUE SPACE.                 
004301 77  WS-A03-SKAPAD               PIC X       VALUE 'N'.                   
004401 77  WS-UPPD-LOCB                PIC X       VALUE 'N'.                   
004501 77  IX                          PIC S9(3)   VALUE ZERO COMP-3.           
004601 77  INDX-2                      PIC S9(5)   VALUE ZERO COMP-3.           
004701 77  RAD-IX                      PIC S9(3)   VALUE ZERO COMP-3.           
004801 77  MAX-RAD                     PIC S9(5)   VALUE +13  COMP-3.           
004901 77  TAB-IX                      PIC S9(5)   VALUE +0   COMP-3.           
005001 77  TAB-IX-MAX                  PIC S9(5)   VALUE +26  COMP-3.           
005101 77  WS-IDDISTR                  PIC 9(4)    VALUE ZERO.                  
005201 77  WS-IDKUNDNR-NUM             PIC 9(6)    VALUE ZERO.                  
005301 77  WS-IDARTNR                  PIC 9(9)    VALUE ZERO.                  
005401 77  WS-KDVALISO                 PIC X(3)    VALUE SPACE.                 
005501 77  WS-OLD-KVLS                 PIC S9(7)   VALUE ZERO COMP-3.           
005601 77  WS-OLD-KVEFRS               PIC S9(7)   VALUE ZERO COMP-3.           
005701 77  WS-KVANTMOT                 PIC 9(7)    VALUE ZERO.                  
005801 77  WS-KVSKROT                  PIC 9(7)    VALUE ZERO.                  
005901 77  WS-RO-KVANT                 PIC S9(7)   VALUE ZERO COMP-3.           
006001 77  WS-RAD-IFYLLD               PIC X       VALUE SPACE.                 
006101 77  W-KVAVIS                    PIC S9(7)   VALUE ZERO COMP-3.           
006201 77  WS-SUMMA-KVANT              PIC 9(7)    VALUE ZERO.                  
006301 77  W-PRARTNTO                  PIC S9(7)V9(2) VALUE ZERO COMP-3.        
006401 77  W-KDFRAKT                   PIC S9(3)      VALUE ZERO COMP-3.        
006501 77  WS-FIXAD-PRARTNTO           PIC S9(7)V9(2) VALUE ZERO COMP-3.        
006601 77  W-KDPRODSL                  PIC 9(2)    VALUE ZERO.                  
006701 77  W-KDPRODSL-LOC              PIC 9(2)    VALUE ZERO.                  
006804 77  W-IDFKNGRP                  PIC 9(5)    VALUE ZERO.                  
006902 77  WS-BAATORDER                PIC X       VALUE SPACE.                 
007002 77  WS-FLYGORDER                PIC X       VALUE SPACE.                 
007102 77  WS-FIKT-FAKT-ANV            PIC X       VALUE SPACE.                 
007202 77  W-DAGENS-DATUM              PIC 9(6)    VALUE ZERO.                  
007302 77  WS-NY-ART                   PIC X       VALUE SPACE.                 
007402 77  WS-TIFAKT                   PIC 9(6)    VALUE ZERO.                  
007502 77  WS-IDLEVNR                  PIC X(5)    VALUE SPACE.                 
007602 77  WS-6302-IDDC-LEV            PIC X(2)    VALUE SPACE.                 
007702 77  WS-6302-IDDISTR             PIC S9(5)   VALUE ZERO   COMP-3.         
007802 77  WS-6302-IDKUNDNR            PIC S9(7)   VALUE ZERO   COMP-3.         
007902 77  W-TIKLOCK                   PIC S9(9)   VALUE ZERO   COMP-3.         
008002 77  W-TID                       PIC 9(8)    VALUE ZERO.                  
008102 77  DAGENS-DATUM                PIC 9(9)    VALUE ZERO.                  
008202 77  W-IDSEKVNR-SAP              PIC S9(3)   VALUE 0   COMP-3.            
008302 77  W-IDSEKVNR-A03              PIC S9(3)   VALUE 0   COMP-3.            
008402 77  W-IDSEKVNR                  PIC S9(3)   VALUE 0   COMP-3.            
008502 77  WS-LOGG-DATUM               PIC S9(8)   VALUE ZERO.                  
008602 77  WS-LOGG-TID                 PIC S9(7)   VALUE ZERO.                  
008702 77  WS-PRIME-LOCATION           PIC X       VALUE 'P'.                   
008802 77  WS-SAP-AAAAMMDD             PIC 9(8)    VALUE ZERO.                  
008902 77  WS-SAP-TTMMSSTH             PIC 9(8)    VALUE ZERO.                  
009002 77  WS-SAP-PRARTSTD             PIC S9(7)V9(2)  VALUE 0 COMP-3.          
009102 77  WS-SAP-IDKUNDNR             PIC X(6).                                
009202 77  WS-SAP-IDFAKT               PIC X(7).                                
009302 77  NOLL-RAKNARE                PIC S9(5)   COMP-3 VALUE ZERO.           
009402 77  WS-KDSORT                   PIC X(2)    VALUE SPACE.                 
009502 77  WS-IDUSER-003               PIC X(5)    VALUE SPACE.                 
009602 77  SPAR-FLINLREP               PIC X       VALUE SPACE.                 
009702 77  WS-FAKTURA-DATUM            PIC X(16)   VALUE SPACE.                 
009802 77  WS-FAKTURA-DATUM2           PIC S9(16)  COMP-3 VALUE ZERO.           
009902 77  WS-6308-TF-PRARTNTO         PIC S9(7)V9(2)    VALUE ZERO.            
010002 77  WS-6308-TF-KDVALISO         PIC X(3)          VALUE SPACE.           
010102                                                                          
010202                                                                          
010302 01  WS-SOEK-IDDC-SEND-REC.                                               
010402     03 WS-SOEK-IDDC-SEND        PIC X(2).                                
010502     03 WS-SOEK-IDDC-REC         PIC X(2).                                
010602                                                                          
010702 01  WS-IDFAKT                          PIC X(7).                         
010802 01  IDFAKT-WS  REDEFINES WS-IDFAKT     PIC 9(7).                         
010902 01  WS-IDORDNR                         PIC X(5).                         
011002 01  IDORDNR-WS REDEFINES WS-IDORDNR    PIC 9(5).                         
011102 01  WS-IDKUNDNR                        PIC X(6).                         
011202 01  IDKUNDNR-WS REDEFINES WS-IDKUNDNR  PIC 9(6).                         
011302 01  WS-IDKOLLI                         PIC X(5).                         
011402 01  IDKOLLI-WS REDEFINES WS-IDKOLLI    PIC 9(5).                         
011502                                                                          
011602 01  AKTUELL-TID.                                                         
011702     03  AKTUELL-TTMM    PIC 9(4).                                        
011802     03  FILLER          PIC 9(4).                                        
011902                                                                          
012002 01  WS-SEKEL-KOLL               PIC 9(6).                                
012102 01  FILLER REDEFINES WS-SEKEL-KOLL.                                      
012202     03  WS-SEKEL                PIC 9(1).                                
012302     03  FILLER                  PIC 9(5).                                
012402                                                                          
012502 01  WS-SEKEL-EKOA03.                                                     
012602     03  WS-EKOA03-SS            PIC 9(2).                                
012702     03  WS-EKOA03-AAMMDD        PIC 9(6).                                
012802 01  WS-AAAAMMDD REDEFINES WS-SEKEL-EKOA03 PIC 9(8).                      
012902                                                                          
013002 01  WS-SEKEL-TEST               PIC 9(6).                                
013102 01  FILLER REDEFINES WS-SEKEL-TEST.                                      
013202     03  WS-SEK                  PIC 9(1).                                
013302     03  FILLER                  PIC 9(5).                                
013402                                                                          
013502 01  WS-SEKEL-DIFF.                                                       
013602     03  WS-DIFF-SS              PIC 9(2).                                
013702     03  WS-DIFF-AAMMDD          PIC 9(6).                                
013802 01  WS-DIFF-AAAAMMDD REDEFINES WS-SEKEL-DIFF PIC 9(8).                   
013902                                                                          
014002 01  WS-IDDC-KOLL.                                                        
014102     03  FILLER                  PIC X(5) VALUE 'WIDDC'.                  
014202     03  WS-IDDC-TID             PIC X(2) VALUE SPACE.                    
014302     03  FILLER                  PIC X    VALUE SPACE.                    
014402                                                                          
014502 01  W-TIME-X.                                                            
014602     03  W-TIME-TT       PIC 9(2).                                        
014702     03  FILLER          PIC 9(6).                                        
014802 01  W-TIME-N            REDEFINES W-TIME-X PIC 9(8).                     
014902                                                                          
015002 01  W-TIAAAAMMDDTTMMSSTH    PIC 9(16)   VALUE ZERO.                      
015102 01  FILLER                  REDEFINES W-TIAAAAMMDDTTMMSSTH.              
015202     03  W-TISEKEL               PIC 9(2).                                
015302     03  W-TIAAMMDDTTMMSSTH-DATE PIC 9(6).                                
015402     03  W-TIAAMMDDTTMMSSTH-TIME PIC 9(8).                                
015502                                                                          
015602 01  W-IDLOPNRM              PIC 9(9)    VALUE ZERO.                      
015702 01  W-0VVDLLLLK             REDEFINES W-IDLOPNRM.                        
015802     03 FILLER               PIC 9(1).                                    
015902     03 W-VVD                PIC 9(3).                                    
016002     03 W-LLLL               PIC 9(4).                                    
016102     03 W-K                  PIC 9(1).                                    
016202                                                                          
016302 77  INDATA-SW                   PIC X       VALUE 'J'.                   
016402     88  INDATA-OK                           VALUE 'J'.                   
016502     88  INDATA-FEL                          VALUE 'N'.                   
016602                                                                          
016702 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
016802     88  NYCKLAR-OK                          VALUE 'J'.                   
016902     88  NYCKLAR-FEL                         VALUE 'N'.                   
017002                                                                          
017102 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
017202     88  EGEN-MID                            VALUE '6309'.                
017302     88  GODK-MID                            VALUE '6309'.                
017402     88  HELP-MID                            VALUE '0551'.                
017502                                                                          
017602 01  PLATS-TABELL.                                                        
017702     03 TABELLRAD  OCCURS 26.                                             
017802        05 WS-ADLAGOMR           PIC 9(3).                                
017902        05 WS-ADGANG             PIC 9(3).                                
018002        05 WS-ADPLATS            PIC 9(5).                                
018102                                                                          
018202                                                                          
018302 01  W-LAGERPLATS-LOCB.                                                   
018402        03  W-ADLAGOMR-LOCB      PIC S9(3)       COMP-3.                  
018502        03  W-ADGANG-LOCB        PIC S9(3)       COMP-3.                  
018602        03  W-ADPLATS-LOCB       PIC S9(5)       COMP-3.                  
018702                                                                          
018802*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
018902 01  GENERELLA-SUBPROGRAM.                                                
019002     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
019102     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
019202     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
019302     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
019402     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
019502     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
019602     03  W006KOM                 PIC X(8)    VALUE 'W006KOM '.            
019702     03  W009KSIF                PIC X(8)    VALUE 'W009KSIF'.            
019802     03  CHECK                   PIC X(8)    VALUE 'CHECK   '.            
019902     03  W335PRIS                PIC X(8)    VALUE 'W335PRIS'.            
020002     03  W510AVG                 PIC X(8)    VALUE 'W510AVG '.            
020102     03  W005WDK7                PIC X(8)    VALUE 'W005WDK7'.            
020202     EJECT                                                                
020302*    --- PARAMETRAR TILL SUBPROGRAM W005WDK7                              
020402*01 -COPY W005WDK7                                                        
020502     EJECT                                                                
020602*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
020702*01 -COPY WMEDAREA                                                        
020802     SKIP3                                                                
020902 01  MESSAGE-CODES.                                                       
021002     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
021102     03  INF-PRESS-PF11          PIC X(3)    VALUE '003'.                 
021202     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '011'.                 
021302     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
021402     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
021502     03  INF-URVAL-SAKNAS        PIC X(3)    VALUE '005'.                 
021602     03  INF-ART-FRAN-ANNAT-FTG  PIC X(3)    VALUE '088'.                 
021702     EJECT                                                                
021802 01  MESSAGE-TEXTS.                                                       
021902     03  INF-GODKANN-INTE-SLUT                                            
022002                                 PIC X(42)   VALUE                        
022102         'CONFIRM NOT LAST LINE IN CASE - PRESS PF11'.                    
022202*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
022302*                                                                         
022402 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
022502     SKIP3                                                                
022602*01 -COPY WMSGINIT                                                        
022702     EJECT                                                                
022802*01 -COPY WWDIST35                                                        
022902     EJECT                                                                
023002*01 -COPY WWDIST79                                                        
023102     EJECT                                                                
023202*01 -COPY WWDC03                                                          
023302     EJECT                                                                
023402*01 -COPY WWDC99                                                          
023502     EJECT                                                                
023602*01 -COPY W510AVG                                                         
023702     EJECT                                                                
023802*01 -COPY W510A03 -PRE EKOTRA03-                                          
023902     EJECT                                                                
024002*01 -COPY W335PRIS                                                        
024102     EJECT                                                                
024202*01 -COPY WDATAREA                                                        
024302     EJECT                                                                
024402*01 -COPY W61236  -PRE FILC-                                              
024502     EJECT                                                                
024602*01 -COPY W61244  -PRE FILC2-                                             
024702     EJECT                                                                
024802*01 -COPY W61247  -PRE FILC3-                                             
024902     EJECT                                                                
025002*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
025102*                                                                         
025202 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
025302     SKIP3                                                                
025402*01  MID -COPY W6I30901                                                   
025502     EJECT                                                                
025602 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
025702     SKIP3                                                                
025802*01  -COPY WMSGAREA                                                       
025902     EJECT                                                                
026002     03  MOD REDEFINES MSG-AREA.                                          
026102*      05  -COPY W6O30901                                                 
026202     EJECT                                                                
026302 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
026402     SKIP3                                                                
026502*01  -COPY WMFSAREA                                                       
026602     EJECT                                                                
026702*    ----DB2                                                              
026802 01  FILLER                      PIC X(16)   VALUE 'SQLCA-AREA'.          
026902       EXEC SQL INCLUDE SQLCA END-EXEC.                                   
027002                                                                          
027102 01  FILLER                      PIC X(16)   VALUE 'SQLCODE-WS'.          
027202 01  DB2-WS.                                                              
027302     03  SQLCODE-WS              PIC 9(3)    VALUE ZERO.                  
027402         88  CURSOR-OK                       VALUE 000.                   
027502         88  LINES-FOUND                     VALUE 000.                   
027602         88  LINES-MISSING                   VALUE 100.                   
027702         88  TABELL-TOM                      VALUE 305.                   
027802         88  RESOURCE-WRONG                  VALUE 904.                   
027902     03  GOOD-SQLCODECODES.                                               
028002         05  GOOD-SQLCODE OCCURS 5                                        
028102             INDEXED BY SQLCODE-IX PIC 9(3).                              
028202 01  RKOD-ABEND-DB2              PIC S9(4)   COMP VALUE +998.             
028302     EJECT                                                                
028402 01  FILLER                      PIC X(16)   VALUE 'TP6FAKT-AREA'.        
028502*01  -COPY TP6FAKT -PRE RAD-                                              
028602                                                                          
028702     EXEC SQL INCLUDE TP6FAKT END-EXEC.                                   
028802                                                                          
028902                                                                          
029002     EJECT                                                                
029102*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
029202*                                                                         
029302     EJECT                                                                
029402 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
029502     SKIP3                                                                
029602 01  NYCKLAR-TILL-DLI.                                                    
029702     03  W-IDARTNR-X.                                                     
029802         05  W-IDARTNR           PIC S9(9)              COMP-3.           
029902                                                                          
030002     03  W-KDSEGKEY-X.                                                    
030102         05  W-KDSEGKEY          PIC X         VALUE '1'.                 
030202                                                                          
030302     03  W-IDKUNDNR-X.                                                    
030402         05  W-IDKUNDNR          PIC S9(7)              COMP-3.           
030502                                                                          
030602     03  W-DAINLEV-X.                                                     
030702         05  W-DAINLEV           PIC 9(16).                               
030802                                                                          
030902     03  W-IDDC                  PIC X(2).                                
031002     03  W-IDDC-B6-X.                                                     
031102         05  W-IDDC-B6           PIC X(2)      VALUE SPACE.               
031202     03  W-IDPTYP                PIC X(3).                                
031302     03  W-IDSKYLT               PIC X(3).                                
031402     03  W-IDKUNDRF              PIC X(10).                               
031502                                                                          
031602     03  W-WDL6A1KY-MIN.                                                  
031702         05  W-SEQA-IDFAKT-MIN    PIC S9(7)             COMP-3.           
031802         05  W-SEQA-IDKUNDRF-MIN  PIC X(10).                              
031902         05  W-SEQA-IDKUNDNR-MIN  PIC S9(7)             COMP-3.           
032002         05  W-SEQA-IDKOLLI-MIN   PIC S9(5)             COMP-3.           
032102         05  W-SEQA-IDARTNR-MIN   PIC S9(9)             COMP-3.           
032202         05  W-SEQA-DAINLEV-MIN   PIC 9(16).                              
032302                                                                          
032402     03  W-WDL6A1KY-MAX.                                                  
032502         05  W-SEQA-IDFAKT-MAX    PIC S9(7)             COMP-3.           
032602         05  W-SEQA-IDKUNDRF-MAX  PIC X(10).                              
032702         05  W-SEQA-IDKUNDNR-MAX  PIC S9(7)             COMP-3.           
032802         05  W-SEQA-IDKOLLI-MAX   PIC S9(5)             COMP-3.           
032902         05  W-SEQA-IDARTNR-MAX   PIC S9(9)             COMP-3.           
033002         05  W-SEQA-DAINLEV-MAX   PIC 9(16).                              
033102                                                                          
033202     03  W-WDL6A1KY-MIN2.                                                 
033302         05  W-IDFAKT-MIN         PIC S9(7)             COMP-3.           
033402         05  FILLER               PIC X(38).                              
033502                                                                          
033602     03  W-WDL6A1KY-MAX2.                                                 
033702         05  W-IDFAKT-MAX         PIC S9(7)             COMP-3.           
033802         05  FILLER               PIC X(38).                              
033902                                                                          
034002     03  W-IDFAKT-X.                                                      
034102         05  W-IDFAKT            PIC S9(7)   VALUE ZERO COMP-3.           
034202                                                                          
034302     03  W-IDLBBET               PIC X(12)   VALUE SPACE.                 
034402                                                                          
034502     03  W-6017KEY-X.                                                     
034602         05  W-6017-IDHTYP      PIC X(4)     VALUE '6017'.                
034702         05  FILLER             PIC X(26)    VALUE LOW-VALUE.             
034802                                                                          
034902     03  W-6301KEY-X.                                                     
035002         05  W-6301-IDHTYP      PIC X(4)     VALUE '6301'.                
035102         05  W-6301-IDDC        PIC X(2).                                 
035202         05  FILLER             PIC X(24)    VALUE LOW-VALUE.             
035302                                                                          
035402     03  W-6305KEY-X.                                                     
035502         05  W-6305-IDHTYP      PIC X(4)     VALUE '6305'.                
035602         05  FILLER             PIC X(26)    VALUE LOW-VALUE.             
035702                                                                          
035802     03  W-4505-KEY-X.                                                    
035902         05  FILLER              PIC X(4)    VALUE '4505'.                
036002         05  4505-IDDC           PIC X(2)    VALUE SPACE.                 
036102         05  FILLER              PIC X(24)   VALUE LOW-VALUE.             
036202                                                                          
036302     03  W-WDJ911KY-X.                                                    
036402         05  W-IDDC-WDJ9         PIC 9(2)    VALUE ZERO.                  
036502         05  W-DASTADAT          PIC S9(9)   VALUE ZERO.                  
036602         05  W-TISTATID          PIC S9(7)   VALUE ZERO.                  
036702         05  W-ADLAGOMR          PIC 9(2)    VALUE ZERO.                  
036802         05  W-ADGANG            PIC 9(2)    VALUE ZERO.                  
036902         05  W-ADPLATS           PIC 9(5)    VALUE ZERO.                  
037002                                                                          
037102     SKIP2                                                                
037202     SKIP2                                                                
037302*    --- STATUS-KOD FRÅN IMS                                              
037402 01  STATUS-WS                   PIC XX.                                  
037502     88  SEGMENT-FINNS                       VALUE '  '.                  
037602     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
037702     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
037802     SKIP2                                                                
037902 01  GODK-STATUSKODER.                                                    
038002     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
038102     SKIP3                                                                
038202 01  SSA1                        PIC X(160).                              
038302 01  SSA2                        PIC X(128).                              
038402 01  SSA3                        PIC X(128).                              
038502     EJECT                                                                
038602*    --- IMS FUNKTIONSKODER                                               
038702*01  -COPY W0003                                                          
038802     EJECT                                                                
038902*    ---  DLI INPUT-OUTPUT AREA                                           
039002 01  FILLER                      PIC X(16) VALUE 'WLLOGA01'.              
039102*01  WLLOGA01  -COPY WDL901                                               
039202     SKIP3                                                                
039302 01  FILLER                      PIC X(16)   VALUE                        
039402                                          'DLI-IO-AREA-WDL6'.             
039502     SKIP3                                                                
039602 01  DLI-IO-AREA-WDL6.                                                    
039702     03  IO-AREA-WDL6            PIC X(300)  VALUE SPACE.                 
039802     SKIP3                                                                
039902     03  WLINLC01 REDEFINES IO-AREA-WDL6.                                 
040002*        05  -COPY WDL601                                                 
040102     EJECT                                                                
040202     03  WLINLC11 REDEFINES IO-AREA-WDL6.                                 
040302*        05  -COPY WDL611                                                 
040402     EJECT                                                                
040502     03  WLINLD01 REDEFINES IO-AREA-WDL6.                                 
040602*        05  -COPY WDL6A1                                                 
040702     EJECT                                                                
040802 01  FILLER                      PIC X(16)   VALUE                        
040902                                          'DLI-IO-AREA-WDK7'.             
041002     SKIP3                                                                
041102 01  DLI-IO-AREA-WDK7.                                                    
041202     03  -COPY WDK711                                                     
041302     EJECT                                                                
041402 01  FILLER                      PIC X(16)   VALUE                        
041502                                          'DLI-IO-AREA-WDK6'.             
041602     SKIP3                                                                
041702 01  DLI-IO-AREA-WDK6.                                                    
041802     03  IO-AREA-WDK6            PIC X(900)  VALUE SPACE.                 
041902     SKIP3                                                                
042002     03  WLARTC01 REDEFINES IO-AREA-WDK6.                                 
042102*        05  -COPY WDK601        -PRE K6-                                 
042202     EJECT                                                                
042302     03  WLARTC11 REDEFINES IO-AREA-WDK6.                                 
042402*        05  -COPY WDK611                                                 
042502     EJECT                                                                
042602 01  FILLER                      PIC X(16)   VALUE                        
042702                                            'DLI-IO-AREA-WDGX'.           
042802     SKIP3                                                                
042902 01  DLI-IO-AREA-WDGX.                                                    
043002     03  IO-AREA-WDGX            PIC X(300)  VALUE SPACE.                 
043102     SKIP3                                                                
043202     03  WL630101 REDEFINES IO-AREA-WDGX.                                 
043302*        05  -COPY WDGX6301                                               
043402     EJECT                                                                
043502     03  WL630111 REDEFINES IO-AREA-WDGX.                                 
043602*        05  -COPY WDGX6302                                               
043702     EJECT                                                                
043802 01  DLI-IO-AREA-WDGX2.                                                   
043902     03  IO-AREA-WDGX2           PIC X(300)  VALUE SPACE.                 
044002     SKIP3                                                                
044102     03  WL630511 REDEFINES IO-AREA-WDGX2.                                
044202*        05  -COPY WDGX6306                                               
044302     EJECT                                                                
044402     03  WL630521 REDEFINES IO-AREA-WDGX2.                                
044502*        05  -COPY WDGX6308                                               
044602     EJECT                                                                
044702 01  FILLER                      PIC X(16)   VALUE                        
044802                                         'DLI-IO-AREA-WDR8'.              
044902     SKIP3                                                                
045002 01  DLI-IO-AREA-WDR8.                                                    
045102     03  IO-AREA-WDR8            PIC X(300)  VALUE SPACE.                 
045202                                                                          
045302     03  WLFILB01 REDEFINES IO-AREA-WDR8.                                 
045402*        05  -COPY WDR801                                                 
045502*        07  -COPY W510EKHA  -PRE R8- -RED FIL-WDR801-DATA                
045602     EJECT                                                                
045702 01  DLI-IO-AREA-WDD3.                                                    
045802     03  IO-AREA-WDD3            PIC X(300)  VALUE SPACE.                 
045902     SKIP3                                                                
046002     03  WLBENA11 REDEFINES IO-AREA-WDD3.                                 
046102*        05  -COPY WDD311                                                 
046202     EJECT                                                                
046302 01  FILLER                      PIC X(16)   VALUE                        
046402                                            'DLI-IO-AREA-W6GX'.           
046502     SKIP3                                                                
046602 01  DLI-IO-AREA-W6GX.                                                    
046702     03  IO-AREA-W6GX            PIC X(300)  VALUE SPACE.                 
046802     SKIP3                                                                
046902     03  W6LOPA11 REDEFINES IO-AREA-W6GX.                                 
047002*        05  -COPY W6GX6018                                               
047102     EJECT                                                                
047202 01  FILLER                      PIC X(16)   VALUE                        
047302                                            'DLI-IO-AREA-4505'.           
047402 01  DLI-IO-AREA-4505.                                                    
047502     03  IO-AREA-4505            PIC X(300)  VALUE SPACE.                 
047602     SKIP3                                                                
047702     03  WL450611 REDEFINES IO-AREA-4505.                                 
047802*        05  -COPY WDGX4506                                               
047902     EJECT                                                                
048002 01  DLI-IO-AREA-FILC.                                                    
048102     03  IO-AREA-FILC            PIC X(300)  VALUE SPACE.                 
048202                                                                          
048302     03  WLFILC01 REDEFINES IO-AREA-FILC.                                 
048402*        05  -COPY WDR301 -PRE FILC-.                                     
048502     EJECT                                                                
048602 01  DLI-IO-AREA-FILC2.                                                   
048702     03  IO-AREA-FILC2           PIC X(300)  VALUE SPACE.                 
048802                                                                          
048902     03  WLFILC01 REDEFINES IO-AREA-FILC2.                                
049002*        05  -COPY WDR301 -PRE FILC2-.                                    
049102     EJECT                                                                
049202 01  DLI-IO-AREA-FILC3.                                                   
049302     03  IO-AREA-FILC3           PIC X(300)  VALUE SPACE.                 
049402                                                                          
049502     03  WLFILC01 REDEFINES IO-AREA-FILC3.                                
049602*        05  -COPY WDR301 -PRE FILC3-.                                    
049702     EJECT                                                                
049802 01  DLI-IO-AREA-LOCB.                                                    
049902     03  IO-AREA-LOCB            PIC X(300)  VALUE SPACE.                 
050002     SKIP3                                                                
050102     03  WLLOCB01 REDEFINES IO-AREA-LOCB.                                 
050202*        05  -COPY WDJ901       -PRE LOCB-                                
050302     EJECT                                                                
050402     03  WLLOCB11 REDEFINES IO-AREA-LOCB.                                 
050502*        05  -COPY WDJ911       -PRE LOCB-                                
050602     EJECT                                                                
050702 01  FILLER             PIC X(16)  VALUE 'DLI-IO-WLSAPA01'.               
050802 01  DLI-IO-WLSAPA01.                                                     
050902*    03  WLSAPA01  -COPY WDR901                                           
051002*    07  -COPY W510EKHA  -RED FIL-WDR901-DATA                             
051102     EJECT                                                                
051202 01  FILLER               PIC X(16)   VALUE 'WDB601 REC '.                
051302 01  DLI-IO-AREA-B601-REC.                                                
051402*    03  -COPY WDB601 -PRE REC-                                           
051502     EJECT                                                                
051602 01  FILLER               PIC X(16)   VALUE 'WDB601 SEND'.                
051702 01  DLI-IO-AREA-B601-SEND.                                               
051802*    03  -COPY WDB601  -PRE SEND-                                         
051902     EJECT                                                                
052002 01  DLI-IO-L6A1.                                                         
052102*    03  -COPY WDL6A1   -PRE WDL6A1-                                      
052202                                                                          
052302     EJECT                                                                
052402 LINKAGE SECTION.                                                         
052502*01  -COPY W0009  -PRE MSG-                                               
052602     EJECT                                                                
052702*01  -COPY W0008  -PRE WLLOGA-                                            
052802     05  FILLER                  PIC X.                                   
052902     EJECT                                                                
053002*01  -COPY W0008  -PRE USEA-                                              
053102     05  FILLER                  PIC X.                                   
053202     EJECT                                                                
053302*01  -COPY W0008  -PRE GX63-                                              
053402     05  FILLER                  PIC X.                                   
053502     EJECT                                                                
053602*01  -COPY W0008  -PRE INLC-                                              
053702     05  FILLER                  PIC X.                                   
053802     EJECT                                                                
053902*01  -COPY W0008  -PRE ARTC-                                              
054002     05  FILLER                  PIC X.                                   
054102     EJECT                                                                
054202*01  -COPY W0008  -PRE WDK7-                                              
054302     05  FILLER                  PIC X.                                   
054402     EJECT                                                                
054502*01  -COPY W0008  -PRE PRIS-WDK7-                                         
054602     05  FILLER                  PIC X.                                   
054702     EJECT                                                                
054802*01  -COPY W0008  -PRE FILB-                                              
054902     05  FILLER                  PIC X.                                   
055002     EJECT                                                                
055102*01  -COPY W0008  -PRE GMTA-                                              
055202     05  FILLER                  PIC X.                                   
055302     EJECT                                                                
055402*01  -COPY W0008  -PRE BETA-                                              
055502     05  FILLER                  PIC X.                                   
055602     EJECT                                                                
055702*01  -COPY W0008  -PRE GPRIA-                                             
055802     05  FILLER                  PIC X.                                   
055902     EJECT                                                                
056002*01  -COPY W0008  -PRE GPRIB-                                             
056102     05  FILLER                  PIC X.                                   
056202     EJECT                                                                
056302*01  -COPY W0008  -PRE GX65-                                              
056402     05  FILLER                  PIC X.                                   
056502     EJECT                                                                
056602*01  -COPY W0008  -PRE 4505-                                              
056702     05  FILLER                  PIC X.                                   
056802     EJECT                                                                
056902*01  -COPY W0008  -PRE 9305-                                              
057002     05  FILLER                  PIC X.                                   
057102     EJECT                                                                
057202*01  -COPY W0008  -PRE AVG-WDB6-                                          
057302     05  FILLER                  PIC X.                                   
057402     EJECT                                                                
057502*01  -COPY W0008  -PRE FILC-                                              
057602     05  FILLER                  PIC X.                                   
057702     EJECT                                                                
057802*01  -COPY W0008  -PRE LOCB-                                              
057902     05  FILLER                  PIC X.                                   
058002     EJECT                                                                
058102*01  -COPY W0008  -PRE SAPA-                                              
058202     05  FILLER                  PIC X.                                   
058302     EJECT                                                                
058402 01  PRIS-COST-WDK6-PCB          PIC X.                                   
058502 01  PRIS-COST-WDK7-PCB          PIC X.                                   
058602 01  PRIS-COST-WDF1-PCB          PIC X.                                   
058702 01  PRIS-COST-9305-PCB          PIC X.                                   
058802 01  PRIS-COST-WDK72-PCB         PIC X.                                   
058902 01  PRIS-COST-WDB6-PCB          PIC X.                                   
059002*01  -COPY W0008  -PRE WDB6-                                              
059102     05  FILLER                  PIC X.                                   
059202*01  -COPY W0008  -PRE WDL6A-                                             
059302     05  FILLER                  PIC X.                                   
059402 PROCEDURE DIVISION  USING MSG-PCB USEA-PCB GX63-PCB                      
059502                           INLC-PCB ARTC-PCB WDK7-PCB                     
059602                           PRIS-WDK7-PCB                                  
059702                           FILB-PCB GX65-PCB 9305-PCB                     
059802                           AVG-WDB6-PCB                                   
059902                           GMTA-PCB BETA-PCB GPRIA-PCB GPRIB-PCB          
060002                           4505-PCB FILC-PCB WLLOGA-PCB LOCB-PCB          
060102                           SAPA-PCB                                       
060202                           PRIS-COST-WDK6-PCB                             
060302                           PRIS-COST-WDK7-PCB                             
060402                           PRIS-COST-WDF1-PCB                             
060502                           PRIS-COST-9305-PCB                             
060602                           PRIS-COST-WDK72-PCB                            
060702                           PRIS-COST-WDB6-PCB                             
060802                           WDB6-PCB WDL6A-PCB.                            
060902 MAIN SECTION.                                                            
061002     ENTRY 'DLITCBL' USING MSG-PCB  USEA-PCB GX63-PCB                     
061102                           INLC-PCB ARTC-PCB WDK7-PCB                     
062001                           PRIS-WDK7-PCB                                  
062101                           FILB-PCB GX65-PCB 9305-PCB                     
062201                           AVG-WDB6-PCB                                   
062301                           GMTA-PCB BETA-PCB GPRIA-PCB GPRIB-PCB          
062401                           4505-PCB FILC-PCB WLLOGA-PCB LOCB-PCB          
062501                           SAPA-PCB                                       
062601                           PRIS-COST-WDK6-PCB                             
062701                           PRIS-COST-WDK7-PCB                             
062801                           PRIS-COST-WDF1-PCB                             
062901                           PRIS-COST-9305-PCB                             
063001                           PRIS-COST-WDK72-PCB                            
063101                           PRIS-COST-WDB6-PCB                             
063201                           WDB6-PCB WDL6A-PCB.                            
063301                                                                          
063401     PERFORM IMS-GET-MSG                                                  
063501                                                                          
063601     IF SEGMENT-FINNS                                                     
063701                                                                          
063801       PERFORM A-INIT                                                     
063901       PERFORM B-KOLLA-NYCKLAR                                            
064001       IF NYCKLAR-OK                                                      
064101         IF MFS-UPDATE                                                    
064201           PERFORM G-KOLLA-INPUT                                          
064301           IF INDATA-OK                                                   
064401             PERFORM H-UPPDATERA                                          
064501           END-IF                                                         
064601         ELSE                                                             
064701           IF MFS-FIRST                                                   
064801             PERFORM C-FOERSTA-SIDA                                       
064901           ELSE                                                           
065001             IF MFS-NEXT                                                  
065101                PERFORM D-NAESTA-SIDA                                     
065201             ELSE                                                         
065301                PERFORM E-SAMMA-SIDA                                      
065401             END-IF                                                       
065501           END-IF                                                         
065601         END-IF                                                           
065701       END-IF                                                             
065801       COMPUTE MSG-KVLL = LENGTH OF MOD-W6O30901 + 4                      
065901       PERFORM IMS-INSERT-MSG                                             
066001     END-IF                                                               
066101                                                                          
066201     MOVE ZERO TO RETURN-CODE                                             
066301     GOBACK                                                               
066401     .                                                                    
066501     EJECT                                                                
066601 A-INIT SECTION.                                                          
066701                                                                          
066801     IF MSG-DUBBLA-TRANSKODER                                             
066901       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W6I30901                 
067001       MOVE MSG-IDTRANS-2  TO MFS-IDTRANS                                 
067101       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
067201     ELSE                                                                 
067301       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W6I30901                  
067401       MOVE MSG-IDTRANS-1  TO MFS-IDTRANS                                 
067501       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
067601     END-IF                                                               
067701                                                                          
067801     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
067901     MOVE MSG-IDPFK   TO MFS-IDPFK                                        
068001     MOVE MFS-IDTRANS TO W-IDTRANS                                        
068101                                                                          
068201     MOVE LOW-VALUE TO MSG-AREA                                           
068301     MOVE 'W6O309N1' TO MFS-IDMOD                                         
068401     MOVE '6309' TO MOD-IDTRANS                                           
068501     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                    
068601                                                                          
068701     IF EGEN-MID OR HELP-MID                                              
068801       CONTINUE                                                           
068901     ELSE                                                                 
069001       MOVE SPACE TO MFS-KDTRTYP                                          
069101       MOVE '7' TO MFS-IDPFK                                              
069201       MOVE MFS-RENSA-FAELT TO MOD-FLSLUT-BEKR                            
069301     END-IF                                                               
069401                                                                          
069501     MOVE 'IDAG'          TO DAT-KDDATFORM                                
069601     CALL WDATKONV USING     DAT-KDDATFORM                                
069701                             DAT-I-TIDATUM                                
069801                             DAT-O-TIDATUM                                
069901                             DAT-KDSVAR                                   
070001                                                                          
070101     MOVE DAT-TIAAMMDD    TO W-DAGENS-DATUM                               
070201     ACCEPT W-TIME-X      FROM TIME                                       
070301     ACCEPT DAGENS-DATUM  FROM DATE                                       
070401     MOVE W-TIME-N        TO W-TIKLOCK                                    
070501                                                                          
070601     MOVE 'W6030900'       TO FILC-FIL-IDPGM                              
070701                              FILC2-FIL-IDPGM                             
070801                              FILC3-FIL-IDPGM                             
070901     MOVE W-DAGENS-DATUM   TO FILC-FIL-TIREGDAT                           
071001                              FILC2-FIL-TIREGDAT                          
071101                              FILC3-FIL-TIREGDAT                          
071201     MOVE 'W61236  '       TO FILC-FIL-IDCPYTXT                           
071301     MOVE 'W61244  '       TO FILC2-FIL-IDCPYTXT                          
071401     MOVE 'W61247  '       TO FILC3-FIL-IDCPYTXT                          
071501     MOVE ZERO             TO FILC-FIL-TIKLOCK                            
071601                              FILC2-FIL-TIKLOCK                           
071701                              FILC3-FIL-TIKLOCK                           
071801     MOVE NEJ              TO WS-FIKT-FAKT-ANV                            
071901     .                                                                    
072001     EJECT                                                                
072101 B-KOLLA-NYCKLAR SECTION.                                                 
072201                                                                          
072301     MOVE ALL '+'           TO MSGI-WMSGINIT                              
072401     MOVE '001'             TO MSGI-KDCALL                                
072501     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
072601     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
072701     MOVE '6309'            TO MSGI-IDTRANS                               
072801     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
072901                                                                          
073001     MOVE MFS-RENSA-FAELT TO MOD-IDDC-REC-IN                              
073101     MOVE MSGI-IDDC       TO REC-WS-IDDC                                  
073201                             WS-IDDC                                      
073301                             W-IDDC-B6                                    
073401     PERFORM IMS-GU-WDB601-REC                                            
073501     MOVE MSGI-IDLAND-SPR TO MED-IDSKYLT                                  
073601     MOVE JA TO NYCKLAR-SW                                                
073701                                                                          
073801     MOVE MFS-RENSA-FAELT TO MOD-IDORDNR-IN                               
073901     IF MID-IDORDNR-IN NOT = ALL '+'                                      
074001        INSPECT MID-IDORDNR-IN REPLACING LEADING SPACE BY ZERO            
074101        MOVE MID-IDORDNR-IN TO WS-IDORDNR                                 
074201        MOVE '7'         TO MFS-IDPFK                                     
074301        MOVE SPACE       TO MFS-KDTRTYP                                   
074401        MOVE MFS-RENSA-FAELT TO MOD-FLSLUT-BEKR                           
074501     ELSE                                                                 
074601        INSPECT MID-IDORDNR-UT REPLACING LEADING SPACE BY ZERO            
074701        MOVE MID-IDORDNR-UT TO WS-IDORDNR                                 
074801     END-IF                                                               
074901     IF WS-IDORDNR NUMERIC                                                
075001         IF WS-IDORDNR = ZERO                                             
075101            MOVE '00001' TO WS-IDORDNR                                    
075201         END-IF                                                           
075301     ELSE                                                                 
075401         MOVE NEJ           TO NYCKLAR-SW                                 
075501     END-IF                                                               
075601                                                                          
075701     MOVE MFS-RENSA-FAELT TO MOD-IDKUNDNR-IN                              
075801     IF MID-IDKUNDNR-IN  NOT = ALL '+'                                    
075901        INSPECT MID-IDKUNDNR-IN REPLACING LEADING SPACE BY ZERO           
076001        MOVE MID-IDKUNDNR-IN TO WS-IDKUNDNR                               
076101        MOVE '7'         TO MFS-IDPFK                                     
076201        MOVE SPACE       TO MFS-KDTRTYP                                   
076301        MOVE MFS-RENSA-FAELT TO MOD-FLSLUT-BEKR                           
076401     ELSE                                                                 
076501        INSPECT MID-IDKUNDNR-UT REPLACING LEADING SPACE BY ZERO           
076601        MOVE MID-IDKUNDNR-UT TO WS-IDKUNDNR                               
076701     END-IF                                                               
076801     IF WS-IDKUNDNR NUMERIC                                               
076901         IF WS-IDKUNDNR = ZERO                                            
077001            MOVE '000001' TO WS-IDKUNDNR                                  
077101         END-IF                                                           
077201     ELSE                                                                 
077301         MOVE NEJ        TO NYCKLAR-SW                                    
077401     END-IF                                                               
077501                                                                          
077601     MOVE MFS-RENSA-FAELT TO MOD-IDKOLLI-IN                               
077701     IF MID-IDKOLLI-IN NOT = ALL '+'                                      
077801        INSPECT MID-IDKOLLI-IN REPLACING LEADING SPACE BY ZERO            
077901        MOVE MID-IDKOLLI-IN TO WS-IDKOLLI                                 
078001        MOVE '7'         TO MFS-IDPFK                                     
078101        MOVE SPACE       TO MFS-KDTRTYP                                   
078201        MOVE MFS-RENSA-FAELT TO MOD-FLSLUT-BEKR                           
078301     ELSE                                                                 
078401        INSPECT MID-IDKOLLI-UT REPLACING LEADING SPACE BY ZERO            
078501        MOVE MID-IDKOLLI-UT TO WS-IDKOLLI                                 
078601     END-IF                                                               
078701     IF WS-IDKOLLI NUMERIC                                                
078801        IF WS-IDKOLLI = ZERO                                              
078901           MOVE '00001' TO WS-IDKOLLI                                     
079001        END-IF                                                            
079101     ELSE                                                                 
079201         MOVE NEJ TO NYCKLAR-SW                                           
079301     END-IF                                                               
079401                                                                          
079501     MOVE MFS-RENSA-FAELT TO MOD-IDDC-SEND-IN                             
079601     IF MID-IDDC-SEND-IN NOT =  ALL '+'                                   
079701        MOVE MID-IDDC-SEND-IN TO SEND-WS-IDDC                             
079801        MOVE '7'         TO MFS-IDPFK                                     
079901        MOVE SPACE       TO MFS-KDTRTYP                                   
080001        MOVE MFS-RENSA-FAELT TO MOD-FLSLUT-BEKR                           
080101     ELSE                                                                 
080201        MOVE MID-IDDC-SEND-UT TO SEND-WS-IDDC                             
080301     END-IF                                                               
080401     MOVE SEND-WS-IDDC TO W-IDDC-B6                                       
080501     PERFORM IMS-GU-WDB601-SEND                                           
080601                                                                          
080701     IF SEND-DCS-CDC    OR SEND-DCS-DDC                                   
080801     OR SEND-DCS-NDC-NA OR SEND-DCS-NDC-PF OR SEND-DCS-NDC-OTHERS         
080901     OR SEND-DCS-SDC OR SEND-DCS-NDC-CN                                   
081001        CONTINUE                                                          
081101     ELSE                                                                 
081201        MOVE NEJ TO NYCKLAR-SW                                            
081301     END-IF                                                               
081401                                                                          
081501     IF SEND-WS-IDDC = REC-WS-IDDC                                        
081601        MOVE NEJ TO NYCKLAR-SW                                            
081701     END-IF                                                               
081801                                                                          
081901     MOVE MFS-RENSA-FAELT TO MOD-IDFAKT-IN                                
082001     IF MID-IDFAKT-IN NOT = ALL '+'                                       
082101        INSPECT MID-IDFAKT-IN REPLACING LEADING SPACE BY ZERO             
082201        MOVE MID-IDFAKT-IN  TO WS-IDFAKT                                  
082301        MOVE '7'         TO MFS-IDPFK                                     
082401        MOVE SPACE       TO MFS-KDTRTYP                                   
082501        MOVE MFS-RENSA-FAELT TO MOD-FLSLUT-BEKR                           
082601     ELSE                                                                 
082701        INSPECT MID-IDFAKT-UT REPLACING LEADING SPACE BY ZERO             
082801        MOVE MID-IDFAKT-UT  TO WS-IDFAKT                                  
082901     END-IF                                                               
083001                                                                          
083101     IF WS-IDFAKT = ZERO                                                  
083201        PERFORM BA-FIXA-FAKTURANR                                         
083301        MOVE JA TO WS-FIKT-FAKT-ANV                                       
083401     END-IF                                                               
083501     IF WS-IDFAKT NUMERIC AND WS-IDFAKT > ZERO                            
083601         CONTINUE                                                         
083701     ELSE                                                                 
083801         MOVE NEJ           TO NYCKLAR-SW                                 
083901     END-IF                                                               
084001                                                                          
084101     IF GODK-MID                                                          
084201     OR NYCKLAR-OK                                                        
084301        MOVE WS-IDFAKT       TO MOD-IDFAKT-UT                             
084401        MOVE WS-IDORDNR      TO MOD-IDORDNR-UT                            
084501        MOVE WS-IDKUNDNR     TO MOD-IDKUNDNR-UT                           
084601        MOVE WS-IDKOLLI      TO MOD-IDKOLLI-UT                            
084701        MOVE SEND-WS-IDDC    TO MOD-IDDC-SEND-UT                          
084801        MOVE REC-WS-IDDC     TO MOD-IDDC-REC-UT                           
084901        INSPECT MOD-IDFAKT-UT REPLACING LEADING ZERO BY SPACE             
085001        INSPECT MOD-IDORDNR-UT REPLACING LEADING ZERO BY SPACE            
085101        INSPECT MOD-IDKUNDNR-UT REPLACING LEADING ZERO BY SPACE           
085201        INSPECT MOD-IDKOLLI-UT REPLACING LEADING ZERO BY SPACE            
085301     ELSE                                                                 
085401        MOVE MFS-RENSA-FAELT TO MOD-IDFAKT-UT                             
085501                                MOD-IDORDNR-UT                            
085601                                MOD-IDKUNDNR-UT                           
085701                                MOD-IDKOLLI-UT                            
085801                                MOD-IDDC-SEND-UT                          
085901                                MOD-FLSLUT-BEKR                           
086001     END-IF                                                               
086101                                                                          
086201     IF NYCKLAR-FEL                                                       
086301       MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                                 
086401       CALL WMEDKONV USING MED-WMEDAREA                                   
086501       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
086601       PERFORM MFS-RENSA-FAELT-IN                                         
086701     END-IF                                                               
086801     .                                                                    
086901     EJECT                                                                
087001 BA-FIXA-FAKTURANR SECTION.                                               
087101     PERFORM DB2-SELECT-TP6FAKT                                           
087201     IF LINES-MISSING  OR TABELL-TOM                                      
087301       MOVE 1 TO RAD-IDFAKT                                               
087401       PERFORM DB2-INSERT-TP6FAKT                                         
087501     ELSE                                                                 
087601       IF RAD-IDFAKT > 98                                                 
087701         MOVE 1 TO RAD-IDFAKT                                             
087801       ELSE                                                               
087901         COMPUTE RAD-IDFAKT = RAD-IDFAKT + 1                              
088001       END-IF                                                             
088101       PERFORM DB2-UPDATE-TP6FAKT                                         
088201     END-IF                                                               
088301     MOVE RAD-IDFAKT TO WS-IDFAKT                                         
088401                        W-IDFAKT                                          
088501     .                                                                    
088601     EJECT                                                                
088701 C-FOERSTA-SIDA SECTION.                                                  
088801                                                                          
088901     PERFORM MFS-RENSA-FAELT-IN                                           
089001     .                                                                    
090001     EJECT                                                                
091001 D-NAESTA-SIDA SECTION.                                                   
092001                                                                          
093001     PERFORM MFS-RENSA-FAELT-IN                                           
093101     .                                                                    
093201     EJECT                                                                
093301 E-SAMMA-SIDA SECTION.                                                    
093401                                                                          
093501     IF EGEN-MID OR HELP-MID                                              
093601       IF MID-INPUT = ALL '+'                                             
093701         PERFORM MFS-RENSA-FAELT-IN                                       
093801       ELSE                                                               
093901         MOVE INF-PRESS-PF11 TO MED-IDMFSINF                              
094001         CALL WMEDKONV USING MED-WMEDAREA                                 
094101         MOVE MED-MFSINF TO MOD-TEMFSINF                                  
094201         PERFORM EA-MID-INDATA-TILL-MOD                                   
094301       END-IF                                                             
094401     ELSE                                                                 
094501       PERFORM MFS-RENSA-FAELT-IN                                         
094601     END-IF                                                               
094701     .                                                                    
094801     EJECT                                                                
094901 EA-MID-INDATA-TILL-MOD SECTION.                                          
095001                                                                          
095101     MOVE MFS-RENSA-FAELT TO MOD-FLSLUT-BEKR                              
095201                                                                          
095301     IF MID-IDUSER-003 = ALL '+'                                          
095401        MOVE MFS-RENSA-FAELT TO MOD-IDUSER-003                            
095501     ELSE                                                                 
095601        MOVE MID-IDUSER-003 TO MOD-IDUSER-003                             
095701        MOVE MFS-ADD-LAES-IN-FAELT                                        
095801                            TO MOD-IDUSER-003-ATTR                        
095901     END-IF                                                               
096001                                                                          
096101     IF MID-KOLLI-KLAR = ALL '+'                                          
096201        MOVE MFS-RENSA-FAELT TO MOD-KOLLI-KLAR                            
096301     ELSE                                                                 
096401        MOVE MID-KOLLI-KLAR TO MOD-KOLLI-KLAR                             
096501        MOVE MFS-ADD-LAES-IN-FAELT                                        
096601                            TO MOD-KOLLI-KLAR-ATTR                        
096701     END-IF                                                               
096801                                                                          
096901     MOVE +1 TO RAD-IX                                                    
097001     PERFORM UNTIL RAD-IX > MAX-RAD                                       
097101                                                                          
097201         IF  MID-IDARTNR(RAD-IX) = ALL '+'                                
097301             MOVE MFS-RENSA-FAELT TO MOD-IDARTNR(RAD-IX)                  
097401         ELSE                                                             
097501             MOVE MID-IDARTNR(RAD-IX) TO MOD-IDARTNR(RAD-IX)              
097601             MOVE MFS-ADD-LAES-IN-FAELT                                   
097701                                TO MOD-IDARTNR-ATTR(RAD-IX)               
097801             INSPECT MOD-IDARTNR(RAD-IX) REPLACING LEADING ZERO           
097901                                         BY SPACE                         
098001         END-IF                                                           
098101                                                                          
098201         IF  MID-KVANTMOT(RAD-IX) = ALL '+'                               
098301             MOVE MFS-RENSA-FAELT TO MOD-KVANTMOT(RAD-IX)                 
098401         ELSE                                                             
098501             MOVE MID-KVANTMOT(RAD-IX) TO MOD-KVANTMOT(RAD-IX)            
098601             MOVE MFS-ADD-LAES-IN-FAELT                                   
098701                                TO MOD-KVANTMOT-ATTR(RAD-IX)              
098801             INSPECT MOD-KVANTMOT(RAD-IX) REPLACING LEADING ZERO          
098901                                         BY SPACE                         
099001         END-IF                                                           
099101                                                                          
099201         IF  MID-KVSKROT(RAD-IX) = ALL '+'                                
099301             MOVE MFS-RENSA-FAELT TO MOD-KVSKROT(RAD-IX)                  
099401         ELSE                                                             
099501             MOVE MID-KVSKROT(RAD-IX) TO MOD-KVSKROT(RAD-IX)              
099601             MOVE MFS-ADD-LAES-IN-FAELT                                   
099701                                TO MOD-KVSKROT-ATTR(RAD-IX)               
099801             INSPECT MOD-KVSKROT(RAD-IX) REPLACING LEADING ZERO           
099901                                         BY SPACE                         
100001         END-IF                                                           
100101                                                                          
100201         IF  MID-ADLAGOMR(RAD-IX) = ALL '+'                               
100301             MOVE MFS-RENSA-FAELT TO MOD-ADLAGOMR(RAD-IX)                 
100401         ELSE                                                             
100501             MOVE MID-ADLAGOMR(RAD-IX) TO MOD-ADLAGOMR(RAD-IX)            
100601             MOVE MFS-ADD-LAES-IN-FAELT                                   
100701                                TO MOD-ADLAGOMR-ATTR(RAD-IX)              
100801             INSPECT MOD-ADLAGOMR(RAD-IX) REPLACING LEADING ZERO          
100901                                         BY SPACE                         
101001         END-IF                                                           
101101                                                                          
101201         IF  MID-ADGANG(RAD-IX) = ALL '+'                                 
101301             MOVE MFS-RENSA-FAELT TO MOD-ADGANG(RAD-IX)                   
101401         ELSE                                                             
101501             MOVE MID-ADGANG(RAD-IX) TO MOD-ADGANG(RAD-IX)                
101601             MOVE MFS-ADD-LAES-IN-FAELT                                   
101701                                TO MOD-ADGANG-ATTR(RAD-IX)                
101801             INSPECT MOD-ADGANG(RAD-IX) REPLACING LEADING ZERO            
101901                                        BY SPACE                          
102001         END-IF                                                           
102101                                                                          
102201         IF  MID-ADPLATS(RAD-IX) = ALL '+'                                
102301             MOVE MFS-RENSA-FAELT TO MOD-ADPLATS(RAD-IX)                  
102401         ELSE                                                             
102501             MOVE MID-ADPLATS(RAD-IX) TO MOD-ADPLATS(RAD-IX)              
102601             MOVE MFS-ADD-LAES-IN-FAELT                                   
102701                                TO MOD-ADPLATS-ATTR(RAD-IX)               
102801             INSPECT MOD-ADPLATS(RAD-IX) REPLACING LEADING ZERO           
102901                                         BY SPACE                         
103001         END-IF                                                           
103101                                                                          
103201         ADD +1 TO RAD-IX                                                 
103301     END-PERFORM                                                          
103401     .                                                                    
103501     EJECT                                                                
103601 G-KOLLA-INPUT SECTION.                                                   
103701                                                                          
103801     MOVE +1 TO TAB-IX                                                    
103901     PERFORM UNTIL TAB-IX > TAB-IX-MAX                                    
104001        MOVE ZERO TO WS-ADLAGOMR(TAB-IX)                                  
104101                     WS-ADGANG(TAB-IX)                                    
104201                     WS-ADPLATS(TAB-IX)                                   
104301        ADD +1 TO TAB-IX                                                  
104401     END-PERFORM                                                          
104501                                                                          
104601     MOVE JA TO INDATA-SW                                                 
104701     MOVE REC-WS-IDDC TO W-IDDC-B6                                        
104801     PERFORM IMS-GU-WDB601-REC                                            
104901     MOVE REC-DCS-FLINLREP TO SPAR-FLINLREP                               
105001                                                                          
105101     IF MID-INPUT = ALL '+'                                               
105201       MOVE ERR-PF11-AND-NO-DATA TO MED-IDMFSFEL                          
105301       CALL WMEDKONV USING MED-WMEDAREA                                   
105401       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
105501       PERFORM MFS-ROER-EJ-FAELT-IN                                       
105601       MOVE MFS-RENSA-FAELT TO MOD-FLSLUT-BEKR                            
105701       MOVE NEJ TO INDATA-SW                                              
105801     ELSE                                                                 
105901       IF SEND-DCS-CDC OR SEND-DCS-DDC OR SEND-DCS-NDC-CN                 
106001       OR SEND-DCS-NDC-PF OR SEND-DCS-SDC OR SEND-DCS-NDC-OTHERS          
106101       OR (SEND-DCS-USA AND REC-DCS-CDC)                                  
106201         MOVE IDFAKT-WS TO W-IDFAKT                                       
106301         MOVE REC-WS-IDDC TO W-6301-IDDC                                  
106401         PERFORM IMS-GHU-WL630111                                         
106501         IF SEGMENT-FINNS                                                 
106601            MOVE 6302-TIFAKT   TO WS-TIFAKT                               
106701            MOVE 6302-IDLEVNR  TO WS-IDLEVNR                              
106801            MOVE 6302-IDDISTR  TO WS-6302-IDDISTR                         
106901                                  WS-IDDISTR                              
107001            MOVE 6302-IDKUNDNR TO WS-6302-IDKUNDNR                        
107101            MOVE 6302-IDDC-LEV TO WS-6302-IDDC-LEV                        
107201            IF 6302-IDDC-SEND = SEND-WS-IDDC                              
107301               CONTINUE                                                   
107401            ELSE                                                          
107501               MOVE NEJ TO INDATA-SW                                      
107601               MOVE INF-URVAL-SAKNAS TO MED-IDMFSINF                      
107701               CALL WMEDKONV USING MED-WMEDAREA                           
107801               MOVE MED-MFSINF TO MOD-TEMFSINF                            
107901            END-IF                                                        
108001         ELSE                                                             
108101            IF WS-IDFAKT < 100                                            
108201              MOVE ZERO  TO WS-TIFAKT                                     
108301              MOVE SPACE TO WS-IDLEVNR                                    
108401              MOVE ZERO  TO WS-6302-IDDISTR                               
108501                            WS-IDDISTR                                    
108601              MOVE ZERO  TO WS-6302-IDKUNDNR                              
108701              MOVE SPACE TO WS-6302-IDDC-LEV                              
108801              MOVE JA           TO WS-FIKT-FAKT-ANV                       
108901            ELSE                                                          
109001              MOVE INF-URVAL-SAKNAS TO MED-IDMFSINF                       
109101              CALL WMEDKONV USING MED-WMEDAREA                            
109201              MOVE MED-MFSINF TO MOD-TEMFSINF                             
109301              MOVE NEJ TO INDATA-SW                                       
109401            END-IF                                                        
109501         END-IF                                                           
109601       END-IF                                                             
109701                                                                          
109801       IF MID-KOLLI-KLAR = ALL '+' OR SPACE                               
109901          MOVE MFS-ALFA-FAELT-FEL TO MOD-KOLLI-KLAR-ATTR                  
110001          MOVE MFS-RENSA-FAELT TO MOD-FLSLUT-BEKR                         
110101          MOVE NEJ TO INDATA-SW                                           
110201       ELSE                                                               
110301***************                                                           
110401          IF SEND-DCS-NDC-PF OR SEND-DCS-NDC-OTHERS                       
110501             IF MID-KOLLI-KLAR = 'J' OR 'X' OR 'Y'                        
110601                MOVE MFS-ALFA-FAELT-RAETT TO MOD-KOLLI-KLAR-ATTR          
110701             ELSE                                                         
110801                MOVE MFS-ALFA-FAELT-FEL TO MOD-KOLLI-KLAR-ATTR            
110901                MOVE MFS-RENSA-FAELT TO MOD-FLSLUT-BEKR                   
111001                MOVE NEJ TO INDATA-SW                                     
111101             END-IF                                                       
111201***************                                                           
111301          ELSE                                                            
111401             IF MID-KOLLI-KLAR = 'X' OR 'Y' OR 'J' OR 'N'                 
111501                MOVE MFS-ALFA-FAELT-RAETT TO MOD-KOLLI-KLAR-ATTR          
111601                IF MID-KOLLI-KLAR = 'X' OR 'Y' OR 'J'                     
111701                   MOVE MFS-RENSA-FAELT TO MOD-FLSLUT-BEKR                
111801                ELSE                                                      
111901                   IF MID-KOLLI-KLAR = 'N'                                
112001                      IF MID-FLSLUT-BEKR = 'N'                            
112101                         MOVE MFS-RENSA-FAELT TO MOD-FLSLUT-BEKR          
112201                      ELSE                                                
112301                         MOVE 'N' TO MOD-FLSLUT-BEKR                      
112401                         MOVE NEJ TO INDATA-SW                            
112501                         MOVE INF-GODKANN-INTE-SLUT                       
112601                                        TO MOD-TEMFSINF                   
112701                      END-IF                                              
112801                   END-IF                                                 
112901                END-IF                                                    
113001             ELSE                                                         
113101                MOVE MFS-ALFA-FAELT-FEL TO MOD-KOLLI-KLAR-ATTR            
113201                MOVE MFS-RENSA-FAELT TO MOD-FLSLUT-BEKR                   
113301                MOVE NEJ TO INDATA-SW                                     
113401             END-IF                                                       
113501          END-IF                                                          
113601       END-IF                                                             
113701                                                                          
113801       MOVE NEJ TO WS-RAD-IFYLLD                                          
113901       MOVE +1 TO RAD-IX                                                  
114001       PERFORM UNTIL RAD-IX > MAX-RAD                                     
114101                                                                          
114201         IF MID-IDARTNR(RAD-IX) NOT = ALL '+'                             
114301           INSPECT MID-IDARTNR(RAD-IX) REPLACING LEADING SPACE            
114401                                       BY ZERO                            
114501           IF MID-IDARTNR(RAD-IX) NOT NUMERIC                             
114601             MOVE MFS-ALFA-FAELT-FEL TO MOD-IDARTNR-ATTR(RAD-IX)          
114701             MOVE NEJ TO INDATA-SW                                        
114801           ELSE                                                           
114901             MOVE JA TO WS-RAD-IFYLLD                                     
115001             MOVE MFS-ALFA-FAELT-RAETT                                    
115101                                TO MOD-IDARTNR-ATTR(RAD-IX)               
115201             MOVE MID-IDARTNR(RAD-IX) TO WS-IDARTNR                       
115301             IF WS-IDARTNR = ZERO                                         
115401                MOVE MFS-ALFA-FAELT-FEL                                   
115501                                TO MOD-IDARTNR-ATTR(RAD-IX)               
115601                 MOVE NEJ TO INDATA-SW                                    
115701             END-IF                                                       
115801           END-IF                                                         
115901         END-IF                                                           
116001                                                                          
116101         IF MID-KVANTMOT(RAD-IX) NOT = ALL '+'                            
116201           INSPECT MID-KVANTMOT(RAD-IX) REPLACING LEADING SPACE           
116301                                       BY ZERO                            
116401           IF MID-KVANTMOT(RAD-IX) NOT NUMERIC                            
116501             MOVE MFS-ALFA-FAELT-FEL TO MOD-KVANTMOT-ATTR(RAD-IX)         
116601             MOVE NEJ TO INDATA-SW                                        
116701           ELSE                                                           
116801             IF MID-KVANTMOT(RAD-IX) = ZERO                               
116901                MOVE MFS-ALFA-FAELT-FEL                                   
117001                                TO MOD-KVANTMOT-ATTR(RAD-IX)              
117101                MOVE NEJ TO INDATA-SW                                     
117201             ELSE                                                         
117301                MOVE MFS-ALFA-FAELT-RAETT                                 
117401                                TO MOD-KVANTMOT-ATTR(RAD-IX)              
117501             END-IF                                                       
117601           END-IF                                                         
117701         END-IF                                                           
117801                                                                          
117901         IF MID-KVSKROT(RAD-IX) NOT = ALL '+'                             
118001           INSPECT MID-KVSKROT(RAD-IX) REPLACING LEADING SPACE            
118101                                       BY ZERO                            
118201           IF MID-KVSKROT(RAD-IX) NOT NUMERIC                             
118301             MOVE MFS-ALFA-FAELT-FEL TO MOD-KVSKROT-ATTR(RAD-IX)          
118401             MOVE NEJ TO INDATA-SW                                        
118501           ELSE                                                           
118601             IF MID-KVSKROT(RAD-IX) = ZERO                                
118701                MOVE MFS-ALFA-FAELT-FEL                                   
118801                                TO MOD-KVSKROT-ATTR(RAD-IX)               
118901                MOVE NEJ TO INDATA-SW                                     
119001             ELSE                                                         
119101                MOVE MFS-ALFA-FAELT-RAETT                                 
119201                                TO MOD-KVSKROT-ATTR(RAD-IX)               
119301             END-IF                                                       
119401           END-IF                                                         
119501         END-IF                                                           
119601                                                                          
119701         IF  MID-KVANTMOT(RAD-IX) = ALL '+'                               
119801         AND MID-KVSKROT(RAD-IX)= ALL '+'                                 
119901           IF MID-IDARTNR(RAD-IX) = ALL '+'                               
120001              CONTINUE                                                    
120101           ELSE                                                           
120201              MOVE MFS-ALFA-FAELT-FEL                                     
120301                                TO MOD-IDARTNR-ATTR(RAD-IX)               
120401              MOVE NEJ TO INDATA-SW                                       
120501           END-IF                                                         
120601         END-IF                                                           
120701                                                                          
120801         IF MID-ADLAGOMR(RAD-IX) NOT = ALL '+'                            
120901           INSPECT MID-ADLAGOMR(RAD-IX) REPLACING                         
121001                   LEADING SPACE BY ZERO                                  
121101           IF MID-ADLAGOMR(RAD-IX) NOT NUMERIC                            
121201             MOVE MFS-ALFA-FAELT-FEL TO MOD-ADLAGOMR-ATTR(RAD-IX)         
121301             MOVE NEJ TO INDATA-SW                                        
121401           ELSE                                                           
121501             MOVE MFS-ALFA-FAELT-RAETT                                    
121601                            TO MOD-ADLAGOMR-ATTR(RAD-IX)                  
121701             MOVE MID-ADLAGOMR(RAD-IX) TO WS-ADLAGOMR(RAD-IX)             
121801           END-IF                                                         
121901         END-IF                                                           
122001                                                                          
122101         IF MID-ADGANG(RAD-IX) NOT = ALL '+'                              
122201           INSPECT MID-ADGANG(RAD-IX) REPLACING                           
122301                   LEADING SPACE BY ZERO                                  
122401           IF MID-ADGANG(RAD-IX) NOT NUMERIC                              
122501             MOVE MFS-ALFA-FAELT-FEL TO MOD-ADGANG-ATTR(RAD-IX)           
122601             MOVE NEJ TO INDATA-SW                                        
122701           ELSE                                                           
122801             MOVE MFS-ALFA-FAELT-RAETT TO MOD-ADGANG-ATTR(RAD-IX)         
122901             MOVE MID-ADGANG(RAD-IX) TO WS-ADGANG(RAD-IX)                 
123001           END-IF                                                         
123101         END-IF                                                           
123201                                                                          
123301         IF MID-ADPLATS(RAD-IX) NOT = ALL '+'                             
123401           INSPECT MID-ADPLATS(RAD-IX) REPLACING                          
123501                   LEADING SPACE BY ZERO                                  
123601           IF MID-ADPLATS(RAD-IX) NOT NUMERIC                             
123701             MOVE MFS-ALFA-FAELT-FEL TO MOD-ADPLATS-ATTR(RAD-IX)          
123801             MOVE NEJ TO INDATA-SW                                        
123901           ELSE                                                           
124001             MOVE MFS-ALFA-FAELT-RAETT TO MOD-ADPLATS-ATTR(RAD-IX)        
124101             MOVE MID-ADPLATS(RAD-IX) TO WS-ADPLATS(RAD-IX)               
124201           END-IF                                                         
124301         END-IF                                                           
124401                                                                          
124501*-- TESTA OM DET ÄR EN STUDSFAKTURA CN-TO-US DISTR 914X                   
124601*-- MAN TAR EMOT PÅ FAKTURA 2 I USA, DC-SEND=11                           
124701         IF INDATA-OK AND (RAD-IX = +1)                                   
124801         AND MID-IDARTNR(RAD-IX) NOT = ALL '+'                            
124901           IF REC-DCS-NDC-NA AND REC-DCS-USA                              
125001             MOVE WS-IDARTNR  TO W-IDARTNR                                
125101             MOVE REC-WS-IDDC TO W-IDDC                                   
125201             PERFORM IMS-GET-WDK711                                       
125301             IF SEGMENT-FINNS                                             
125401               MOVE SLAG-IDDC-REF  TO WS-IDDC                             
125501               IF NDC-CN                                                  
125601                 IF SLAG-IDDC-REF = WS-6302-IDDC-LEV                      
125701                   CONTINUE                                               
125801                 ELSE                                                     
125901                   MOVE MFS-ALFA-FAELT-FEL                                
126001                                 TO MOD-IDARTNR-ATTR(RAD-IX)              
126101                   MOVE NEJ TO INDATA-SW                                  
126201                   MOVE INF-ART-FRAN-ANNAT-FTG TO MED-IDMFSINF            
126301                   CALL WMEDKONV USING MED-WMEDAREA                       
126401                   MOVE MED-MFSINF TO MOD-TEMFSINF                        
126501                 END-IF                                                   
126601               ELSE                                                       
126701                 IF WS-6302-IDDC-LEV = SPACE                              
126801                   CONTINUE                                               
126901                 ELSE                                                     
127001                   MOVE MFS-ALFA-FAELT-FEL                                
127101                                 TO MOD-IDARTNR-ATTR(RAD-IX)              
127201                   MOVE NEJ TO INDATA-SW                                  
127301                   MOVE INF-ART-FRAN-ANNAT-FTG TO MED-IDMFSINF            
127401                   CALL WMEDKONV USING MED-WMEDAREA                       
127501                   MOVE MED-MFSINF TO MOD-TEMFSINF                        
127601                 END-IF                                                   
127701               END-IF                                                     
127801             END-IF                                                       
127901           END-IF                                                         
128001                                                                          
128101           IF REC-DCS-CDC                                                 
128201             MOVE WS-IDARTNR  TO W-IDARTNR                                
128301             PERFORM IMS-GHU-WLARTC11                                     
128401             IF SEGMENT-FINNS                                             
128501               IF CLAG-IDDC-REF = SPACE                                   
128601                 IF SEND-DCS-NDC-CN OR SEND-DCS-USA                       
128701                   MOVE MFS-ALFA-FAELT-FEL                                
128801                                 TO MOD-IDARTNR-ATTR(RAD-IX)              
128901                   MOVE NEJ TO INDATA-SW                                  
129001                   MOVE INF-ART-FRAN-ANNAT-FTG TO MED-IDMFSINF            
129101                   CALL WMEDKONV USING MED-WMEDAREA                       
129201                   MOVE MED-MFSINF TO MOD-TEMFSINF                        
129301                 END-IF                                                   
129401               ELSE                                                       
129501                 MOVE CLAG-IDDC-REF  TO WS-IDDC                           
129601                 IF NDC-CN                                                
129701                   IF SEND-DCS-NDC-CN                                     
129801                     CONTINUE                                             
129901                   ELSE                                                   
130001                     MOVE MFS-ALFA-FAELT-FEL                              
130101                                   TO MOD-IDARTNR-ATTR(RAD-IX)            
130201                     MOVE NEJ TO INDATA-SW                                
130301                     MOVE INF-ART-FRAN-ANNAT-FTG TO MED-IDMFSINF          
130401                     CALL WMEDKONV USING MED-WMEDAREA                     
130501                     MOVE MED-MFSINF TO MOD-TEMFSINF                      
130601                   END-IF                                                 
130701                 END-IF                                                   
130801                 IF NDC-US                                                
130901                   IF SEND-DCS-USA                                        
131001                     CONTINUE                                             
131101                   ELSE                                                   
131201                     MOVE MFS-ALFA-FAELT-FEL                              
131301                                   TO MOD-IDARTNR-ATTR(RAD-IX)            
131401                     MOVE NEJ TO INDATA-SW                                
131501                     MOVE INF-ART-FRAN-ANNAT-FTG TO MED-IDMFSINF          
131601                     CALL WMEDKONV USING MED-WMEDAREA                     
131701                     MOVE MED-MFSINF TO MOD-TEMFSINF                      
131801                   END-IF                                                 
131901                 END-IF                                                   
132001               END-IF                                                     
132101             END-IF                                                       
132201           END-IF                                                         
132301         END-IF                                                           
132401                                                                          
132501         IF INDATA-OK                                                     
132601         AND MID-IDARTNR(RAD-IX) NOT = ALL '+'                            
132701            IF  (MID-ADLAGOMR(RAD-IX) = ALL '+' OR SPACE)                 
132801            AND (MID-ADGANG(RAD-IX)   = ALL '+' OR SPACE)                 
132901            AND (MID-ADPLATS(RAD-IX)  = ALL '+' OR SPACE)                 
133001                                                                          
133101               MOVE WS-IDARTNR  TO W-IDARTNR                              
133201               MOVE REC-WS-IDDC TO W-IDDC                                 
133301               IF REC-DCS-CDC                                             
133401                  PERFORM IMS-GHU-WLARTC11                                
133501                  IF SEGMENT-FINNS                                        
133601                     IF CLAG-ADLAGOMR = ZERO                              
133701                     AND CLAG-ADGANG = ZERO                               
133801                     AND CLAG-ADPLATS = ZERO                              
133901                        MOVE MFS-ALFA-FAELT-FEL                           
134001                                 TO MOD-ADLAGOMR-ATTR(RAD-IX)             
134101                                    MOD-ADGANG-ATTR(RAD-IX)               
134201                                    MOD-ADPLATS-ATTR(RAD-IX)              
134301                        MOVE NEJ TO INDATA-SW                             
134401                     END-IF                                               
134501                  ELSE                                                    
134601                     MOVE MFS-ALFA-FAELT-FEL                              
134701                                 TO MOD-ADLAGOMR-ATTR(RAD-IX)             
134801                                    MOD-ADGANG-ATTR(RAD-IX)               
134901                                    MOD-ADPLATS-ATTR(RAD-IX)              
135001                     MOVE NEJ    TO INDATA-SW                             
135101                  END-IF                                                  
135201               ELSE                                                       
135301                  PERFORM IMS-GET-WDK711                                  
135401                  IF SEGMENT-FINNS                                        
135501                     IF SLAG-ADLAGOMR = ZERO                              
135601                     AND SLAG-ADGANG = ZERO                               
135701                     AND SLAG-ADPLATS = ZERO                              
135801                        MOVE MFS-ALFA-FAELT-FEL                           
135901                                 TO MOD-ADLAGOMR-ATTR(RAD-IX)             
136001                                    MOD-ADGANG-ATTR(RAD-IX)               
136101                                    MOD-ADPLATS-ATTR(RAD-IX)              
136201                        MOVE NEJ TO INDATA-SW                             
136301                     END-IF                                               
136401                  ELSE                                                    
136501                     MOVE MFS-ALFA-FAELT-FEL                              
136601                                 TO MOD-ADLAGOMR-ATTR(RAD-IX)             
136701                                    MOD-ADGANG-ATTR(RAD-IX)               
136801                                    MOD-ADPLATS-ATTR(RAD-IX)              
136901                     MOVE NEJ    TO INDATA-SW                             
137001                  END-IF                                                  
137101               END-IF                                                     
137201            ELSE                                                          
137301               MOVE WS-IDARTNR   TO W-IDARTNR                             
137401               MOVE REC-WS-IDDC  TO W-IDDC                                
137501               IF REC-DCS-CDC                                             
137601                  PERFORM IMS-GHU-WLARTC11                                
137701                  IF SEGMENT-FINNS                                        
137801                     IF CLAG-ADLAGOMR = ZERO                              
137901                     AND CLAG-ADGANG = ZERO                               
138001                     AND CLAG-ADPLATS = ZERO                              
138101                        MOVE MFS-ALFA-FAELT-RAETT                         
138201                                 TO MOD-ADLAGOMR-ATTR(RAD-IX)             
138301                                    MOD-ADGANG-ATTR(RAD-IX)               
138401                                    MOD-ADPLATS-ATTR(RAD-IX)              
138501                     ELSE                                                 
138601                        MOVE MFS-ALFA-FAELT-FEL                           
138701                                 TO MOD-ADLAGOMR-ATTR(RAD-IX)             
138801                                    MOD-ADGANG-ATTR(RAD-IX)               
138901                                    MOD-ADPLATS-ATTR(RAD-IX)              
139001                        MOVE NEJ TO INDATA-SW                             
139101                     END-IF                                               
139201                  END-IF                                                  
139301               ELSE                                                       
139401                  PERFORM IMS-GET-WDK711                                  
139501                  IF SEGMENT-FINNS                                        
139601                     IF SLAG-ADLAGOMR = ZERO                              
139701                     AND SLAG-ADGANG = ZERO                               
139801                     AND SLAG-ADPLATS = ZERO                              
139901                        MOVE MFS-ALFA-FAELT-RAETT                         
140001                                 TO MOD-ADLAGOMR-ATTR(RAD-IX)             
140101                                    MOD-ADGANG-ATTR(RAD-IX)               
140201                                    MOD-ADPLATS-ATTR(RAD-IX)              
140301                     ELSE                                                 
140401                        MOVE MFS-ALFA-FAELT-FEL                           
140501                                 TO MOD-ADLAGOMR-ATTR(RAD-IX)             
140601                                    MOD-ADGANG-ATTR(RAD-IX)               
140701                                    MOD-ADPLATS-ATTR(RAD-IX)              
140801                        MOVE NEJ TO INDATA-SW                             
140901                     END-IF                                               
141001                  END-IF                                                  
141101               END-IF                                                     
141201            END-IF                                                        
141301                                                                          
141401         END-IF                                                           
141501                                                                          
141601         IF INDATA-OK                                                     
141701            MOVE WS-IDARTNR TO W-IDARTNR                                  
141801            PERFORM IMS-GU-WLARTC01                                       
141901            IF SEGMENT-FINNS                                              
142001               IF K6-ART-KDERS-UTG = ZERO                                 
142101                  CONTINUE                                                
142201               ELSE                                                       
142301                  MOVE MFS-ALFA-FAELT-FEL                                 
142401                                TO MOD-IDARTNR-ATTR(RAD-IX)               
142501                  MOVE NEJ TO INDATA-SW                                   
142601               END-IF                                                     
142701            ELSE                                                          
142801               MOVE MFS-ALFA-FAELT-FEL                                    
142901                                TO MOD-IDARTNR-ATTR(RAD-IX)               
143001               MOVE NEJ TO INDATA-SW                                      
143101            END-IF                                                        
143201         END-IF                                                           
143301                                                                          
143401         ADD +1 TO RAD-IX                                                 
143501       END-PERFORM                                                        
143601                                                                          
143701     END-IF                                                               
143801                                                                          
143901     IF MID-KOLLI-KLAR = 'X' OR 'Y' OR 'J'                                
144001        IF MID-IDUSER-003 = ALL '+' OR SPACE                              
144101           IF REC-DCS-FLBINNUT = JA                                       
144201              MOVE MFS-ALFA-FAELT-FEL TO MOD-IDUSER-003-ATTR              
144301              MOVE NEJ TO INDATA-SW                                       
144401           ELSE                                                           
144501              MOVE SPACE TO WS-IDUSER-003                                 
144601           END-IF                                                         
144701        ELSE                                                              
144801           MOVE MID-IDUSER-003 TO WS-IDUSER-003                           
144901        END-IF                                                            
145001     ELSE                                                                 
145101        IF MID-IDUSER-003 = ALL '+' OR SPACE                              
145201           MOVE SPACE TO WS-IDUSER-003                                    
145301        ELSE                                                              
145401           MOVE MFS-ALFA-FAELT-FEL TO MOD-IDUSER-003-ATTR                 
145501           MOVE NEJ TO INDATA-SW                                          
145601        END-IF                                                            
145701     END-IF                                                               
145801                                                                          
145901     IF INDATA-FEL                                                        
146001       MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                          
146101       CALL WMEDKONV USING MED-WMEDAREA                                   
146201       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
146301       PERFORM MFS-ROER-EJ-FAELT-IN                                       
146401     END-IF                                                               
146501     .                                                                    
146601     EJECT                                                                
146701 H-UPPDATERA SECTION.                                                     
146801                                                                          
146901     MOVE REC-WS-IDDC TO W-IDDC                                           
147001     MOVE NEJ         TO WS-A03-SKAPAD                                    
147101                                                                          
147201     MOVE +1 TO RAD-IX                                                    
147301     PERFORM UNTIL RAD-IX > MAX-RAD                                       
147401                                                                          
147501        MOVE ZERO TO WS-KVANTMOT                                          
147601                     WS-KVSKROT                                           
147701                                                                          
147801        IF MID-IDARTNR(RAD-IX) NOT = ALL '+'                              
147901        AND MID-IDARTNR(RAD-IX) NOT = SPACE                               
148001          INSPECT MID-IDARTNR(RAD-IX) REPLACING                           
148101                    LEADING SPACE BY ZERO                                 
148201          MOVE MID-IDARTNR(RAD-IX)  TO WS-IDARTNR                         
148301          MOVE WS-IDARTNR           TO W-IDARTNR                          
148401          IF MID-KVANTMOT(RAD-IX) NOT = ALL '+'                           
148501            INSPECT MID-KVANTMOT(RAD-IX) REPLACING                        
148601                    LEADING SPACE BY ZERO                                 
148701            MOVE MID-KVANTMOT(RAD-IX) TO WS-KVANTMOT                      
148801          END-IF                                                          
148901          IF MID-KVSKROT(RAD-IX) NOT = ALL '+'                            
149001            INSPECT MID-KVSKROT(RAD-IX) REPLACING                         
149101                    LEADING SPACE BY ZERO                                 
149201            MOVE MID-KVSKROT(RAD-IX) TO WS-KVSKROT                        
149301          END-IF                                                          
149401                                                                          
149501          COMPUTE WS-SUMMA-KVANT = WS-KVSKROT + WS-KVANTMOT               
149601                                                                          
149701          IF WS-IDDISTR = ZERO                                            
149801            PERFORM HCX-FIXA-DISTRIKT                                     
149901          END-IF                                                          
150001          MOVE WS-IDDISTR TO DIST35-IDDISTR                               
150101                                                                          
150201          IF REC-DCS-CDC                                                  
150301             PERFORM IMS-GHU-WLARTC11                                     
150401             IF SEGMENT-FINNS                                             
150501                PERFORM HE-UPPDATERA-BEF-ART-CDC                          
150601                MOVE NEJ TO WS-NY-ART                                     
150701             END-IF                                                       
150801          ELSE                                                            
150901             PERFORM IMS-GET-WDK711                                       
151001             IF SEGMENT-FINNS                                             
151101                PERFORM HA-UPPDATERA-BEF-ART                              
151201                MOVE NEJ TO WS-NY-ART                                     
151301             ELSE                                                         
151401                PERFORM HB-UPPDATERA-NY-ART                               
151501                MOVE JA TO WS-NY-ART                                      
151601             END-IF                                                       
151701          END-IF                                                          
151801          IF NDC-US AND DIST35-NDCCN-NDCUS-REFILL                         
151901            PERFORM HC-UPPDATERA-INLAGGNING                               
152001          ELSE                                                            
152101            PERFORM HC-UPPDATERA-INLAGGNING-PV                            
152201            IF REC-DCS-NDC-NA OR REC-DCS-NDC-PF OR                        
152301               REC-DCS-NDC-OTHERS                                         
152401               IF WS-NY-ART = JA                                          
152501                  PERFORM HD-SKAPA-REFILLTRANS                            
152601               END-IF                                                     
152701            END-IF                                                        
152801          END-IF                                                          
152901        END-IF                                                            
153001                                                                          
153101        ADD +1 TO RAD-IX                                                  
153201     END-PERFORM                                                          
153301                                                                          
153401     IF WS-A03-SKAPAD = JA                                                
153501        IF SEND-DCS-CDC OR SEND-DCS-NDC-PF OR SEND-DCS-DDC                
153601        OR SEND-DCS-SDC OR SEND-DCS-NDC-OTHERS                            
153701           CONTINUE                                                       
153801        ELSE                                                              
153901          IF WS-FIKT-FAKT-ANV = JA                                        
154001             IF MID-KOLLI-KLAR = 'X' OR 'Y' OR 'J'                        
154101                MOVE 'Y' TO EKOTRA03-FLSLUT                               
154201             END-IF                                                       
154301          END-IF                                                          
154401        END-IF                                                            
154501        PERFORM S041-SKRIV-A03                                            
154601     END-IF                                                               
154701                                                                          
154801     IF SEND-DCS-CDC OR SEND-DCS-NDC-PF OR SEND-DCS-DDC                   
154901     OR SEND-DCS-SDC OR SEND-DCS-NDC-CN OR SEND-DCS-NDC-OTHERS            
155001     OR (SEND-DCS-USA AND REC-DCS-CDC)                                    
155101        CONTINUE                                                          
155201     ELSE                                                                 
155301       IF WS-FIKT-FAKT-ANV = JA                                           
155401          IF MID-KOLLI-KLAR = 'X' OR 'Y' OR 'J'                           
155501             MOVE IDFAKT-WS TO W-IDFAKT                                   
155601             PERFORM IMS-GHU-WL630511                                     
155701             IF SEGMENT-FINNS                                             
155801                MOVE 'J' TO 6306-FLKLAR                                   
155901                PERFORM IMS-REPL-WL630511                                 
156001             END-IF                                                       
156101          END-IF                                                          
156201       END-IF                                                             
156301     END-IF                                                               
156401                                                                          
156501     MOVE INF-UPDATE-DONE TO MED-IDMFSINF                                 
156601     CALL WMEDKONV USING MED-WMEDAREA                                     
156701     MOVE MED-MFSINF TO MOD-TEMFSINF                                      
156801     PERFORM MFS-RENSA-FAELT-IN                                           
156901     .                                                                    
157001     EJECT                                                                
157101                                                                          
157201 HA-UPPDATERA-BEF-ART SECTION.                                            
157301                                                                          
157401     MOVE NEJ TO WS-UPPD-LOCB                                             
157501                                                                          
157601     IF MID-ADLAGOMR(RAD-IX) = ALL '+' OR SPACE                           
157701        MOVE SLAG-ADLAGOMR       TO WS-ADLAGOMR(RAD-IX)                   
157801     ELSE                                                                 
157901        MOVE WS-ADLAGOMR(RAD-IX) TO SLAG-ADLAGOMR                         
158001                                    W-ADLAGOMR-LOCB                       
158101        MOVE JA TO WS-UPPD-LOCB                                           
158201     END-IF                                                               
158301     IF MID-ADGANG(RAD-IX) = ALL '+' OR SPACE                             
158401        MOVE SLAG-ADGANG         TO WS-ADGANG(RAD-IX)                     
158501     ELSE                                                                 
158601        MOVE WS-ADGANG(RAD-IX)   TO SLAG-ADGANG                           
158701                                    W-ADGANG-LOCB                         
158801        MOVE JA TO WS-UPPD-LOCB                                           
158901     END-IF                                                               
159001     IF MID-ADPLATS(RAD-IX) = ALL '+' OR SPACE                            
159101        MOVE SLAG-ADPLATS        TO WS-ADPLATS(RAD-IX)                    
159201     ELSE                                                                 
159301        MOVE WS-ADPLATS(RAD-IX)  TO SLAG-ADPLATS                          
159401                                    W-ADPLATS-LOCB                        
159501        MOVE JA TO WS-UPPD-LOCB                                           
159601     END-IF                                                               
159701                                                                          
159801     MOVE SLAG-KVLS          TO WS-OLD-KVLS                               
159901     MOVE SLAG-KVEFRS        TO WS-OLD-KVEFRS                             
160001     IF WS-KVANTMOT > ZERO                                                
160101        ADD WS-KVANTMOT      TO SLAG-KVLS                                 
160201        MOVE WS-KVANTMOT     TO WS-RO-KVANT                               
160301        MOVE ZERO            TO W-KVAVIS                                  
160401                                                                          
160501        IF WS-UPPD-LOCB = JA                                              
160601           PERFORM S13-UPPDATERA-WDJ9                                     
160701        END-IF                                                            
160801        PERFORM IMS-REPL-WDK711                                           
160901        PERFORM HAA-FLYTTA-SALDOLOGG-DATA                                 
161001        IF REC-DCS-NDC-NA OR REC-DCS-NDC-PF OR REC-DCS-NDC-OTHERS         
161101           PERFORM S01-EV-RO-TACKNING                                     
161201        END-IF                                                            
161301     END-IF                                                               
161401     .                                                                    
161501     EJECT                                                                
161601                                                                          
161701 HAA-FLYTTA-SALDOLOGG-DATA SECTION.                                       
161801     PERFORM S11-FLYTTA-SALDOLOGG-DATA                                    
161901     MOVE SLAG-KVAKS-PAV    TO LOGG-KVAKS-PAV                             
162001     MOVE SLAG-KVEFRS       TO LOGG-KVEFRS                                
162101     PERFORM S12-ISRT-SALDOLOGG                                           
162201     .                                                                    
162301     EJECT                                                                
162401                                                                          
162501 HB-UPPDATERA-NY-ART SECTION.                                             
162601                                                                          
162701     MOVE ZERO TO W-KVAVIS                                                
162801                  WS-OLD-KVLS                                             
162901                  WS-OLD-KVEFRS                                           
163001                                                                          
163101     MOVE ALL '+'      TO WDK7-W005WDK7                                   
163201     MOVE 'WDK711'     TO WDK7-IDSEGM                                     
163301     MOVE W-IDARTNR    TO WDK7-IDARTNR-KFB                                
163401     MOVE W-IDDC       TO WDK7-IDDC-KFB                                   
163501                          WDK7-IDDC                                       
163601     MOVE WS-ADLAGOMR(RAD-IX)    TO WDK7-ADLAGOMR                         
163701     MOVE WS-ADGANG(RAD-IX)      TO WDK7-ADGANG                           
163801     MOVE WS-ADPLATS(RAD-IX)     TO WDK7-ADPLATS                          
163901     IF REC-DCS-NDC-NA OR REC-DCS-NDC-PF OR REC-DCS-NDC-OTHERS            
164001       IF NDC-US AND DIST35-NDCCN-NDCUS-REFILL                            
164101         CONTINUE                                                         
164201       ELSE                                                               
164301         MOVE JA                 TO WDK7-FLORDSP                          
164401                                    WDK7-FLSPBULK                         
164501       END-IF                                                             
164601     END-IF                                                               
164701                                                                          
164801     CALL W005WDK7 USING WDK7-W005WDK7 WDB6-PCB ARTC-PCB WDK7-PCB         
164901     MOVE WDK7-WDK711  TO SLAG-WDK711                                     
165001                                                                          
165101     MOVE WS-ADLAGOMR(RAD-IX)    TO W-ADLAGOMR-LOCB                       
165201     MOVE WS-ADGANG(RAD-IX)      TO W-ADGANG-LOCB                         
165301     MOVE WS-ADPLATS(RAD-IX)     TO W-ADPLATS-LOCB                        
165401     MOVE WS-KVANTMOT            TO SLAG-KVLS                             
165501                                                                          
165601     PERFORM S13-UPPDATERA-WDJ9                                           
165701     PERFORM S11-FLYTTA-SALDOLOGG-DATA                                    
165801     MOVE SLAG-KVAKS-PAV    TO LOGG-KVAKS-PAV                             
165901     MOVE SLAG-KVEFRS       TO LOGG-KVEFRS                                
166001     PERFORM S12-ISRT-SALDOLOGG                                           
166101     .                                                                    
166201     EJECT                                                                
166301                                                                          
166401 HC-UPPDATERA-INLAGGNING-PV SECTION.                                      
166501                                                                          
166601     IF WS-IDDISTR = ZERO                                                 
166701       PERFORM HCX-FIXA-DISTRIKT                                          
166801     END-IF                                                               
166901                                                                          
167001     PERFORM IMS-GU-WLARTC01                                              
167101     MOVE K6-ART-KDPRODSL   TO W-KDPRODSL                                 
167201     MOVE K6-ART-KDSORT     TO WS-KDSORT                                  
167301     MOVE K6-ART-IDFKNGRP   TO W-IDFKNGRP                                 
167401     PERFORM IMS-GHU-WLARTC11                                             
167501     MOVE CLAG-KDPSLLOC      TO W-KDPRODSL-LOC                            
167601     MOVE CLAG-PRARTSTD      TO WS-SAP-PRARTSTD                           
167701                                                                          
167801     IF SEND-DCS-CDC OR SEND-DCS-NDC-PF OR SEND-DCS-DDC                   
167901     OR SEND-DCS-SDC OR SEND-DCS-NDC-CN OR SEND-DCS-NDC-OTHERS            
168001     OR (SEND-DCS-USA AND REC-DCS-CDC)                                    
168101        IF REC-DCS-SDC                                                    
168201        OR REC-DCS-NDC-PF                                                 
168301        OR REC-DCS-NDC-OTHERS                                             
168401        OR REC-DCS-CDC                                                    
168501           MOVE ZERO                TO WS-FIXAD-PRARTNTO                  
168601           MOVE SPACE               TO WS-KDVALISO                        
168701           IF SEND-DCS-DDC                                                
168801              MOVE '1441 '          TO INL-IDLEVNR                        
168901           ELSE                                                           
169001              MOVE SEND-DCS-IDLEVNR-DC TO INL-IDLEVNR                     
169101           END-IF                                                         
169201           PERFORM S08-SKAPA-SDC-NDC-HIST-MOT-PV                          
169301           MOVE ZERO TO W-KVAVIS                                          
169401           PERFORM S14-SKAPA-SAP-TRANS-PV                                 
169501           IF SEND-DCS-DDC                                                
169601              PERFORM HCB-SKAPA-DIFF-TRANS                                
169701           END-IF                                                         
169801***        IF  SEND-DCS-CDC                                               
169901***        AND SEND-DCS-FLINLREP = JA                                     
170001              MOVE SPAR-FLINLREP TO FILC3-FLINLREP                        
170101              PERFORM HCC-SKAPA-LDC-TRANS                                 
170201***        END-IF                                                         
170301                                                                          
170401           IF (SEND-DCS-USA AND REC-DCS-CDC) OR                           
170501              (SEND-DCS-NDC-CN AND REC-DCS-CDC)                           
170601                                                                          
170701             PERFORM S02-PRIS-TILLAMPNING                                 
170801             PERFORM S03-SKAPA-LEVANM-TRANS                               
170901           END-IF                                                         
171001        END-IF                                                            
171101                                                                          
171201        IF REC-DCS-NDC-NA                                                 
171301           PERFORM S02-PRIS-TILLAMPNING                                   
171401           MOVE PRIS-PRARTNTO       TO WS-FIXAD-PRARTNTO                  
171501           MOVE 'SEK'               TO WS-KDVALISO                        
171601           IF PRIS-KDVALISO NOT = 'SEK' AND NOT = SPACE                   
171701             MOVE PRIS-KDVALISO     TO WS-KDVALISO                        
171801           END-IF                                                         
171901           IF SEND-DCS-DDC                                                
172001              MOVE '1441 '          TO INL-IDLEVNR                        
172101           ELSE                                                           
172201              MOVE SEND-DCS-IDLEVNR-DC TO INL-IDLEVNR                     
172301           END-IF                                                         
172401                                                                          
172501           PERFORM S08-SKAPA-SDC-NDC-HIST-MOT-PV                          
172601           PERFORM S04-SKAPA-EKOTRANS-A03                                 
172701           MOVE SPAR-FLINLREP TO FILC3-FLINLREP                           
172801           PERFORM HCC-SKAPA-LDC-TRANS                                    
172901                                                                          
173001           PERFORM IMS-GET-WDK711                                         
173101           MOVE SLAG-PRAVCOST       TO EKOTRA03-PRAVCOST-OLD              
173201           MOVE WS-KVANTMOT         TO AVG-KVANTMOT                       
173301                                       EKOTRA03-KVANTMOT                  
173401           MOVE WS-KVSKROT          TO EKOTRA03-KVSKROT                   
173501           ADD WS-KVSKROT           TO AVG-KVANTMOT                       
173601           MOVE '61'                TO EKOTRA03-KDANMORS                  
173701                                                                          
173801           PERFORM S03-SKAPA-LEVANM-TRANS                                 
173901                                                                          
174001           PERFORM S06-OMRAKN-MEDELPRIS-LAB                               
174101           MOVE W-TIME-N         TO AKTUELL-TID                           
174201           PERFORM S09-FIXA-LOKALTID                                      
174301           MOVE MSGI-TILOKDAT    TO SLAG-TIAVCOST                         
174401           MOVE AVG-PRAVCOST-NEW TO EKOTRA03-PRAVCOST                     
174501                                    SLAG-PRAVCOST                         
174601           MOVE AVG-REMARKUP     TO EKOTRA03-REMARKUP                     
174701           MOVE AVG-PRKURS       TO EKOTRA03-PRKURS                       
174801                                                                          
174901           IF WS-KVANTMOT > ZERO                                          
175001              PERFORM IMS-REPL-WDK711                                     
175101           ELSE                                                           
175201              MOVE EKOTRA03-PRAVCOST-OLD TO EKOTRA03-PRAVCOST             
175301           END-IF                                                         
175401        END-IF                                                            
175501     ELSE                                                                 
175601                                                                          
175701        IF (SEND-DCS-NDC-NA AND SEND-DCS-USA)                             
175801        AND (REC-DCS-NDC-NA AND REC-DCS-CANADA)                           
175901        OR (SEND-DCS-NDC-NA AND SEND-DCS-CANADA)                          
176001        AND (REC-DCS-NDC-NA AND REC-DCS-USA)                              
176101                                                                          
176201            MOVE SEND-WS-IDDC  TO W-IDDC                                  
176301            PERFORM IMS-GET-WDK711                                        
176401            MOVE SLAG-PRAVCOST TO WS-FIXAD-PRARTNTO                       
176501            IF SEND-DCS-NDC-NA AND SEND-DCS-CANADA                        
176601               MOVE 'CAD'      TO WS-KDVALISO                             
176701            ELSE                                                          
176801               MOVE 'USD'      TO WS-KDVALISO                             
176901            END-IF                                                        
177001                                                                          
177101            MOVE REC-WS-IDDC TO W-IDDC                                    
177201            PERFORM IMS-GET-WDK711                                        
177301            MOVE SEND-DCS-IDLEVNR-DC TO INL-IDLEVNR                       
177401            PERFORM S08-SKAPA-SDC-NDC-HIST-MOT-PV                         
177501                                                                          
177601            PERFORM S04-SKAPA-EKOTRANS-A03                                
177701            MOVE SPAR-FLINLREP TO FILC3-FLINLREP                          
177801            PERFORM HCC-SKAPA-LDC-TRANS                                   
177901                                                                          
178001            MOVE SLAG-PRAVCOST       TO EKOTRA03-PRAVCOST-OLD             
178101            MOVE WS-KVANTMOT         TO AVG-KVANTMOT                      
178201                                        EKOTRA03-KVANTMOT                 
178301            ADD WS-KVSKROT           TO AVG-KVANTMOT                      
178401            MOVE WS-KVSKROT          TO EKOTRA03-KVSKROT                  
178501            MOVE '61'                TO EKOTRA03-KDANMORS                 
178601                                                                          
178701            PERFORM S03-SKAPA-LEVANM-TRANS                                
178801                                                                          
178901            PERFORM S06-OMRAKN-MEDELPRIS-LAB                              
179001            MOVE W-TIME-N         TO AKTUELL-TID                          
179101            PERFORM S09-FIXA-LOKALTID                                     
179201            MOVE MSGI-TILOKDAT    TO SLAG-TIAVCOST                        
179301            MOVE AVG-PRAVCOST-NEW TO EKOTRA03-PRAVCOST                    
179401                                     SLAG-PRAVCOST                        
179501            MOVE AVG-REMARKUP     TO EKOTRA03-REMARKUP                    
179601            MOVE AVG-PRKURS       TO EKOTRA03-PRKURS                      
179701                                                                          
179801            IF WS-KVANTMOT > ZERO                                         
179901               PERFORM IMS-REPL-WDK711                                    
180001            ELSE                                                          
180101               MOVE EKOTRA03-PRAVCOST-OLD TO EKOTRA03-PRAVCOST            
180201            END-IF                                                        
180301                                                                          
180401         ELSE                                                             
180501                                                                          
180601            IF (SEND-DCS-NDC-NA AND SEND-DCS-USA)                         
180701            AND (REC-DCS-NDC-NA AND REC-DCS-USA)                          
180801                                                                          
180901                MOVE SEND-WS-IDDC TO W-IDDC                               
181001                PERFORM IMS-GET-WDK711                                    
181101                MOVE SLAG-PRAVCOST TO WS-FIXAD-PRARTNTO                   
181201                MOVE 'USD'         TO WS-KDVALISO                         
181301                                                                          
181401                MOVE REC-WS-IDDC TO W-IDDC                                
181501                PERFORM IMS-GET-WDK711                                    
181601                MOVE SEND-DCS-IDLEVNR-DC TO INL-IDLEVNR                   
181701                PERFORM S08-SKAPA-SDC-NDC-HIST-MOT-PV                     
181801                                                                          
181901                MOVE SPAR-FLINLREP TO FILC3-FLINLREP                      
182001                PERFORM HCC-SKAPA-LDC-TRANS                               
182101                                                                          
182201                PERFORM S04-SKAPA-EKOTRANS-A03                            
182301                MOVE SLAG-PRAVCOST                                        
182401                               TO EKOTRA03-PRAVCOST-OLD                   
182501                MOVE WS-KVANTMOT         TO EKOTRA03-KVANTMOT             
182601                                            AVG-KVANTMOT                  
182701                ADD WS-KVSKROT           TO AVG-KVANTMOT                  
182801                MOVE WS-KVSKROT          TO EKOTRA03-KVSKROT              
182901                MOVE '61'                TO EKOTRA03-KDANMORS             
183001                                                                          
183101                PERFORM S06-OMRAKN-MEDELPRIS-LAB                          
183201                MOVE W-TIME-N         TO AKTUELL-TID                      
183301                PERFORM S09-FIXA-LOKALTID                                 
183401                MOVE MSGI-TILOKDAT    TO SLAG-TIAVCOST                    
183501                MOVE AVG-PRAVCOST-NEW TO EKOTRA03-PRAVCOST                
183601                                          SLAG-PRAVCOST                   
183701                MOVE AVG-REMARKUP     TO EKOTRA03-REMARKUP                
183801                MOVE AVG-PRKURS       TO EKOTRA03-PRKURS                  
183901                MOVE 1.0              TO EKOTRA03-PRKURS                  
184001                                                                          
184101                IF WS-KVANTMOT > ZERO                                     
184201                   PERFORM IMS-REPL-WDK711                                
184301                ELSE                                                      
184401                   MOVE EKOTRA03-PRAVCOST-OLD TO EKOTRA03-PRAVCOST        
184501                END-IF                                                    
184601                                                                          
184701                MOVE SEND-DCS-IDLEVNR-DC TO INL-IDLEVNR                   
184801                PERFORM S07-SKAPA-NDC-HIST-SANDANDE                       
184901                                                                          
185001                MOVE ZERO              TO 6306-TIFAKT                     
185101                MOVE IDFAKT-WS         TO 6306-IDFAKT                     
185201                                          W-IDFAKT                        
185301                MOVE REC-WS-IDDC       TO 6306-IDDC-REC                   
185401                MOVE SEND-WS-IDDC      TO 6306-IDDC-SEND                  
185501                MOVE WS-6302-IDDC-LEV  TO 6306-IDDC-LEV                   
185601                PERFORM IMS-GHU-WL630511                                  
185701                IF SEGMENT-FINNS                                          
185801                   CONTINUE                                               
185901                ELSE                                                      
186001                   MOVE 'N'            TO 6306-FLKLAR                     
186101                                          6306-FLDIRLEV                   
186201                   PERFORM IMS-ISRT-WL630511                              
186301                END-IF                                                    
186401                                                                          
186501           END-IF                                                         
186601        END-IF                                                            
186701     END-IF                                                               
186801     .                                                                    
186901     EJECT                                                                
187001                                                                          
187101 HC-UPPDATERA-INLAGGNING SECTION.                                         
187201                                                                          
187301     IF WS-IDDISTR = ZERO                                                 
187401       PERFORM HCX-FIXA-DISTRIKT                                          
187501     END-IF                                                               
187601                                                                          
187701     PERFORM IMS-GU-WLARTC01                                              
187801     MOVE K6-ART-KDPRODSL   TO W-KDPRODSL                                 
187901     MOVE K6-ART-KDSORT     TO WS-KDSORT                                  
188001     MOVE K6-ART-IDFKNGRP   TO W-IDFKNGRP                                 
188101     PERFORM IMS-GHU-WLARTC11                                             
188201     MOVE CLAG-KDPSLLOC      TO W-KDPRODSL-LOC                            
188301     MOVE CLAG-PRARTSTD      TO WS-SAP-PRARTSTD                           
188401                                                                          
188501     IF SEND-DCS-CDC OR SEND-DCS-DDC                                      
188601        PERFORM S02-PRIS-TILLAMPNING                                      
188701        MOVE PRIS-PRARTNTO       TO WS-FIXAD-PRARTNTO                     
188801        MOVE 'USD'               TO WS-KDVALISO                           
188901        IF PRIS-KDVALISO NOT = 'SEK' AND NOT = SPACE                      
189001          MOVE PRIS-KDVALISO     TO WS-KDVALISO                           
189101        END-IF                                                            
189201        MOVE SEND-DCS-IDLEVNR-DC TO INL-IDLEVNR                           
189301                                                                          
189401        PERFORM S08-SKAPA-SDC-NDC-HIST-MOT                                
189501        MOVE SPAR-FLINLREP TO FILC3-FLINLREP                              
189601        PERFORM HCC-SKAPA-LDC-TRANS                                       
189701                                                                          
189801        PERFORM IMS-GET-WDK711                                            
189901        MOVE WS-KVANTMOT         TO AVG-KVANTMOT                          
190001        ADD WS-KVSKROT           TO AVG-KVANTMOT                          
190101                                                                          
190201        PERFORM S03-SKAPA-LEVANM-TRANS                                    
190301                                                                          
190401        PERFORM S06-OMRAKN-MEDELPRIS                                      
190501        MOVE W-TIME-N         TO AKTUELL-TID                              
190601        PERFORM S09-FIXA-LOKALTID                                         
190701        MOVE MSGI-TILOKDAT    TO SLAG-TIAVCOST                            
190801        MOVE AVG-PRAVCOST-NEW TO SLAG-PRAVCOST                            
190901                                                                          
191001        PERFORM S14-SKAPA-SAP-TRANS                                       
191101                                                                          
191201        IF WS-KVANTMOT > ZERO                                             
191301           PERFORM IMS-REPL-WDK711                                        
191401        END-IF                                                            
191501                                                                          
191601     END-IF                                                               
191701     .                                                                    
191801     EJECT                                                                
191901 HCX-FIXA-DISTRIKT SECTION.                                               
192001                                                                          
192101     IF REC-DCS-CDC AND (SEND-DCS-SDC OR SEND-DCS-NDC-PF OR               
192201                                         SEND-DCS-NDC-OTHERS)             
192301       MOVE SEND-DCS-IDDISTR-RETUR  TO WS-IDDISTR                         
192401       MOVE SEND-DCS-IDKUNDNR-RETUR TO WS-IDKUNDNR-NUM                    
192501       MOVE WS-IDKUNDNR-NUM         TO WS-IDKUNDNR                        
192601     ELSE                                                                 
192701       MOVE ZERO TO WS-IDDISTR                                            
192801                                                                          
192901       MOVE SEND-WS-IDDC TO WS-SOEK-IDDC-SEND                             
193001       MOVE REC-WS-IDDC  TO WS-SOEK-IDDC-REC                              
193101                                                                          
193201       IF SEND-DCS-DDC                                                    
193301          MOVE WC-CDC-SE TO WS-SOEK-IDDC-SEND                             
193401       END-IF                                                             
193501                                                                          
193601       SEARCH ALL WWDC03-IDDISTR                                          
193701         AT END                                                           
193801           MOVE ZERO     TO WS-IDDISTR                                    
193901*          MOVE 'EJ TRÄFF I TABELL TEXTXX' TO FELTEXT                     
194001*          DISPLAY FELTEXT                                                
194101*          CALL FELLOG                                                    
194201         WHEN WWDC03-IDDC-SEND-REC(WWDC03-IX)                             
194301           = WS-SOEK-IDDC-SEND-REC                                        
194401             MOVE WWDC03-SOK-IDDISTR(WWDC03-IX) TO WS-IDDISTR             
194501       END-SEARCH                                                         
194601     END-IF                                                               
194701     .                                                                    
194801     EJECT                                                                
194901                                                                          
195001 HCB-SKAPA-DIFF-TRANS SECTION.                                            
195101                                                                          
195201     MOVE WS-TIFAKT       TO WS-SEKEL-TEST                                
195301                             WS-DIFF-AAMMDD                               
195401     IF WS-SEK = 9                                                        
195501        MOVE 19           TO WS-DIFF-SS                                   
195601     ELSE                                                                 
195701        MOVE 20           TO WS-DIFF-SS                                   
195801     END-IF                                                               
195901     MOVE WS-DIFF-AAAAMMDD  TO FILC2-DAFAKT                               
196001     MOVE W-IDARTNR         TO FILC2-IDARTNR                              
196101     MOVE SEND-WS-IDDC      TO FILC2-IDDC-SEND                            
196201     MOVE REC-WS-IDDC       TO FILC2-IDDC-REC                             
196301     MOVE W-IDFAKT          TO FILC2-IDFAKT                               
196401     MOVE WS-IDORDNR        TO FILC2-IDKUNDRF                             
196501     MOVE ZERO              TO FILC2-PRARTBES-PR                          
196601     MOVE WS-IDLEVNR        TO FILC2-IDLEVNR                              
196701     IF WS-KVANTMOT > ZERO                                                
196801        MOVE WS-KVANTMOT       TO FILC2-KVANTAL                           
196901        MOVE 'FOUND'           TO FILC2-AVVIKELSETYP                      
197001        MOVE FILC2-W61244      TO FILC2-FIL-WDR301-DATA                   
197101        ACCEPT W-TID FROM TIME                                            
197201        IF W-TID = FILC2-FIL-TIKLOCK                                      
197301           ADD +1              TO FILC2-FIL-IDSEKVNR                      
197401        ELSE                                                              
197501           MOVE W-TID          TO FILC2-FIL-TIKLOCK                       
197601           MOVE +1             TO FILC2-FIL-IDSEKVNR                      
197701        END-IF                                                            
197801        PERFORM IMS-ISRT-WLFILC2                                          
197901     END-IF                                                               
198001                                                                          
198101     IF WS-KVSKROT > ZERO                                                 
198201        MOVE WS-KVSKROT        TO FILC2-KVANTAL                           
198301        MOVE 'FOUND/DAM'       TO FILC2-AVVIKELSETYP                      
198401        MOVE FILC2-W61244      TO FILC2-FIL-WDR301-DATA                   
198501        ACCEPT W-TID FROM TIME                                            
198601        IF W-TID = FILC2-FIL-TIKLOCK                                      
198701           ADD +1              TO FILC2-FIL-IDSEKVNR                      
198801        ELSE                                                              
198901           MOVE W-TID          TO FILC2-FIL-TIKLOCK                       
199001           MOVE +1             TO FILC2-FIL-IDSEKVNR                      
199101        END-IF                                                            
199201        PERFORM IMS-ISRT-WLFILC2                                          
199301     END-IF                                                               
199401     .                                                                    
199501     EJECT                                                                
199601 HCC-SKAPA-LDC-TRANS SECTION.                                             
199701                                                                          
199801     MOVE WS-TIFAKT       TO WS-SEKEL-TEST                                
199901                             WS-DIFF-AAMMDD                               
200001     IF WS-SEK = 9                                                        
200101        MOVE 19           TO WS-DIFF-SS                                   
200201     ELSE                                                                 
200301        MOVE 20           TO WS-DIFF-SS                                   
200401     END-IF                                                               
200501     MOVE WS-DIFF-AAAAMMDD  TO FILC3-DAFAKT                               
200601     MOVE W-IDARTNR         TO FILC3-IDARTNR                              
200701     IF WS-6302-IDDC-LEV NOT = SPACE                                      
200801       MOVE WS-6302-IDDC-LEV TO FILC3-IDDC-SEND                           
200901     ELSE                                                                 
201001       MOVE SEND-WS-IDDC     TO FILC3-IDDC-SEND                           
201101     END-IF                                                               
201201     MOVE REC-WS-IDDC       TO FILC3-IDDC-REC                             
201301***  MOVE W-IDFAKT          TO FILC3-IDFAKT                               
201401     MOVE WS-IDFAKT         TO FILC3-IDFAKT                               
201501     MOVE WS-IDORDNR        TO FILC3-IDKUNDRF                             
201601     MOVE WS-IDKUNDNR       TO FILC3-IDKUNDNR                             
201701     MOVE WS-IDKOLLI        TO FILC3-IDKOLLI                              
201801     MOVE WS-IDORDNR        TO FILC3-IDKUNDRF                             
201901     MOVE ZERO              TO FILC3-PRARTSTD                             
202001                                                                          
202101     MOVE WS-SUMMA-KVANT    TO FILC3-KVANTAL                              
202201     MOVE ZERO              TO FILC3-KVAVIS                               
202301     MOVE 'FOUND'           TO FILC3-AVVIKELSETYP                         
202401     MOVE 2                 TO FILC3-KDSORT1                              
202501     MOVE W-DAGENS-DATUM    TO FILC3-DAREGDAT                             
202601     ADD 20000000           TO FILC3-DAREGDAT                             
202701     IF MID-IDUSER-003 = ALL '+'                                          
202801        MOVE SPACE            TO FILC3-IDUSER                             
202901     ELSE                                                                 
203001        MOVE MID-IDUSER-003   TO FILC3-IDUSER                             
203101     END-IF                                                               
203201     MOVE FILC3-W61247      TO FILC3-FIL-WDR301-DATA                      
203301     ACCEPT W-TID FROM TIME                                               
203401     IF W-TID = FILC3-FIL-TIKLOCK                                         
203501        ADD +1              TO FILC3-FIL-IDSEKVNR                         
203601     ELSE                                                                 
203701        MOVE W-TID          TO FILC3-FIL-TIKLOCK                          
203801        MOVE +1             TO FILC3-FIL-IDSEKVNR                         
203901     END-IF                                                               
204001     PERFORM IMS-ISRT-WLFILC3                                             
204101                                                                          
204201     IF WS-KVSKROT > ZERO                                                 
204301        MOVE WS-KVSKROT        TO FILC3-KVANTAL                           
204401        MOVE 'DAM'             TO FILC3-AVVIKELSETYP                      
204501        MOVE 5                 TO FILC3-KDSORT1                           
204601        MOVE FILC3-W61247      TO FILC3-FIL-WDR301-DATA                   
204701        ACCEPT W-TID FROM TIME                                            
204801        IF W-TID = FILC3-FIL-TIKLOCK                                      
204901           ADD +1              TO FILC3-FIL-IDSEKVNR                      
205001        ELSE                                                              
205101           MOVE W-TID          TO FILC3-FIL-TIKLOCK                       
205201           MOVE +1             TO FILC3-FIL-IDSEKVNR                      
205301        END-IF                                                            
205401        PERFORM IMS-ISRT-WLFILC3                                          
205501     END-IF                                                               
205601     .                                                                    
205701     EJECT                                                                
205801 HD-SKAPA-REFILLTRANS SECTION.                                            
205901                                                                          
206001     MOVE W-IDARTNR         TO FILC-IDARTNR                               
206101     MOVE WS-KVANTMOT       TO FILC-KVLEVANM                              
206201     MOVE WS-KVSKROT        TO FILC-KVSKROT                               
206301     MOVE SEND-WS-IDDC      TO FILC-IDDC-SEND                             
206401     MOVE REC-WS-IDDC       TO FILC-IDDC-REC                              
206501     MOVE W-DAGENS-DATUM    TO FILC-TILEVANM                              
206601     MOVE FILC-W61236       TO FILC-FIL-WDR301-DATA                       
206701     ACCEPT W-TID FROM TIME                                               
206801     IF W-TID = FILC-FIL-TIKLOCK                                          
206901        ADD +1              TO FILC-FIL-IDSEKVNR                          
207001     ELSE                                                                 
207101        MOVE W-TID          TO FILC-FIL-TIKLOCK                           
207201        MOVE +1             TO FILC-FIL-IDSEKVNR                          
207301     END-IF                                                               
207401     PERFORM IMS-ISRT-WLFILC                                              
207501     .                                                                    
207601     EJECT                                                                
207701                                                                          
207801 HE-UPPDATERA-BEF-ART-CDC SECTION.                                        
207901                                                                          
208001     MOVE NEJ TO WS-UPPD-LOCB                                             
208101                                                                          
208201     IF MID-ADLAGOMR(RAD-IX) = ALL '+' OR SPACE                           
208301        MOVE CLAG-ADLAGOMR       TO WS-ADLAGOMR(RAD-IX)                   
208401     ELSE                                                                 
208501        MOVE WS-ADLAGOMR(RAD-IX) TO CLAG-ADLAGOMR                         
208601                                    W-ADLAGOMR-LOCB                       
208701        MOVE JA TO WS-UPPD-LOCB                                           
208801     END-IF                                                               
208901     IF MID-ADGANG(RAD-IX) = ALL '+' OR SPACE                             
209001        MOVE CLAG-ADGANG         TO WS-ADGANG(RAD-IX)                     
209101     ELSE                                                                 
209201        MOVE WS-ADGANG(RAD-IX)   TO CLAG-ADGANG                           
209301                                    W-ADGANG-LOCB                         
209401        MOVE JA TO WS-UPPD-LOCB                                           
209501     END-IF                                                               
209601     IF MID-ADPLATS(RAD-IX) = ALL '+' OR SPACE                            
209701        MOVE CLAG-ADPLATS        TO WS-ADPLATS(RAD-IX)                    
209801     ELSE                                                                 
209901        MOVE WS-ADPLATS(RAD-IX)  TO CLAG-ADPLATS                          
210001                                    W-ADPLATS-LOCB                        
210101        MOVE JA TO WS-UPPD-LOCB                                           
210201     END-IF                                                               
210301                                                                          
210401     MOVE CLAG-KVLS          TO WS-OLD-KVLS                               
210501     IF WS-KVANTMOT > ZERO                                                
210601        ADD WS-KVANTMOT      TO CLAG-KVLS                                 
210701        MOVE WS-KVANTMOT     TO WS-RO-KVANT                               
210801        MOVE ZERO            TO W-KVAVIS                                  
210901                                                                          
211001        IF WS-UPPD-LOCB = JA                                              
211101           PERFORM S13-UPPDATERA-WDJ9                                     
211201        END-IF                                                            
211301        PERFORM IMS-REPL-WLARTC11                                         
211401        PERFORM HEA-FLYTTA-SALDOLOGG-DATA                                 
211501        PERFORM S01-EV-RO-TACKNING-CDC                                    
211601     END-IF                                                               
211701     .                                                                    
211801     EJECT                                                                
211901                                                                          
212001 HEA-FLYTTA-SALDOLOGG-DATA SECTION.                                       
212101     PERFORM S11-FLYTTA-SALDOLOGG-DATA                                    
212201     MOVE CLAG-KVAKS-PAV    TO LOGG-KVAKS-PAV                             
212301     MOVE CLAG-KVEFRS       TO LOGG-KVEFRS                                
212401     PERFORM S12-ISRT-SALDOLOGG                                           
212501     .                                                                    
212601     EJECT                                                                
212701                                                                          
212801 MFS-RENSA-FAELT-IN SECTION.                                              
212901                                                                          
213001     MOVE MFS-RENSA-FAELT TO MOD-KOLLI-KLAR                               
213101                             MOD-FLSLUT-BEKR                              
213201                             MOD-IDUSER-003                               
213301     MOVE +1 TO RAD-IX                                                    
213401     PERFORM UNTIL RAD-IX > MAX-RAD                                       
213501        MOVE MFS-RENSA-FAELT TO MOD-IDARTNR(RAD-IX)                       
213601                                MOD-KVANTMOT(RAD-IX)                      
213701                                MOD-KVSKROT(RAD-IX)                       
213801                                MOD-ADLAGOMR(RAD-IX)                      
213901                                MOD-ADGANG(RAD-IX)                        
214001                                MOD-ADPLATS(RAD-IX)                       
214101        ADD +1 TO RAD-IX                                                  
214201     END-PERFORM                                                          
214301     .                                                                    
214401     EJECT                                                                
214501                                                                          
214601 MFS-ROER-EJ-FAELT-IN SECTION.                                            
214701                                                                          
214801     MOVE MFS-ROER-EJ-FAELT TO MOD-KOLLI-KLAR                             
214901                               MOD-IDUSER-003                             
215001     MOVE +1 TO RAD-IX                                                    
215101     PERFORM UNTIL RAD-IX > MAX-RAD                                       
215201        MOVE MFS-ROER-EJ-FAELT TO MOD-IDARTNR(RAD-IX)                     
215301                                MOD-KVANTMOT(RAD-IX)                      
215401                                MOD-KVSKROT(RAD-IX)                       
215501                                MOD-ADLAGOMR(RAD-IX)                      
215601                                MOD-ADGANG(RAD-IX)                        
215701                                MOD-ADPLATS(RAD-IX)                       
215801        ADD +1 TO RAD-IX                                                  
215901     END-PERFORM                                                          
216001     .                                                                    
216101     EJECT                                                                
216201                                                                          
216301 S01-EV-RO-TACKNING SECTION.                                              
216401                                                                          
216501     IF SLAG-KVROS-BULK > ZERO                                            
216601     OR SLAG-KVROS-DAG > ZERO                                             
216701        IF SLAG-KDLEVSP = ZERO                                            
216801           MOVE W-IDARTNR      TO 4506-IDARTNR                            
216901           MOVE +1             TO 4506-KDTAKORS                           
217001           MOVE WS-RO-KVANT    TO 4506-KVANTMOT                           
217101           MOVE REC-WS-IDDC    TO 4505-IDDC                               
217201           PERFORM IMS-ISRT-4506                                          
217301           MOVE SPACE          TO 4506-WDGX4506                           
217401        END-IF                                                            
217501     END-IF                                                               
217601     .                                                                    
217701     EJECT                                                                
217801                                                                          
217901 S01-EV-RO-TACKNING-CDC SECTION.                                          
218001                                                                          
218101     IF CLAG-KVROS > ZERO                                                 
218201        IF CLAG-KDLEVSP = ZERO                                            
218301           MOVE W-IDARTNR      TO 4506-IDARTNR                            
218401           MOVE +1             TO 4506-KDTAKORS                           
218501           MOVE WS-RO-KVANT    TO 4506-KVANTMOT                           
218601           MOVE REC-WS-IDDC    TO 4505-IDDC                               
218701           PERFORM IMS-ISRT-4506                                          
218801           MOVE SPACE          TO 4506-WDGX4506                           
218901        END-IF                                                            
219001     END-IF                                                               
219101     .                                                                    
219201     EJECT                                                                
219301                                                                          
219401 S02-PRIS-TILLAMPNING SECTION.                                            
219501                                                                          
219601*--- KOD 61 FÖR DISTR 9111 OCH 9211 EXP-TO-CDC, 914X CN-TO-US             
219701     MOVE ZERO   TO WS-6308-TF-PRARTNTO                                   
219801     MOVE SPACE  TO WS-6308-TF-KDVALISO                                   
219901                                                                          
220001     MOVE 1                   TO PRIS-KDCALL                              
220101     MOVE 'W6030900'          TO PRIS-IDPGM                               
220201     MOVE W-IDARTNR           TO PRIS-IDARTNR                             
220301     IF WS-6302-IDDISTR = ZERO                                            
220401       MOVE WS-IDDISTR        TO PRIS-IDDISTR                             
220501     ELSE                                                                 
220601       MOVE WS-6302-IDDISTR   TO PRIS-IDDISTR                             
220701     END-IF                                                               
220801     MOVE ZERO                TO PRIS-IDKUNDNR                            
220901     MOVE +1                  TO PRIS-KDORDKL                             
221001                                 PRIS-KVBEART                             
221101     MOVE SEND-WS-IDDC        TO PRIS-IDDC                                
221201     MOVE NEJ                 TO PRIS-FLINVEST                            
221301                                                                          
221401     CALL W335PRIS USING PRIS-W335PRIS ARTC-PCB                           
221501                                   PRIS-WDK7-PCB GMTA-PCB                 
221601                                   BETA-PCB GPRIA-PCB GPRIB-PCB           
221701                                   PRIS-COST-WDK6-PCB                     
221801                                   PRIS-COST-WDK7-PCB                     
221901                                   PRIS-COST-WDF1-PCB                     
222001                                   PRIS-COST-9305-PCB                     
222101                                   PRIS-COST-WDK72-PCB                    
222201                                   PRIS-COST-WDB6-PCB                     
222301                                                                          
222401                                                                          
222501     IF PRIS-KDSVAR = SPACE                                               
222601        MOVE PRIS-PRARTNTO    TO WS-FIXAD-PRARTNTO                        
222701                                                                          
222801        IF PRIS-PRAVCOST > ZERO                                           
222901          MOVE PRIS-PRAVCOST  TO WS-6308-TF-PRARTNTO                      
223001        ELSE                                                              
223101          MOVE PRIS-PRARTNTO  TO WS-6308-TF-PRARTNTO                      
223201        END-IF                                                            
223301        MOVE PRIS-KDVALISO    TO WS-6308-TF-KDVALISO                      
223401                                                                          
223501     ELSE                                                                 
223601        MOVE 'FEL RETURKOD FRÅN W335PRIS' TO FELTEXT                      
223701        DISPLAY FELTEXT                                                   
223801        CALL FELLOG                                                       
223901     END-IF                                                               
224001     .                                                                    
224101     EJECT                                                                
224201                                                                          
224301 S03-SKAPA-LEVANM-TRANS SECTION.                                          
224401                                                                          
224501     MOVE WS-TIFAKT         TO 6306-TIFAKT                                
224601     MOVE IDFAKT-WS         TO 6306-IDFAKT                                
224701                               W-IDFAKT                                   
224801     MOVE REC-WS-IDDC       TO 6306-IDDC-REC                              
224901     MOVE SEND-WS-IDDC      TO 6306-IDDC-SEND                             
225001     MOVE WS-6302-IDDC-LEV  TO 6306-IDDC-LEV                              
225101                                                                          
225201     PERFORM IMS-GHU-WL630511                                             
225301     IF SEGMENT-FINNS                                                     
225401        IF WS-6302-IDDISTR = ZERO                                         
225501          MOVE WS-IDDISTR       TO 6308-IDDISTR                           
225601                                   DIST79-IDDISTR                         
225701                                   DIST35-IDDISTR                         
225801        ELSE                                                              
225901          MOVE WS-6302-IDDISTR  TO 6308-IDDISTR                           
226001                                   DIST79-IDDISTR                         
226101                                   DIST35-IDDISTR                         
226201        END-IF                                                            
226301        IF WS-6302-IDKUNDNR = ZERO                                        
226401          MOVE WS-IDKUNDNR      TO 6308-IDKUNDNR                          
226501        ELSE                                                              
226601          MOVE WS-6302-IDKUNDNR TO 6308-IDKUNDNR                          
226701        END-IF                                                            
226801                                                                          
226901        MOVE IDFAKT-WS      TO 6308-IDRAPPNR                              
227001        MOVE INL-KDFRAKT    TO 6308-KDFRAKT                               
227101        MOVE INL-IDKUNDRF   TO 6308-IDKUNDRF                              
227201        MOVE INL-IDKOLLI    TO 6308-IDKOLLI                               
227301        MOVE WS-KVANTMOT    TO 6308-KVLEVANM                              
227401        ADD  WS-KVSKROT     TO 6308-KVLEVANM                              
227501                                                                          
227601        IF (SEND-DCS-NDC-NA AND SEND-DCS-USA)                             
227701        AND (REC-DCS-NDC-NA AND REC-DCS-CANADA)                           
227801        OR (SEND-DCS-NDC-NA AND SEND-DCS-CANADA)                          
227901        AND (REC-DCS-NDC-NA AND REC-DCS-USA)                              
228001            MOVE '11'       TO 6308-KDANMORS                              
228101        ELSE                                                              
228201            MOVE '61'       TO 6308-KDANMORS                              
228301        END-IF                                                            
228401                                                                          
228501**- ÄNDRING 2004-10 FÖR ATT KLARA LOCAL-CURRENCY NA.INL-PRARTNTO          
228601**- KOMMER FRÅN BILL-IT OCH ÄR I LOCAL VALUTA.OBS! GÄLLER EJ NDCUS        
228701**- -NDCCA/NDCCA-NDCUS SOM ANVÄNDER PRAVCOST. DIST. 874X,8751,            
228801**- ÄNDRING 2018-08 GLOBAL EXPORT                                         
228901**- OBS! REFILL US-TO-CDC DISTR 9211 + CN-TO-CDC DISTR 9111               
229001**- OBS! MAN HÄMTAR RÄTT PRIS OCH VALUTA FRÅN W335PRIS FÖR LEV.ANM        
229101**- 9211 DC4X ANVÄNDER PRAVCOST OCH USD,9111 DC71 PRAVCOST OCH CNY        
229201**- STUDSFAKTURA NR 2 SOM MAN TAR EMOT PÅ HAR STANDARDPRIS OCH SEK        
229301**- CN-TO-US DISTR 914X (US-TO-CN WEB) HÄMTAR RÄTT PRIS OCH VALUTA        
229401**- FRÅN W335PRIS FÖR LEV.ANM                                             
229501**- OBS! OM INL-PRARTNTO = 0 SÅ ÄR WDK711-PRAVCOST = 0 DISTR 9X11         
229601**- FAKTURAN HÄMTAR PRIS FRÅN ORDERRADEN(FIX PRIS)WDL6 FRÅN K711          
229701**- ------------------------------------------------------------          
229801        IF (SEND-DCS-NDC-NA AND SEND-DCS-USA)                             
229901        AND (REC-DCS-NDC-NA AND REC-DCS-CANADA)                           
230001        OR (SEND-DCS-NDC-NA AND SEND-DCS-CANADA)                          
230101        AND (REC-DCS-NDC-NA AND REC-DCS-USA)                              
230201            MOVE INL-PRARTNTO    TO 6308-PRARTBTO                         
230301            MOVE ZERO            TO 6308-PRARTBTO-LOC                     
230401            MOVE INL-KDVALISO    TO 6308-KDVALISO                         
230501        ELSE                                                              
230601          IF DIST79-DEALER-PRICE                                          
230701            MOVE INL-PRARTNTO    TO 6308-PRARTBTO-LOC                     
230801            MOVE ZERO            TO 6308-PRARTBTO                         
230901            MOVE INL-KDVALISO    TO 6308-KDVALISO                         
231001          ELSE                                                            
231101            IF DIST35-NONVCC-CDC-REFILL                                   
231201            OR DIST35-NDCCN-NDCUS-REFILL                                  
231301                                                                          
231401              MOVE WS-6308-TF-PRARTNTO  TO 6308-PRARTBTO                  
231501              MOVE ZERO                 TO 6308-PRARTBTO-LOC              
231601              MOVE WS-6308-TF-KDVALISO  TO 6308-KDVALISO                  
231701            ELSE                                                          
231801              MOVE INL-PRARTNTO    TO 6308-PRARTBTO                       
231901              MOVE ZERO            TO 6308-PRARTBTO-LOC                   
232001              MOVE 'SEK'           TO 6308-KDVALISO                       
232101            END-IF                                                        
232201          END-IF                                                          
232301        END-IF                                                            
232401                                                                          
232501        MOVE W-IDARTNR      TO 6308-IDARTNR                               
232601        MOVE ZERO           TO 6308-IDRADNR                               
232701                               6308-KDEMBLEV                              
232801        MOVE W-DAGENS-DATUM TO 6308-TILEVANM                              
232901                                                                          
233001        PERFORM IMS-ISRT-WL630521                                         
233101        PERFORM UNTIL NOT SEGMENT-FINNS-REDAN                             
233201           ADD +1 TO 6308-IDRADNR                                         
233301           PERFORM IMS-ISRT-WL630521                                      
233401        END-PERFORM                                                       
233501     ELSE                                                                 
233601        MOVE 'N'            TO 6306-FLKLAR                                
233701                               6306-FLDIRLEV                              
233801        PERFORM IMS-ISRT-WL630511                                         
233901                                                                          
234001        IF WS-6302-IDDISTR = ZERO                                         
234101          MOVE WS-IDDISTR       TO 6308-IDDISTR                           
234201                                   DIST79-IDDISTR                         
234301        ELSE                                                              
234401          MOVE WS-6302-IDDISTR  TO 6308-IDDISTR                           
234501                                   DIST79-IDDISTR                         
234601        END-IF                                                            
234701        IF WS-6302-IDKUNDNR = ZERO                                        
234801          MOVE WS-IDKUNDNR      TO 6308-IDKUNDNR                          
234901        ELSE                                                              
235001          MOVE WS-6302-IDKUNDNR TO 6308-IDKUNDNR                          
235101        END-IF                                                            
235201                                                                          
235301        MOVE IDFAKT-WS      TO 6308-IDRAPPNR                              
235401        MOVE INL-KDFRAKT    TO 6308-KDFRAKT                               
235501        MOVE INL-IDKUNDRF   TO 6308-IDKUNDRF                              
235601        MOVE INL-IDKOLLI    TO 6308-IDKOLLI                               
235701        MOVE WS-KVANTMOT    TO 6308-KVLEVANM                              
235801        ADD  WS-KVSKROT     TO 6308-KVLEVANM                              
235901        IF (SEND-DCS-NDC-NA AND SEND-DCS-USA)                             
236001        AND (REC-DCS-NDC-NA AND REC-DCS-CANADA)                           
236101        OR (SEND-DCS-NDC-NA AND SEND-DCS-CANADA)                          
236201        AND (REC-DCS-NDC-NA AND REC-DCS-USA)                              
236301            MOVE '11'       TO 6308-KDANMORS                              
236401        ELSE                                                              
236501            MOVE '61'       TO 6308-KDANMORS                              
236601        END-IF                                                            
236701                                                                          
236801        IF DIST79-DEALER-PRICE                                            
236901          MOVE INL-PRARTNTO    TO 6308-PRARTBTO-LOC                       
237001          MOVE ZERO            TO 6308-PRARTBTO                           
237101          MOVE INL-KDVALISO    TO 6308-KDVALISO                           
237201        ELSE                                                              
237301          IF DIST35-NONVCC-CDC-REFILL                                     
237401          OR DIST35-NDCCN-NDCUS-REFILL                                    
237501                                                                          
237601            MOVE WS-6308-TF-PRARTNTO  TO 6308-PRARTBTO                    
237701            MOVE ZERO                 TO 6308-PRARTBTO-LOC                
237801            MOVE WS-6308-TF-KDVALISO  TO 6308-KDVALISO                    
237901          ELSE                                                            
238001            MOVE INL-PRARTNTO    TO 6308-PRARTBTO                         
238101            MOVE ZERO            TO 6308-PRARTBTO-LOC                     
238201            MOVE 'SEK'           TO 6308-KDVALISO                         
238301          END-IF                                                          
238401        END-IF                                                            
238501                                                                          
238601        MOVE W-IDARTNR      TO 6308-IDARTNR                               
238701        MOVE ZERO           TO 6308-IDRADNR                               
238801                               6308-KDEMBLEV                              
238901        MOVE W-DAGENS-DATUM TO 6308-TILEVANM                              
239001                                                                          
239101        PERFORM IMS-ISRT-WL630521                                         
239201        PERFORM UNTIL NOT SEGMENT-FINNS-REDAN                             
239301           ADD +1 TO 6308-IDRADNR                                         
239401           PERFORM IMS-ISRT-WL630521                                      
239501        END-PERFORM                                                       
239601     END-IF                                                               
239701     .                                                                    
239801     EJECT                                                                
239901                                                                          
240001 S04-SKAPA-EKOTRANS-A03 SECTION.                                          
240101                                                                          
240201     IF WS-A03-SKAPAD = JA                                                
240301        PERFORM S041-SKRIV-A03                                            
240401     END-IF                                                               
240501                                                                          
240601     IF SEND-DCS-CDC OR SEND-DCS-DDC                                      
240701        MOVE INL-IDDISTR  TO DIST35-IDDISTR                               
240801                             DIST79-IDDISTR                               
240901        IF DIST35-REFILL-NA                                               
241001           MOVE 'T10'           TO EKOTRA03-KDEKOHT                       
241101           IF DIST35-CDC-NDC41-REFILL                                     
241201           OR DIST35-CDC-NDC43-REFILL                                     
241301           OR DIST35-CDC-NDC44-REFILL                                     
241401           OR DIST35-CDC-NDC45-REFILL                                     
241501           OR DIST35-CDC-NDC46-REFILL                                     
241502           OR DIST35-CDC-NDC47-REFILL                                     
241601              MOVE 53           TO EKOTRA03-IDFTG                         
241701           ELSE                                                           
241801              MOVE 54           TO EKOTRA03-IDFTG                         
241901           END-IF                                                         
242001        ELSE                                                              
242101           IF DIST35-REFILL-NA-JAP                                        
242201              MOVE 'I20'        TO EKOTRA03-KDEKOHT                       
242301              IF DIST35-JAP-NDC41-REFILL                                  
242401              OR DIST35-JAP-NDC43-REFILL                                  
242501              OR DIST35-JAP-NDC44-REFILL                                  
242601                 MOVE 53         TO EKOTRA03-IDFTG                        
242701              ELSE                                                        
242801                 MOVE 54         TO EKOTRA03-IDFTG                        
242901              END-IF                                                      
243001           END-IF                                                         
243101        END-IF                                                            
243201        IF DIST79-DEALER-PRICE                                            
243301          MOVE INL-KDVALISO      TO EKOTRA03-KDVALISO                     
243401        ELSE                                                              
243501          MOVE 'SEK'             TO EKOTRA03-KDVALISO                     
243601        END-IF                                                            
243701     ELSE                                                                 
243801        IF (SEND-DCS-NDC-NA AND SEND-DCS-USA)                             
243901        AND (REC-DCS-NDC-NA AND REC-DCS-CANADA)                           
244001             MOVE 'T50'      TO EKOTRA03-KDEKOHT                          
244101             MOVE 54         TO EKOTRA03-IDFTG                            
244201             MOVE 'USD'      TO EKOTRA03-KDVALISO                         
244301        ELSE                                                              
244401           IF (SEND-DCS-NDC-NA AND SEND-DCS-CANADA)                       
244501           AND (REC-DCS-NDC-NA AND REC-DCS-USA)                           
244601             MOVE 'T40'      TO EKOTRA03-KDEKOHT                          
244701             MOVE 53         TO EKOTRA03-IDFTG                            
244801             MOVE 'CAD'      TO EKOTRA03-KDVALISO                         
244901           ELSE                                                           
245001              IF (SEND-DCS-NDC-NA AND SEND-DCS-USA)                       
245101              AND (REC-DCS-NDC-NA AND REC-DCS-USA)                        
245201                  MOVE 'T30' TO EKOTRA03-KDEKOHT                          
245301                  MOVE 53    TO EKOTRA03-IDFTG                            
245401                  MOVE 'USD' TO EKOTRA03-KDVALISO                         
245501              END-IF                                                      
245601           END-IF                                                         
245701        END-IF                                                            
245801     END-IF                                                               
245901                                                                          
246001     MOVE 'A03'           TO EKOTRA03-IDPTYP                              
246101******************** ANDRAT 990430                                        
246201     IF SEND-DCS-CDC OR SEND-DCS-DDC                                      
246301        MOVE WC-CDC-SE    TO EKOTRA03-IDDC-SEND                           
246401     ELSE                                                                 
246501        MOVE SEND-WS-IDDC TO EKOTRA03-IDDC-SEND                           
246601     END-IF                                                               
246701******************** ANDRAT 990430                                        
246801     MOVE INL-IDDISTR     TO EKOTRA03-IDDISTR                             
246901     MOVE INL-IDKUNDNR    TO EKOTRA03-IDKUNDNR                            
247001     MOVE ZERO            TO EKOTRA03-IDORDNR7                            
247101     MOVE INL-IDORDNR5    TO EKOTRA03-IDORDNR7                            
247201     MOVE INL-PRARTNTO    TO EKOTRA03-PRARTNTO                            
247301                                                                          
247401     MOVE INL-TIINLINL    TO WS-SEKEL-KOLL                                
247501                             WS-EKOA03-AAMMDD                             
247601     IF WS-SEKEL = 9                                                      
247701        MOVE 19           TO WS-EKOA03-SS                                 
247801     ELSE                                                                 
247901        MOVE 20           TO WS-EKOA03-SS                                 
248001     END-IF                                                               
248101     MOVE WS-AAAAMMDD     TO EKOTRA03-DAINLINL                            
248201                             EKOTRA03-DAFAKT                              
248301     IF SEND-DCS-CDC OR SEND-DCS-DDC                                      
248401        MOVE WS-TIFAKT       TO WS-SEKEL-KOLL                             
248501                                WS-EKOA03-AAMMDD                          
248601        IF WS-SEKEL = 9                                                   
248701           MOVE 19           TO WS-EKOA03-SS                              
248801        ELSE                                                              
248901           MOVE 20           TO WS-EKOA03-SS                              
249001        END-IF                                                            
249101        MOVE WS-AAAAMMDD     TO EKOTRA03-DAFAKT                           
249201     END-IF                                                               
249301                                                                          
249401     MOVE INL-KVAVIS      TO EKOTRA03-KVLEVART                            
249501     MOVE INL-PRKURS      TO EKOTRA03-PRKURS                              
249601     MOVE INL-IDKOLLI     TO EKOTRA03-IDKOLLI                             
249701     MOVE WS-OLD-KVLS     TO EKOTRA03-KVLS-OLD                            
249801     MOVE REC-WS-IDDC     TO EKOTRA03-IDDC-REC                            
249901     MOVE IDFAKT-WS       TO EKOTRA03-IDFAKT                              
250001     MOVE W-IDARTNR       TO EKOTRA03-IDARTNR                             
250101     MOVE W-KDPRODSL      TO EKOTRA03-KDPRODSL                            
250201     MOVE W-KDPRODSL-LOC  TO EKOTRA03-KDPSLLOC                            
250301     MOVE SPACE           TO EKOTRA03-FLSLUT                              
250401                                                                          
250501     MOVE JA              TO WS-A03-SKAPAD                                
250601     .                                                                    
250701     EJECT                                                                
250801                                                                          
250901 S041-SKRIV-A03 SECTION.                                                  
251001                                                                          
251101     MOVE 'W6030900'        TO FIL-IDPGM IN FIL-WDR801                    
251201     MOVE W-DAGENS-DATUM    TO FIL-TIREGDAT                               
251301     ADD +1                 TO W-TIKLOCK                                  
251401     MOVE W-TIKLOCK         TO FIL-TIKLOCK IN FIL-WDR801                  
251501     ADD +1                 TO W-IDSEKVNR-A03                             
251601     MOVE W-IDSEKVNR-A03    TO FIL-IDSEKVNR IN FIL-WDR801                 
251701     MOVE 'W510'            TO FIL-CT-IDSYSTEM IN FIL-WDR801              
251801     MOVE 'A03'             TO FIL-CT-IDPTYP IN FIL-WDR801                
251901     MOVE ' '               TO FIL-CT-IDVTYP IN FIL-WDR801                
252001     MOVE EKOTRA03-W510A03  TO FIL-WDR801-DATA IN FIL-WDR801              
252101                                                                          
252201     PERFORM IMS-ISRT-WLFILB01                                            
252301                                                                          
252401     PERFORM UNTIL NOT SEGMENT-FINNS-REDAN                                
252501        ADD +1 TO FIL-IDSEKVNR IN FIL-WDR801                              
252601        PERFORM IMS-ISRT-WLFILB01                                         
252701     END-PERFORM                                                          
252801                                                                          
252901     MOVE NEJ TO WS-A03-SKAPAD                                            
253001     .                                                                    
253101     EJECT                                                                
253201                                                                          
253301 S06-OMRAKN-MEDELPRIS-LAB SECTION.                                        
253401                                                                          
253501     MOVE REC-WS-IDDC    TO AVG-IDDC                                      
253601     MOVE SLAG-PRAVCOST  TO AVG-PRAVCOST-OLD                              
253701     COMPUTE AVG-KVLS-OLD = WS-OLD-KVLS + WS-OLD-KVEFRS                   
253801     MOVE INL-PRARTNTO   TO AVG-PRARTNTO                                  
253901     MOVE W-KDPRODSL-LOC TO AVG-KDPSLLOC                                  
254001     MOVE W-KDPRODSL     TO AVG-KDPRODSL                                  
254101     MOVE W-IDFKNGRP     TO AVG-IDFKNGRP                                  
254201     MOVE WS-KDVALISO    TO AVG-KDVALISO                                  
254301     MOVE INL-IDDISTR    TO DIST79-IDDISTR                                
254401                                                                          
254501     IF SEND-DCS-CDC OR SEND-DCS-DDC                                      
254601        IF DIST79-DEALER-PRICE                                            
254701          MOVE '010'      TO AVG-KDCALL                                   
254801          MOVE 1.0        TO AVG-PRKURS                                   
254901          MOVE 'USD'      TO AVG-KDVALISO                                 
255001        ELSE                                                              
255101          MOVE '010'      TO AVG-KDCALL                                   
255201          MOVE INL-PRKURS TO AVG-PRKURS                                   
255301        END-IF                                                            
255401     ELSE                                                                 
255501        IF (SEND-DCS-NDC-NA AND SEND-DCS-USA)                             
255601        AND (REC-DCS-NDC-NA AND REC-DCS-CANADA)                           
255701          IF DIST79-DEALER-PRICE                                          
255801            MOVE '030'      TO AVG-KDCALL                                 
255901            MOVE 1.0        TO AVG-PRKURS                                 
256001            MOVE 'USD'      TO AVG-KDVALISO                               
256101          ELSE                                                            
256201            MOVE '030'      TO AVG-KDCALL                                 
256301            MOVE INL-PRKURS TO AVG-PRKURS                                 
256401          END-IF                                                          
256501        ELSE                                                              
256601           IF (SEND-DCS-NDC-NA AND SEND-DCS-CANADA)                       
256701           AND (REC-DCS-NDC-NA AND REC-DCS-USA)                           
256801             IF DIST79-DEALER-PRICE                                       
256901               MOVE '030'      TO AVG-KDCALL                              
257001               MOVE 1.0        TO AVG-PRKURS                              
257101               MOVE 'USD'      TO AVG-KDVALISO                            
257201             ELSE                                                         
257301               MOVE '030'        TO AVG-KDCALL                            
257401               MOVE INL-PRKURS   TO AVG-PRKURS                            
257501             END-IF                                                       
257601           ELSE                                                           
257701              IF (SEND-DCS-NDC-NA AND SEND-DCS-USA)                       
257801              AND (REC-DCS-NDC-NA AND REC-DCS-USA)                        
257901                IF DIST79-DEALER-PRICE                                    
258001                  MOVE '020'      TO AVG-KDCALL                           
258101                  MOVE 1.0        TO AVG-PRKURS                           
258201                  MOVE 'USD'      TO AVG-KDVALISO                         
258301                ELSE                                                      
258401                  MOVE '020'      TO AVG-KDCALL                           
258501                  MOVE ZERO       TO AVG-PRKURS                           
258601                END-IF                                                    
258701              END-IF                                                      
258801           END-IF                                                         
258901        END-IF                                                            
259001     END-IF                                                               
259101                                                                          
259201     MOVE W-DAGENS-DATUM(1:2)     TO AVG-TIAA                             
259301     MOVE W-DAGENS-DATUM(3:2)     TO AVG-TIMM                             
259401     CALL W510AVG USING AVG-W510AVG 9305-PCB                              
259501                        AVG-WDB6-PCB                                      
259601     IF AVG-KDSVAR = SPACE                                                
259701        CONTINUE                                                          
259801     ELSE                                                                 
259901        MOVE 'FEL RETURKOD FRÅN W510AVG' TO FELTEXT                       
260001        DISPLAY FELTEXT                                                   
260101        CALL FELLOG                                                       
260201     END-IF                                                               
260301     .                                                                    
260401     EJECT                                                                
260501                                                                          
260601 S06-OMRAKN-MEDELPRIS    SECTION.                                         
260701                                                                          
260801     MOVE REC-WS-IDDC    TO AVG-IDDC                                      
260901     MOVE SLAG-PRAVCOST  TO AVG-PRAVCOST-OLD                              
261001     COMPUTE AVG-KVLS-OLD = WS-OLD-KVLS + WS-OLD-KVEFRS                   
261101     MOVE INL-PRARTNTO   TO AVG-PRARTNTO                                  
261201     MOVE W-KDPRODSL-LOC TO AVG-KDPSLLOC                                  
261301     MOVE W-KDPRODSL     TO AVG-KDPRODSL                                  
261401     MOVE W-IDFKNGRP     TO AVG-IDFKNGRP                                  
261501     MOVE WS-KDVALISO    TO AVG-KDVALISO                                  
261601     MOVE INL-IDDISTR    TO DIST79-IDDISTR                                
261701                                                                          
261801     MOVE W-DAGENS-DATUM(1:2)     TO AVG-TIAA                             
261901     MOVE W-DAGENS-DATUM(3:2)     TO AVG-TIMM                             
262001                                                                          
262101     IF SEND-DCS-CDC OR SEND-DCS-DDC                                      
262201**** HÄMTA RÄTT FAKTURAMÅNAD FÖR ATT RÄKNA UT RÄTT AVERAGECOST            
262301        COMPUTE WS-FAKTURA-DATUM2 = 9999999999999999                      
262401                                  - INL-DAINLEV                           
262501        MOVE WS-FAKTURA-DATUM2     TO WS-FAKTURA-DATUM                    
262601        MOVE WS-FAKTURA-DATUM(3:2) TO AVG-TIAA                            
262701        MOVE WS-FAKTURA-DATUM(5:2) TO AVG-TIMM                            
262801****                                                                      
262901        IF DIST79-DEALER-PRICE                                            
263001            MOVE '010'      TO AVG-KDCALL                                 
263101            MOVE 1.0        TO AVG-PRKURS                                 
263201            MOVE 'USD'      TO AVG-KDVALISO                               
263301        ELSE                                                              
263401            MOVE '010'      TO AVG-KDCALL                                 
263501            MOVE INL-PRKURS TO AVG-PRKURS                                 
263601        END-IF                                                            
263701     ELSE                                                                 
263801        IF ((SEND-DCS-NDC-CN OR SEND-DCS-SDC)                             
263901        AND (NDC-US ))                                                    
264001           IF DIST79-DEALER-PRICE                                         
264101****CHECK    MOVE '022'    TO AVG-KDCALL                                  
264201             MOVE 1.0      TO AVG-PRKURS                                  
264301             MOVE 'USD'    TO AVG-KDVALISO                                
264401           ELSE                                                           
264501****CHECK    MOVE '021'    TO AVG-KDCALL                                  
264601             MOVE ZERO     TO AVG-PRKURS                                  
264701           END-IF                                                         
264801        END-IF                                                            
264901     END-IF                                                               
265001                                                                          
265101     CALL W510AVG USING AVG-W510AVG 9305-PCB                              
265201                        AVG-WDB6-PCB                                      
265301     IF AVG-KDSVAR = SPACE                                                
265401        CONTINUE                                                          
265501     ELSE                                                                 
265601        MOVE 'FEL RETURKOD FRÅN W510AVG' TO FELTEXT                       
265701        DISPLAY FELTEXT                                                   
265801        CALL FELLOG                                                       
265901     END-IF                                                               
266001     .                                                                    
266101     EJECT                                                                
266201                                                                          
266301 S07-SKAPA-NDC-HIST-SANDANDE SECTION.                                     
266401                                                                          
266501**** NYTT IDLEVNR 970918                                                  
266601     MOVE REC-DCS-IDLEVNR-DC TO INL-IDLEVNR                               
266701****                                                                      
266801     MOVE W-IDARTNR         TO ART-IDARTNR                                
266901     PERFORM IMS-ISRT-WLINLC01                                            
267001     ACCEPT W-TIAAMMDDTTMMSSTH-DATE FROM DATE                             
267101     ACCEPT W-TIAAMMDDTTMMSSTH-TIME FROM TIME                             
267201     MOVE FUNCTION CURRENT-DATE (1:2) TO W-TISEKEL                        
267301                                                                          
267401     COMPUTE W-DAINLEV = 9999999999999999                                 
267501                       - W-TIAAAAMMDDTTMMSSTH                             
267601     END-COMPUTE                                                          
267701                                                                          
267801     MOVE W-DAINLEV         TO INL-DAINLEV                                
267901     MOVE ZERO              TO INL-ADLAGOMR                               
268001                               INL-ADGANG                                 
268101                               INL-ADPLATS                                
268201     MOVE SPACE             TO INL-FLMAKUL                                
268301                               INL-FLSKAKOL                               
268401                               INL-ADINLOMR                               
268501                               INL-IDANALYS                               
268601     MOVE 'N'               TO INL-FLPRIO                                 
268701                               INL-FLTULLST                               
268801     MOVE SEND-WS-IDDC      TO INL-IDDC                                   
268901     MOVE SPACE             TO INL-IDDC-LEV                               
269001     MOVE ZERO              TO INL-IDLOPNRM                               
269101     MOVE IDFAKT-WS         TO INL-IDFAKT                                 
269201     MOVE WS-IDDISTR        TO INL-IDDISTR                                
269301     MOVE WS-IDKUNDNR       TO INL-IDKUNDNR                               
269401     MOVE SPACE             TO INL-IDKUNDRF                               
269501     MOVE WS-IDORDNR        TO INL-IDORDNR5                               
269601     MOVE WS-IDKOLLI        TO INL-IDKOLLI                                
269701     MOVE 'R34'             TO INL-IDPTYP                                 
269801     MOVE ZERO              TO INL-KDFRAKT                                
269901     MOVE SPACE             TO INL-KDKOLLI                                
270001                               INL-KDVALISO                               
270101                               INL-IDKST                                  
270201     MOVE WS-IDUSER-003     TO INL-IDUSER-003                             
270301     MOVE ZERO              TO INL-KVANTMOT                               
270401                               INL-KVART-SKROT                            
270501                               INL-KDRT                                   
270601                               INL-IDKONTO                                
270701     SUBTRACT WS-SUMMA-KVANT FROM W-KVAVIS                                
270801                      GIVING   INL-KVAVIS                                 
270901     MOVE ZERO              TO INL-PRARTNTO                               
271001                               INL-PRKURS                                 
271101                               INL-TIBERANK                               
271201                               INL-TIINLINL                               
271301                               INL-TIINLMOT                               
271401                               INL-TIINLMTI                               
271501                               INL-TIINLITI                               
271601                               INL-KVTULRET                               
271701                               INL-KVRETUR                                
271801                               INL-KDAVVANT                               
271901                               INL-TIAVIDAT                               
272001                                                                          
272101     PERFORM IMS-ISRT-WLINLC11                                            
272201     PERFORM UNTIL SEGMENT-FINNS                                          
272301       SUBTRACT 1 FROM W-DAINLEV                                          
272401       MOVE W-DAINLEV TO INL-DAINLEV                                      
272501       PERFORM IMS-ISRT-WLINLC11                                          
272601     END-PERFORM                                                          
272701     .                                                                    
272801     EJECT                                                                
272901                                                                          
273001 S08-SKAPA-SDC-NDC-HIST-MOT-PV SECTION.                                   
273101                                                                          
273201     MOVE WS-IDARTNR TO ART-IDARTNR                                       
273301     PERFORM IMS-ISRT-WLINLC01                                            
273401                                                                          
273501     ACCEPT W-TIAAMMDDTTMMSSTH-DATE FROM DATE                             
273601     ACCEPT W-TIAAMMDDTTMMSSTH-TIME FROM TIME                             
273701     MOVE FUNCTION CURRENT-DATE (1:2) TO W-TISEKEL                        
273801                                                                          
273901     COMPUTE W-DAINLEV = 9999999999999999                                 
274001                       - W-TIAAAAMMDDTTMMSSTH                             
274101     END-COMPUTE                                                          
274201                                                                          
274301     MOVE W-DAINLEV           TO INL-DAINLEV                              
274401     MOVE WS-ADLAGOMR(RAD-IX) TO INL-ADLAGOMR                             
274501     MOVE WS-ADGANG(RAD-IX)   TO INL-ADGANG                               
274601     MOVE WS-ADPLATS(RAD-IX)  TO INL-ADPLATS                              
274701     MOVE REC-WS-IDDC         TO INL-IDDC                                 
274801     MOVE SPACE               TO INL-IDDC-LEV                             
274901     MOVE IDFAKT-WS           TO INL-IDFAKT                               
275001                                                                          
275101     MOVE WS-IDDISTR          TO INL-IDDISTR                              
275201                                 DIST35-IDDISTR                           
275301     MOVE WS-IDKUNDNR         TO INL-IDKUNDNR                             
275401     MOVE SPACE               TO INL-IDKUNDRF                             
275501     MOVE WS-IDORDNR          TO INL-IDORDNR5                             
275601     MOVE WS-IDKOLLI          TO INL-IDKOLLI                              
275701     MOVE 'R32'               TO INL-IDPTYP                               
275801     MOVE WS-IDUSER-003       TO INL-IDUSER-003                           
275901     MOVE WS-KVANTMOT         TO INL-KVANTMOT                             
276001     MOVE WS-KVSKROT          TO INL-KVART-SKROT                          
276101     MOVE ZERO                TO INL-KVAVIS                               
276201     MOVE WS-FIXAD-PRARTNTO   TO INL-PRARTNTO                             
276301     IF DIST35-RETUR                                                      
276401       MOVE +8                TO INL-KDRT                                 
276501     ELSE                                                                 
276601       MOVE ZERO              TO INL-KDRT                                 
276701     END-IF                                                               
276801     MOVE ZERO                TO INL-IDLOPNRM                             
276901                                 INL-KDFRAKT                              
277001                                 INL-PRKURS                               
277101                                 INL-TIBERANK                             
277201                                 INL-IDKONTO                              
277301                                 INL-KVTULRET                             
277401                                 INL-KVRETUR                              
277501                                 INL-KDAVVANT                             
277601                                 INL-TIAVIDAT                             
277701     MOVE SPACE               TO INL-KDVALISO                             
277801                                 INL-KDKOLLI                              
277901                                 INL-FLMAKUL                              
278001                                 INL-FLSKAKOL                             
278101                                 INL-IDKST                                
278201                                 INL-ADINLOMR                             
278301                                 INL-IDANALYS                             
278401     MOVE 'N'                 TO INL-FLPRIO                               
278501                                 INL-FLTULLST                             
278601     MOVE W-TIME-N            TO AKTUELL-TID                              
278701     IF REC-DCS-NDC-NA OR REC-DCS-NDC-PF OR REC-DCS-NDC-OTHERS            
278801        PERFORM S09-FIXA-LOKALTID                                         
278901        MOVE MSGI-TILOKDAT    TO INL-TIINLINL                             
279001                                 INL-TIINLMOT                             
279101        MOVE MSGI-TILOKTID    TO INL-TIINLMTI                             
279201                                 INL-TIINLITI                             
279301     ELSE                                                                 
279401        MOVE W-DAGENS-DATUM   TO INL-TIINLINL                             
279501                                 INL-TIINLMOT                             
279601        MOVE AKTUELL-TTMM     TO INL-TIINLITI                             
279701                                 INL-TIINLMTI                             
279801     END-IF                                                               
279901                                                                          
280001     MOVE WS-KDVALISO         TO INL-KDVALISO                             
280101                                                                          
280201     PERFORM IMS-ISRT-WLINLC11                                            
280301     PERFORM UNTIL SEGMENT-FINNS                                          
280401       SUBTRACT 1 FROM W-DAINLEV                                          
280501       MOVE W-DAINLEV TO INL-DAINLEV                                      
280601       PERFORM IMS-ISRT-WLINLC11                                          
280701     END-PERFORM                                                          
280801     .                                                                    
280901     EJECT                                                                
281001                                                                          
281101 S08-SKAPA-SDC-NDC-HIST-MOT SECTION.                                      
281201     MOVE WS-IDARTNR TO ART-IDARTNR                                       
281301     PERFORM IMS-ISRT-WLINLC01                                            
281401                                                                          
281501     MOVE LOW-VALUE           TO W-WDL6A1KY-MIN2                          
281601     MOVE HIGH-VALUE          TO W-WDL6A1KY-MAX2                          
281701     MOVE IDFAKT-WS           TO W-IDFAKT-MIN                             
281801                                 W-IDFAKT-MAX                             
281901     PERFORM IMS-GU-WDL6A1                                                
282001     IF SEGMENT-SAKNAS                                                    
282101       ACCEPT W-TIAAMMDDTTMMSSTH-DATE FROM DATE                           
282201       ACCEPT W-TIAAMMDDTTMMSSTH-TIME FROM TIME                           
282301       MOVE FUNCTION CURRENT-DATE (1:2) TO W-TISEKEL                      
282401                                                                          
282501       COMPUTE W-DAINLEV = 9999999999999999                               
282601                         - W-TIAAAAMMDDTTMMSSTH                           
282701       END-COMPUTE                                                        
282801     ELSE                                                                 
282901**** NYA ARTIKLAR SKALL HA SAMMA DATUM SOM URSPRUNGSFAKTURAN              
283001       MOVE WDL6A1-SEQA-DAINLEV TO W-DAINLEV                              
283101     END-IF                                                               
283201                                                                          
283301     MOVE W-DAINLEV           TO INL-DAINLEV                              
283401     MOVE WS-ADLAGOMR(RAD-IX) TO INL-ADLAGOMR                             
283501     MOVE WS-ADGANG(RAD-IX)   TO INL-ADGANG                               
283601     MOVE WS-ADPLATS(RAD-IX)  TO INL-ADPLATS                              
283701     MOVE REC-WS-IDDC         TO INL-IDDC                                 
283801     MOVE SPACE               TO INL-IDDC-LEV                             
283901     MOVE IDFAKT-WS           TO INL-IDFAKT                               
284001                                                                          
284101     MOVE WS-IDDISTR          TO INL-IDDISTR                              
284201     MOVE WS-IDKUNDNR         TO INL-IDKUNDNR                             
284301     MOVE SPACE               TO INL-IDKUNDRF                             
284401     MOVE WS-IDORDNR          TO INL-IDORDNR5                             
284501     MOVE WS-IDKOLLI          TO INL-IDKOLLI                              
284601     MOVE 'R32'               TO INL-IDPTYP                               
284701     MOVE WS-IDUSER-003       TO INL-IDUSER-003                           
284801     MOVE WS-KVANTMOT         TO INL-KVANTMOT                             
284901     MOVE WS-KVSKROT          TO INL-KVART-SKROT                          
285001     MOVE ZERO                TO INL-KVAVIS                               
285101     MOVE WS-FIXAD-PRARTNTO   TO INL-PRARTNTO                             
285201     MOVE ZERO                TO INL-IDLOPNRM                             
285301                                 INL-KDFRAKT                              
285401                                 INL-PRKURS                               
285501                                 INL-TIBERANK                             
285601                                 INL-KDRT                                 
285701                                 INL-IDKONTO                              
285801                                 INL-KVTULRET                             
285901                                 INL-KVRETUR                              
286001                                 INL-KDAVVANT                             
286101                                 INL-TIAVIDAT                             
286201     MOVE SPACE               TO INL-KDVALISO                             
286301                                 INL-KDKOLLI                              
286401                                 INL-FLMAKUL                              
286501                                 INL-FLSKAKOL                             
286601                                 INL-IDKST                                
286701                                 INL-ADINLOMR                             
286801                                 INL-IDANALYS                             
286901     MOVE 'N'                 TO INL-FLPRIO                               
287001                                 INL-FLTULLST                             
287101     MOVE W-TIME-N            TO AKTUELL-TID                              
287201     PERFORM S09-FIXA-LOKALTID                                            
287301     MOVE MSGI-TILOKDAT    TO INL-TIINLINL                                
287401                              INL-TIINLMOT                                
287501     MOVE MSGI-TILOKTID    TO INL-TIINLMTI                                
287601                              INL-TIINLITI                                
287701     MOVE WS-KDVALISO         TO INL-KDVALISO                             
287801     PERFORM IMS-ISRT-WLINLC11                                            
287901     PERFORM UNTIL SEGMENT-FINNS                                          
288001       SUBTRACT 1 FROM W-DAINLEV                                          
288101       MOVE W-DAINLEV TO INL-DAINLEV                                      
288201       PERFORM IMS-ISRT-WLINLC11                                          
288301     END-PERFORM                                                          
288401     .                                                                    
288501     EJECT                                                                
288601 S09-FIXA-LOKALTID SECTION.                                               
288701                                                                          
288801     MOVE '011'            TO MSGI-KDCALL                                 
288901     MOVE REC-WS-IDDC      TO WS-IDDC-TID                                 
289001     MOVE WS-IDDC-KOLL     TO MSGI-IDUSER                                 
289101     MOVE W-DAGENS-DATUM   TO MSGI-TILOKDAT                               
289201     MOVE AKTUELL-TTMM     TO MSGI-TILOKTID                               
289301                                                                          
289401     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
289501     IF MSGI-KDSVAR = 'F'                                                 
289601        MOVE 'FEL RETURKOD FRÅN WMSGINIT' TO FELTEXT                      
289701        DISPLAY FELTEXT                                                   
289801        CALL FELLOG                                                       
289901     END-IF                                                               
290001     .                                                                    
290101     EJECT                                                                
290201                                                                          
290301 S11-FLYTTA-SALDOLOGG-DATA SECTION.                                       
290401     MOVE FUNCTION CURRENT-DATE(1:8) TO DAGENS-DATUM                      
290501     ACCEPT W-TIKLOCK                FROM TIME                            
290601     COMPUTE LOGG-DAREGDAT-9KOMPL = 99999999 - DAGENS-DATUM               
290701     COMPUTE LOGG-TIKLOCK-9KOMPL  = 999999999 - W-TIKLOCK                 
290801     MOVE 9                   TO LOGG-IDSEKVNR                            
290901     MOVE 'INBO'              TO LOGG-IDHUVTYP                            
291001     MOVE 'R32'               TO LOGG-IDSUBTYP                            
291101     MOVE 'W6030900'          TO LOGG-IDPGM                               
291201     MOVE '6309'              TO LOGG-IDTRANS                             
291301     MOVE MSG-SIGNON-USERID   TO LOGG-IDUSER                              
291401     MOVE SPACE               TO LOGG-REF                                 
291501     MOVE WS-IDKUNDNR         TO LOGG-IDKUNDNR                            
291601     MOVE WS-IDORDNR          TO LOGG-IDORDNR5                            
291701     MOVE WS-IDARTNR          TO LOGG-IDARTNR                             
291801     MOVE REC-WS-IDDC         TO LOGG-IDDC                                
291901     MOVE WS-IDFAKT           TO LOGG-IDFAKT                              
292001*   ---SALDOFÖRÄNDRINGAR PÅ WDK711                                        
292101*   ---LOGGAS PÅ WDL9                                                     
292201     MOVE SPACE               TO LOGG-IDTECKEN-KVAKS-PAV                  
292301     MOVE SPACE               TO LOGG-IDTECKEN-KVEFRS                     
292401     MOVE SPACE               TO LOGG-IDTECKEN-KVAKS                      
292501     MOVE WS-KVANTMOT         TO LOGG-KVART-SALDO                         
292601     MOVE '+'                 TO LOGG-IDTECKEN-KVLS                       
292701     IF REC-DCS-CDC                                                       
292801        MOVE CLAG-KVLS        TO LOGG-KVLS                                
292901        COMPUTE LOGG-KVAKS     = CLAG-KVAKS-CDC                           
293001                               + CLAG-KVAKS-T                             
293101     ELSE                                                                 
293201        MOVE SLAG-KVLS        TO LOGG-KVLS                                
293301        MOVE SLAG-KVAKS-SDC   TO LOGG-KVAKS                               
293401     END-IF                                                               
293501     MOVE '00000000'          TO LOGG-DAREGDAT-LADD                       
293601     .                                                                    
293701     EJECT                                                                
293801                                                                          
293901 S12-ISRT-SALDOLOGG SECTION.                                              
294001     PERFORM IMS-ISRT-WDL901                                              
294101     IF SEGMENT-FINNS-REDAN                                               
294201       PERFORM UNTIL NOT SEGMENT-FINNS-REDAN                              
294301         SUBTRACT 1         FROM LOGG-IDSEKVNR                            
294401         PERFORM IMS-ISRT-WDL901                                          
294501       END-PERFORM                                                        
294601     END-IF                                                               
294701     .                                                                    
294801     EJECT                                                                
294901                                                                          
295001 S13-UPPDATERA-WDJ9 SECTION.                                              
295101                                                                          
295201     PERFORM IMS-GU-LOCB01                                                
295301     IF SEGMENT-SAKNAS                                                    
295401       MOVE W-IDARTNR TO LOCB-ART-IDARTNR                                 
295501       PERFORM IMS-ISRT-LOCB01                                            
295601       PERFORM IMS-GU-LOCB01                                              
295701     END-IF                                                               
295801     IF SEGMENT-FINNS                                                     
295901       PERFORM UNTIL SEGMENT-SAKNAS OR LOCB-HIST-KDLOC = 'P'              
296001         PERFORM IMS-GHNP-LOCB11                                          
296101         IF SEGMENT-FINNS AND LOCB-HIST-KDLOC = 'P'                       
296201           MOVE FUNCTION CURRENT-DATE(1:8) TO LOCB-HIST-DASTODAT          
296301           MOVE MSGI-IDUSER TO LOCB-HIST-IDUSER-STO                       
296401           PERFORM IMS-REPL-LOCB11                                        
296501         END-IF                                                           
296601       END-PERFORM                                                        
296701       MOVE FUNCTION CURRENT-DATE(1:8)  TO WS-LOGG-DATUM                  
296801       MOVE FUNCTION CURRENT-DATE(9:6)  TO WS-LOGG-TID                    
296901       COMPUTE LOCB-HIST-DASTADAT-9KOMPL = 99999999 -                     
297001                                                 WS-LOGG-DATUM            
297101       COMPUTE LOCB-HIST-TISTATID-9KOMPL = 999999 - WS-LOGG-TID           
297201       MOVE MSGI-IDDC                   TO LOCB-HIST-IDDC                 
297301       MOVE WS-PRIME-LOCATION           TO LOCB-HIST-KDLOC                
297401       MOVE W-ADLAGOMR-LOCB             TO LOCB-HIST-ADLAGOMR             
297501       MOVE W-ADGANG-LOCB               TO LOCB-HIST-ADGANG               
297601       MOVE W-ADPLATS-LOCB              TO LOCB-HIST-ADPLATS              
297701       MOVE MSGI-IDUSER                 TO LOCB-HIST-IDUSER               
297801       MOVE SPACE                       TO LOCB-HIST-IDUSER-STO           
297901       MOVE ZERO                        TO LOCB-HIST-DASTODAT             
298001                                                                          
298101       PERFORM IMS-ISRT-LOCB11                                            
298201     END-IF                                                               
298301     .                                                                    
298401     EJECT                                                                
298501 S14-SKAPA-SAP-TRANS-PV SECTION.                                          
298601******************************************************************        
298701* ÄT SAP  UPPDAT-TRANS WDR9 SKAPAS VID IDDC-SEND = CDC                    
298801* 980420  BÅDE FÖR SDC OCH NDC. TRANSEN SKA ERSÄTTA 'AVV'-TRANS           
298901*         (WDR8) FÖR SDC OCH TILLKOMMER FÖR NDC. 'AVV'-TRANS              
299001*         LIGGER KVAR TILLS VIDARE (WDR8).                                
299101******************************************************************        
299201                                                                          
299301     MOVE 'W6030900'                  TO FIL-IDPGM IN FIL-WDR901          
299401     MOVE FUNCTION CURRENT-DATE (1:8) TO WS-SAP-AAAAMMDD                  
299501     MOVE WS-SAP-AAAAMMDD             TO FIL-DAREGDAT                     
299601                                         EKH-DAVERDAT                     
299701     MOVE FUNCTION CURRENT-DATE (9:8) TO WS-SAP-TTMMSSTH                  
299801     MOVE WS-SAP-TTMMSSTH             TO FIL-TIKLOCK IN FIL-WDR901        
299901     ADD +1                           TO W-IDSEKVNR-SAP                   
300001     MOVE W-IDSEKVNR-SAP            TO FIL-IDSEKVNR IN FIL-WDR901         
300101     IF SEND-DCS-NDC-CN                                                   
300201     OR (SEND-DCS-USA AND REC-DCS-CDC)                                    
300301       MOVE '102'                     TO EKH-KDEKHHT                      
300401       MOVE '122'                     TO EKH-KDEKSHT                      
300501     ELSE                                                                 
300601       MOVE WS-IDDISTR TO DIST35-IDDISTR                                  
300701       IF DIST35-RETUR                                                    
300801         MOVE '502'                     TO EKH-KDEKHHT                    
300901         MOVE '504'                     TO EKH-KDEKSHT                    
301001       ELSE                                                               
301101         MOVE '503'                     TO EKH-KDEKHHT                    
301201         MOVE '504'                     TO EKH-KDEKSHT                    
301301       END-IF                                                             
301401     END-IF                                                               
301501     MOVE 'DET  '                     TO EKH-KDEKNIVA                     
301601     MOVE SEND-WS-IDDC                TO EKH-IDDC-SEND                    
301701     MOVE REC-WS-IDDC                 TO EKH-IDDC-REC                     
301801     MOVE WS-IDDISTR                  TO EKH-IDDISTR                      
301901                                         DIST35-IDDISTR                   
302001     MOVE WS-IDKUNDNR                 TO EKH-IDKUNDNR                     
302101                                                                          
302201     MOVE ZERO TO NOLL-RAKNARE                                            
302301     MOVE WS-IDFAKT                   TO WS-SAP-IDFAKT                    
302401     INSPECT WS-SAP-IDFAKT TALLYING NOLL-RAKNARE                          
302501          FOR LEADING ZERO                                                
302601     ADD +1 TO NOLL-RAKNARE                                               
302701     UNSTRING WS-SAP-IDFAKT         INTO EKH-IDVERGL                      
302801          WITH POINTER NOLL-RAKNARE                                       
302901                                                                          
303001     MOVE WS-SAP-PRARTSTD             TO EKH-PRARTSTD                     
303101     MOVE W-KDPRODSL                  TO EKH-KDPRODSL                     
303201     MOVE ZERO                        TO EKH-KDPSLLOC                     
303301                                         EKH-PRARTNTO                     
303401                                         EKH-PRARTSJK                     
303501                                         EKH-PRHEMTAG                     
303601                                         EKH-PRINK                        
303701                                         EKH-PRDIRLON                     
303801                                         EKH-PRDMTRL                      
303901                                         EKH-PROVRPAL                     
304001                                         EKH-SUBEL                        
304101     MOVE W-IDARTNR                   TO EKH-IDARTNR                      
304201     MOVE SPACE                       TO EKH-FLLSBOK                      
304301     MOVE 'SEK'                       TO EKH-KDVALISO                     
304401********* EV ÄNDRING PRKURS                                               
304501     MOVE 1.00                        TO EKH-PRKURS                       
304601*********                                                                 
304701     MOVE WS-KVANTMOT                 TO EKH-KVANTAL                      
304801     MOVE 'W510EKHA'                  TO FIL-IDCPYTXT IN                  
304901                                                      FIL-WDR901          
305001     MOVE '6309'                      TO EKH-IDTRANS                      
305101     MOVE MSG-SIGNON-USERID           TO FIL-IDUSER IN FIL-WDR901         
305201     IF DIST35-RETUR                                                      
305301        MOVE +8                       TO EKH-KDRT                         
305401     ELSE                                                                 
305501        MOVE ZERO                     TO EKH-KDRT                         
305601     END-IF                                                               
305701     MOVE ZERO                        TO EKH-BEVAT                        
305801                                         EKH-IDANALYS                     
305901                                         EKH-IDKONTO                      
306001                                         EKH-KDANMORS                     
306101                                         EKH-SUVAT                        
306201                                         EKH-KDFRAKT                      
306301                                         EKH-PRLANDCO                     
306401                                         EKH-DAAVIDAT                     
306501                                         EKH-IDAVINR                      
306601                                         EKH-KDAVVTYP                     
306701                                         EKH-KVANTMOT                     
306801                                         EKH-KVAVIS                       
306901     MOVE WS-KDSORT                   TO EKH-KDSORT                       
307001     MOVE SPACE                       TO EKH-KDTRADP                      
307101                                         EKH-IDKST                        
307201                                         EKH-IDLEVNR                      
307301                                         EKH-FLDCET                       
307401                                         EKH-IDKUNDRF                     
307501                                         EKH-IDFAKT-EXP                   
307601                                                                          
307701     PERFORM IMS-ISRT-WLSAPA01                                            
307801                                                                          
307901     PERFORM UNTIL SEGMENT-FINNS                                          
308001         ADD +1 TO FIL-IDSEKVNR IN FIL-WDR901                             
308101         PERFORM IMS-ISRT-WLSAPA01                                        
308201     END-PERFORM                                                          
308301     .                                                                    
308401     EJECT                                                                
308501 S14-SKAPA-SAP-TRANS SECTION.                                             
308601******************************************************************        
308701* ÄT SAP  UPPDAT-TRANS WDR9 SKAPAS VID IDDC-SEND = CDC                    
308801* 980420  BÅDE FÖR SDC OCH NDC. TRANSEN SKA ERSÄTTA 'AVV'-TRANS           
308901*         (WDR8) FÖR SDC OCH TILLKOMMER FÖR NDC. 'AVV'-TRANS              
309001*         LIGGER KVAR TILLS VIDARE (WDR8).                                
309101******************************************************************        
309201                                                                          
309301     MOVE 'W6030900'                  TO FIL-IDPGM IN FIL-WDR801          
309401     ACCEPT FIL-TIREGDAT IN FIL-WDR801 FROM DATE                          
309501     MOVE FUNCTION CURRENT-DATE (1:8) TO WS-SAP-AAAAMMDD                  
309601     MOVE WS-SAP-AAAAMMDD             TO R8-EKH-DAVERDAT                  
309701     MOVE FUNCTION CURRENT-DATE (9:8) TO WS-SAP-TTMMSSTH                  
309801     MOVE WS-SAP-TTMMSSTH             TO FIL-TIKLOCK IN FIL-WDR801        
309901     ADD +1                           TO W-IDSEKVNR-SAP                   
310001     MOVE W-IDSEKVNR-SAP            TO FIL-IDSEKVNR  IN FIL-WDR801        
310101     IF SEND-DCS-CDC OR SEND-DCS-DDC                                      
310201       MOVE '102'                       TO R8-EKH-KDEKHHT                 
310301**** FOR BOUNCE FLOW                                                      
310401       IF DIST35-NONVCC-NONVCC-REFILL                                     
310501         MOVE '132'                     TO R8-EKH-KDEKSHT                 
310601       ELSE                                                               
310701         MOVE '122'                     TO R8-EKH-KDEKSHT                 
310801       END-IF                                                             
310901     END-IF                                                               
311001     MOVE 'DET  '                     TO R8-EKH-KDEKNIVA                  
311101     MOVE SEND-WS-IDDC                TO R8-EKH-IDDC-SEND                 
311201     MOVE REC-WS-IDDC                 TO R8-EKH-IDDC-REC                  
311301     MOVE WS-IDDISTR                  TO R8-EKH-IDDISTR                   
311401     MOVE WS-IDKUNDNR                 TO R8-EKH-IDKUNDNR                  
311501                                                                          
311601     MOVE ZERO TO NOLL-RAKNARE                                            
311701     MOVE WS-IDFAKT                   TO WS-SAP-IDFAKT                    
311801     INSPECT WS-SAP-IDFAKT TALLYING NOLL-RAKNARE                          
311901          FOR LEADING ZERO                                                
312001     ADD +1 TO NOLL-RAKNARE                                               
312101     UNSTRING WS-SAP-IDFAKT         INTO R8-EKH-IDVERGL                   
312201          WITH POINTER NOLL-RAKNARE                                       
312301                                                                          
312401     MOVE SLAG-PRAVCOST               TO R8-EKH-PRARTSTD                  
312501     MOVE W-KDPRODSL                  TO R8-EKH-KDPRODSL                  
312601     MOVE ZERO                        TO R8-EKH-KDPSLLOC                  
312701     MOVE INL-PRARTNTO                TO R8-EKH-PRARTNTO                  
312801     MOVE ZERO                        TO R8-EKH-PRHEMTAG                  
312901                                         R8-EKH-PRARTSJK                  
313001                                         R8-EKH-PRINK                     
313101                                         R8-EKH-PRDIRLON                  
313201                                         R8-EKH-PRDMTRL                   
313301                                         R8-EKH-PROVRPAL                  
313401                                         R8-EKH-SUBEL                     
313501     MOVE W-IDARTNR                   TO R8-EKH-IDARTNR                   
313601     MOVE SPACE                       TO R8-EKH-FLLSBOK                   
313701     MOVE 'USD'                       TO R8-EKH-KDVALISO                  
313801********* EV ÄNDRING PRKURS                                               
313901     MOVE 1.00                        TO R8-EKH-PRKURS                    
314001*********                                                                 
314101     MOVE WS-KVANTMOT                 TO R8-EKH-KVANTAL                   
314201     MOVE 'W561EKHA'                  TO FIL-IDCPYTXT IN                  
314301                                                      FIL-WDR801          
314401     MOVE '6309'                      TO R8-EKH-IDTRANS                   
314501     MOVE ZERO                        TO R8-EKH-BEVAT                     
314601                                         R8-EKH-IDANALYS                  
314701                                         R8-EKH-IDKONTO                   
314801                                         R8-EKH-KDANMORS                  
314901                                         R8-EKH-SUVAT                     
315001                                         R8-EKH-KDFRAKT                   
315101                                         R8-EKH-PRLANDCO                  
315201                                         R8-EKH-DAAVIDAT                  
315301                                         R8-EKH-IDAVINR                   
315401                                         R8-EKH-KDAVVTYP                  
315501                                         R8-EKH-KDRT                      
315601                                         R8-EKH-KVANTMOT                  
315701                                         R8-EKH-KVAVIS                    
315801     MOVE WS-KDSORT                   TO R8-EKH-KDSORT                    
315901     MOVE 'US01'                      TO R8-EKH-KDTRADP                   
316001     MOVE SPACE                       TO R8-EKH-IDLEVNR                   
316101                                         R8-EKH-IDKST                     
316201     MOVE SPACE                       TO R8-EKH-FLDCET                    
316301     MOVE SPACE                       TO R8-EKH-IDKUNDRF                  
316401     MOVE SPACE                       TO R8-EKH-IDFAKT-EXP                
316501                                                                          
316601     PERFORM IMS-ISRT-WLFILB01                                            
316701                                                                          
316801     PERFORM UNTIL SEGMENT-FINNS                                          
316901         ADD +1 TO FIL-IDSEKVNR IN FIL-WDR801                             
317001         PERFORM IMS-ISRT-WLFILB01                                        
317101     END-PERFORM                                                          
317201     .                                                                    
317301     EJECT                                                                
317401* --- IMS SEKTIONER ---                                                   
317501     SKIP3                                                                
317601                                                                          
317701 IMS-GET-MSG SECTION.                                                     
317801                                                                          
317901     MOVE '  QC' TO GODK-STATUSKODER                                      
318001     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
318101     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
318201     PERFORM IMS-STATUSKONTROLL                                           
318301     .                                                                    
318401     SKIP3                                                                
318501                                                                          
318601 IMS-INSERT-MSG SECTION.                                                  
318701                                                                          
318801     IF SWEDISH-TEXT                                                      
318901        IF MSGI-IDLAND-SPR NOT = 'GB'                                     
319001           MOVE '0' TO MFS-KDHUVOMR                                       
319101        END-IF                                                            
319201     END-IF                                                               
319301                                                                          
319401     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
319501     MOVE SPACE TO GODK-STATUSKODER                                       
319601     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
319701                                                                          
319801     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
319901     PERFORM IMS-STATUSKONTROLL                                           
320001     .                                                                    
320101     EJECT                                                                
320201                                                                          
320301 IMS-GHU-WL630111 SECTION.                                                
320401                                                                          
320501     STRING 'WL630101(WDGXKEY = ' W-6301KEY-X ')'                         
320601          DELIMITED BY SIZE INTO SSA1                                     
320701     STRING 'WL630111(IDFAKT  = ' W-IDFAKT-X ')'                          
320801          DELIMITED BY SIZE INTO SSA2                                     
320901     MOVE '  GE' TO GODK-STATUSKODER                                      
321001     CALL CBLTDLI USING GHU GX63-PCB DLI-IO-AREA-WDGX SSA1 SSA2           
321101                                                                          
321201     MOVE GX63-STATUS-CODE TO STATUS-WS                                   
321301     PERFORM IMS-STATUSKONTROLL                                           
321401     .                                                                    
321501     SKIP3                                                                
321601                                                                          
321701 IMS-GET-WDK711 SECTION.                                                  
321801                                                                          
321901     STRING 'WDK701  (IDARTNR = ' W-IDARTNR-X ')'                         
322001          DELIMITED BY SIZE INTO SSA1                                     
322101     STRING 'WDK711  (IDDC    = ' W-IDDC    ')'                           
322201          DELIMITED BY SIZE INTO SSA2                                     
322301     MOVE '  GE' TO GODK-STATUSKODER                                      
322401     CALL CBLTDLI USING GHU WDK7-PCB DLI-IO-AREA-WDK7 SSA1 SSA2           
322501     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
322601     PERFORM IMS-STATUSKONTROLL                                           
322701     .                                                                    
322801     EJECT                                                                
322901                                                                          
323001 IMS-REPL-WDK711 SECTION.                                                 
323101                                                                          
323201     MOVE '  ' TO GODK-STATUSKODER                                        
323301     CALL CBLTDLI USING REPL WDK7-PCB DLI-IO-AREA-WDK7                    
323401     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
323501     PERFORM IMS-STATUSKONTROLL                                           
323601     .                                                                    
323701     SKIP3                                                                
323801                                                                          
323901 IMS-GU-WLARTC01   SECTION.                                               
324001                                                                          
324101     STRING 'WLARTC01(IDARTNR = ' W-IDARTNR-X ')'                         
324201          DELIMITED BY SIZE INTO SSA1                                     
324301     MOVE '  GE' TO GODK-STATUSKODER                                      
324401     CALL CBLTDLI USING GU  ARTC-PCB DLI-IO-AREA-WDK6 SSA1                
324501     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
324601     PERFORM IMS-STATUSKONTROLL                                           
324701     .                                                                    
324801     SKIP3                                                                
324901                                                                          
325001 IMS-GHU-WLARTC11     SECTION.                                            
325101                                                                          
325201     STRING 'WLARTC01(IDARTNR = ' W-IDARTNR-X ')'                         
325301          DELIMITED BY SIZE INTO SSA1                                     
325401     STRING 'WLARTC11(KDSEGKEY= ' W-KDSEGKEY-X ')'                        
325501          DELIMITED BY SIZE INTO SSA2                                     
325601     MOVE SPACE  TO GODK-STATUSKODER                                      
325701     CALL CBLTDLI USING GHU ARTC-PCB DLI-IO-AREA-WDK6 SSA1 SSA2           
325801     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
325901     PERFORM IMS-STATUSKONTROLL                                           
326001     .                                                                    
326101     SKIP3                                                                
326201                                                                          
326301 IMS-REPL-WLARTC11 SECTION.                                               
326401                                                                          
326501     MOVE '  ' TO GODK-STATUSKODER                                        
326601     CALL CBLTDLI USING REPL ARTC-PCB DLI-IO-AREA-WDK6                    
326701     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
326801     PERFORM IMS-STATUSKONTROLL                                           
326901     .                                                                    
327001     SKIP3                                                                
327101                                                                          
327201 IMS-GU-WDL6A1   SECTION.                                                 
327301     STRING 'WDL6A1  (WDL6A1KY=>' W-WDL6A1KY-MIN2                         
327401                    '&WDL6A1KY=<' W-WDL6A1KY-MAX2 ')'                     
327501          DELIMITED BY SIZE INTO SSA1                                     
327601     MOVE '  GE' TO GODK-STATUSKODER                                      
327701     CALL CBLTDLI USING GU WDL6A-PCB DLI-IO-L6A1 SSA1                     
327801     MOVE WDL6A-STATUS-CODE TO STATUS-WS                                  
327901     PERFORM IMS-STATUSKONTROLL                                           
328001     .                                                                    
328101     SKIP3                                                                
328201                                                                          
328301 IMS-ISRT-WLINLC01   SECTION.                                             
328401                                                                          
328501     MOVE 'WLINLC01 '      TO SSA1                                        
328601     MOVE '  II'           TO GODK-STATUSKODER                            
328701     CALL CBLTDLI USING ISRT INLC-PCB DLI-IO-AREA-WDL6 SSA1               
328801     MOVE INLC-STATUS-CODE TO STATUS-WS                                   
328901     PERFORM IMS-STATUSKONTROLL                                           
329001     .                                                                    
329101     SKIP3                                                                
329201                                                                          
329301 IMS-ISRT-WLINLC11   SECTION.                                             
329401                                                                          
329501     STRING 'WLINLC01(IDARTNR  =' W-IDARTNR-X ')'                         
329601          DELIMITED BY SIZE INTO SSA1                                     
329701     MOVE 'WLINLC11 '         TO SSA2                                     
329801     MOVE '  II'              TO GODK-STATUSKODER                         
329901     CALL CBLTDLI USING ISRT INLC-PCB DLI-IO-AREA-WDL6 SSA1 SSA2          
330001     MOVE INLC-STATUS-CODE TO STATUS-WS                                   
330101     PERFORM IMS-STATUSKONTROLL                                           
330201     .                                                                    
330301     EJECT                                                                
330401                                                                          
330501 IMS-ISRT-WLFILB01 SECTION.                                               
330601                                                                          
330701     MOVE '  ' TO GODK-STATUSKODER                                        
330801     MOVE 'WLFILB01 ' TO SSA1                                             
330901     CALL CBLTDLI USING ISRT FILB-PCB DLI-IO-AREA-WDR8 SSA1               
331001     MOVE FILB-STATUS-CODE TO STATUS-WS                                   
331101     PERFORM IMS-STATUSKONTROLL                                           
331201     .                                                                    
331301     SKIP3                                                                
331401                                                                          
331501 IMS-GHU-WL630511 SECTION.                                                
331601     STRING 'WL630501(WDGXKEY  =' W-6305KEY-X ')'                         
331701          DELIMITED BY SIZE INTO SSA1                                     
331801     STRING 'WL630511(IDFAKT   =' W-IDFAKT-X  ')'                         
331901          DELIMITED BY SIZE INTO SSA2                                     
332001     MOVE 'GE  ' TO GODK-STATUSKODER                                      
332101     CALL CBLTDLI USING GHU GX65-PCB DLI-IO-AREA-WDGX2 SSA1 SSA2          
332201     MOVE GX65-STATUS-CODE TO STATUS-WS                                   
332301     PERFORM IMS-STATUSKONTROLL                                           
332401     .                                                                    
332501     SKIP2                                                                
332601                                                                          
332701 IMS-GU-WL630501 SECTION.                                                 
332801     STRING 'WL630501(WDGXKEY  =' W-6305KEY-X ')'                         
332901          DELIMITED BY SIZE INTO SSA1                                     
333001     MOVE 'GE  ' TO GODK-STATUSKODER                                      
333101     CALL CBLTDLI USING GHU GX65-PCB DLI-IO-AREA-WDGX2 SSA1               
333201     MOVE GX65-STATUS-CODE TO STATUS-WS                                   
333301     PERFORM IMS-STATUSKONTROLL                                           
333401     .                                                                    
333501     SKIP2                                                                
333601                                                                          
333701 IMS-GET-WL630511-FIRST SECTION.                                          
333801     STRING 'WL630511*F(IDFAKT   =' W-IDFAKT-X  ')'                       
333901          DELIMITED BY SIZE INTO SSA1                                     
334001     MOVE 'GE  ' TO GODK-STATUSKODER                                      
334101     CALL CBLTDLI USING GNP GX65-PCB DLI-IO-AREA-WDGX2 SSA1               
334201     MOVE GX65-STATUS-CODE TO STATUS-WS                                   
334301     PERFORM IMS-STATUSKONTROLL                                           
334401     .                                                                    
334501     SKIP2                                                                
334601                                                                          
334701 IMS-ISRT-WL630511 SECTION.                                               
334801     STRING 'WL630501(WDGXKEY  =' W-6305KEY-X  ')'                        
334901          DELIMITED BY SIZE INTO SSA1                                     
335001     MOVE 'WL630511 ' TO SSA2                                             
335101     MOVE '  ' TO GODK-STATUSKODER                                        
335201     CALL CBLTDLI USING ISRT GX65-PCB DLI-IO-AREA-WDGX2 SSA1 SSA2         
335301     MOVE GX65-STATUS-CODE TO STATUS-WS                                   
335401     PERFORM IMS-STATUSKONTROLL                                           
335501     .                                                                    
335601     EJECT                                                                
335701                                                                          
335801 IMS-ISRT-WL630521 SECTION.                                               
335901     STRING 'WL630501(WDGXKEY  =' W-6305KEY-X    ')'                      
336001          DELIMITED BY SIZE INTO SSA1                                     
336101     STRING 'WL630511(IDFAKT   =' W-IDFAKT-X      ')'                     
336201          DELIMITED BY SIZE INTO SSA2                                     
336301     MOVE 'WL630521 ' TO SSA3                                             
336401     MOVE 'II  ' TO GODK-STATUSKODER                                      
336501     CALL CBLTDLI USING ISRT GX65-PCB DLI-IO-AREA-WDGX2                   
336601                                      SSA1 SSA2 SSA3                      
336701     MOVE GX65-STATUS-CODE TO STATUS-WS                                   
336801     PERFORM IMS-STATUSKONTROLL                                           
336901     .                                                                    
337001     SKIP3                                                                
337101                                                                          
337201 IMS-REPL-WL630511 SECTION.                                               
337301     MOVE SPACE TO GODK-STATUSKODER                                       
337401     CALL CBLTDLI USING REPL GX65-PCB DLI-IO-AREA-WDGX2                   
337501     MOVE GX65-STATUS-CODE TO STATUS-WS                                   
337601     PERFORM IMS-STATUSKONTROLL                                           
337701     .                                                                    
337801     EJECT                                                                
337901                                                                          
338001 IMS-ISRT-4506 SECTION.                                                   
338101     STRING 'WL450501(WDGXKEY  =' W-4505-KEY-X ')'                        
338201          DELIMITED BY SIZE INTO SSA1                                     
338301     MOVE 'WL450511 ' TO SSA2                                             
338401     MOVE '  ' TO GODK-STATUSKODER                                        
338501     CALL CBLTDLI USING ISRT 4505-PCB DLI-IO-AREA-4505 SSA1 SSA2          
338601     MOVE 4505-STATUS-CODE TO STATUS-WS                                   
338701     PERFORM IMS-STATUSKONTROLL                                           
338801     .                                                                    
338901     SKIP3                                                                
339001                                                                          
339101 IMS-ISRT-WLFILC SECTION.                                                 
339201     STRING 'WLFILC01    '                                                
339301          DELIMITED BY SIZE INTO SSA1                                     
339401     MOVE '   ' TO GODK-STATUSKODER                                       
339501     CALL CBLTDLI USING ISRT FILC-PCB DLI-IO-AREA-FILC SSA1               
339601     MOVE FILC-STATUS-CODE TO STATUS-WS                                   
339701     PERFORM IMS-STATUSKONTROLL                                           
339801     .                                                                    
339901     EJECT                                                                
340001 IMS-ISRT-WLFILC2 SECTION.                                                
340101     STRING 'WLFILC01    '                                                
340201          DELIMITED BY SIZE INTO SSA1                                     
340301     MOVE '   ' TO GODK-STATUSKODER                                       
340401     CALL CBLTDLI USING ISRT FILC-PCB DLI-IO-AREA-FILC2 SSA1              
340501     MOVE FILC-STATUS-CODE TO STATUS-WS                                   
340601     PERFORM IMS-STATUSKONTROLL                                           
340701     .                                                                    
340801     EJECT                                                                
340901 IMS-ISRT-WLFILC3 SECTION.                                                
341001     STRING 'WLFILC01    '                                                
341101          DELIMITED BY SIZE INTO SSA1                                     
341201     MOVE '   ' TO GODK-STATUSKODER                                       
341301     CALL CBLTDLI USING ISRT FILC-PCB DLI-IO-AREA-FILC3 SSA1              
341401     MOVE FILC-STATUS-CODE TO STATUS-WS                                   
341501     PERFORM IMS-STATUSKONTROLL                                           
341601     .                                                                    
341701     EJECT                                                                
341801 IMS-ISRT-WDL901 SECTION.                                                 
341901                                                                          
342001     MOVE 'WLLOGA01 ' TO SSA1                                             
342101     MOVE '  II' TO GODK-STATUSKODER                                      
342201     CALL CBLTDLI USING ISRT WLLOGA-PCB WLLOGA01 SSA1                     
342301     MOVE WLLOGA-STATUS-CODE TO STATUS-WS                                 
342401     PERFORM IMS-STATUSKONTROLL                                           
342501     .                                                                    
342601     SKIP2                                                                
342701 IMS-ISRT-WLSAPA01 SECTION.                                               
342801     MOVE 'WLSAPA01 ' TO SSA1                                             
342901     MOVE '  II' TO GODK-STATUSKODER                                      
343001     CALL CBLTDLI USING ISRT SAPA-PCB WLSAPA01 SSA1                       
343101     MOVE SAPA-STATUS-CODE TO STATUS-WS                                   
343201     PERFORM IMS-STATUSKONTROLL                                           
343301     .                                                                    
343401     EJECT                                                                
343501 IMS-GU-LOCB01 SECTION.                                                   
343601                                                                          
343701     STRING 'WLLOCB01(IDARTNR  =' W-IDARTNR-X ')'                         
343801          DELIMITED BY SIZE INTO SSA1                                     
343901     MOVE '  GE' TO GODK-STATUSKODER                                      
344001     CALL CBLTDLI USING GU LOCB-PCB DLI-IO-AREA-LOCB SSA1                 
344101     MOVE LOCB-STATUS-CODE TO STATUS-WS                                   
344201     PERFORM IMS-STATUSKONTROLL                                           
344301     .                                                                    
344401     EJECT                                                                
344501                                                                          
344601 IMS-ISRT-LOCB01 SECTION.                                                 
344701                                                                          
344801     MOVE 'WLLOCB01 ' TO SSA1                                             
344901     MOVE '  ' TO GODK-STATUSKODER                                        
345001     CALL CBLTDLI USING ISRT LOCB-PCB DLI-IO-AREA-LOCB SSA1               
345101     MOVE LOCB-STATUS-CODE TO STATUS-WS                                   
345201     PERFORM IMS-STATUSKONTROLL                                           
345301     .                                                                    
345401     SKIP3                                                                
345501                                                                          
345601 IMS-GHNP-LOCB11 SECTION.                                                 
345701                                                                          
345801     STRING 'WLLOCB11(IDDC     =' W-IDDC   ')'                            
345901             DELIMITED BY SIZE INTO SSA1                                  
346001     MOVE '  GE' TO GODK-STATUSKODER                                      
346101     CALL CBLTDLI USING GHNP LOCB-PCB DLI-IO-AREA-LOCB SSA1               
346201     MOVE LOCB-STATUS-CODE TO STATUS-WS                                   
346301     PERFORM IMS-STATUSKONTROLL                                           
346401                                                                          
346501     EJECT                                                                
346601     .                                                                    
346701                                                                          
346801 IMS-REPL-LOCB11 SECTION.                                                 
346901                                                                          
347001     MOVE '  ' TO GODK-STATUSKODER                                        
347101     CALL CBLTDLI USING REPL LOCB-PCB DLI-IO-AREA-LOCB                    
347201     MOVE LOCB-STATUS-CODE TO STATUS-WS                                   
347301     PERFORM IMS-STATUSKONTROLL                                           
347401     .                                                                    
347501     EJECT                                                                
347601     SKIP3                                                                
347701                                                                          
347801 IMS-ISRT-LOCB11 SECTION.                                                 
347901                                                                          
348001     STRING 'WLLOCB01(IDARTNR  =' W-IDARTNR-X ')'                         
348101          DELIMITED BY SIZE INTO SSA1                                     
348201     MOVE 'WLLOCB11 ' TO SSA2                                             
348301     MOVE '  II' TO GODK-STATUSKODER                                      
348401     CALL CBLTDLI USING ISRT LOCB-PCB DLI-IO-AREA-LOCB SSA1 SSA2          
348501     MOVE LOCB-STATUS-CODE TO STATUS-WS                                   
348601     PERFORM IMS-STATUSKONTROLL                                           
348701     .                                                                    
348801     SKIP3                                                                
348901                                                                          
349001 IMS-GU-WDB601-REC  SECTION.                                              
349101     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
349201          DELIMITED BY SIZE INTO SSA1                                     
349301     MOVE '  ' TO GODK-STATUSKODER                                        
349401     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601-REC SSA1             
349501     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
349601     PERFORM IMS-STATUSKONTROLL                                           
349701     IF SEGMENT-SAKNAS                                                    
349801        MOVE SPACE TO REC-DCS-KDDC                                        
349901     END-IF                                                               
350001     .                                                                    
350101     EJECT                                                                
350201                                                                          
350301 IMS-GU-WDB601-SEND SECTION.                                              
350401     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
350501          DELIMITED BY SIZE INTO SSA1                                     
350601     MOVE '  GE' TO GODK-STATUSKODER                                      
350701     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601-SEND SSA1            
350801     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
350901     PERFORM IMS-STATUSKONTROLL                                           
351001     IF SEGMENT-SAKNAS                                                    
351101        MOVE SPACE TO SEND-DCS-KDDC                                       
351201     END-IF                                                               
351301     .                                                                    
351401     EJECT                                                                
351501 IMS-STATUSKONTROLL SECTION.                                              
351601                                                                          
351701     SET STATUS-IX TO 1                                                   
351801     SEARCH GODK-STATUS                                                   
351901       AT END                                                             
352001         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
352101         DELIMITED BY SIZE INTO FELTEXT                                   
352201         CALL FELLOG                                                      
352301       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
352401         CONTINUE                                                         
352501     END-SEARCH                                                           
352601     .                                                                    
352701     EJECT                                                                
352801 DB2-SELECT-TP6FAKT SECTION.                                              
352901     MOVE 000100305 TO GOOD-SQLCODECODES                                  
353001     EXEC SQL                                                             
353101         SELECT  IDFAKT                                                   
353201                                                                          
353301         INTO :RAD-IDFAKT                                                 
353401                                                                          
353501         FROM    TP6FAKT                                                  
353601                                                                          
353701     END-EXEC                                                             
353801     MOVE SQLCODE TO SQLCODE-WS                                           
353901     PERFORM DB2-STATUS-CHECK                                             
354001     .                                                                    
354101     EJECT                                                                
354201                                                                          
354301 DB2-INSERT-TP6FAKT SECTION.                                              
354401     MOVE 000       TO GOOD-SQLCODECODES                                  
354501     EXEC SQL                                                             
354601         INSERT                                                           
354701         INTO TP6FAKT                                                     
354801               (IDFAKT)                                                   
354901         VALUES                                                           
355001              (:RAD-IDFAKT)                                               
355101                                                                          
355201     END-EXEC                                                             
355301     MOVE SQLCODE TO SQLCODE-WS                                           
355401     PERFORM DB2-STATUS-CHECK                                             
355501     .                                                                    
355601     EJECT                                                                
355701                                                                          
355801 DB2-UPDATE-TP6FAKT SECTION.                                              
355901     MOVE 000     TO GOOD-SQLCODECODES                                    
356001     EXEC SQL                                                             
356101         UPDATE TP6FAKT                                                   
356201         SET IDFAKT   = :RAD-IDFAKT                                       
356301     END-EXEC                                                             
356401                                                                          
356501     MOVE SQLCODE TO SQLCODE-WS                                           
356601     PERFORM DB2-STATUS-CHECK                                             
356701     .                                                                    
356801     EJECT                                                                
356901                                                                          
357001 DB2-STATUS-CHECK     SECTION.                                            
357101     SET SQLCODE-IX TO 1                                                  
357201     SEARCH GOOD-SQLCODE                                                  
357301       AT END                                                             
357401          STRING 'INVALID DB2 SQL STATUS CODE: ' SQLCODE-WS               
357501          DELIMITED BY SIZE INTO ERROR-TEXT                               
357601          CALL ABEND USING RKOD-ABEND-DB2                                 
357701       WHEN GOOD-SQLCODE (SQLCODE-IX) = SQLCODE-WS CONTINUE               
357801     END-SEARCH                                                           
357901     .                                                                    
