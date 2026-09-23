000100 ID DIVISION.                                                             
000200 PROGRAM-ID.             W2131210.                                        
000300 AUTHOR.                 IDK, GÖTEBORG.                                   
000400 DATE-WRITTEN.           NOV 1978.                                        
000500     SKIP3                                                                
000600     REMARKS.                                                             
000700*    SKIP1                                                                
000800*    PROGRAMMET ÄR ETT SUBPROGRAM TILL W21312 OCH SKÖTER                  
000900*    SAMTLIGA IMS-CALL MOT WDK6 OCH WDF1.                                 
001000*    UPPDATERINGARNA MOT DESSA BASER SKRIVS UT PÅ FIL W21314              
001100*    UPPDATERINGAR AV HÄNDELSE 2214  SKRIVS UT PÅ FIL W21332              
001200*    SKIP3                                                                
001300 ENVIRONMENT DIVISION.                                                    
001400 INPUT-OUTPUT SECTION.                                                    
001500 FILE-CONTROL.                                                            
001600     SKIP2                                                                
001700*--------------------------------------- TRANSAKTIONER FÖR                
001800*                                        UPPDATERING AV                   
001900*                                        WDK6 OCH WDF1                    
002000     SELECT W21314 ASSIGN UT-S-W21312D3.                                  
002100*                                                                         
002200*--------------------------------------- TRANSAKTIONER FÖR                
002300*                                        UPPDATERING AV                   
002400*                                        HÄNDELSEBAS                      
002500     SELECT W21332 ASSIGN UT-S-W21312D4.                                  
002600*                                                                         
002700     SKIP3                                                                
002800 DATA DIVISION.                                                           
002900 FILE SECTION.                                                            
003000     SKIP2                                                                
003100 FD  W21314                                                               
003200     RECORDING F                                                          
003300     BLOCK 0                                                              
003400     LABEL RECORD STANDARD.                                               
003500 01  U14-POST.                                                            
003600*05   -COPY W21314      -L                                                
003700     SKIP3                                                                
003800 FD  W21332                                                               
003900     RECORDING F                                                          
004000     BLOCK 0                                                              
004100     LABEL RECORD STANDARD.                                               
004200 01  U32-POST.                                                            
004300*05   -COPY W2132213    -L                                                
004400     EJECT                                                                
004500 WORKING-STORAGE SECTION.                                                 
004600     SKIP1                                                                
004700                                                                          
004800*    -- CHECKED BY WY2000                                                 
004900 01  SUBPROGRAM.                                                          
005000     03  CBLTDLI             PIC X(8)    VALUE 'CBLTDLI'.                 
005100     03  FELLOG              PIC X(8)    VALUE 'FELLOG'.                  
005200     EJECT                                                                
005300*--------------------------------------- AREA FÖR W21314-POST             
005400*                                                                         
005500*01  AREA  -COPY W21314      -PRE U14-                                    
005600     SKIP2                                                                
005700*--------------------------------------- AREA FÖR W21332-POST             
005800*                                                                         
005900 01  U32-AREA                 PIC X(14).                                  
006000     SKIP2                                                                
006100*01  AREA  -COPY W2132213    -PRE 2213-   -RED U32-AREA                   
006200     EJECT                                                                
006300*--------------------------------------- ARBETSAREOR TILL                 
006400*                                        IMS-SEKTIONERNA                  
006500 01  IMS-WS.                                                              
006600     03  FILLER              PIC X(8)    VALUE 'IMS-WS'.                  
006700     SKIP3                                                                
006800*--------------------------------------- STATUS FRÅN IMS                  
006900     SKIP1                                                                
007000     03  WS-STATUS           PIC X(2).                                    
007100         88  SEGMENT-FINNS               VALUE '  '.                      
007200         88  SEGMENT-SAKNAS              VALUE 'GE'.                      
007300         88  SEGMENT-SLUT                VALUE 'GB'.                      
007400     SKIP3                                                                
007500     03  SSA1                PIC X(64).                                   
007600     03  SSA2                PIC X(64).                                   
007700     03  SSA3                PIC X(64).                                   
007800     SKIP3                                                                
007900     03  KONSTANTER.                                                      
008000         05  JA              PIC X       VALUE 'J'.                       
008100         05  NEJ             PIC X       VALUE 'N'.                       
008200     03  FLAGGOR.                                                         
008300         05  SW-FORSTA       PIC X       VALUE 'J'.                       
008310         05  SW-FORSTA-GSEC  PIC X       VALUE 'J'.                       
008400     03  OPPNA-FILER         PIC S9(3) COMP-3 VALUE +001.                 
008500     03  STANG-FILER         PIC S9(3) COMP-3 VALUE +999.                 
008600     SKIP3                                                                
008700     03  W-IDLEVNR-X.                                                     
008800         05  W-IDLEVNR       PIC  X(5)   VALUE SPACE.                     
008810     03  W-IDLEVNR-SHIP-X.                                                
008820         05  W-IDLEVNR-SHIP  PIC  X(5)   VALUE SPACE.                     
008900     03  W-IDARTNR-X.                                                     
009000         05  W-IDARTNR       PIC S9(9)               COMP-3.              
009100     03  W-KDCLAGER-X.                                                    
009200         05  W-KDCLAGER      PIC S9                  COMP-3.              
009300     03  W-KDERS-NOLL-X.                                                  
009400         05  W-KDERS-NOLL    PIC S9(3)   VALUE +0    COMP-3.              
009500     03  W-WDG3KEY-2213-X.                                                
009600         05  FILLER          PIC X(04)   VALUE '2213'.                    
009700         05  FILLER          PIC X(26)   VALUE LOW-VALUE.                 
009800     SKIP3                                                                
009900 01  GODK-STATUSKODER.                                                    
010000     03  GODK-STATUS OCCURS 10 INDEXED BY STATUS-IX PIC X(2).             
010100     SKIP3                                                                
010200*    -COPY W0003.                                                         
010300     EJECT                                                                
010400 01  DLI-IO-AREA-01          PIC X(200).                                  
010500     SKIP3                                                                
010600*01  WLARTC01 -COPY WDK601     -RED DLI-IO-AREA-01.                       
010700     EJECT                                                                
010800 01  DLI-IO-AREA-11          PIC X(900).                                  
010900     SKIP3                                                                
011000*01  WLARTC11 -COPY WDK611     -RED DLI-IO-AREA-11.                       
011100     EJECT                                                                
011200 01  DLI-IO-AREA             PIC X(200).                                  
011300     SKIP3                                                                
011400*01  WLLEVA01 -COPY WDF101     -PRE LEV- -RED DLI-IO-AREA.                
011500     EJECT                                                                
011800 LINKAGE SECTION.                                                         
011900*01  AREA -COPY W213L121   -PRE LINK-                                     
012000     EJECT                                                                
012100*01  -COPY W0008 -PRE LEVA-                                               
012200         05  FILLER          PIC X.                                       
012300     EJECT                                                                
012400*01  -COPY W0008 -PRE ARTC-                                               
012500         05  FILLER          PIC X.                                       
012600     EJECT                                                                
012610*01  -COPY W0008 -PRE WDK6G-                                              
012620         05  FILLER          PIC X.                                       
012630     EJECT                                                                
012700 PROCEDURE DIVISION USING LINK-AREA LEVA-PCB ARTC-PCB WDK6G-PCB.          
012800                                                                          
012900     SKIP3                                                                
013000     MOVE JA TO LINK-FLJANEJ-ANROP                                        
013100     MOVE LINK-IDLEVNR TO W-IDLEVNR                                       
013200                          W-IDLEVNR-SHIP                                  
016701*                                                                         
016702     EVALUATE LINK-KDCALL                                                 
016710       WHEN +001                                                          
016720         PERFORM I-OPPNA-FILER                                            
016740       WHEN +999                                                          
016750         PERFORM J-STANG-FILER                                            
016780       WHEN +101                                                          
016790         PERFORM A-LAES-WLLEVA01                                          
016792       WHEN +102                                                          
016793         PERFORM B-ISRT-WLLEVA01                                          
016795       WHEN +103                                                          
016796         PERFORM C-DLET-WLLEVA01                                          
016798       WHEN +104                                                          
016800         PERFORM D-REPL-WLLEVA01                                          
016801       WHEN +204                                                          
016802         PERFORM D-REPL-WLLEVA01                                          
016803       WHEN +105                                                          
016804         PERFORM E-LAES-ARTIKELINFO                                       
016805       WHEN +109                                                          
016806         PERFORM G-LAES-ARTIKELINFO-R17                                   
016808       WHEN +206                                                          
016809         PERFORM F-REPL-ARTIKELINFO                                       
016811       WHEN +110                                                          
016812         PERFORM FA-REPL-ARTIKELINFO-MFG                                  
016814       WHEN +111                                                          
016815         PERFORM FB-REPL-ARTIKELINFO-SHP                                  
016817       WHEN +108                                                          
016818         PERFORM H-ISRT-WDGX2214                                          
016828     END-EVALUATE                                                         
016829*                                                                         
016830     MOVE ZERO TO RETURN-CODE                                             
016900     GOBACK                                                               
017000     EJECT                                                                
017100     .                                                                    
017200 A-LAES-WLLEVA01 SECTION.                                                 
017300     SKIP1                                                                
017400     PERFORM IMS-GET-WLLEVA01                                             
017500     MOVE JA TO SW-FORSTA                                                 
017510     MOVE JA TO SW-FORSTA-GSEC                                            
017600     IF SEGMENT-SAKNAS                                                    
017700         MOVE LOW-VALUE TO LINK-AREA                                      
017800         MOVE NEJ       TO LINK-FLJANEJ-ANROP                             
017900     ELSE                                                                 
018000         MOVE LEV-LEV-KDLEVTYP     TO LINK-KDLEVTYP                       
018100         MOVE LEV-LEV-KDSPRAK      TO LINK-KDSPRAK                        
018200         MOVE LEV-LEV-FLRSADR      TO LINK-FLRSADR                        
018300         MOVE LEV-LEV-KDGK         TO LINK-KDGK                           
018400         MOVE LEV-LEV-KVDAGAR-TTC1 TO LINK-KVDAGAR-TTC1                   
018500         MOVE LEV-LEV-KVDAGAR-TTC2 TO LINK-KVDAGAR-TTC2                   
018600         MOVE LEV-LEV-KVVECKOR-LT  TO LINK-KVVECKOR-LT                    
018700         MOVE LEV-LEV-KVVECKOR-AT  TO LINK-KVVECKOR-AT                    
018800         MOVE LEV-LEV-IDLPKOLL     TO LINK-IDLPKOLL                       
018900         MOVE LEV-LEV-PGTABELL     TO LINK-PG-TABELL                      
019000         MOVE LEV-LEV-LEVDAGTAB    TO LINK-LEVDAGTAB                      
019100     END-IF                                                               
019200     EJECT                                                                
019300     .                                                                    
019400 B-ISRT-WLLEVA01 SECTION.                                                 
019500     SKIP1                                                                
019600     PERFORM S01-NOLLA-U14-AREA                                           
019700     MOVE ISRT              TO U14-ATGARD                                 
019800     MOVE 'WDF101'          TO U14-IDSEGM                                 
020000     MOVE LINK-IDLEVNR      TO U14-IDLEVNR                                
020200     MOVE LINK-KDLEVTYP     TO U14-KDLEVTYP                               
020400     MOVE LINK-KDSPRAK      TO U14-KDSPRAK                                
020600     MOVE LINK-FLRSADR      TO U14-FLRSADR                                
020800     MOVE LINK-KDGK         TO U14-KDGK                                   
021000     MOVE LINK-KVDAGAR-TTC1 TO U14-KVDAGAR-TTC1                           
021200     MOVE LINK-KVDAGAR-TTC2 TO U14-KVDAGAR-TTC2                           
021400     MOVE LINK-KVVECKOR-LT  TO U14-KVVECKOR-LT                            
021600     MOVE LINK-KVVECKOR-AT  TO U14-KVVECKOR-AT                            
021800     MOVE LINK-IDLPKOLL     TO U14-IDLPKOLL                               
022000     MOVE LINK-PG-TABELL    TO U14-PGTABELL                               
022200     MOVE LINK-LEVDAGTAB    TO U14-LEVDAGTAB                              
022400     MOVE LINK-KDCALL       TO U14-KDCALL                                 
022600     WRITE U14-POST  FROM U14-AREA                                        
022700*****PERFORM IMS-ISRT-WLLEVA01                                            
022800     EJECT                                                                
022900     .                                                                    
023000 C-DLET-WLLEVA01 SECTION.                                                 
023100     SKIP1                                                                
023200     PERFORM IMS-GET-WLLEVA01                                             
023300     PERFORM S01-NOLLA-U14-AREA                                           
023400     MOVE DLET              TO U14-ATGARD                                 
023500     MOVE 'WDF101'          TO U14-IDSEGM                                 
023600     MOVE LEV-LEV-IDLEVNR   TO U14-IDLEVNR                                
023700     MOVE LINK-KDCALL       TO U14-KDCALL                                 
023800                                                                          
023900     WRITE U14-POST  FROM U14-AREA                                        
024000*****PERFORM IMS-DLET-LEVA                                                
024100     EJECT                                                                
024200     .                                                                    
024300 D-REPL-WLLEVA01 SECTION.                                                 
024400     SKIP1                                                                
024500     PERFORM S01-NOLLA-U14-AREA                                           
024600     MOVE REPL              TO U14-ATGARD                                 
024700     MOVE 'WDF101'          TO U14-IDSEGM                                 
024800     MOVE LINK-IDLEVNR      TO U14-IDLEVNR                                
025000     MOVE LINK-KDLEVTYP     TO U14-KDLEVTYP                               
025200     MOVE LINK-KDSPRAK      TO U14-KDSPRAK                                
025400     MOVE LINK-FLRSADR      TO U14-FLRSADR                                
025600     MOVE LINK-KDGK         TO U14-KDGK                                   
025800     MOVE LINK-KVDAGAR-TTC1 TO U14-KVDAGAR-TTC1                           
026000     MOVE LINK-KVDAGAR-TTC2 TO U14-KVDAGAR-TTC2                           
026200     MOVE LINK-KVVECKOR-LT  TO U14-KVVECKOR-LT                            
026400     MOVE LINK-KVVECKOR-AT  TO U14-KVVECKOR-AT                            
026600     MOVE LINK-IDLPKOLL     TO U14-IDLPKOLL                               
026800     MOVE LINK-PG-TABELL    TO U14-PGTABELL                               
027000     MOVE LINK-LEVDAGTAB    TO U14-LEVDAGTAB                              
027200     MOVE LINK-KDCALL       TO U14-KDCALL                                 
027400     WRITE U14-POST  FROM U14-AREA                                        
027500*****PERFORM IMS-REPL-LEVA                                                
027600     EJECT                                                                
027700     .                                                                    
027800 E-LAES-ARTIKELINFO SECTION.                                              
027900     SKIP1                                                                
028000     IF SW-FORSTA = JA                                                    
028100        PERFORM IMS-GET-WLARTC01-FORSTA                                   
028200        MOVE NEJ TO SW-FORSTA                                             
028300     ELSE                                                                 
028400        PERFORM IMS-GET-WLARTC01                                          
028500     END-IF                                                               
028600     SKIP1                                                                
028700     IF SEGMENT-FINNS                                                     
028800         MOVE ART-IDARTNR      TO W-IDARTNR                               
028900         MOVE ART-IDARTNR      TO LINK-IDARTNR                            
029000         PERFORM IMS-GET-WLARTC11                                         
029100         MOVE CLAG-KDHF        TO LINK-KDHF                               
029200         MOVE CLAG-KVDAGAR-TT  TO LINK-KVDAGAR-TT-ARTNR                   
029300         MOVE CLAG-KVVECKOR-LT TO LINK-KVVECKOR-LT-ARTNR                  
029400         MOVE CLAG-KVVECKOR-AT TO LINK-KVVECKOR-AT-ARTNR                  
029500         MOVE CLAG-FLMANLT     TO LINK-FLMANLT-ARTNR                      
029600         MOVE CLAG-FLMANAT     TO LINK-FLMANAT-ARTNR                      
029700         MOVE CLAG-KDAVT       TO LINK-KDAVT-ARTNR                        
029800         MOVE CLAG-KDKSP       TO LINK-KDKSP-ARTNR                        
029900         MOVE CLAG-IDPLANGR-AG TO LINK-IDPLANGR-AG-ARTNR                  
030000         MOVE CLAG-IDANSK      TO LINK-IDANSK-ARTNR                       
030100         MOVE CLAG-KDGK        TO LINK-KDGK-ARTNR                         
030200         MOVE CLAG-KDLTK       TO LINK-KDLTK-ARTNR                        
030300         MOVE CLAG-FLMANGK     TO LINK-FLMANGK-ARTNR                      
030400     ELSE                                                                 
030500         MOVE LOW-VALUE     TO LINK-AREA                                  
030600         MOVE NEJ TO LINK-FLJANEJ-ANROP                                   
030700     END-IF                                                               
030800     EJECT                                                                
030900     .                                                                    
031000 F-REPL-ARTIKELINFO SECTION.                                              
031100     SKIP1                                                                
031200     MOVE LINK-IDARTNR           TO W-IDARTNR                             
031500     PERFORM S01-NOLLA-U14-AREA                                           
031600     MOVE REPL                   TO U14-ATGARD                            
031700     MOVE 'WDK611'               TO U14-IDSEGM                            
031800     MOVE W-IDARTNR              TO U14-IDARTNR                           
032000     MOVE LINK-KVDAGAR-TT-ARTNR  TO U14-KVDAGAR-TT                        
032200     MOVE LINK-KVVECKOR-LT-ARTNR TO U14-KVVECKOR-LT                       
032400     MOVE LINK-KVVECKOR-AT-ARTNR TO U14-KVVECKOR-AT                       
032600     MOVE LINK-FLMANLT-ARTNR     TO U14-FLMANLT                           
032800     MOVE LINK-FLMANAT-ARTNR     TO U14-FLMANAT                           
033000     MOVE LINK-KDAVT-ARTNR       TO U14-KDAVT                             
033200     MOVE LINK-KDKSP-ARTNR       TO U14-KDKSP                             
033400     MOVE LINK-IDPLANGR-AG-ARTNR TO U14-IDPLANGR-AG                       
033600     MOVE LINK-IDANSK-ARTNR      TO U14-IDANSK                            
033900     MOVE LINK-KDGK-ARTNR        TO U14-KDGK                              
034100     MOVE LINK-FLMANGK-ARTNR     TO U14-FLMANGK                           
034300     MOVE LINK-KDCALL            TO U14-KDCALL                            
034500     WRITE U14-POST  FROM U14-AREA                                        
034510*****PERFORM IMS-REPL-ARTC                                                
034520     EJECT                                                                
034530     .                                                                    
034600                                                                          
034710 FA-REPL-ARTIKELINFO-MFG SECTION.                                         
034730     MOVE LINK-IDARTNR TO W-IDARTNR                                       
034760     PERFORM S01-NOLLA-U14-AREA                                           
034770     MOVE REPL                   TO U14-ATGARD                            
034780     MOVE 'WDK611'               TO U14-IDSEGM                            
034790     MOVE W-IDARTNR              TO U14-IDARTNR                           
034798     MOVE LINK-FLMANLT-ARTNR     TO U14-FLMANLT                           
034800     MOVE LINK-FLMANAT-ARTNR     TO U14-FLMANAT                           
034802     MOVE LINK-KDAVT-ARTNR       TO U14-KDAVT                             
034804     MOVE LINK-KDKSP-ARTNR       TO U14-KDKSP                             
034806     MOVE LINK-IDPLANGR-AG-ARTNR TO U14-IDPLANGR-AG                       
034808     MOVE LINK-IDANSK-ARTNR      TO U14-IDANSK                            
034813     MOVE LINK-FLMANGK-ARTNR     TO U14-FLMANGK                           
034815     MOVE LINK-KDCALL            TO U14-KDCALL                            
034817     WRITE U14-POST  FROM U14-AREA                                        
034819*****PERFORM IMS-REPL-ARTC                                                
034820     EJECT                                                                
034900     .                                                                    
034901 FB-REPL-ARTIKELINFO-SHP SECTION.                                         
034902     MOVE LINK-IDARTNR TO W-IDARTNR                                       
034903     PERFORM S01-NOLLA-U14-AREA                                           
034904     MOVE REPL                   TO U14-ATGARD                            
034905     MOVE 'WDK611'               TO U14-IDSEGM                            
034906     MOVE W-IDARTNR              TO U14-IDARTNR                           
034908     MOVE LINK-KVDAGAR-TT-ARTNR  TO U14-KVDAGAR-TT                        
034909     MOVE LINK-KVVECKOR-LT-ARTNR TO U14-KVVECKOR-LT                       
034910     MOVE LINK-KVVECKOR-AT-ARTNR TO U14-KVVECKOR-AT                       
034911     MOVE LINK-KDGK-ARTNR        TO U14-KDGK                              
034912     MOVE LINK-KDCALL            TO U14-KDCALL                            
034918     WRITE U14-POST  FROM U14-AREA                                        
034919*****PERFORM IMS-REPL-ARTC                                                
034920     EJECT                                                                
034921     .                                                                    
034922 G-LAES-ARTIKELINFO-R17 SECTION.                                          
034930     IF SW-FORSTA-GSEC = JA                                               
034940        PERFORM IMS-GU-WDK6G1-FORSTA                                      
034950        MOVE NEJ TO SW-FORSTA-GSEC                                        
034960     ELSE                                                                 
034970        PERFORM IMS-GN-WDK6G1                                             
034980     END-IF                                                               
034991     IF SEGMENT-FINNS                                                     
034992         MOVE ART-IDARTNR      TO W-IDARTNR                               
034993         MOVE ART-IDARTNR      TO LINK-IDARTNR                            
034994         PERFORM IMS-GNP-WDK611                                           
034995         MOVE CLAG-KDHF        TO LINK-KDHF                               
034996         MOVE CLAG-KVDAGAR-TT  TO LINK-KVDAGAR-TT-ARTNR                   
034997         MOVE CLAG-KVVECKOR-LT TO LINK-KVVECKOR-LT-ARTNR                  
034998         MOVE CLAG-KVVECKOR-AT TO LINK-KVVECKOR-AT-ARTNR                  
034999         MOVE CLAG-FLMANLT     TO LINK-FLMANLT-ARTNR                      
035000         MOVE CLAG-FLMANAT     TO LINK-FLMANAT-ARTNR                      
035001         MOVE CLAG-KDAVT       TO LINK-KDAVT-ARTNR                        
035002         MOVE CLAG-KDKSP       TO LINK-KDKSP-ARTNR                        
035003         MOVE CLAG-IDPLANGR-AG TO LINK-IDPLANGR-AG-ARTNR                  
035004         MOVE CLAG-IDANSK      TO LINK-IDANSK-ARTNR                       
035005         MOVE CLAG-KDGK        TO LINK-KDGK-ARTNR                         
035006         MOVE CLAG-KDLTK       TO LINK-KDLTK-ARTNR                        
035007         MOVE CLAG-FLMANGK     TO LINK-FLMANGK-ARTNR                      
035008     ELSE                                                                 
035009         MOVE LOW-VALUE        TO LINK-AREA                               
035010         MOVE NEJ              TO LINK-FLJANEJ-ANROP                      
035011     END-IF                                                               
035012     EJECT                                                                
035013     .                                                                    
035020 H-ISRT-WDGX2214 SECTION.                                                 
035100     SKIP1                                                                
035200     MOVE SPACE           TO U32-AREA                                     
035300     MOVE LINK-IDARTNR    TO 2213-IDARTNR                                 
035400     MOVE '2213'          TO 2213-IDHTYP                                  
035500                                                                          
035600     WRITE U32-POST FROM U32-AREA                                         
035900     .                                                                    
036000     EJECT                                                                
036100 I-OPPNA-FILER   SECTION.                                                 
036200     SKIP3                                                                
036300     OPEN OUTPUT W21314                                                   
036400                 W21332                                                   
036500     .                                                                    
036600     EJECT                                                                
036700 J-STANG-FILER   SECTION.                                                 
036800     SKIP3                                                                
036900     CLOSE       W21314                                                   
037000                 W21332                                                   
037100     .                                                                    
037200     EJECT                                                                
037300 S01-NOLLA-U14-AREA SECTION.                                              
037400     SKIP3                                                                
037500     MOVE SPACE TO U14-AREA                                               
037600     MOVE ZERO  TO U14-IDARTNR                                            
037700                   U14-KVDAGAR-TT                                         
037800                   U14-KDAVT                                              
037900                   U14-KDKSP                                              
038000                   U14-IDANSK                                             
038100                   U14-IDPLANGR-AG                                        
038200                                                                          
038300                   U14-KDLEVTYP                                           
038400                   U14-KDSPRAK                                            
038500                   U14-FLRSADR                                            
038600                   U14-KDGK                                               
038700                   U14-KVDAGAR-TTC1                                       
038800                   U14-KVDAGAR-TTC2                                       
038900                   U14-KVVECKOR-LT                                        
039000                   U14-KVVECKOR-AT                                        
039100                   U14-IDLPKOLL                                           
039200                   U14-PGTABELL                                           
039300                   U14-LEVDAGTAB                                          
039400                                                                          
039500     EJECT                                                                
039600     .                                                                    
039700 IMS-GET-WLLEVA01 SECTION.                                                
039800     SKIP1                                                                
039900     STRING 'WLLEVA01(IDLEVNR  =' W-IDLEVNR-X ')'                         
040000     DELIMITED BY SIZE INTO SSA1                                          
040100     MOVE '  GE' TO GODK-STATUSKODER                                      
040200     CALL CBLTDLI USING GHU LEVA-PCB DLI-IO-AREA SSA1                     
040300     MOVE LEVA-STATUS-CODE TO WS-STATUS                                   
040400     PERFORM IMS-STATUSKONTROLL                                           
040500     EJECT                                                                
040600     .                                                                    
040700 IMS-GET-WLARTC01-FORSTA SECTION.                                         
040800     SKIP1                                                                
040900     STRING 'WLARTC01(WDK6ASEQ =' W-IDLEVNR-X                             
041000                    '&KDERS    =' W-KDERS-NOLL-X ')'                      
041100     DELIMITED BY SIZE INTO SSA1                                          
041200     MOVE '  GE' TO GODK-STATUSKODER                                      
041300     CALL CBLTDLI USING GU ARTC-PCB DLI-IO-AREA-01 SSA1                   
041400     MOVE ARTC-STATUS-CODE TO WS-STATUS                                   
041500     PERFORM IMS-STATUSKONTROLL                                           
041600     EJECT                                                                
041700     .                                                                    
041800 IMS-GET-WLARTC01 SECTION.                                                
041900     SKIP1                                                                
042000     STRING 'WLARTC01(WDK6ASEQ =' W-IDLEVNR-X                             
042100                    '&KDERS    =' W-KDERS-NOLL-X ')'                      
042200     DELIMITED BY SIZE INTO SSA1                                          
042300     MOVE '  GE' TO GODK-STATUSKODER                                      
042400     CALL CBLTDLI USING GN ARTC-PCB DLI-IO-AREA-01 SSA1                   
042500     MOVE ARTC-STATUS-CODE TO WS-STATUS                                   
042600     PERFORM IMS-STATUSKONTROLL                                           
042700     EJECT                                                                
042800     .                                                                    
042810 IMS-GET-WLARTC11 SECTION.                                                
042820     SKIP1                                                                
042830     MOVE 'WLARTC11*F' TO SSA1                                            
042840     MOVE '  ' TO GODK-STATUSKODER                                        
042850     CALL CBLTDLI USING GHNP ARTC-PCB DLI-IO-AREA-11 SSA1                 
042860     MOVE ARTC-STATUS-CODE TO WS-STATUS                                   
042870     PERFORM IMS-STATUSKONTROLL                                           
042880     EJECT                                                                
042890     .                                                                    
042900 IMS-GU-WDK6G1-FORSTA SECTION.                                            
042920     STRING 'WDK601  (WDK6GSEQ =' W-IDLEVNR-SHIP-X                        
042930                    '&KDERS    =' W-KDERS-NOLL-X ')'                      
042940     DELIMITED BY SIZE INTO SSA1                                          
042950     MOVE '  GE' TO GODK-STATUSKODER                                      
042960     CALL CBLTDLI USING GU WDK6G-PCB DLI-IO-AREA-01 SSA1                  
042970     MOVE WDK6G-STATUS-CODE TO WS-STATUS                                  
042980     PERFORM IMS-STATUSKONTROLL                                           
042991     .                                                                    
042992 IMS-GN-WDK6G1  SECTION.                                                  
042994     STRING 'WDK601  (WDK6GSEQ =' W-IDLEVNR-SHIP-X                        
042995                    '&KDERS    =' W-KDERS-NOLL-X ')'                      
042996     DELIMITED BY SIZE INTO SSA1                                          
042997     MOVE '  GE' TO GODK-STATUSKODER                                      
042998     CALL CBLTDLI USING GN WDK6G-PCB DLI-IO-AREA-01 SSA1                  
042999     MOVE WDK6G-STATUS-CODE TO WS-STATUS                                  
043000     PERFORM IMS-STATUSKONTROLL                                           
043002     .                                                                    
043010 IMS-GNP-WDK611   SECTION.                                                
043200     MOVE 'WDK611'     TO SSA1                                            
043300     MOVE '  ' TO GODK-STATUSKODER                                        
043400     CALL CBLTDLI USING GNP WDK6G-PCB DLI-IO-AREA-11 SSA1                 
043500     MOVE WDK6G-STATUS-CODE TO WS-STATUS                                  
043600     PERFORM IMS-STATUSKONTROLL                                           
043800     .                                                                    
043900 IMS-STATUSKONTROLL SECTION.                                              
044000     SKIP1                                                                
044100     SET STATUS-IX TO 1                                                   
044200     SEARCH GODK-STATUS AT END                                            
044300     DISPLAY IMS-WS                                                       
044400     CALL FELLOG                                                          
044500     WHEN GODK-STATUS (STATUS-IX) = WS-STATUS                             
044600     CONTINUE                                                             
044700     END-SEARCH                                                           
044800     .                                                                    
