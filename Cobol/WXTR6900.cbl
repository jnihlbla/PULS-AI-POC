000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     WXTR6900.                                                
000300 AUTHOR.         CONNY EGHOLT.                                            
000400 DATE-WRITTEN.   03/06/04.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*                                                                         
000800*    FUNKTION:                                                            
000900*        SKAPAR EN LISTA ÖVER NYA/ÄNDRADE/BORTTAGNA LAGERPLATSER          
001000*        PÅ SDC24                                                         
001100*                                                                         
001200*                                                                         
001300*    ABENDKODER:                                                          
001400*        U0016 -  . . . .                                                 
001500*        U1000 -  . . . .                                                 
001600*                                                                         
001700                                                                          
001800     SKIP3                                                                
001900 ENVIRONMENT DIVISION.                                                    
002000     SKIP2                                                                
002100 INPUT-OUTPUT SECTION.                                                    
002200                                                                          
002300 FILE-CONTROL.                                                            
002400     SKIP2                                                                
002500*          --- FÖRRA VECKANS SDC-EXTRAKT                                  
002600     SELECT SDCWEE-1                   ASSIGN TO WXTR69D1.                
002700     SKIP2                                                                
002800*          --- DENNA VECKANS SDCEXTRAKT                                   
002900     SELECT SDCWEE-0                   ASSIGN TO WXTR69D2.                
003000     SKIP2                                                                
003100*          --- LAGERPLATSFÖRÄNDRINGAR I SDC-LAGER                         
003200     SELECT WXTR69                     ASSIGN TO WXTR69D3.                
003300     EJECT                                                                
003400 DATA DIVISION.                                                           
003500     SKIP3                                                                
003600 FILE SECTION.                                                            
003700     SKIP3                                                                
003800 FD  SDCWEE-1                                                             
003900     RECORDING       F                                                    
004000     BLOCK CONTAINS  0.                                                   
004100                                                                          
004200*01  -COPY WSDCPART      -L.                                              
004300     SKIP3                                                                
004400 FD  SDCWEE-0                                                             
004500     RECORDING       F                                                    
004600     BLOCK CONTAINS  0.                                                   
004700                                                                          
004800*01  -COPY WSDCPART      -L.                                              
004900     SKIP3                                                                
005000 FD  WXTR69                                                               
005100     RECORDING       V                                                    
005200     BLOCK CONTAINS  0.                                                   
005300                                                                          
005400*01  POST -COPY WXTR69 -PRE  UT-  -L.                                     
005500     EJECT                                                                
005600 WORKING-STORAGE SECTION.                                                 
005700                                                                          
005800 77  IDPGM                       PIC X(8)    VALUE 'WXTR6900'.            
005900 77  JA                          PIC X       VALUE 'J'.                   
006000 77  NEJ                         PIC X       VALUE 'N'.                   
006100 77  TAB                         PIC X       VALUE x'05'.                 
006200                                                                          
006300 01  WS-ADLAGOMR-OLD    PIC X(2) JUST RIGHT  VALUE space.                 
006400 01  WS-ADGANG-OLD      PIC X(2) JUST RIGHT  VALUE space.                 
006500 01  WS-ADPLATS-OLD     PIC X(5) JUST RIGHT  VALUE space.                 
006600 01  WS-ADLAGOMR-NEW    PIC X(2) JUST RIGHT  VALUE space.                 
006700 01  WS-ADGANG-NEW      PIC X(2) JUST RIGHT  VALUE space.                 
006800 01  WS-ADPLATS-NEW     PIC X(5) JUST RIGHT  VALUE space.                 
006900                                                                          
007000 77  SDCWEE-EOF-SW               PIC X       VALUE 'N'.                   
007100     88  END-OF-SDCWEE-1                     VALUE 'J'.                   
007110     88  NOT-END-OF-SDCWEE-1                 VALUE 'N'.                   
007200                                                                          
007300 77  SDCWEE-EOF-SW               PIC X       VALUE 'N'.                   
007400     88  END-OF-SDCWEE-0                     VALUE 'J'.                   
007410     88  NOT-END-OF-SDCWEE-0                 VALUE 'N'.                   
007500     EJECT                                                                
007600 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
007700 01  FILLER REDEFINES DAGENS-DATUM.                                       
007800     03  DAGENS-DATUM-AAR        PIC 9(2).                                
007900     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
008000     03  DAGENS-DATUM-DAG        PIC 9(2).                                
008100 01  FILLER REDEFINES DAGENS-DATUM.                                       
008200     03  DAGENS-DATUM-XLS        PIC 999999.                              
008300 01  DAGENS-VECKA.                                                        
008400     03 DAGENS-DATUM-VECKA     PIC 9(2).                                  
008500     EJECT                                                                
008600 01  DYNAMISKA-SUBPROGRAM.                                                
008700*                                                                         
008800     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
008900     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
009000     03  DATKORT                 PIC X(8)    VALUE 'DATKORT'.             
009100     SKIP2                                                                
009200*    --- PARAMETRAR TILL ABEND                                            
009300                                                                          
009400 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
009500 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
009600 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
009700     SKIP2                                                                
009800 01  FELTEXT.                                                             
009900     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
010000     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
010100                                                                          
010200     EJECT                                                                
010300*    --- PARAMETRAR TILL DATKORT                                          
010400 01  PROGRAM-NAMN                PIC X(6)    VALUE 'WXTR69'.              
010500     SKIP2                                                                
010600 01  DATUMKORT-ID                PIC X(6)    VALUE 'WDATUM'.              
010700     SKIP2                                                                
010800*01  -COPY WDATKORT                                                       
010900     EJECT                                                                
011000*    --- PARAMETRAR TILL POSTSUM                                          
011100*                                                                         
011200*01  -COPY W0005   -PRE  POSTSUM-                                         
011300     EJECT                                                                
011400                                                                          
011500 01  IN1-AREA-START    PIC X(24)   VALUE  'IN1-AREA-START  '.             
011600*01  AREA -COPY WSDCPART     -PRE IN1-                                    
011700     EJECT                                                                
011800                                                                          
011900 01  IN0-AREA-START    PIC X(24)   VALUE  'IN0-AREA-START  '.             
012000*01  AREA -COPY WSDCPART     -PRE IN0-                                    
012100     EJECT                                                                
012200                                                                          
012300 01  UT-AREA-START     PIC X(24)   VALUE  'UT-AREA-START  '.              
012400*01  AREA -COPY WXTR69     -PRE UT-                                       
012500     EJECT                                                                
012600                                                                          
012700 01  UT-RUB1-AREA.                                                        
012800     03  FILLER             PIC X(5) VALUE ' Run:'.                       
012900     03  FILLER             PIC X    VALUE x'05'.                         
013000     03  FILLER             PIC X    VALUE '*'.                           
013100     03  RUB-DATUM          PIC X(6) VALUE SPACE.                         
013200     03  FILLER             PIC X    VALUE x'05'.                         
013300     03  filler             PIC X(26) VALUE 'New/Changed/Del. addr        
013400-                                       'esses'.                          
013500     03  FILLER             PIC X    VALUE x'05'.                         
013600     03  FILLER             PIC X(5) VALUE 'Week:'.                       
013700     03  RUB-VECKA-1        PIC Z(2).                                     
013800     03  FILLER             PIC X    VALUE x'05'.                         
013900     03  FILLER             PIC X(5) VALUE 'Week:'.                       
014000     03  RUB-VECKA-0        PIC Z(2).                                     
014100     03  FILLER             PIC X    VALUE x'05'.                         
014200                                                                          
014300     EJECT                                                                
014400 01  UT-RUB2-AREA.                                                        
014500     03  RUB-IDDC           PIC X(3) VALUE ' DC'.                         
014600     03  FILLER             PIC X    VALUE x'05'.                         
014700     03  RUB-IDARTNR        PIC X(8) VALUE 'Part No.'.                    
014800     03  FILLER             PIC X    VALUE x'05'.                         
014900     03  RUB-BEART-ENG      PIC X(25) VALUE 'Description'.                
015000     03  FILLER             PIC X    VALUE x'05'.                         
015100     03  RUB-OLD            PIC X(11) VALUE 'Old address'.                
015200     03  FILLER             PIC X    VALUE x'05'.                         
015300     03  RUB-NEW            PIC X(11) VALUE 'New address'.                
015400     03  FILLER             PIC X    VALUE x'05'.                         
015500     EJECT                                                                
015600                                                                          
015700 PROCEDURE DIVISION.                                                      
015800 MAIN SECTION.                                                            
015900     SKIP2                                                                
016000                                                                          
016100     Perform A-INIT                                                       
016200*    Infilerna är sorterade i artikelnummerordning                        
016300     Perform S01-LAES-SDCWEE-1                                            
016400     Perform Until END-OF-SDCWEE-1                                        
016500                Or IN1-IDDC = '24'                                        
016600        Perform S01-LAES-SDCWEE-1                                         
016700     End-Perform                                                          
016800                                                                          
016900     Perform S02-LAES-SDCWEE-0                                            
017000     Perform Until END-OF-SDCWEE-0                                        
017100                Or IN0-IDDC = '24'                                        
017200        Perform S02-LAES-SDCWEE-0                                         
017300     End-Perform                                                          
017400                                                                          
017500     Perform Until END-OF-SDCWEE-1 And END-OF-SDCWEE-0                    
017600*       Håll på så länge det finns poster i någon fil                     
017710        IF NOT-END-OF-SDCWEE-1 AND NOT-END-OF-SDCWEE-0                    
017800*          Det finns poster i båda filerna                                
017900           If IN1-IDARTNR = IN0-IDARTNR                                   
018000*             Match (Gammal + Ny)                                         
018100                                                                          
018200              Perform B-SKRIV-FRAN-BADA-FILERNA                           
018300                                                                          
018400              Perform S01-LAES-SDCWEE-1                                   
018500              Perform Until END-OF-SDCWEE-1                               
018600                         Or IN1-IDDC = '24'                               
018700                 Perform S01-LAES-SDCWEE-1                                
018800              End-Perform                                                 
018900                                                                          
019000              Perform S02-LAES-SDCWEE-0                                   
019100              Perform Until END-OF-SDCWEE-0                               
019200                         Or IN0-IDDC = '24'                               
019300                 Perform S02-LAES-SDCWEE-0                                
019400              End-Perform                                                 
019500           Else                                                           
019600              If IN1-IDARTNR > IN0-IDARTNR                                
019700*                Omatch ( Nytillkommen artikel )                          
019800                 Perform C-SKRIV-FRAN-NYA-FILEN                           
019900                                                                          
020000                 Perform S02-LAES-SDCWEE-0                                
020100                                                                          
020200                 Perform Until END-OF-SDCWEE-0                            
020300                            Or IN0-IDDC = '24'                            
020400                    Perform S02-LAES-SDCWEE-0                             
020500                 End-Perform                                              
020600              Else                                                        
020700*                Omatch ( Borttagen artikel )                             
020800                 Perform D-SKRIV-FRAN-GAMLA-FILEN                         
020900                 Perform S01-LAES-SDCWEE-1                                
021000                 Perform Until END-OF-SDCWEE-1                            
021100                            Or IN1-IDDC = '24'                            
021200                    Perform S01-LAES-SDCWEE-1                             
021300                 End-Perform                                              
021400              End-If                                                      
021500           End-If                                                         
021600        Else                                                              
021700*          Det finns bara poster i en av filerna                          
021800           If END-OF-SDCWEE-1                                             
021900*             Endast artiklar kvar i nya filen                            
022000              Perform Until END-OF-SDCWEE-0                               
022100                 Perform C-SKRIV-FRAN-NYA-FILEN                           
022200                 Perform S02-LAES-SDCWEE-0                                
022300                 Perform Until END-OF-SDCWEE-0                            
022400                            Or IN0-IDDC = '24'                            
022500                    Perform S02-LAES-SDCWEE-0                             
022600                 End-Perform                                              
022700              End-Perform                                                 
022800           Else                                                           
022900              If END-OF-SDCWEE-0                                          
023000*                Endast artiklar kvar i förra filen                       
023100                 Perform Until END-OF-SDCWEE-1                            
023200                    Perform D-SKRIV-FRAN-GAMLA-FILEN                      
023300                    Perform S01-LAES-SDCWEE-1                             
023400                    Perform Until END-OF-SDCWEE-1                         
023500                               Or IN1-IDDC = '24'                         
023600                       Perform S01-LAES-SDCWEE-1                          
023700                    End-Perform                                           
023800                 End-Perform                                              
023900              Else                                                        
024000                 DISPLAY 'Felprogrammerat'                                
024100                 Move RKOD-ABEND-UTAN-DUMP To RKOD-ABEND                  
024200                 Perform S99-ABEND                                        
024300              End-If                                                      
024400           End-If                                                         
024500        End-If                                                            
024600     End-Perform                                                          
024700                                                                          
024800     Perform Z-FINIT                                                      
024900                                                                          
025000     Move ZERO To RETURN-CODE                                             
025100     Goback                                                               
025200     .                                                                    
025300     EJECT                                                                
025400 A-INIT SECTION.                                                          
025500                                                                          
025600     Open INPUT  SDCWEE-1                                                 
025700                 SDCWEE-0                                                 
025800     Open OUTPUT WXTR69                                                   
025900     SKIP2                                                                
026000*    Accept DAGENS-DATUM  From DATE                                       
026100                                                                          
026200     Call DATKORT Using PROGRAM-NAMN DATUMKORT-ID DATUMKORT               
026300     Move D-AAR              To DAGENS-DATUM-AAR                          
026400     Move D-MAANAD           To DAGENS-DATUM-MAANAD                       
026500     Move D-DAG              To DAGENS-DATUM-DAG                          
026600     Move D-VECKA            To DAGENS-DATUM-VECKA                        
026700                                                                          
026800     Move DAGENS-DATUM-XLS   To RUB-DATUM                                 
026900     Move DAGENS-DATUM-VECKA To RUB-VECKA-0                               
027000*    Move 22 To RUB-VECKA-0                                               
027100     Subtract 1 From DAGENS-DATUM-VECKA Giving RUB-VECKA-1                
027200*    Move 18 To RUB-VECKA-1                                               
027300                                                                          
027400     Write UT-POST From UT-RUB1-AREA                                      
027500     Write UT-POST From UT-RUB2-AREA                                      
027600                                                                          
027700     Move TAB  To UT-TAB-1 UT-TAB-2 UT-TAB-3                              
027800                  UT-TAB-6 UT-TAB-9                                       
027900     Move SPACE To UT-TAB-4 UT-TAB-5 UT-TAB-7 UT-TAB-8                    
028000                                                                          
028100     Move IDPGM TO POSTSUM-PROGNAMN                                       
028200     .                                                                    
028300     EJECT                                                                
028400 B-SKRIV-FRAN-BADA-FILERNA SECTION.                                       
028500     SKIP2                                                                
028600     If  IN1-ADLAGOMR = IN0-ADLAGOMR                                      
028700     And IN1-ADGANG   = IN0-ADGANG                                        
028800     And IN1-ADPLATS  = IN0-ADPLATS                                       
028900*       --- ingen förändring har skett                                    
029000        Continue                                                          
029100     Else                                                                 
029200        Move IN0-IDDC      To UT-IDDC                                     
029300        Move IN0-IDARTNR   To UT-IDARTNR                                  
029400        Move IN0-BEART-ENG To UT-BEART-ENG                                
029500                                                                          
029600        Move IN1-ADLAGOMR    To WS-ADLAGOMR-OLD                           
029700        Move WS-ADLAGOMR-OLD To UT-ADLAGOMR-OLD                           
029800        Move IN1-ADGANG      To WS-ADGANG-OLD                             
029900        Move WS-ADGANG-OLD   To UT-ADGANG-OLD                             
030000        Move IN1-ADPLATS     To WS-ADPLATS-OLD                            
030100        Move WS-ADPLATS-OLD  To UT-ADPLATS-OLD                            
030200                                                                          
030300        Move IN0-ADLAGOMR    To WS-ADLAGOMR-NEW                           
030400        Move WS-ADLAGOMR-NEW To UT-ADLAGOMR-NEW                           
030500        Move IN0-ADGANG      To WS-ADGANG-NEW                             
030600        Move WS-ADGANG-NEW   To UT-ADGANG-NEW                             
030700        Move IN0-ADPLATS     To WS-ADPLATS-NEW                            
030800        Move WS-ADPLATS-NEW  To UT-ADPLATS-NEW                            
030900                                                                          
031000        Perform S11-SKRIV-WXTR69                                          
031100     End-If                                                               
031200     .                                                                    
031300     EJECT                                                                
031400 C-SKRIV-FRAN-NYA-FILEN    SECTION.                                       
031500     SKIP2                                                                
031600     Move IN0-IDDC      To UT-IDDC                                        
031700     Move IN0-IDARTNR   To UT-IDARTNR                                     
031800     Move IN0-BEART-ENG To UT-BEART-ENG                                   
031900                                                                          
032000     Move IN0-ADLAGOMR    To WS-ADLAGOMR-NEW                              
032100     Move WS-ADLAGOMR-NEW To UT-ADLAGOMR-NEW                              
032200                                                                          
032300     Move IN0-ADGANG      To WS-ADGANG-NEW                                
032400     Move WS-ADGANG-NEW   To UT-ADGANG-NEW                                
032500                                                                          
032600     Move IN0-ADPLATS     To WS-ADPLATS-NEW                               
032700     Move WS-ADPLATS-NEW  To UT-ADPLATS-NEW                               
032800                                                                          
032900     Move 'NA'          To UT-ADLAGOMR-OLD                                
033000     Move SPACE         To UT-ADGANG-OLD                                  
033100                           UT-ADPLATS-OLD                                 
033200                                                                          
033300     Perform S11-SKRIV-WXTR69                                             
033400     .                                                                    
033500     EJECT                                                                
033600 D-SKRIV-FRAN-GAMLA-FILEN  SECTION.                                       
033700     SKIP2                                                                
033800     Move IN1-IDDC      To UT-IDDC                                        
033900     Move IN1-IDARTNR   To UT-IDARTNR                                     
034000     Move IN1-BEART-ENG To UT-BEART-ENG                                   
034100                                                                          
034200     Move IN1-ADLAGOMR    To WS-ADLAGOMR-OLD                              
034300     Move WS-ADLAGOMR-OLD To UT-ADLAGOMR-OLD                              
034400                                                                          
034500     Move IN1-ADGANG      To WS-ADGANG-OLD                                
034600     Move WS-ADGANG-OLD   To UT-ADGANG-OLD                                
034700                                                                          
034800     Move IN1-ADPLATS     To WS-ADPLATS-OLD                               
034900     Move WS-ADPLATS-OLD  To UT-ADPLATS-OLD                               
035000                                                                          
035100     Move 'NA'          To UT-ADLAGOMR-NEW                                
035200     Move SPACE         To UT-ADGANG-NEW                                  
035300                           UT-ADPLATS-NEW                                 
035400                                                                          
035500     Perform S11-SKRIV-WXTR69                                             
035600     continue                                                             
035700     .                                                                    
035800     EJECT                                                                
035900 Z-FINIT SECTION.                                                         
036000     Close SDCWEE-1                                                       
036100           SDCWEE-0                                                       
036200           WXTR69                                                         
036300     SKIP2                                                                
036400     Move 'S' TO POSTSUM-OPKOD                                            
036500     Call POSTSUM Using POSTSUM-PARM                                      
036600     .                                                                    
036700     EJECT                                                                
036800                                                                          
036900 S01-LAES-SDCWEE-1  SECTION.                                              
037000     SKIP2                                                                
037100     Read SDCWEE-1 Into IN1-AREA                                          
037200     At End                                                               
037300        Move HIGH-VALUE To IN1-AREA                                       
037400        Set END-OF-SDCWEE-1 To TRUE                                       
037500     Not At End                                                           
037600        If IN1-IDDC = '24'                                                
037700           Move 'SDCWEE1' To POSTSUM-FDNAMN                               
037800           Move 'WXTR69D1' To POSTSUM-DDNAMN2                             
037900           Move 'In1' To POSTSUM-TRANSTYP                                 
038000           Call POSTSUM Using POSTSUM-PARM                                
038100        End-If                                                            
038200     End-Read                                                             
038300     .                                                                    
038400     EJECT                                                                
038500                                                                          
038600 S02-LAES-SDCWEE-0  SECTION.                                              
038700     SKIP2                                                                
038800     Read SDCWEE-0 Into IN0-AREA                                          
038900     At End                                                               
039000        Move HIGH-VALUE To IN0-AREA                                       
039100        Set END-OF-SDCWEE-0 To TRUE                                       
039200     Not At End                                                           
039300        If IN0-IDDC = '24'                                                
039400           Move 'SDCWEE0' To POSTSUM-FDNAMN                               
039500           Move 'WXTR69D2' To POSTSUM-DDNAMN2                             
039600           Move 'In0'  To POSTSUM-TRANSTYP                                
039700           Call POSTSUM Using POSTSUM-PARM                                
039800        End-If                                                            
039900     End-Read                                                             
040000     .                                                                    
040100     EJECT                                                                
040200 S11-SKRIV-WXTR69 SECTION.                                                
040300                                                                          
040400     Write UT-POST From UT-AREA                                           
040500                                                                          
040600     Move 'UT'      To POSTSUM-TRANSTYP                                   
040700     Move 'WXTR69'  To POSTSUM-FDNAMN                                     
040800     Move 'WXTR69D3' To POSTSUM-DDNAMN2                                   
040900     Call POSTSUM Using POSTSUM-PARM                                      
041000     .                                                                    
041100     EJECT                                                                
041200 S99-ABEND SECTION.                                                       
041300                                                                          
041400     SKIP2                                                                
041500     Move 'S' To POSTSUM-OPKOD                                            
041600     Call POSTSUM Using POSTSUM-PARM                                      
041700     Call ABEND Using RKOD-ABEND                                          
041800     .                                                                    
