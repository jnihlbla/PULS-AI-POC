000010 ID DIVISION.                                                             
000020 PROGRAM-ID.                 W4406600.                                    
000030 AUTHOR.                     STEFANO GIOBBI.                              
000040     DATE-WRITTEN.           JUN 1991.                                    
000050*                                                                         
000060     REMARKS.                                                             
000080*                                                                         
000090*    FUNKTION.                                                            
000100*                                                                         
000110*    LÄSER ARTREG (WDK9) MED SB.                                          
000120*    SALDOINFORMATION LISTAS PÅ FILER.                                    
000130*                                                                         
000140     EJECT                                                                
000150 ENVIRONMENT DIVISION.                                                    
000160 INPUT-OUTPUT SECTION.                                                    
000170 FILE-CONTROL.                                                            
000180     SKIP2                                                                
000200                                                                          
000210     SELECT W44066           ASSIGN TO      W44066D1.                     
000211     SELECT W44074           ASSIGN TO      W44066D2.                     
000220     EJECT                                                                
000230 DATA DIVISION.                                                           
000240 FILE SECTION.                                                            
000250     SKIP2                                                                
000260 FD  W44066                                                               
000270     LABEL RECORD STANDARD                                                
000280     RECORDING F                                                          
000290     BLOCK CONTAINS 0.                                                    
000300*01  POST -COPY W440066   -PRE W44066-  -L.                               
000320     EJECT                                                                
000322 FD  W44074                                                               
000323     LABEL RECORD STANDARD                                                
000324     RECORDING F                                                          
000325     BLOCK CONTAINS 0.                                                    
000326*01  POST -COPY W440074   -PRE W44074-  -L.                               
000327     EJECT                                                                
000330 WORKING-STORAGE SECTION.                                                 
000340     SKIP2                                                                
000341                                                                          
000342*    -- CHECKED BY WY2000                                                 
000350*    ---- GENERELLA KONSTANTER                                            
000360 77  JA                          PIC X       VALUE 'J'.                   
000370 77  NEJ                         PIC X       VALUE 'N'.                   
000380                                                                          
000500 01  DYNAMISKA-SUBPROGRAM.                                                
000510   03  POSTSUM                   PIC X(8)    VALUE 'POSTSUM'.             
000520   03  CBLTDLI                   PIC X(8)    VALUE 'CBLTDLI'.             
000530   03  FELLOG                    PIC X(8)    VALUE 'FELLOG '.             
000531     EJECT                                                                
000550*    ---- PARAMETRAR TILL POSTSUM                                         
000560                                                                          
000570*01  -COPY W0005      -PRE POSTSUM-.                                      
000590     EJECT                                                                
000600*    ---- UTAREA FÖR W44066-POST                                          
000610                                                                          
000620 01  FILLER                      PIC X(16)   VALUE                        
000630                                             'W-W44066-POST'.             
000631     SKIP3                                                                
000640*01  AREA -COPY W440066    -PRE U66-.                                     
000650     EJECT                                                                
000651*    ---- UTAREA FÖR W44074-POST                                          
000652                                                                          
000653 01  FILLER                      PIC X(16)   VALUE                        
000654                                             'W-W44074-POST'.             
000655     SKIP3                                                                
000656*01  AREA -COPY W440074    -PRE U74-.                                     
000660     EJECT                                                                
000670                                                                          
000680*    ---- ARBETS-AREOR FÖR IMS-SEKTIONERNA.                               
000690                                                                          
000700 01  FILLER                      PIC X(8)   VALUE 'IMS-WS  '.             
000710                                                                          
000720*    ---- STATUSKOD FRÅN IMS                                              
000730                                                                          
000740 01  STATUS-WS                   PIC XX.                                  
000750     88  SEGMENT-FINNS                      VALUE '  '.                   
000760     88  SEGMENT-SLUT                       VALUE 'GB'.                   
000770                                                                          
000780 01  GODK-STATUSKODER.                                                    
000790   03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                  
000800                                                                          
000810 01  SSA1                        PIC X(40).                               
000880     EJECT                                                                
000890*01      -COPY W0003.                                                     
000910     EJECT                                                                
000920 01  FILLER                      PIC X(16)  VALUE                         
000930                                            'DLI-IO-AREA'.                
000940 01  DLI-IO-AREA.                                                         
000960*                                                                         
000970*  03  WLARTM01 -COPY WDK901                                              
000990     EJECT                                                                
001090 LINKAGE SECTION.                                                         
001100     SKIP2                                                                
001110*    -COPY W0008 -PRE WDK9-.                                              
001130    05  FILLER                   PIC XX.                                  
001140     EJECT                                                                
001150 PROCEDURE DIVISION  USING WDK9-PCB.                                      
001160     ENTRY 'DLITCBL' USING WDK9-PCB.                                      
001161                                                                          
001170 STYR SECTION.                                                            
001171                                                                          
001180     PERFORM A-INIT                                                       
001190     PERFORM IMS-GET-WDK9                                                 
001200     PERFORM UNTIL SEGMENT-SLUT                                           
001210       IF WDK9-SEG-NAME-FB = 'WDK901  '                                   
001221         PERFORM B-SKRIV-W44066-UTPOST                                    
001222         PERFORM C-SKRIV-W44074-UTPOST                                    
001223       END-IF                                                             
001420       PERFORM IMS-GET-WDK9                                               
001430     END-PERFORM                                                          
001440                                                                          
001450     PERFORM Z-FINIT                                                      
001460     MOVE    ZERO TO RETURN-CODE                                          
001470     GOBACK                                                               
001480     .                                                                    
001490     EJECT                                                                
001500 A-INIT SECTION.                                                          
001510                                                                          
001520     OPEN OUTPUT W44066                                                   
001521                 W44074                                                   
001522     MOVE 'W4406600'         TO POSTSUM-PROGNAMN                          
001560     .                                                                    
001570     EJECT                                                                
001580 B-SKRIV-W44066-UTPOST SECTION.                                           
001581                                                                          
001582     MOVE  ART-IDARTNR           TO    U66-IDARTNR                        
001583                                                                          
001584     MOVE  ART-KVOKS-BULK        TO    U66-KVOKS-BULK                     
001586     MOVE  ART-KVOKS-DAG         TO    U66-KVOKS-DAG                      
001588     MOVE  ART-KVOKS-VOR         TO    U66-KVOKS-VOR                      
001591                                                                          
001592     MOVE  ART-KVPREAVB-BULK     TO    U66-KVPREAVB-BULK                  
001594     MOVE  ART-KVPREAVB-DAG      TO    U66-KVPREAVB-DAG                   
001596     MOVE  ART-KVPREAVB-VOR      TO    U66-KVPREAVB-VOR                   
001598                                                                          
001599     MOVE  ART-KVPRERO-BULK      TO    U66-KVPRERO-BULK                   
001601     MOVE  ART-KVPRERO-DAG       TO    U66-KVPRERO-DAG                    
001610                                                                          
001611     MOVE 'W44066D1'         TO POSTSUM-DDNAMN2                           
001612     MOVE 'W44066  '         TO POSTSUM-FDNAMN                            
001613                                                                          
001614     WRITE W44066-POST           FROM  U66-AREA                           
001615                                                                          
001616     MOVE 'U66 '                 TO    POSTSUM-TRANSTYP                   
001617     CALL  POSTSUM               USING POSTSUM-PARM                       
001618     .                                                                    
001619     EJECT                                                                
001620 C-SKRIV-W44074-UTPOST SECTION.                                           
001621                                                                          
001622     MOVE  ART-IDARTNR           TO    U74-IDARTNR                        
001623     MOVE  ART-KVOFFERT          TO    U74-KVOFFERT                       
001625                                                                          
001645     WRITE W44074-POST           FROM  U74-AREA                           
001646                                                                          
001648     MOVE 'W44066D2'         TO POSTSUM-DDNAMN2                           
001649     MOVE 'W44074  '         TO POSTSUM-FDNAMN                            
001650                                                                          
001651     MOVE  'U74 '                TO    POSTSUM-TRANSTYP                   
001652     CALL  POSTSUM               USING POSTSUM-PARM                       
001653     .                                                                    
001654     EJECT                                                                
001655 Z-FINIT SECTION.                                                         
001656                                                                          
001660     CLOSE W44066                                                         
001661           W44074                                                         
001670     MOVE  'S'     TO    POSTSUM-OPKOD                                    
001680     CALL  POSTSUM USING POSTSUM-PARM                                     
001690     .                                                                    
001700     EJECT                                                                
001713*                                                                         
001729 IMS-GET-WDK9 SECTION.                                                    
001730                                                                          
001742     MOVE    '  GAGKGB'       TO    GODK-STATUSKODER                      
001750     CALL    CBLTDLI          USING GN   WDK9-PCB DLI-IO-AREA             
001760     MOVE    WDK9-STATUS-CODE TO    STATUS-WS                             
001770     PERFORM IMS-STATUSKONTROLL                                           
001780     .                                                                    
001790     SKIP3                                                                
001800 IMS-STATUSKONTROLL SECTION.                                              
001810                                                                          
001820     SET    STATUS-IX TO 1                                                
001830     SEARCH GODK-STATUS                                                   
001840       AT END CALL FELLOG                                                 
001850       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
001851         CONTINUE                                                         
001860     END-SEARCH                                                           
001870     .                                                                    
