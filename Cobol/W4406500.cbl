000010 ID DIVISION.                                                             
000020 PROGRAM-ID.                 W4406500.                                    
000030 AUTHOR.                     STEFANO GIOBBI.                              
000040     DATE-WRITTEN.           JUN 1991.                                    
000050*                                                                         
000060     REMARKS.                                                             
000080*                                                                         
000090*    FUNKTION.                                                            
000100*                                                                         
000110*    LÄSER RO-REG (WDA5) MED SB.                                          
000120*    SALDOINFORMATION LISTAS PÅ FIL.                                      
000130*                                                                         
000140     EJECT                                                                
000150 ENVIRONMENT DIVISION.                                                    
000160 INPUT-OUTPUT SECTION.                                                    
000170 FILE-CONTROL.                                                            
000180     SKIP2                                                                
000200                                                                          
000210     SELECT W44065           ASSIGN TO      W44065D1.                     
000220     EJECT                                                                
000230 DATA DIVISION.                                                           
000240 FILE SECTION.                                                            
000250     SKIP2                                                                
000260 FD  W44065                                                               
000270     LABEL RECORD STANDARD                                                
000280     RECORDING F                                                          
000290     BLOCK CONTAINS 0.                                                    
000300*01  POST -COPY W440065   -PRE W44065-  -L.                               
000320     EJECT                                                                
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
000532*    ---- ARBETSFÄLT                                                      
000533                                                                          
000534 01  W-FLSKROT                   PIC X.                                   
000535     88  BEORDRAD                            VALUE 'J'.                   
000536     88  EJ-BEORDRAD                         VALUE 'N'.                   
000540     EJECT                                                                
000550*    ---- PARAMETRAR TILL POSTSUM                                         
000560                                                                          
000570*01  -COPY W0005      -PRE POSTSUM-.                                      
000590     EJECT                                                                
000600*    ---- UTAREA FÖR W44065-POST                                          
000610                                                                          
000620 01  FILLER                      PIC X(16)   VALUE                        
000630                                             'W-W44065-POST'.             
000631     SKIP3                                                                
000640*01  AREA -COPY W440065    -PRE UT-.                                      
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
000820                                                                          
000830*    ---- NYCKLAR OCH SÖKFÄLT TILL DLI                                    
000840                                                                          
000850 01  NYCKLAR-TILL-DLI.                                                    
000860   03  W-WDA501KY-X.                                                      
000861     05  W-IDGMTREF.                                                      
000870       07  W-IDDISTR             PIC S9(5)   COMP-3 VALUE +0.             
000871       07  W-IDKUNDNR            PIC S9(7)   COMP-3 VALUE +0.             
000873       07  W-IDKUNDRF            PIC X(10)          VALUE SPACE.          
000874       07  W-IDARTNR             PIC S9(9)   COMP-3 VALUE +0.             
000875       07  W-IDLOPNR             PIC S9(3)   COMP-3 VALUE +0.             
000880     EJECT                                                                
000890*01      -COPY W0003.                                                     
000910     EJECT                                                                
000920 01  FILLER                      PIC X(16)  VALUE                         
000930                                            'DLI-IO-AREA'.                
000940 01  DLI-IO-AREA.                                                         
000960*                                                                         
000970*  03  WLORDP01 -COPY WDA501                                              
000990     EJECT                                                                
001090 LINKAGE SECTION.                                                         
001100     SKIP2                                                                
001110*    -COPY W0008 -PRE WDA5-.                                              
001130    05  FILLER                   PIC XX.                                  
001140     EJECT                                                                
001150 PROCEDURE DIVISION  USING WDA5-PCB.                                      
001160     ENTRY 'DLITCBL' USING WDA5-PCB.                                      
001161                                                                          
001170 STYR SECTION.                                                            
001171                                                                          
001180     PERFORM A-INIT                                                       
001190     PERFORM IMS-GET-WDA5                                                 
001200     PERFORM UNTIL SEGMENT-SLUT                                           
001221       PERFORM B-SKRIV-UTPOST                                             
001420       PERFORM IMS-GET-WDA5                                               
001430     END-PERFORM                                                          
001440                                                                          
001450     PERFORM Z-FINIT                                                      
001460     MOVE    ZERO TO RETURN-CODE                                          
001470     GOBACK                                                               
001480     .                                                                    
001490     EJECT                                                                
001500 A-INIT SECTION.                                                          
001510                                                                          
001520     OPEN OUTPUT W44065                                                   
001530     MOVE 'W4406500'         TO POSTSUM-PROGNAMN                          
001540     MOVE 'W44065D1'         TO POSTSUM-DDNAMN2                           
001550     MOVE 'W44065  '         TO POSTSUM-FDNAMN                            
001560     .                                                                    
001570     EJECT                                                                
001580 B-SKRIV-UTPOST SECTION.                                                  
001581                                                                          
001582     IF (RAD-KDTPOTYP > +0 AND NOT = +4) AND                              
001583        RAD-KDSTARAD = '3'  AND                                           
001584        RAD-DARODAT  = ZERO                                               
001585                                                                          
001586       MOVE  RAD-IDARTNR   TO    UT-IDARTNR                               
001587       MOVE  RAD-IDDC      TO    UT-IDDC                                  
001588       MOVE  RAD-KDORDKL   TO    UT-KDORDKL                               
001589       MOVE  RAD-KVART     TO    UT-KVART                                 
001590                                                                          
001591       WRITE W44065-POST   FROM  UT-AREA                                  
001592                                                                          
001600       MOVE 'TPO '         TO    POSTSUM-TRANSTYP                         
001610       CALL  POSTSUM       USING POSTSUM-PARM                             
001611                                                                          
001612     END-IF                                                               
001620     .                                                                    
001630     EJECT                                                                
001640 Z-FINIT SECTION.                                                         
001650                                                                          
001660     CLOSE W44065                                                         
001670     MOVE  'S'     TO    POSTSUM-OPKOD                                    
001680     CALL  POSTSUM USING POSTSUM-PARM                                     
001690     .                                                                    
001700     EJECT                                                                
001713*                                                                         
001729 IMS-GET-WDA5 SECTION.                                                    
001730                                                                          
001742     MOVE    '  GAGKGB'       TO    GODK-STATUSKODER                      
001750     CALL    CBLTDLI          USING GN   WDA5-PCB DLI-IO-AREA             
001760     MOVE    WDA5-STATUS-CODE TO    STATUS-WS                             
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
