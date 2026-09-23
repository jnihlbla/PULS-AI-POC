000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W4160400.                                                
000400 AUTHOR.         PER BERGH.                                               
000500 DATE-WRITTEN.   91/09/18.                                                
000600                                                                          
000700     REMARKS.                                                             
000800*                                                                         
000900*    FUNCTION.                                                            
001000*                                                                         
001100*        PROGRAMMET LÄSER WDJ2 (WLSATG) MED SB                            
001200*                                                                         
001300*                                                                         
001400*        PROGRAMMET SKAPAR EN FIL AVSEDD ATT LÄSAS                        
001500*        AV E+-PROGRAMMET W4160500 DÄR LARMLISTA                          
001600*        SKAPAS. EJ FÄRDIGBEHANDLADE, MER ÄN 90                           
001601*        DAGAR GAMLA SATSORDER OAVSETT ARBETSDAGAR                        
001610*        OCH SEMESTER SKRIVS UT PÅ FIL.                                   
001620*                                                                         
001800*    INDATA.                                                              
001900*        DB           WDJ2                                                
002000*                                                                         
002100*                                                                         
002200*                                                                         
002300*   UTDATA.                                                               
002400*        FIL          W41604                                              
002500                                                                          
002600     SKIP3                                                                
002700 ENVIRONMENT DIVISION.                                                    
002800 INPUT-OUTPUT SECTION.                                                    
002900 FILE-CONTROL.                                                            
003000     SKIP2                                                                
003100*    SATSORDERFIL W41604   OUTPUT                                         
003200     SELECT W41604 ASSIGN TO W41604D1.                                    
003300     EJECT                                                                
003400 DATA DIVISION.                                                           
003500 FILE SECTION.                                                            
003600      SKIP2                                                               
003700 FD  W41604                                                               
003800     LABEL RECORD STANDARD                                                
003900     RECORDING F                                                          
004000     BLOCK CONTAINS 0.                                                    
004100 01  W41604-POST  PIC X(31).                                              
004300     EJECT                                                                
004400 WORKING-STORAGE SECTION.                                                 
004401*    -- CHECKED BY WY2000                                                 
004402     SKIP3                                                                
004500 77  IDPGM                       PIC X(08)   VALUE 'W4160400'.            
004600                                                                          
004700 77  INDX                        PIC S9(4)   VALUE +0 COMP SYNC.          
004800                                                                          
004900                                                                          
005000 01 FILLER                      PIC  X(12) VALUE 'ARBETSAREOR '.          
005001                                                                          
005011 01 DATUM-Y2K.                                                            
005012    03  DATUM-AAR-Y2K                          PIC  9(4).                 
005013    03  DATUM-MAAN-Y2K                         PIC  9(2).                 
005014    03  DATUM-DAG-Y2K                          PIC  9(2).                 
005015                                                                          
005016 EJECT                                                                    
005020 01 UTAREA.                                                               
005100 SKIP2                                                                    
005120     03  UT-IDANSK               PIC S9(03)         COMP-3.               
005200     03  UT-IDARTNR              PIC S9(09)         COMP-3.               
005300     03  UT-IDORDNST.                                                     
005310       05  UT-IDORDNSB           PIC S9(05)         COMP-3.               
005320       05  UT-IDORDNSS           PIC S9             COMP-3.               
005400     03  UT-IDPRODNR             PIC S9(07)         COMP-3.               
005500     03  UT-KDSATSTA             PIC X.                                   
005510     03  UT-KVBEART              PIC S9(07)         COMP-3.               
005520     03  UT-TIREGDAT             PIC S9(07)         COMP-3.               
005550     03  UT-TIBEGPAC             PIC S9(07)         COMP-3.               
005570     03  UT-IDDISTR              PIC S9(05)         COMP-3.               
005600*                                                                         
006000     EJECT                                                                
006100*    --- SUBPROGRAM AND PARAMETER-AREAS                                   
006200 01  GENERAL-SUBPROGRAM.                                                  
006300     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
006400     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI'.             
006500     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
006600     EJECT                                                                
006700*    --- PARAMETRAR FÖR  SUBPROGRAM POSTSUM                               
006800*   -COPY W0005 -PRE POSTSUM-.                                            
007020     EJECT                                                                
007100*    --- STATUS-CODE FROM IMS                                             
007200 01  STATUS-WS                   PIC XX.                                  
007300     88  SEGMENT-FINNS                       VALUE '  '.                  
007400     88  SEGMENT-SLUT                        VALUE 'GB'.                  
007500     SKIP2                                                                
007600 01  GODK-STATUSKODER.                                                    
007700     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
007800 01  SSA1                        PIC X(64).                               
008100     EJECT                                                                
008200*    --- IMS FUNKTIONSKODER                                               
008300*01  -COPY W0003                                                          
008500     EJECT                                                                
008510*    ---  DLI INPUT-OUTPUT AREA                                           
008520                                                                          
008530 01  FILLER         PIC X(16) VALUE 'DLI-IO-WLSATG01'.                    
008540 01  DLI-IO-WLSATG01.                                                     
008550*    03  -COPY WDJ201                                                     
008560     EJECT                                                                
010400 LINKAGE SECTION.                                                         
010500                                                                          
010600*01  -COPY W0008      -PRE SATG-                                          
010800     05  FILLER                  PIC X.                                   
010900     EJECT                                                                
011000                                                                          
011100 PROCEDURE DIVISION  USING  SATG-PCB.                                     
011200     ENTRY 'DLITCBL' USING  SATG-PCB.                                     
011300                                                                          
011400     PERFORM A-INIT                                                       
011500     PERFORM IMS-GET-WDJ2                                                 
011600     PERFORM UNTIL SEGMENT-SLUT                                           
011604       IF SHUV-DAREGDAT < DATUM-Y2K                                       
011711            MOVE SHUV-IDANSK     TO UT-IDANSK                             
011720            MOVE SHUV-IDARTNR    TO UT-IDARTNR                            
011800            MOVE SHUV-IDORDNSB   TO UT-IDORDNSB                           
011810            MOVE SHUV-IDORDNSS   TO UT-IDORDNSS                           
011830            MOVE SHUV-IDPRODNR   TO UT-IDPRODNR                           
011840            MOVE SHUV-KDSATSTA   TO UT-KDSATSTA                           
011850            MOVE SHUV-KVBEART    TO UT-KVBEART                            
011851            MOVE SHUV-DAREGDAT (3:6)   TO UT-TIREGDAT                     
011860            MOVE SHUV-TIBEGPAC   TO UT-TIBEGPAC                           
011890            MOVE SHUV-IDDISTR    TO UT-IDDISTR                            
012400                                                                          
012500            PERFORM B-SKRIV-UTFIL                                         
012600       END-IF                                                             
012700       PERFORM IMS-GET-WDJ2                                               
012800     END-PERFORM                                                          
012900     PERFORM Z-FINAL                                                      
013000     MOVE ZERO TO RETURN-CODE                                             
013100     GOBACK                                                               
013200     .                                                                    
013300     EJECT                                                                
013400 A-INIT SECTION.                                                          
013500                                                                          
013600     OPEN OUTPUT W41604                                                   
013700     MOVE 'W41604'             TO POSTSUM-PROGNAMN                        
013800     MOVE 'W41604D1'           TO POSTSUM-DDNAMN2                         
013900     MOVE 'W41604  '           TO POSTSUM-FDNAMN                          
013912     MOVE FUNCTION CURRENT-DATE (1:8) TO DATUM-Y2K                        
013916     IF DATUM-MAAN-Y2K > 3                                                
013930        SUBTRACT 3 FROM DATUM-MAAN-Y2K                                    
013940     ELSE                                                                 
013950        ADD 12 TO DATUM-MAAN-Y2K                                          
013960        SUBTRACT 3 FROM DATUM-MAAN-Y2K                                    
013970        SUBTRACT 1 FROM DATUM-AAR-Y2K                                     
013990     END-IF                                                               
014000     .                                                                    
014100     EJECT                                                                
014200 B-SKRIV-UTFIL SECTION.                                                   
014300                                                                          
014400     WRITE W41604-POST FROM UTAREA                                        
014500                                                                          
014600     CALL POSTSUM    USING POSTSUM-PARM                                   
014700     .                                                                    
014800     EJECT                                                                
014900 Z-FINAL  SECTION.                                                        
015000                                                                          
015100     CLOSE W41604                                                         
015200                                                                          
015300     MOVE 'S' TO POSTSUM-OPKOD                                            
015400       CALL POSTSUM  USING POSTSUM-PARM                                   
015500     .                                                                    
015600     EJECT                                                                
015700 IMS-GET-WDJ2 SECTION.                                                    
015800                                                                          
015900     MOVE '  GAGKGB' TO GODK-STATUSKODER                                  
016000     CALL CBLTDLI USING GN SATG-PCB DLI-IO-WLSATG01                       
016100     MOVE SATG-STATUS-CODE TO STATUS-WS                                   
016200     PERFORM IMS-STATUSCONTROL                                            
016300     .                                                                    
016400     EJECT                                                                
016500 IMS-STATUSCONTROL SECTION.                                               
016600                                                                          
016700     SET STATUS-IX TO 1                                                   
016800     SEARCH GODK-STATUS                                                   
016900       AT END CALL FELLOG                                                 
017000       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
017100     END-SEARCH                                                           
017200     .                                                                    
