000100*** EDIT ALLOWED                                                          
000200 01  DYN-AREA.                                                            
000300*                                 INNAN EN FIL KAN ALLOKERAS              
000400*                                 DYNAMISKT MÅSTE ETT DCB BYGGAS.         
000500*                                 DCB:ET BYGGS MED SUBPROGRAMMET          
000600*                                 WFILREAD ELLER WFILWRT BEROENDE         
000700*                                 PÅ OM FILEN SKA LÄSAS ELLER             
000800*                                 SKRIVAS. SUBPROGRAMMEN GER ETT          
000900*                                 ETT FD-NAMN SOM SKA ANVÄNDAS VID        
001000*                                 ALLOKERINGEN.                           
001100*                                                                         
001200*                                 PARAMETRAR TILL WDYNALC FÖR             
001300*                                 DYNAMISK ALLOKERING AV EN FIL.          
001400*                                                                         
001500*                                 MAN ANGER ETT FD-NAMN PLUS              
001600*                                 SAMMA PARAMETRAR SOM MAN SKULLE         
001700*                                 ANGETT PÅ ETT DD-KORT.                  
001800*                                 DDNAMNET SOM ANGES I SELECT-            
001900*                                 SATSEN BYTS UT I SAMBAND MED            
002000*                                 ALLOKERINGEN, OCH SKA INTE              
002100*                                 FINNAS MED I JCL:EN.                    
002200*                                                                         
002300*                                 EXEMPEL PÅ ANROP:                       
002400*                                                                         
002500*                                 MOVE 'R.X.X' TO DYN-DSNAME              
002600*                                 MOVE '+0'    TO DYN-GENMBR              
002700*                                 MOVE 'O'   TO DYN-DISP1                 
002800*                                 CALL DYNALC USING FD-NAMN               
002900*                                                   DYN-AREA              
003000*                                                                         
003100*                                 OM ALLOKERINEN GÅR BRA (DYN-            
003200*                                 KDSVAR = SPACE), KAN MAN EFTER          
003300*                                 ANROPET ÖPPNA STÄNGA OCH HANTERA        
003400*                                 FD-NAMNET MED SUBPROGRAMMEN             
003500*                                 WFILREAD OCH WFILWRT.                   
003600*                                                                         
003700*                                 -------------------------------         
003800     03 DYN-FREE             PIC X                VALUE 'C'.              
003900*                                 AVALLOKERA FILEN VID CLOSE              
004000*                                 ELLER STEGSLUT.                         
004100*                                 C=CLOSE,E=END                           
004200     03 DYN-DSNAME           PIC X(44)            VALUE SPACE.            
004300*                                 DSNAMN UTAN GENERATION/MEDLEM.          
004400     03 DYN-GENMBR           PIC X(8)             VALUE SPACE.            
004500*                                 RELATIVT GENERATIONSNR                  
004600*                                 ELLER MEDLEMSNAMN I PDS                 
004700     03 DYN-DISP1            PIC X                VALUE SPACE.            
004800*                                 INITIALSTATUS FÖR FILEN                 
004900*                                 O=OLD,M=MOD,N=NEW,S=SHR                 
005000     03 DYN-DISP2            PIC X                VALUE SPACE.            
005100*                                 NORMAL DISPOSITION.                     
005200*                                 U=UNCATLG,C=CATLG,D=DELETE              
005300*                                 K=KEEP (PASS FINNS INTE)                
005400     03 DYN-DISP3            PIC X                VALUE SPACE.            
005500*                                 ONORMAL DISPOSITION.                    
005600*                                 U=UNCATLG,C=CATLG,D=DELETE              
005700*                                 K=KEEP (PASS FINNS INTE)                
005800     03 DYN-STGCLASS         PIC X(8)             VALUE SPACE.            
005900*                                 STORAGE CLASS (SMS).                    
006000     03 DYN-MGMCLASS         PIC X(8)             VALUE SPACE.            
006100*                                 MANAGEMENT CLASS (SMS).                 
006200     03 DYN-DATACLASS        PIC X(8)             VALUE SPACE.            
006300*                                 DATA CLASS (SMS).                       
006400     03 DYN-SPACETYPE        PIC X                VALUE SPACE.            
006500*                                 TYP AV SPACE-ENHET                      
006600*                                 T=TRK,C=CYL,B=BLOCKS                    
006700*                                 OM B ANGES, MÅSTE ÄVEN                  
006800*                                 BLKSIZE FYLLAS I.                       
006900     03 DYN-AVGREC           PIC X                VALUE SPACE.            
007000*                                 SPACE-ALLOKERING MED AVGREC             
007100*                                 U, K ELLER M                            
007200*                                 LRECL MÅSTE ÄVEN FYLLAS I               
007300     03 FILLER               PIC X(2).                                    
007400*                                                                         
007500     03 DYN-SPACE1           PIC S9(9)  COMP SYNC VALUE ZERO.             
007600*                                 PRIMÄR SPACE-ALLOKERING                 
007700     03 DYN-SPACE2           PIC S9(9)  COMP SYNC VALUE ZERO.             
007800*                                 SEKUNDÄR SPACE-ALLOKERING               
007900     03 DYN-RLSE             PIC X                VALUE SPACE.            
008000*                                 R = SLÄPP OUTNYTTJAD SPACE              
008100     03 DYN-CONTIG           PIC X                VALUE SPACE.            
008200*                                 C=CONTIG,M=MXIG,A=ALX                   
008300     03 DYN-ROUND            PIC X                VALUE SPACE.            
008400*                                 R=ROUND                                 
008500     03 DYN-UNIT             PIC X(8)             VALUE SPACE.            
008600*                                 PP,MD,WD,T9 ETC                         
008700     03 DYN-UNITCOUNT        PIC S9(4) COMP SYNC  VALUE ZERO.             
008800*                                 UNIT COUNT                              
008900     03 DYN-EXPDT            PIC X(5)             VALUE SPACE.            
009000*                                 EXPIRATION DATE.                        
009100*                                 FORMAT: YYDDD                           
009200     03 DYN-RETPD            PIC S9(4) COMP SYNC  VALUE ZERO.             
009300*                                 ANTAL DAGAR RETENTION PERIOD            
009400     03 DYN-DEFER            PIC X                VALUE SPACE.            
009500*                                 D = ALLOKERA VOLYM VID OPEN             
009600     03 DYN-BLKSIZE          PIC S9(4) COMP SYNC  VALUE ZERO.             
009700*                                 BLOCK STORLEK                           
009800     03 DYN-BUFNO            PIC S9(4) COMP SYNC  VALUE ZERO.             
009900*                                 ANTAL BUFFRAR                           
010000     03 DYN-DSORG            PIC X(3)             VALUE SPACE.            
010100*                                 PS, DA, PO ETC                          
010200     03 DYN-LRECL            PIC S9(4) COMP SYNC  VALUE ZERO.             
010300*                                 LOGICAL RECORD LENGTH                   
010400     03 DYN-RECFM            PIC X(3)             VALUE SPACE.            
010500*                                 POSTFORMAT. FB, VB ETC                  
010600*                                                                         
010700     03 DYN-SYSOUT           PIC X                VALUE SPACE.            
010800*                                 SYSOUT-CLASS                            
010900     03 DYN-FORMS            PIC X(4)             VALUE SPACE.            
011000*                                 FORMSNR                                 
011100     03 DYN-CHARS            PIC X(8)             VALUE SPACE.            
011200*                                 CHARACTER ARRANGEMENT TABLE             
011300     03 DYN-COPIES           PIC S9(4)  COMP SYNC VALUE ZERO.             
011400*                                 ANTAL KOPIOR                            
011500     03 DYN-DEST             PIC X(8)             VALUE SPACE.            
011600*                                 SYSOUT DESTINATION                      
011700     03 DYN-FCB              PIC X(4)             VALUE SPACE.            
011800*                                 FCB IMAGE ID                            
011900     03 DYN-HOLD             PIC X                VALUE SPACE.            
012000*                                 H = HOLD SYSOUT                         
012100*                                                                         
012200     03 DYN-KDSVAR           PIC X.                                       
012300      88 DYN-KDSVAR-OK       VALUE ' '.                                   
012400      88 DYN-KDSVAR-FEL      VALUE 'F'.                                   
012500*                                 SVARSKOD FRÅN SUBPROGRAM                
012600     03 DYN-DDNAME           PIC X(8)             VALUE SPACE.            
012610*                                 H = HOLD SYSOUT                         
012700*                                                                         
012800*** END COPY WDYNAREAC0  LENGTH=148   OLD LENGTH=197                      
