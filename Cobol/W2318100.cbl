000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W2318100.                                                
000400*AUTHOR.         BODIL LINDAHL.                                           
000500*DATE-WRITTEN.   94/08/30.                                                
000600                                                                          
000700*    REMARKS.                                                             
000800*                                                                         
000900*    FUNKTION:                                                            
000910*      - LÄSER FIL W23178                                                 
000911*      - LÄSER FIL W231PP MED URVAL FRÅN BILD 2324                        
000912*      - SKAPAR LISTA SORTIMENTANALYS SAMTLIGA ARTIKLAR                   
000913*               / LISTA SORTIMENTANALYS MED URVAL FRÅN BILD 2324          
000914*                                                                         
000915     SKIP3                                                                
000916 ENVIRONMENT DIVISION.                                                    
000917     SKIP2                                                                
000918 INPUT-OUTPUT SECTION.                                                    
000919     EJECT                                                                
000920 FILE-CONTROL.                                                            
000930     SKIP2                                                                
000940*          --- GRUNDFIL ANALYS-LISTOR                                     
000950     SELECT W23178                     ASSIGN TO W23181D1.                
000960*          --- LISTA                                                      
000970     SELECT W23181-001                 ASSIGN TO W23181D2.                
000980*          --- URVAL FRÅN BILD 2324                                       
000990     SELECT W231PP                     ASSIGN TO W23181D3.                
001000                                                                          
001100     EJECT                                                                
001200 DATA DIVISION.                                                           
001300     SKIP3                                                                
001400 FILE SECTION.                                                            
001500     SKIP3                                                                
001600 FD  W23178                                                               
001700     RECORDING       F                                                    
001800     BLOCK CONTAINS  0.                                                   
001900     SKIP2                                                                
002000*01  -COPY W231781A      -L.                                              
002100     SKIP3                                                                
002200 FD  W231PP                                                               
002300     RECORDING       F                                                    
002400     BLOCK CONTAINS  0.                                                   
002500     SKIP2                                                                
002600 01  PARM                PIC X(80).                                       
002700     SKIP3                                                                
002800 FD  W23181-001                                                           
002900     RECORDING       F                                                    
003000     BLOCK CONTAINS  0.                                                   
003100     SKIP2                                                                
003200 01  W23181-001-RAD              PIC X(177).                              
003300     EJECT                                                                
003400 WORKING-STORAGE SECTION.                                                 
003410     SKIP2                                                                
003411                                                                          
003412*    -- CHECKED BY WY2000                                                 
003420 77  IDPGM                       PIC X(8)    VALUE 'W2318100'.            
003430 77  JA                          PIC X       VALUE 'J'.                   
003440 77  NEJ                         PIC X       VALUE 'N'.                   
003450 77  SW-TRAEFF                   PIC X       VALUE 'N'.                   
003460 77  SW-TOTALLISTA               PIC X       VALUE 'N'.                   
003461 77  SW-LISTAOK                  PIC X       VALUE 'N'.                   
003462                                                                          
003470 77  IX1                         PIC S9(3)   VALUE ZERO COMP-3.           
003480 77  IX2                         PIC S9(3)   VALUE ZERO COMP-3.           
003490 77  IX3                         PIC S9(3)   VALUE ZERO COMP-3.           
003500 77  IX4                         PIC S9(3)   VALUE ZERO COMP-3.           
003600 77  IX5                         PIC S9(3)   VALUE ZERO COMP-3.           
003700 77  IX6                         PIC S9(3)   VALUE ZERO COMP-3.           
003710 77  IX7                         PIC S9(3)   VALUE ZERO COMP-3.           
003711 77  IX8                         PIC S9(3)   VALUE ZERO COMP-3.           
003712 77  IX9                         PIC S9(3)   VALUE ZERO COMP-3.           
003713 77  IX10                        PIC S9(3)   VALUE ZERO COMP-3.           
003720 77  ART-IX                      PIC S9(3)   VALUE ZERO COMP-3.           
003730 77  ART-IX-MAX                  PIC S9(3)   VALUE +90  COMP-3.           
003740 77  PSUM-IX                     PIC S9(3)   VALUE ZERO COMP-3.           
003750 77  PSUM-IX-MAX                 PIC S9(3)   VALUE +9   COMP-3.           
003760 77  FSUM-IX                     PIC S9(3)   VALUE ZERO COMP-3.           
003770 77  FSUM-IX-MAX                 PIC S9(3)   VALUE +10  COMP-3.           
003771                                                                          
003780 77  WS-PROC-KVANT               PIC S9(3)V9(1) VALUE ZERO COMP-3.        
003790 77  WS-PROC-ARSPROG             PIC S9(3)V9(1) VALUE ZERO COMP-3.        
003800 77  WS-PROC-ARSFORB             PIC S9(3)V9(1) VALUE ZERO COMP-3.        
003900 77  WS-PROC-OLAGER              PIC S9(3)V9(1) VALUE ZERO COMP-3.        
004000 77  WS-PROC-SLAGER              PIC S9(3)V9(1) VALUE ZERO COMP-3.        
004100 77  WS-PROC-AVVIK               PIC S9(3)V9(1) VALUE ZERO COMP-3.        
004110 77  WS-PROC-KVINORD             PIC S9(3)V9(1) VALUE ZERO COMP-3.        
004120 77  WS-PROC-EJAVBOK             PIC S9(3)V9(1) VALUE ZERO COMP-3.        
004130 77  WS-PROC-KVFYSAVV            PIC S9(3)V9(1) VALUE ZERO COMP-3.        
004140 77  WS-PROC-OMSHAST             PIC S9(3)V9(1) VALUE ZERO COMP-3.        
004150 77  WS-PROC-TACKTID             PIC S9(3)V9(1) VALUE ZERO COMP-3.        
004151                                                                          
004160 77  WS-EJAVBOK                  PIC S9(11)V9(2)                          
004170                                  VALUE ZERO COMP-3.                      
004171 77  WS-KVQ                      PIC S9(9)V9(2) VALUE ZERO COMP-3.        
004172 77  WS-KVLS-TOT                 PIC S9(11)     VALUE ZERO COMP-3.        
004173 77  WS-KVRESS-TOT               PIC S9(11)     VALUE ZERO COMP-3.        
004174 77  WS-KVOKS-TOT                PIC S9(11)     VALUE ZERO COMP-3.        
004175 77  WS-KVOI                     PIC S9(11)V9(2)                          
004176                                   VALUE ZERO COMP-3.                     
004177 77  WS-KVDISP                   PIC S9(11)     VALUE ZERO COMP-3.        
004178 77  WS-KVDISP-DEC               PIC S9(11)V9(2)                          
004179                                   VALUE ZERO COMP-3.                     
004180 77  WS-KVDISP-PR                PIC S9(11)V9(2)                          
004181                                   VALUE ZERO COMP-3.                     
004182 77  WS-KVDISP-PRIS              PIC S9(11)V9(2)                          
004183                                   VALUE ZERO COMP-3.                     
004184 77  WS-KVDISP-SNITT             PIC S9(11)V9(2)                          
004185                                   VALUE ZERO COMP-3.                     
004187 77  WS-SERVG                    PIC S9(7)V9(1) VALUE ZERO COMP-3.        
004188 77  WS-KVINORD                  PIC S9(9)      VALUE ZERO COMP-3.        
004189 77  WS-KVPB                     PIC S9(9)V9(2) VALUE ZERO COMP-3.        
004191 77  WS-ARSFORB-KR               PIC 9(11)      VALUE ZERO.               
004192 77  WS-ARSFORB                  PIC 9(11)V9(2) VALUE ZERO.               
004196 77  WS-ARSPROG-KR               PIC 9(11).                               
004197 77  WS-ARSPROG                  PIC 9(11)V9(2) VALUE ZERO.               
004201 77  WS-SLAGER-KR                PIC 9(11)       VALUE ZERO.              
004202 77  WS-SLAGER                   PIC S9(11)V9(2) VALUE ZERO.              
004206 77  WS-OLAGER-KR                PIC 9(11)      VALUE ZERO.               
004207 77  WS-OLAGER                   PIC 9(11)V9(2) VALUE ZERO.               
004211 77  WS-AVVIK-KR                 PIC S9(11)       VALUE ZERO.             
004212 77  WS-AVVIK-KRONOR             PIC S9(11)V9(2)  VALUE ZERO.             
004213 77  WS-AVVIK-RAKN               PIC S9(11)V9(2).                         
004217 77  WS-OMSHAST                  PIC 9(11)V9(1)  VALUE ZERO.              
004218 77  WS-TACKTID                  PIC 9(11)V9(2)  VALUE ZERO.              
004219 77  WS-TACKTID-VECKOR           PIC 9(11)V9(1)  VALUE ZERO.              
004220                                                                          
004226 77  W23178-EOF-SW               PIC X       VALUE 'N'.                   
004227     88  END-OF-W23178                       VALUE 'J'.                   
004228                                                                          
004229 77  W231PP-EOF-SW               PIC X       VALUE 'N'.                   
004230     88  END-OF-W231PP                       VALUE 'J'.                   
004231                                                                          
004232 01  WS-DAGENS-DATUM             PIC 9(6).                                
004233                                                                          
004234 01  DAGENS-DATUM                PIC 9(6).                                
004235 01  FILLER REDEFINES DAGENS-DATUM.                                       
004236     03  DAGENS-AAR              PIC 9(2).                                
004237     03  DAGENS-MAANAD           PIC 9(2).                                
004238     03  DAGENS-DAG              PIC 9(2).                                
004239                                                                          
004240 01  DAGENS-VECKA                PIC 9(4).                                
004241 01  FILLER REDEFINES DAGENS-VECKA.                                       
004242     03  D-VECKA-AAR             PIC 9(2).                                
004243     03  D-VECKA-VECKA           PIC 9(2).                                
004244                                                                          
004245 01  VECKOR.                                                              
004246     03  AAVVD                   PIC 9(5).                                
004247     03  FILLER REDEFINES AAVVD.                                          
004248         05  AAVV                PIC 9(4).                                
004249         05  D                   PIC 9(1).                                
004250                                                                          
004251 01  PARAM-TILL-W009VADD.                                                 
004252     03  W009VADD-DATUM          PIC S9(5) COMP-3.                        
004253     03  W009VADD-ANTAL          PIC S9(3) COMP-3.                        
004254     EJECT                                                                
004256 01  ART-TABELL.                                                          
004257     03 ART-RAD OCCURS 90.                                                
004260        05  ART-KVANT            PIC S9(9)      VALUE ZERO COMP-3.        
004270        05  ART-PROC-KVANT       PIC S9(7)V9(1) VALUE ZERO COMP-3.        
004280        05  ART-ARSFORB          PIC S9(9)      VALUE ZERO COMP-3.        
004290        05  ART-PROC-ARSFORB     PIC S9(7)V9(1) VALUE ZERO COMP-3.        
004300        05  ART-ARSPROG          PIC S9(9)      VALUE ZERO COMP-3.        
004400        05  ART-PROC-ARSPROG     PIC S9(7)V9(1) VALUE ZERO COMP-3.        
004410        05  ART-SLAGER           PIC S9(9)      VALUE ZERO COMP-3.        
004420        05  ART-PROC-SLAGER      PIC S9(7)V9(1) VALUE ZERO COMP-3.        
004430        05  ART-OLAGER           PIC S9(9)      VALUE ZERO COMP-3.        
004431        05  ART-PROC-OLAGER      PIC S9(7)V9(1) VALUE ZERO COMP-3.        
004432        05  ART-AVVIK            PIC S9(9)      VALUE ZERO COMP-3.        
004433        05  ART-PROC-AVVIK       PIC S9(7)V9(1) VALUE ZERO COMP-3.        
004434        05  ART-KVINORD          PIC S9(9)      VALUE ZERO COMP-3.        
004435        05  ART-PROC-KVINORD     PIC S9(7)V9(1) VALUE ZERO COMP-3.        
004436        05  ART-EJAVBOK         PIC S9(11)V9(2) VALUE ZERO COMP-3.        
004437        05  ART-PROC-EJAVBOK     PIC S9(7)V9(1) VALUE ZERO COMP-3.        
004438        05  ART-KVFYSAVV         PIC S9(9)      VALUE ZERO COMP-3.        
004439        05  ART-PROC-KVFYSAVV    PIC S9(7)V9(1) VALUE ZERO COMP-3.        
004440        05  ART-OMSHAST          PIC S9(7)V9(1) VALUE ZERO COMP-3.        
004450        05  ART-PROC-OMSHAST     PIC S9(7)V9(1) VALUE ZERO COMP-3.        
004460        05  ART-TACKTID          PIC S9(9)V9(1) VALUE ZERO COMP-3.        
004470        05  ART-PROC-TACKTID     PIC S9(7)V9(1) VALUE ZERO COMP-3.        
004480        05  ART-SERVG-BTO        PIC S9(7)V9(1) VALUE ZERO COMP-3.        
004490        05  ART-SERVG-NTO        PIC S9(7)V9(1) VALUE ZERO COMP-3.        
004491*******  ARBETSFÄLT                                                       
004492        05  WS-ART-AVVIK         PIC S9(11)V9(2) VALUE ZERO.              
004493        05  WS-ART-ARSFORB       PIC S9(11)V9(2) VALUE ZERO.              
004494        05  WS-ART-SLAGER        PIC S9(11)V9(2) VALUE ZERO.              
004495        05  WS-ART-OLAGER        PIC S9(11)V9(2) VALUE ZERO.              
004496        05  WS-ART-KVLS          PIC S9(11)      VALUE ZERO.              
004497        05  WS-ART-KVRESS        PIC S9(11)      VALUE ZERO.              
004498        05  WS-ART-KVOKS         PIC S9(11)      VALUE ZERO.              
004499        05  WS-ART-KVOI          PIC S9(11)V9(2) VALUE ZERO.              
004500        05  WS-ART-KVDISP-SNITT  PIC S9(11)V9(2) VALUE ZERO.              
004501        05  WS-ART-ARSPROG       PIC S9(11)V9(2) VALUE ZERO.              
004502        05  WS-ART-KVDISP        PIC S9(11)      VALUE ZERO.              
004503        05  WS-ART-KVDISP-PR     PIC S9(11)V9(2) VALUE ZERO.              
004504        05  WS-ART-KVINORD       PIC S9(11)      VALUE ZERO.              
004505        05  WS-ART-KVAVBRAD      PIC 9(11)V9(2)  VALUE ZERO.              
004509        05  WS-ART-KVFYSAVV      PIC 9(11)V9(2)  VALUE ZERO.              
004510        05  WS-ART-EJAVBOK      PIC S9(11)V9(2) VALUE ZERO COMP-3.        
004513     EJECT                                                                
004514 01  PSUM-TABELL.                                                         
004515     03 PSUM-RAD OCCURS 9.                                                
004516        05  PSUM-KVANT          PIC S9(9)      VALUE ZERO COMP-3.         
004517        05  PSUM-PROC-KVANT     PIC S9(7)V9(1) VALUE ZERO COMP-3.         
004518        05  PSUM-ARSFORB        PIC S9(9)      VALUE ZERO COMP-3.         
004519        05  PSUM-PROC-ARSFORB   PIC S9(7)V9(1) VALUE ZERO COMP-3.         
004520        05  PSUM-ARSPROG        PIC S9(9)      VALUE ZERO COMP-3.         
004521        05  PSUM-PROC-ARSPROG   PIC S9(7)V9(1) VALUE ZERO COMP-3.         
004522        05  PSUM-SLAGER         PIC S9(9)      VALUE ZERO COMP-3.         
004523        05  PSUM-PROC-SLAGER    PIC S9(7)V9(1) VALUE ZERO COMP-3.         
004524        05  PSUM-OLAGER         PIC S9(9)      VALUE ZERO COMP-3.         
004525        05  PSUM-PROC-OLAGER    PIC S9(7)V9(1) VALUE ZERO COMP-3.         
004526        05  PSUM-AVVIK          PIC S9(9)      VALUE ZERO COMP-3.         
004527        05  PSUM-PROC-AVVIK     PIC S9(7)V9(1) VALUE ZERO COMP-3.         
004528        05  PSUM-KVINORD        PIC S9(9)      VALUE ZERO COMP-3.         
004529        05  PSUM-PROC-KVINORD   PIC S9(7)V9(1) VALUE ZERO COMP-3.         
004530        05  PSUM-EJAVBOK        PIC S9(9)V9(2) VALUE ZERO COMP-3.         
004531        05  PSUM-PROC-EJAVBOK   PIC S9(7)V9(1) VALUE ZERO COMP-3.         
004532        05  PSUM-KVFYSAVV       PIC S9(9)      VALUE ZERO COMP-3.         
004533        05  PSUM-PROC-KVFYSAVV  PIC S9(7)V9(1) VALUE ZERO COMP-3.         
004534        05  PSUM-OMSHAST        PIC S9(7)V9(1) VALUE ZERO COMP-3.         
004535        05  PSUM-PROC-OMSHAST   PIC S9(7)V9(1) VALUE ZERO COMP-3.         
004536        05  PSUM-TACKTID        PIC S9(9)V9(1) VALUE ZERO COMP-3.         
004537        05  PSUM-PROC-TACKTID   PIC S9(7)V9(1) VALUE ZERO COMP-3.         
004538        05  PSUM-SERVG-BTO      PIC S9(7)V9(1) VALUE ZERO COMP-3.         
004539        05  PSUM-SERVG-NTO      PIC S9(7)V9(1) VALUE ZERO COMP-3.         
004540*******  ARBETSFÄLT                                                       
004541        05  WS-PSUM-ARSFORB      PIC S9(11)V9(2) VALUE ZERO.              
004542        05  WS-PSUM-ARSPROG      PIC S9(11)V9(2) VALUE ZERO.              
004543        05  WS-PSUM-SLAGER       PIC S9(11)V9(2) VALUE ZERO.              
004544        05  WS-PSUM-OLAGER       PIC S9(11)V9(2) VALUE ZERO.              
004545        05  WS-PSUM-AVVIK        PIC S9(11)V9(2) VALUE ZERO.              
004546        05  WS-PSUM-KVLS         PIC S9(11)      VALUE ZERO.              
004547        05  WS-PSUM-KVRESS       PIC S9(11)      VALUE ZERO.              
004548        05  WS-PSUM-KVOKS        PIC S9(11)      VALUE ZERO.              
004549        05  WS-PSUM-KVOI         PIC S9(11)V9(2) VALUE ZERO.              
004550        05  WS-PSUM-KVDISP-SNITT PIC S9(11)V9(2) VALUE ZERO.              
004551        05  WS-PSUM-KVDISP       PIC S9(11)      VALUE ZERO.              
004552        05  WS-PSUM-KVDISP-PR    PIC S9(11)V9(2) VALUE ZERO.              
004553        05  WS-PSUM-KVINORD      PIC S9(11)      VALUE ZERO.              
004554        05  WS-PSUM-KVAVBRAD     PIC 9(11)V9(2)  VALUE ZERO.              
004558        05  WS-PSUM-KVFYSAVV     PIC 9(11)V9(2)  VALUE ZERO.              
004559        05  WS-PSUM-EJAVBOK     PIC S9(9)V9(2) VALUE ZERO COMP-3.         
004562     EJECT                                                                
004563 01  FSUM-TABELL.                                                         
004564     03 FSUM-RAD OCCURS 10.                                               
004565        05  FSUM-KVANT          PIC S9(9)      VALUE ZERO COMP-3.         
004566        05  FSUM-PROC-KVANT     PIC S9(7)V9(1) VALUE ZERO COMP-3.         
004567        05  FSUM-ARSFORB        PIC S9(9)      VALUE ZERO COMP-3.         
004568        05  FSUM-PROC-ARSFORB   PIC S9(7)V9(1) VALUE ZERO COMP-3.         
004569        05  FSUM-ARSPROG        PIC S9(9)      VALUE ZERO COMP-3.         
004570        05  FSUM-PROC-ARSPROG   PIC S9(7)V9(1) VALUE ZERO COMP-3.         
004571        05  FSUM-SLAGER         PIC S9(9)      VALUE ZERO COMP-3.         
004572        05  FSUM-PROC-SLAGER    PIC S9(7)V9(1) VALUE ZERO COMP-3.         
004573        05  FSUM-OLAGER         PIC S9(9)      VALUE ZERO COMP-3.         
004574        05  FSUM-PROC-OLAGER    PIC S9(7)V9(1) VALUE ZERO COMP-3.         
004575        05  FSUM-AVVIK          PIC S9(9)      VALUE ZERO COMP-3.         
004576        05  FSUM-PROC-AVVIK     PIC S9(7)V9(1) VALUE ZERO COMP-3.         
004577        05  FSUM-KVINORD        PIC S9(9)      VALUE ZERO COMP-3.         
004578        05  FSUM-PROC-KVINORD   PIC S9(7)V9(1) VALUE ZERO COMP-3.         
004579        05  FSUM-EJAVBOK        PIC S9(9)V9(2) VALUE ZERO COMP-3.         
004580        05  FSUM-PROC-EJAVBOK   PIC S9(7)V9(1) VALUE ZERO COMP-3.         
004581        05  FSUM-KVFYSAVV       PIC S9(9)      VALUE ZERO COMP-3.         
004582        05  FSUM-PROC-KVFYSAVV  PIC S9(7)V9(1) VALUE ZERO COMP-3.         
004583        05  FSUM-OMSHAST        PIC S9(7)V9(1) VALUE ZERO COMP-3.         
004584        05  FSUM-PROC-OMSHAST   PIC S9(7)V9(1) VALUE ZERO COMP-3.         
004585        05  FSUM-TACKTID        PIC S9(9)V9(1) VALUE ZERO COMP-3.         
004586        05  FSUM-PROC-TACKTID   PIC S9(7)V9(1) VALUE ZERO COMP-3.         
004587        05  FSUM-SERVG-BTO      PIC S9(7)V9(1) VALUE ZERO COMP-3.         
004588        05  FSUM-SERVG-NTO      PIC S9(7)V9(1) VALUE ZERO COMP-3.         
004589*******  ARBETSFÄLT                                                       
004590        05  WS-FSUM-SLAGER       PIC S9(11)V9(2) VALUE ZERO.              
004591        05  WS-FSUM-OLAGER       PIC S9(11)V9(2) VALUE ZERO.              
004592        05  WS-FSUM-AVVIK        PIC S9(11)V9(2) VALUE ZERO.              
004593        05  WS-FSUM-KVLS         PIC S9(11)      VALUE ZERO.              
004594        05  WS-FSUM-KVRESS       PIC S9(11)      VALUE ZERO.              
004595        05  WS-FSUM-KVOKS        PIC S9(11)      VALUE ZERO.              
004596        05  WS-FSUM-KVOI         PIC S9(11)V9(2) VALUE ZERO.              
004597        05  WS-FSUM-KVDISP-SNITT PIC S9(11)V9(2) VALUE ZERO.              
004598        05  WS-FSUM-ARSPROG      PIC S9(11)V9(2) VALUE ZERO.              
004599        05  WS-FSUM-ARSFORB      PIC S9(11)V9(2) VALUE ZERO.              
004600        05  WS-FSUM-KVDISP       PIC S9(11)      VALUE ZERO.              
004601        05  WS-FSUM-KVDISP-PR    PIC S9(11)V9(2) VALUE ZERO.              
004602        05  WS-FSUM-KVINORD      PIC S9(9)       VALUE ZERO.              
004603        05  WS-FSUM-KVAVBRAD     PIC 9(9)V9(2)   VALUE ZERO.              
004607        05  WS-FSUM-KVFYSAVV     PIC 9(9)V9(2)  VALUE ZERO.               
004608        05  WS-FSUM-EJAVBOK     PIC S9(9)V9(2) VALUE ZERO COMP-3.         
004611     EJECT                                                                
004612 01  TOTAL-RUTA.                                                          
004613     03  TOT-KVANT              PIC S9(9)      VALUE ZERO COMP-3.         
004614     03  TOT-ARSFORB            PIC S9(9)      VALUE ZERO COMP-3.         
004615     03  TOT-ARSPROG            PIC S9(9)      VALUE ZERO COMP-3.         
004616     03  TOT-SLAGER             PIC S9(9)      VALUE ZERO COMP-3.         
004617     03  TOT-PROC-SLAGER        PIC S9(7)V9(1) VALUE ZERO COMP-3.         
004618     03  TOT-OLAGER             PIC S9(9)      VALUE ZERO COMP-3.         
004619     03  TOT-PROC-OLAGER        PIC S9(7)V9(1) VALUE ZERO COMP-3.         
004620     03  TOT-AVVIK              PIC S9(9)      VALUE ZERO COMP-3.         
004621     03  TOT-PROC-AVVIK         PIC S9(7)V9(1) VALUE ZERO COMP-3.         
004622     03  TOT-KVINORD            PIC S9(9)      VALUE ZERO COMP-3.         
004623     03  TOT-EJAVBOK            PIC S9(7)V9(2) VALUE ZERO COMP-3.         
004624     03  TOT-KVFYSAVV           PIC S9(7)      VALUE ZERO COMP-3.         
004625     03  TOT-OMSHAST            PIC S9(7)V9(1) VALUE ZERO COMP-3.         
004626     03  TOT-TACKTID            PIC S9(9)V9(1) VALUE ZERO COMP-3.         
004627     03  TOT-SERVG-BTO          PIC S9(7)V9(1) VALUE ZERO COMP-3.         
004628     03  TOT-SERVG-NTO          PIC S9(7)V9(1) VALUE ZERO COMP-3.         
004629*******  ARBETSFÄLT                                                       
004630     03  WS-TOT-ARSFORB      PIC S9(11)V9(2) VALUE ZERO.                  
004631     03  WS-TOT-SLAGER       PIC S9(11)V9(2) VALUE ZERO.                  
004632     03  WS-TOT-OLAGER       PIC S9(11)V9(2) VALUE ZERO.                  
004633     03  WS-TOT-AVVIK        PIC S9(11)V9(2) VALUE ZERO.                  
004634     03  WS-TOT-KVLS         PIC S9(11)      VALUE ZERO.                  
004635     03  WS-TOT-KVRESS       PIC S9(11)      VALUE ZERO.                  
004636     03  WS-TOT-KVOKS        PIC S9(11)      VALUE ZERO.                  
004637     03  WS-TOT-KVOI         PIC S9(11)V9(2) VALUE ZERO.                  
004638     03  WS-TOT-KVDISP-SNITT PIC S9(11)V9(2) VALUE ZERO.                  
004639     03  WS-TOT-ARSPROG      PIC S9(11)V9(2) VALUE ZERO.                  
004640     03  WS-TOT-KVDISP       PIC S9(11)      VALUE ZERO.                  
004641     03  WS-TOT-KVDISP-PR    PIC S9(11)V9(2) VALUE ZERO.                  
004642     03  WS-TOT-KVINORD      PIC S9(11)      VALUE ZERO.                  
004643     03  WS-TOT-KVAVBRAD     PIC 9(9)V9(2)   VALUE ZERO.                  
004644     03  WS-TOT-KVFYSAVV     PIC 9(9)V9(2)  VALUE ZERO.                   
004645     03  WS-TOT-EJAVBOK         PIC S9(7)V9(2) VALUE ZERO COMP-3.         
004648     EJECT                                                                
004649 01  DYNAMISKA-SUBPROGRAM.                                                
004650*                                                                         
004660     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
004670     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
004671     03  DATKORT                 PIC X(8)    VALUE 'DATKORT'.             
004672     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
004673     03  W009VADD                PIC X(8)    VALUE 'W009VADD'.            
004680                                                                          
004690     EJECT                                                                
004700 01  PARAM-TILL-DATKORT.                                                  
004710     03  PROG-ID                 PIC X(8)    VALUE 'W2318100'.            
004720     03  KORT-ID                 PIC X(6)    VALUE 'WDATUM'.              
004900*03  -COPY WDATKORT                                                       
005000     EJECT                                                                
005021*03  -COPY WDATAREA                                                       
005022     EJECT                                                                
005023*    --- PARAMETRAR TILL POSTSUM                                          
005024*                                                                         
005030*01  -COPY W0005   -PRE  POSTSUM-                                         
005040     EJECT                                                                
005100 01  IN-AREA-START               PIC X(24)   VALUE                        
005200                                 'IN-AREA-START    '.                     
005300*01  AREA  -COPY W231781A   -PRE IN-                                      
005400     EJECT                                                                
005500 01  PARM-AREA-START             PIC X(24)   VALUE                        
005600                                 'PARM-AREA-START  '.                     
005700 01  PARM-AREA                   PIC X(25).                               
005710 01  FILLER REDEFINES PARM-AREA.                                          
005800     03  PARM-IDUSER             PIC X(8).                                
005900     03  PARM-KATEGORI-FOM       PIC X.                                   
006000     03  PARM-KATEGORI-TOM       PIC X.                                   
006100     03  PARM-KDPRODSL-FOM       PIC X(2).                                
006200     03  PARM-KDPRODSL-TOM       PIC X(2).                                
006300     03  PARM-IDLEVNR            PIC X(5).                                
006500     03  PARM-IDANSK-FOM         PIC X(3).                                
006600     03  PARM-IDANSK-TOM         PIC X(3).                                
006700 01  FILLER REDEFINES PARM-AREA.                                          
006800     03  PARM-IN-IDUSER             PIC X(8).                             
006810     03  PARM-IN-KATEGORI-FOM       PIC 9.                                
006811     03  PARM-IN-KATEGORI-TOM       PIC 9.                                
006812     03  PARM-IN-KDPRODSL-FOM       PIC 9(2).                             
006813     03  PARM-IN-KDPRODSL-TOM       PIC 9(2).                             
006815     03  PARM-IN-IDLEVNR            PIC X(5).                             
006816     03  PARM-IN-IDANSK-FOM         PIC 9(3).                             
006817     03  PARM-IN-IDANSK-TOM         PIC 9(3).                             
006818     EJECT                                                                
006819 01  W001-AREA-START             PIC X(24)   VALUE                        
006820                                 'W001-AREA-START  '.                     
006830     SKIP2                                                                
006840 01  W001-HJALPAREOR.                                                     
006850*                                                                         
006860     03  W001-SKIP               PIC 9(3) COMP-3  VALUE 1.                
006870     03  W001-ANTAL-RADER                                                 
006880                                 PIC 9(3)    VALUE 999.                   
006890     03  W001-MAX-RADER-PER-SIDA                                          
006900                                 PIC 9(3)    VALUE 60.                    
007000     03  W001-MAX-POSITIONER-PER-RAD                                      
007100                                 PIC 9(3)    VALUE 177.                   
007110     03  W001-LISTNR             PIC X(11)   VALUE 'W23181-001'.          
007111     03  W001-SIDRAKNARE         PIC S9(5)   COMP-3 VALUE ZERO.           
007112     SKIP2                                                                
007113 01  W001-RAD.                                                            
007114     03  FILLER                  PIC X(176)  VALUE SPACE.                 
007115     EJECT                                                                
007116 01  W001-RUBRIK1.                                                        
007117*                                                                         
007118     03  FILLER                  PIC X(3)    VALUE SPACE.                 
007120     03  FILLER                  PIC X(18)                                
007121                              VALUE 'VOLVO CAR PARTS   '.                 
007122     03  FILLER                  PIC X(12)                                
007123                                 VALUE 'W23181-001'.                      
007124     03  FILLER                  PIC X(20)                                
007125             VALUE 'SORTIMENTANALYS    '.                                 
007126     03  FILLER                  PIC X(6)    VALUE 'VECKA '.              
007127     03  W001-AKTUELL-VECKA      PIC 9(4)    VALUE ZERO.                  
007128     03  FILLER                  PIC X(6)    VALUE SPACE.                 
007129     03  W001-USER               PIC X(5)    VALUE SPACE.                 
007130     03  FILLER                  PIC X       VALUE SPACE.                 
007131     03  W001-IDUSER             PIC X(8)    VALUE SPACE.                 
007132     03  FILLER                  PIC X(31)   VALUE SPACE.                 
007133     03  W001-DATUM              PIC XXBXXBXX.                            
007134     03  FILLER                  PIC X(3)    VALUE SPACE.                 
007135     03  FILLER                  PIC X(4)    VALUE 'SID '.                
007136     03  W001-SID                PIC Z(4)9.                               
007137     EJECT                                                                
007138 01  W001-RUBRIK2.                                                        
007139*                                                                         
007140     03  FILLER                  PIC X(02)   VALUE SPACE.                 
007141     03  HORIZTAB                PIC X       VALUE X'05'.                 
007142     03  FILLER                  PIC X(14)                                
007143             VALUE 'KATEGORI FOM  '.                                      
007144     03  HORIZTAB                PIC X       VALUE X'05'.                 
007145     03  W001-KAT-FOM            PIC X       VALUE SPACE.                 
007146     03  HORIZTAB                PIC X       VALUE X'05'.                 
007148     03  FILLER                  PIC X(4)    VALUE 'TOM '.                
007149     03  HORIZTAB                PIC X       VALUE X'05'.                 
007150     03  W001-KAT-TOM            PIC X       VALUE SPACE.                 
007151     03  HORIZTAB                PIC X       VALUE X'05'.                 
007153     03  FILLER                  PIC X(12)                                
007154             VALUE 'PRODSL FOM  '.                                        
007155     03  HORIZTAB                PIC X       VALUE X'05'.                 
007156     03  W001-KDPRODSL-FOM       PIC 9(2)    VALUE ZERO.                  
007157     03  HORIZTAB                PIC X       VALUE X'05'.                 
007159     03  FILLER                  PIC X(4)    VALUE 'TOM '.                
007160     03  HORIZTAB                PIC X       VALUE X'05'.                 
007161     03  W001-KDPRODSL-TOM       PIC 9(2)    VALUE ZERO.                  
007162     03  HORIZTAB                PIC X       VALUE X'05'.                 
007164     03  FILLER                  PIC X(7)                                 
007165             VALUE 'LEVNR  '.                                             
007166     03  HORIZTAB                PIC X       VALUE X'05'.                 
007167     03  W001-IDLEVNR            PIC X(5)    VALUE SPACE.                 
007168     03  HORIZTAB                PIC X       VALUE X'05'.                 
007170     03  FILLER                  PIC X(12)                                
007171             VALUE 'ANSKNR FOM  '.                                        
007172     03  HORIZTAB                PIC X       VALUE X'05'.                 
007173     03  W001-IDANSK-FOM         PIC 9(3)    VALUE ZERO.                  
007174     03  HORIZTAB                PIC X       VALUE X'05'.                 
007176     03  FILLER                  PIC X(4)    VALUE 'TOM '.                
007177     03  HORIZTAB                PIC X       VALUE X'05'.                 
007178     03  W001-IDANSK-TOM         PIC 9(3)    VALUE ZERO.                  
007179     03  HORIZTAB                PIC X       VALUE X'05'.                 
007180     EJECT                                                                
007181 01  W001-RUBRIK3.                                                        
007182*                                                                         
007183     03  FILLER                  PIC X(2)    VALUE SPACE.                 
007184     03  HORIZTAB                PIC X       VALUE X'05'.                 
007185     03  FILLER                  PIC X(9)                                 
007186                             VALUE 'PRISKLASS'.                           
007187     03  HORIZTAB                PIC X       VALUE X'05'.                 
007188     03  FILLER                  PIC X(7)    VALUE 'A (-9)'.              
007189     03  HORIZTAB                PIC X       VALUE X'05'.                 
007190     03  FILLER                  PIC X       VALUE SPACE.                 
007191     03  HORIZTAB                PIC X       VALUE X'05'.                 
007192     03  FILLER                  PIC X(8)    VALUE 'B (-26)'.             
007193     03  HORIZTAB                PIC X       VALUE X'05'.                 
007194     03  FILLER                  PIC X       VALUE SPACE.                 
007195     03  HORIZTAB                PIC X       VALUE X'05'.                 
007196     03  FILLER                  PIC X(8)    VALUE 'C (-87)'.             
007197     03  HORIZTAB                PIC X       VALUE X'05'.                 
007198     03  FILLER                  PIC X       VALUE SPACE.                 
007199     03  HORIZTAB                PIC X       VALUE X'05'.                 
007200     03  FILLER                  PIC X(9)    VALUE 'D (-260)'.            
007201     03  HORIZTAB                PIC X       VALUE X'05'.                 
007202     03  FILLER                  PIC X       VALUE SPACE.                 
007203     03  HORIZTAB                PIC X       VALUE X'05'.                 
007204     03  FILLER                  PIC X(9)    VALUE 'E (-867)'.            
007205     03  HORIZTAB                PIC X       VALUE X'05'.                 
007206     03  FILLER                  PIC X       VALUE SPACE.                 
007207     03  HORIZTAB                PIC X       VALUE X'05'.                 
007208     03  FILLER                  PIC X(10)   VALUE 'F (-2600)'.           
007209     03  HORIZTAB                PIC X       VALUE X'05'.                 
007210     03  FILLER                  PIC X       VALUE SPACE.                 
007211     03  HORIZTAB                PIC X       VALUE X'05'.                 
007212     03  FILLER                  PIC X(10)   VALUE 'G (-8700)'.           
007213     03  HORIZTAB                PIC X       VALUE X'05'.                 
007214     03  FILLER                  PIC X       VALUE SPACE.                 
007215     03  HORIZTAB                PIC X       VALUE X'05'.                 
007216     03  FILLER                  PIC X(10)   VALUE 'H(-27000)'.           
007217     03  HORIZTAB                PIC X       VALUE X'05'.                 
007218     03  FILLER                  PIC X       VALUE SPACE.                 
007219     03  HORIZTAB                PIC X       VALUE X'05'.                 
007220     03  FILLER                  PIC X(10)   VALUE 'I(-87000)'.           
007221     03  HORIZTAB                PIC X       VALUE X'05'.                 
007222     03  FILLER                  PIC X       VALUE SPACE.                 
007223     03  HORIZTAB                PIC X       VALUE X'05'.                 
007224     03  FILLER                  PIC X(10)   VALUE 'J(87000-)'.           
007225     03  HORIZTAB                PIC X       VALUE X'05'.                 
007226     03  FILLER                  PIC X       VALUE SPACE.                 
007227     03  HORIZTAB                PIC X       VALUE X'05'.                 
007228     03  FILLER                  PIC X(11)   VALUE 'TOTALT'.              
007229     03  HORIZTAB                PIC X       VALUE X'05'.                 
007230     03  FILLER                  PIC X       VALUE SPACE.                 
007231     03  HORIZTAB                PIC X       VALUE X'05'.                 
007232     EJECT                                                                
007233 01  W001-DETALJRAD-1.                                                    
007234     03  FILLER                  PIC X       VALUE SPACE.                 
007235     03  W001-DET1-PRISKLASS     PIC X       VALUE SPACE.                 
007236     03  HORIZTAB                PIC X       VALUE X'05'.                 
007237     03  FILLER                  PIC X(12)   VALUE 'ANT ARTIKLAR'.        
007238     03  HORIZTAB                PIC X       VALUE X'05'.                 
007239     03  W001-DET1-KVANTA        PIC Z(7)9.                               
007240     03  HORIZTAB                PIC X       VALUE X'05'.                 
007241     03  W001-DET1-P-KVANTA      PIC Z9V,9.                               
007242     03  HORIZTAB                PIC X       VALUE X'05'.                 
007243     03  W001-DET1-KVANTB        PIC Z(7)9.                               
007244     03  HORIZTAB                PIC X       VALUE X'05'.                 
007245     03  W001-DET1-P-KVANTB      PIC Z9V,9.                               
007246     03  HORIZTAB                PIC X       VALUE X'05'.                 
007247     03  W001-DET1-KVANTC        PIC Z(7)9.                               
007248     03  HORIZTAB                PIC X       VALUE X'05'.                 
007249     03  W001-DET1-P-KVANTC      PIC Z9V,9.                               
007250     03  HORIZTAB                PIC X       VALUE X'05'.                 
007251     03  W001-DET1-KVANTD        PIC Z(7)9.                               
007252     03  HORIZTAB                PIC X       VALUE X'05'.                 
007253     03  W001-DET1-P-KVANTD      PIC Z9V,9.                               
007254     03  HORIZTAB                PIC X       VALUE X'05'.                 
007255     03  W001-DET1-KVANTE        PIC Z(7)9.                               
007256     03  HORIZTAB                PIC X       VALUE X'05'.                 
007257     03  W001-DET1-P-KVANTE      PIC Z9V,9.                               
007258     03  HORIZTAB                PIC X       VALUE X'05'.                 
007259     03  W001-DET1-KVANTF        PIC Z(7)9.                               
007260     03  HORIZTAB                PIC X       VALUE X'05'.                 
007261     03  W001-DET1-P-KVANTF      PIC Z9V,9.                               
007262     03  HORIZTAB                PIC X       VALUE X'05'.                 
007263     03  W001-DET1-KVANTG        PIC Z(7)9.                               
007264     03  HORIZTAB                PIC X       VALUE X'05'.                 
007265     03  W001-DET1-P-KVANTG      PIC Z9V,9.                               
007266     03  HORIZTAB                PIC X       VALUE X'05'.                 
007267     03  W001-DET1-KVANTH        PIC Z(7)9.                               
007268     03  HORIZTAB                PIC X       VALUE X'05'.                 
007269     03  W001-DET1-P-KVANTH      PIC Z9V,9.                               
007270     03  HORIZTAB                PIC X       VALUE X'05'.                 
007271     03  W001-DET1-KVANTI        PIC Z(7)9.                               
007272     03  HORIZTAB                PIC X       VALUE X'05'.                 
007273     03  W001-DET1-P-KVANTI      PIC Z9V,9.                               
007274     03  HORIZTAB                PIC X       VALUE X'05'.                 
007275     03  W001-DET1-KVANTJ        PIC Z(7)9.                               
007276     03  HORIZTAB                PIC X       VALUE X'05'.                 
007277     03  W001-DET1-P-KVANTJ      PIC Z9V,9.                               
007278     03  HORIZTAB                PIC X       VALUE X'05'.                 
007279     03  W001-DET1-TOT           PIC Z(13)9.                              
007280     03  HORIZTAB                PIC X       VALUE X'05'.                 
007281     03  W001-DET1-P-TOT         PIC ZZ9V,9  BLANK WHEN ZERO.             
007282     03  HORIZTAB                PIC X       VALUE X'05'.                 
007283     EJECT                                                                
007284 01  W001-DETALJRAD-2.                                                    
007285     03  FILLER                  PIC XX      VALUE SPACE.                 
007286     03  HORIZTAB                PIC X       VALUE X'05'.                 
007287     03  FILLER                  PIC X(12)   VALUE 'ARSFORBRUKN '.        
007288     03  HORIZTAB                PIC X       VALUE X'05'.                 
007289     03  W001-DET2-ARSFORBA      PIC Z(7)9.                               
007290     03  HORIZTAB                PIC X       VALUE X'05'.                 
007291     03  W001-DET2-P-ARSFORBA    PIC Z9V,9.                               
007292     03  HORIZTAB                PIC X       VALUE X'05'.                 
007293     03  W001-DET2-ARSFORBB      PIC Z(7)9.                               
007294     03  HORIZTAB                PIC X       VALUE X'05'.                 
007295     03  W001-DET2-P-ARSFORBB    PIC Z9V,9.                               
007296     03  HORIZTAB                PIC X       VALUE X'05'.                 
007297     03  W001-DET2-ARSFORBC      PIC Z(7)9.                               
007298     03  HORIZTAB                PIC X       VALUE X'05'.                 
007299     03  W001-DET2-P-ARSFORBC    PIC Z9V,9.                               
007300     03  HORIZTAB                PIC X       VALUE X'05'.                 
007301     03  W001-DET2-ARSFORBD      PIC Z(7)9.                               
007302     03  HORIZTAB                PIC X       VALUE X'05'.                 
007303     03  W001-DET2-P-ARSFORBD    PIC Z9V,9.                               
007304     03  HORIZTAB                PIC X       VALUE X'05'.                 
007305     03  W001-DET2-ARSFORBE      PIC Z(7)9.                               
007306     03  HORIZTAB                PIC X       VALUE X'05'.                 
007307     03  W001-DET2-P-ARSFORBE    PIC Z9V,9.                               
007308     03  HORIZTAB                PIC X       VALUE X'05'.                 
007309     03  W001-DET2-ARSFORBF      PIC Z(7)9.                               
007310     03  HORIZTAB                PIC X       VALUE X'05'.                 
007311     03  W001-DET2-P-ARSFORBF    PIC Z9V,9.                               
007312     03  HORIZTAB                PIC X       VALUE X'05'.                 
007313     03  W001-DET2-ARSFORBG      PIC Z(7)9.                               
007314     03  HORIZTAB                PIC X       VALUE X'05'.                 
007315     03  W001-DET2-P-ARSFORBG    PIC Z9V,9.                               
007316     03  HORIZTAB                PIC X       VALUE X'05'.                 
007317     03  W001-DET2-ARSFORBH      PIC Z(7)9.                               
007318     03  HORIZTAB                PIC X       VALUE X'05'.                 
007319     03  W001-DET2-P-ARSFORBH    PIC Z9V,9.                               
007320     03  HORIZTAB                PIC X       VALUE X'05'.                 
007321     03  W001-DET2-ARSFORBI      PIC Z(7)9.                               
007322     03  HORIZTAB                PIC X       VALUE X'05'.                 
007323     03  W001-DET2-P-ARSFORBI    PIC Z9V,9.                               
007324     03  HORIZTAB                PIC X       VALUE X'05'.                 
007325     03  W001-DET2-ARSFORBJ      PIC Z(7)9.                               
007326     03  HORIZTAB                PIC X       VALUE X'05'.                 
007327     03  W001-DET2-P-ARSFORBJ    PIC Z9V,9.                               
007328     03  HORIZTAB                PIC X       VALUE X'05'.                 
007329     03  W001-DET2-TOT           PIC Z(13)9.                              
007330     03  HORIZTAB                PIC X       VALUE X'05'.                 
007331     03  W001-DET2-P-TOT         PIC ZZ9V,9  BLANK WHEN ZERO.             
007332     03  HORIZTAB                PIC X       VALUE X'05'.                 
007333     EJECT                                                                
007334 01  W001-DETALJRAD-3.                                                    
007335     03  FILLER                  PIC XX      VALUE SPACE.                 
007336     03  HORIZTAB                PIC X       VALUE X'05'.                 
007337     03  FILLER                  PIC X(12)   VALUE 'ARSPROGNOS  '.        
007338     03  HORIZTAB                PIC X       VALUE X'05'.                 
007339     03  W001-DET3-ARSPROGA      PIC Z(7)9.                               
007340     03  HORIZTAB                PIC X       VALUE X'05'.                 
007341     03  W001-DET3-P-ARSPROGA    PIC Z9V,9.                               
007342     03  HORIZTAB                PIC X       VALUE X'05'.                 
007343     03  W001-DET3-ARSPROGB      PIC Z(7)9.                               
007344     03  HORIZTAB                PIC X       VALUE X'05'.                 
007345     03  W001-DET3-P-ARSPROGB    PIC Z9V,9.                               
007346     03  HORIZTAB                PIC X       VALUE X'05'.                 
007347     03  W001-DET3-ARSPROGC      PIC Z(7)9.                               
007348     03  HORIZTAB                PIC X       VALUE X'05'.                 
007349     03  W001-DET3-P-ARSPROGC    PIC Z9V,9.                               
007350     03  HORIZTAB                PIC X       VALUE X'05'.                 
007351     03  W001-DET3-ARSPROGD      PIC Z(7)9.                               
007352     03  HORIZTAB                PIC X       VALUE X'05'.                 
007353     03  W001-DET3-P-ARSPROGD    PIC Z9V,9.                               
007354     03  HORIZTAB                PIC X       VALUE X'05'.                 
007355     03  W001-DET3-ARSPROGE      PIC Z(7)9.                               
007356     03  HORIZTAB                PIC X       VALUE X'05'.                 
007357     03  W001-DET3-P-ARSPROGE    PIC Z9V,9.                               
007358     03  HORIZTAB                PIC X       VALUE X'05'.                 
007359     03  W001-DET3-ARSPROGF      PIC Z(7)9.                               
007360     03  HORIZTAB                PIC X       VALUE X'05'.                 
007361     03  W001-DET3-P-ARSPROGF    PIC Z9V,9.                               
007362     03  HORIZTAB                PIC X       VALUE X'05'.                 
007363     03  W001-DET3-ARSPROGG      PIC Z(7)9.                               
007364     03  HORIZTAB                PIC X       VALUE X'05'.                 
007365     03  W001-DET3-P-ARSPROGG    PIC Z9V,9.                               
007366     03  HORIZTAB                PIC X       VALUE X'05'.                 
007367     03  W001-DET3-ARSPROGH      PIC Z(7)9.                               
007368     03  HORIZTAB                PIC X       VALUE X'05'.                 
007369     03  W001-DET3-P-ARSPROGH    PIC Z9V,9.                               
007370     03  HORIZTAB                PIC X       VALUE X'05'.                 
007371     03  W001-DET3-ARSPROGI      PIC Z(7)9.                               
007372     03  HORIZTAB                PIC X       VALUE X'05'.                 
007373     03  W001-DET3-P-ARSPROGI    PIC Z9V,9.                               
007374     03  HORIZTAB                PIC X       VALUE X'05'.                 
007375     03  W001-DET3-ARSPROGJ      PIC Z(7)9.                               
007376     03  HORIZTAB                PIC X       VALUE X'05'.                 
007377     03  W001-DET3-P-ARSPROGJ    PIC Z9V,9.                               
007378     03  HORIZTAB                PIC X       VALUE X'05'.                 
007379     03  W001-DET3-TOT           PIC Z(13)9.                              
007380     03  HORIZTAB                PIC X       VALUE X'05'.                 
007381     03  W001-DET3-P-TOT         PIC ZZ9V,9  BLANK WHEN ZERO.             
007382     03  HORIZTAB                PIC X       VALUE X'05'.                 
007383     EJECT                                                                
007384 01  W001-DETALJRAD-4.                                                    
007385     03  FILLER                  PIC XX      VALUE SPACE.                 
007386     03  HORIZTAB                PIC X       VALUE X'05'.                 
007387     03  FILLER                  PIC X(12)   VALUE 'SAK-LAGER   '.        
007388     03  HORIZTAB                PIC X       VALUE X'05'.                 
007389     03  W001-DET4-SLAGERA       PIC Z(7)9.                               
007390     03  HORIZTAB                PIC X       VALUE X'05'.                 
007391     03  W001-DET4-P-SLAGERA     PIC Z9V,9.                               
007392     03  HORIZTAB                PIC X       VALUE X'05'.                 
007393     03  W001-DET4-SLAGERB       PIC Z(7)9.                               
007394     03  HORIZTAB                PIC X       VALUE X'05'.                 
007395     03  W001-DET4-P-SLAGERB     PIC Z9V,9.                               
007396     03  HORIZTAB                PIC X       VALUE X'05'.                 
007397     03  W001-DET4-SLAGERC       PIC Z(7)9.                               
007398     03  HORIZTAB                PIC X       VALUE X'05'.                 
007399     03  W001-DET4-P-SLAGERC     PIC Z9V,9.                               
007400     03  HORIZTAB                PIC X       VALUE X'05'.                 
007401     03  W001-DET4-SLAGERD       PIC Z(7)9.                               
007402     03  HORIZTAB                PIC X       VALUE X'05'.                 
007403     03  W001-DET4-P-SLAGERD     PIC Z9V,9.                               
007404     03  HORIZTAB                PIC X       VALUE X'05'.                 
007405     03  W001-DET4-SLAGERE       PIC Z(7)9.                               
007406     03  HORIZTAB                PIC X       VALUE X'05'.                 
007407     03  W001-DET4-P-SLAGERE     PIC Z9V,9.                               
007408     03  HORIZTAB                PIC X       VALUE X'05'.                 
007409     03  W001-DET4-SLAGERF       PIC Z(7)9.                               
007410     03  HORIZTAB                PIC X       VALUE X'05'.                 
007411     03  W001-DET4-P-SLAGERF     PIC Z9V,9.                               
007412     03  HORIZTAB                PIC X       VALUE X'05'.                 
007413     03  W001-DET4-SLAGERG       PIC Z(7)9.                               
007414     03  HORIZTAB                PIC X       VALUE X'05'.                 
007415     03  W001-DET4-P-SLAGERG     PIC Z9V,9.                               
007416     03  HORIZTAB                PIC X       VALUE X'05'.                 
007417     03  W001-DET4-SLAGERH       PIC Z(7)9.                               
007418     03  HORIZTAB                PIC X       VALUE X'05'.                 
007419     03  W001-DET4-P-SLAGERH     PIC Z9V,9.                               
007420     03  HORIZTAB                PIC X       VALUE X'05'.                 
007421     03  W001-DET4-SLAGERI       PIC Z(7)9.                               
007422     03  HORIZTAB                PIC X       VALUE X'05'.                 
007423     03  W001-DET4-P-SLAGERI     PIC Z9V,9.                               
007424     03  HORIZTAB                PIC X       VALUE X'05'.                 
007425     03  W001-DET4-SLAGERJ       PIC Z(7)9.                               
007426     03  HORIZTAB                PIC X       VALUE X'05'.                 
007427     03  W001-DET4-P-SLAGERJ     PIC Z9V,9.                               
007428     03  HORIZTAB                PIC X       VALUE X'05'.                 
007429     03  W001-DET4-TOT           PIC Z(13)9.                              
007430     03  HORIZTAB                PIC X       VALUE X'05'.                 
007431     03  W001-DET4-P-TOT         PIC ZZ9V,9  BLANK WHEN ZERO.             
007432     03  HORIZTAB                PIC X       VALUE X'05'.                 
007433     EJECT                                                                
007434 01  W001-DETALJRAD-5.                                                    
007435     03  FILLER                  PIC XX      VALUE SPACE.                 
007436     03  HORIZTAB                PIC X       VALUE X'05'.                 
007437     03  FILLER                  PIC X(12)   VALUE 'OMS-LAGER   '.        
007438     03  HORIZTAB                PIC X       VALUE X'05'.                 
007439     03  W001-DET5-OLAGERA       PIC Z(7)9.                               
007440     03  HORIZTAB                PIC X       VALUE X'05'.                 
007441     03  W001-DET5-P-OLAGERA     PIC Z9V,9.                               
007442     03  HORIZTAB                PIC X       VALUE X'05'.                 
007443     03  W001-DET5-OLAGERB       PIC Z(7)9.                               
007444     03  HORIZTAB                PIC X       VALUE X'05'.                 
007445     03  W001-DET5-P-OLAGERB     PIC Z9V,9.                               
007446     03  HORIZTAB                PIC X       VALUE X'05'.                 
007447     03  W001-DET5-OLAGERC       PIC Z(7)9.                               
007448     03  HORIZTAB                PIC X       VALUE X'05'.                 
007449     03  W001-DET5-P-OLAGERC     PIC Z9V,9.                               
007450     03  HORIZTAB                PIC X       VALUE X'05'.                 
007451     03  W001-DET5-OLAGERD       PIC Z(7)9.                               
007452     03  HORIZTAB                PIC X       VALUE X'05'.                 
007453     03  W001-DET5-P-OLAGERD     PIC Z9V,9.                               
007454     03  HORIZTAB                PIC X       VALUE X'05'.                 
007455     03  W001-DET5-OLAGERE       PIC Z(7)9.                               
007456     03  HORIZTAB                PIC X       VALUE X'05'.                 
007457     03  W001-DET5-P-OLAGERE     PIC Z9V,9.                               
007458     03  HORIZTAB                PIC X       VALUE X'05'.                 
007459     03  W001-DET5-OLAGERF       PIC Z(7)9.                               
007460     03  HORIZTAB                PIC X       VALUE X'05'.                 
007461     03  W001-DET5-P-OLAGERF     PIC Z9V,9.                               
007462     03  HORIZTAB                PIC X       VALUE X'05'.                 
007463     03  W001-DET5-OLAGERG       PIC Z(7)9.                               
007464     03  HORIZTAB                PIC X       VALUE X'05'.                 
007465     03  W001-DET5-P-OLAGERG     PIC Z9V,9.                               
007466     03  HORIZTAB                PIC X       VALUE X'05'.                 
007467     03  W001-DET5-OLAGERH       PIC Z(7)9.                               
007468     03  HORIZTAB                PIC X       VALUE X'05'.                 
007469     03  W001-DET5-P-OLAGERH     PIC Z9V,9.                               
007470     03  HORIZTAB                PIC X       VALUE X'05'.                 
007471     03  W001-DET5-OLAGERI       PIC Z(7)9.                               
007472     03  HORIZTAB                PIC X       VALUE X'05'.                 
007473     03  W001-DET5-P-OLAGERI     PIC Z9V,9.                               
007474     03  HORIZTAB                PIC X       VALUE X'05'.                 
007475     03  W001-DET5-OLAGERJ       PIC Z(7)9.                               
007476     03  HORIZTAB                PIC X       VALUE X'05'.                 
007477     03  W001-DET5-P-OLAGERJ     PIC Z9V,9.                               
007478     03  HORIZTAB                PIC X       VALUE X'05'.                 
007479     03  W001-DET5-TOT           PIC Z(13)9.                              
007480     03  HORIZTAB                PIC X       VALUE X'05'.                 
007481     03  W001-DET5-P-TOT         PIC ZZ9V,9  BLANK WHEN ZERO.             
007482     03  HORIZTAB                PIC X       VALUE X'05'.                 
007483     EJECT                                                                
007484 01  W001-DETALJRAD-6.                                                    
007485     03  FILLER                  PIC XX      VALUE SPACE.                 
007486     03  HORIZTAB                PIC X       VALUE X'05'.                 
007487     03  FILLER                  PIC X(12)   VALUE 'AVVIK LAGER '.        
007488     03  HORIZTAB                PIC X       VALUE X'05'.                 
007489     03  W001-DET6-AVVIKA        PIC -(7)9.                               
007490     03  HORIZTAB                PIC X       VALUE X'05'.                 
007491     03  W001-DET6-P-AVVIKA      PIC Z9V,9.                               
007492     03  HORIZTAB                PIC X       VALUE X'05'.                 
007493     03  W001-DET6-AVVIKB        PIC -(7)9.                               
007494     03  HORIZTAB                PIC X       VALUE X'05'.                 
007495     03  W001-DET6-P-AVVIKB      PIC Z9V,9.                               
007496     03  HORIZTAB                PIC X       VALUE X'05'.                 
007497     03  W001-DET6-AVVIKC        PIC -(7)9.                               
007498     03  HORIZTAB                PIC X       VALUE X'05'.                 
007499     03  W001-DET6-P-AVVIKC      PIC Z9V,9.                               
007500     03  HORIZTAB                PIC X       VALUE X'05'.                 
007501     03  W001-DET6-AVVIKD        PIC -(7)9.                               
007502     03  HORIZTAB                PIC X       VALUE X'05'.                 
007503     03  W001-DET6-P-AVVIKD      PIC Z9V,9.                               
007504     03  HORIZTAB                PIC X       VALUE X'05'.                 
007505     03  W001-DET6-AVVIKE        PIC -(7)9.                               
007506     03  HORIZTAB                PIC X       VALUE X'05'.                 
007507     03  W001-DET6-P-AVVIKE      PIC Z9V,9.                               
007508     03  HORIZTAB                PIC X       VALUE X'05'.                 
007509     03  W001-DET6-AVVIKF        PIC -(7)9.                               
007510     03  HORIZTAB                PIC X       VALUE X'05'.                 
007511     03  W001-DET6-P-AVVIKF      PIC Z9V,9.                               
007512     03  HORIZTAB                PIC X       VALUE X'05'.                 
007513     03  W001-DET6-AVVIKG        PIC -(7)9.                               
007514     03  HORIZTAB                PIC X       VALUE X'05'.                 
007515     03  W001-DET6-P-AVVIKG      PIC Z9V,9.                               
007516     03  HORIZTAB                PIC X       VALUE X'05'.                 
007517     03  W001-DET6-AVVIKH        PIC -(7)9.                               
007518     03  HORIZTAB                PIC X       VALUE X'05'.                 
007519     03  W001-DET6-P-AVVIKH      PIC Z9V,9.                               
007520     03  HORIZTAB                PIC X       VALUE X'05'.                 
007521     03  W001-DET6-AVVIKI        PIC -(7)9.                               
007522     03  HORIZTAB                PIC X       VALUE X'05'.                 
007523     03  W001-DET6-P-AVVIKI      PIC Z9V,9.                               
007524     03  HORIZTAB                PIC X       VALUE X'05'.                 
007525     03  W001-DET6-AVVIKJ        PIC -(7)9.                               
007526     03  HORIZTAB                PIC X       VALUE X'05'.                 
007527     03  W001-DET6-P-AVVIKJ      PIC Z9V,9.                               
007528     03  HORIZTAB                PIC X       VALUE X'05'.                 
007529     03  W001-DET6-TOT           PIC -(13)9.                              
007530     03  HORIZTAB                PIC X       VALUE X'05'.                 
007531     03  W001-DET6-P-TOT         PIC ZZ9V,9  BLANK WHEN ZERO.             
007532     03  HORIZTAB                PIC X       VALUE X'05'.                 
007533     EJECT                                                                
007534 01  W001-DETALJRAD-7.                                                    
007535     03  FILLER                  PIC XX      VALUE SPACE.                 
007536     03  HORIZTAB                PIC X       VALUE X'05'.                 
007537     03  FILLER                  PIC X(12)   VALUE 'ANT ORDERRAD'.        
007538     03  HORIZTAB                PIC X       VALUE X'05'.                 
007539     03  W001-DET7-KVINORDA      PIC Z(7)9.                               
007540     03  HORIZTAB                PIC X       VALUE X'05'.                 
007541     03  W001-DET7-P-KVINORDA    PIC Z9V,9.                               
007542     03  HORIZTAB                PIC X       VALUE X'05'.                 
007543     03  W001-DET7-KVINORDB      PIC Z(7)9.                               
007544     03  HORIZTAB                PIC X       VALUE X'05'.                 
007545     03  W001-DET7-P-KVINORDB    PIC Z9V,9.                               
007546     03  HORIZTAB                PIC X       VALUE X'05'.                 
007547     03  W001-DET7-KVINORDC      PIC Z(7)9.                               
007548     03  HORIZTAB                PIC X       VALUE X'05'.                 
007549     03  W001-DET7-P-KVINORDC    PIC Z9V,9.                               
007550     03  HORIZTAB                PIC X       VALUE X'05'.                 
007551     03  W001-DET7-KVINORDD      PIC Z(7)9.                               
007552     03  HORIZTAB                PIC X       VALUE X'05'.                 
007553     03  W001-DET7-P-KVINORDD    PIC Z9V,9.                               
007554     03  HORIZTAB                PIC X       VALUE X'05'.                 
007555     03  W001-DET7-KVINORDE      PIC Z(7)9.                               
007556     03  HORIZTAB                PIC X       VALUE X'05'.                 
007557     03  W001-DET7-P-KVINORDE    PIC Z9V,9.                               
007558     03  HORIZTAB                PIC X       VALUE X'05'.                 
007559     03  W001-DET7-KVINORDF      PIC Z(7)9.                               
007560     03  HORIZTAB                PIC X       VALUE X'05'.                 
007561     03  W001-DET7-P-KVINORDF    PIC Z9V,9.                               
007562     03  HORIZTAB                PIC X       VALUE X'05'.                 
007563     03  W001-DET7-KVINORDG      PIC Z(7)9.                               
007564     03  HORIZTAB                PIC X       VALUE X'05'.                 
007565     03  W001-DET7-P-KVINORDG    PIC Z9V,9.                               
007566     03  HORIZTAB                PIC X       VALUE X'05'.                 
007567     03  W001-DET7-KVINORDH      PIC Z(7)9.                               
007568     03  HORIZTAB                PIC X       VALUE X'05'.                 
007569     03  W001-DET7-P-KVINORDH    PIC Z9V,9.                               
007570     03  HORIZTAB                PIC X       VALUE X'05'.                 
007571     03  W001-DET7-KVINORDI      PIC Z(7)9.                               
007572     03  HORIZTAB                PIC X       VALUE X'05'.                 
007573     03  W001-DET7-P-KVINORDI    PIC Z9V,9.                               
007574     03  HORIZTAB                PIC X       VALUE X'05'.                 
007575     03  W001-DET7-KVINORDJ      PIC Z(7)9.                               
007576     03  HORIZTAB                PIC X       VALUE X'05'.                 
007577     03  W001-DET7-P-KVINORDJ    PIC Z9V,9.                               
007578     03  HORIZTAB                PIC X       VALUE X'05'.                 
007579     03  W001-DET7-TOT           PIC Z(13)9.                              
007580     03  HORIZTAB                PIC X       VALUE X'05'.                 
007581     03  W001-DET7-P-TOT         PIC ZZ9V,9  BLANK WHEN ZERO.             
007582     03  HORIZTAB                PIC X       VALUE X'05'.                 
007583     EJECT                                                                
007584 01  W001-DETALJRAD-8.                                                    
007585     03  FILLER                  PIC XX      VALUE SPACE.                 
007586     03  HORIZTAB                PIC X       VALUE X'05'.                 
007587     03  FILLER                  PIC X(14)   VALUE 'ANT EJ AVBOK'.        
007588     03  HORIZTAB                PIC X       VALUE X'05'.                 
007589     03  W001-DET8-EJAVBOKA      PIC Z(5)9.                               
007590     03  HORIZTAB                PIC X       VALUE X'05'.                 
007591     03  W001-DET8-P-EJAVBOKA    PIC Z9V,9.                               
007592     03  HORIZTAB                PIC X       VALUE X'05'.                 
007593     03  W001-DET8-EJAVBOKB      PIC Z(5)9.                               
007594     03  HORIZTAB                PIC X       VALUE X'05'.                 
007595     03  W001-DET8-P-EJAVBOKB    PIC Z9V,9.                               
007596     03  HORIZTAB                PIC X       VALUE X'05'.                 
007597     03  W001-DET8-EJAVBOKC      PIC Z(5)9.                               
007598     03  HORIZTAB                PIC X       VALUE X'05'.                 
007599     03  W001-DET8-P-EJAVBOKC    PIC Z9V,9.                               
007600     03  HORIZTAB                PIC X       VALUE X'05'.                 
007601     03  W001-DET8-EJAVBOKD      PIC Z(5)9.                               
007602     03  HORIZTAB                PIC X       VALUE X'05'.                 
007603     03  W001-DET8-P-EJAVBOKD    PIC Z9V,9.                               
007604     03  HORIZTAB                PIC X       VALUE X'05'.                 
007605     03  W001-DET8-EJAVBOKE      PIC Z(5)9.                               
007606     03  HORIZTAB                PIC X       VALUE X'05'.                 
007607     03  W001-DET8-P-EJAVBOKE    PIC Z9V,9.                               
007608     03  HORIZTAB                PIC X       VALUE X'05'.                 
007609     03  W001-DET8-EJAVBOKF      PIC Z(5)9.                               
007610     03  HORIZTAB                PIC X       VALUE X'05'.                 
007611     03  W001-DET8-P-EJAVBOKF    PIC Z9V,9.                               
007612     03  HORIZTAB                PIC X       VALUE X'05'.                 
007613     03  W001-DET8-EJAVBOKG      PIC Z(5)9.                               
007614     03  HORIZTAB                PIC X       VALUE X'05'.                 
007615     03  W001-DET8-P-EJAVBOKG    PIC Z9V,9.                               
007616     03  HORIZTAB                PIC X       VALUE X'05'.                 
007617     03  W001-DET8-EJAVBOKH      PIC Z(5)9.                               
007618     03  HORIZTAB                PIC X       VALUE X'05'.                 
007619     03  W001-DET8-P-EJAVBOKH    PIC Z9V,9.                               
007620     03  HORIZTAB                PIC X       VALUE X'05'.                 
007621     03  W001-DET8-EJAVBOKI      PIC Z(5)9.                               
007622     03  HORIZTAB                PIC X       VALUE X'05'.                 
007623     03  W001-DET8-P-EJAVBOKI    PIC Z9V,9.                               
007624     03  HORIZTAB                PIC X       VALUE X'05'.                 
007625     03  W001-DET8-EJAVBOKJ      PIC Z(5)9.                               
007626     03  HORIZTAB                PIC X       VALUE X'05'.                 
007627     03  W001-DET8-P-EJAVBOKJ    PIC Z9V,9.                               
007628     03  HORIZTAB                PIC X       VALUE X'05'.                 
007629     03  W001-DET8-TOT           PIC Z(11)9.                              
007630     03  HORIZTAB                PIC X       VALUE X'05'.                 
007631     03  W001-DET8-P-TOT         PIC ZZ9V,9  BLANK WHEN ZERO.             
007632     03  HORIZTAB                PIC X       VALUE X'05'.                 
007633     EJECT                                                                
007634 01  W001-DETALJRAD-9.                                                    
007635     03  FILLER                  PIC XX      VALUE SPACE.                 
007636     03  HORIZTAB                PIC X       VALUE X'05'.                 
007637     03  FILLER                  PIC X(12)   VALUE 'ANT FYS AVV '.        
007638     03  HORIZTAB                PIC X       VALUE X'05'.                 
007639     03  W001-DET9-KVFYSAVVA     PIC Z(5)9.                               
007640     03  HORIZTAB                PIC X       VALUE X'05'.                 
007641     03  W001-DET9-P-KVFYSAVVA   PIC Z9V,9.                               
007642     03  HORIZTAB                PIC X       VALUE X'05'.                 
007643     03  W001-DET9-KVFYSAVVB     PIC Z(5)9.                               
007644     03  HORIZTAB                PIC X       VALUE X'05'.                 
007645     03  W001-DET9-P-KVFYSAVVB   PIC Z9V,9.                               
007646     03  HORIZTAB                PIC X       VALUE X'05'.                 
007647     03  W001-DET9-KVFYSAVVC     PIC Z(5)9.                               
007648     03  HORIZTAB                PIC X       VALUE X'05'.                 
007649     03  W001-DET9-P-KVFYSAVVC   PIC Z9V,9.                               
007650     03  HORIZTAB                PIC X       VALUE X'05'.                 
007651     03  W001-DET9-KVFYSAVVD     PIC Z(5)9.                               
007652     03  HORIZTAB                PIC X       VALUE X'05'.                 
007653     03  W001-DET9-P-KVFYSAVVD   PIC Z9V,9.                               
007654     03  HORIZTAB                PIC X       VALUE X'05'.                 
007655     03  W001-DET9-KVFYSAVVE     PIC Z(5)9.                               
007656     03  HORIZTAB                PIC X       VALUE X'05'.                 
007657     03  W001-DET9-P-KVFYSAVVE   PIC Z9V,9.                               
007658     03  HORIZTAB                PIC X       VALUE X'05'.                 
007659     03  W001-DET9-KVFYSAVVF     PIC Z(5)9.                               
007660     03  HORIZTAB                PIC X       VALUE X'05'.                 
007661     03  W001-DET9-P-KVFYSAVVF   PIC Z9V,9.                               
007662     03  HORIZTAB                PIC X       VALUE X'05'.                 
007663     03  W001-DET9-KVFYSAVVG     PIC Z(5)9.                               
007664     03  HORIZTAB                PIC X       VALUE X'05'.                 
007665     03  W001-DET9-P-KVFYSAVVG   PIC Z9V,9.                               
007666     03  HORIZTAB                PIC X       VALUE X'05'.                 
007667     03  W001-DET9-KVFYSAVVH     PIC Z(5)9.                               
007668     03  HORIZTAB                PIC X       VALUE X'05'.                 
007669     03  W001-DET9-P-KVFYSAVVH   PIC Z9V,9.                               
007670     03  HORIZTAB                PIC X       VALUE X'05'.                 
007671     03  W001-DET9-KVFYSAVVI     PIC Z(5)9.                               
007672     03  HORIZTAB                PIC X       VALUE X'05'.                 
007673     03  W001-DET9-P-KVFYSAVVI   PIC Z9V,9.                               
007674     03  HORIZTAB                PIC X       VALUE X'05'.                 
007675     03  W001-DET9-KVFYSAVVJ     PIC Z(5)9.                               
007676     03  HORIZTAB                PIC X       VALUE X'05'.                 
007677     03  W001-DET9-P-KVFYSAVVJ   PIC Z9V,9.                               
007678     03  HORIZTAB                PIC X       VALUE X'05'.                 
007679     03  W001-DET9-TOT           PIC Z(11)9.                              
007680     03  HORIZTAB                PIC X       VALUE X'05'.                 
007681     03  W001-DET9-P-TOT         PIC ZZ9V,9  BLANK WHEN ZERO.             
007682     03  HORIZTAB                PIC X       VALUE X'05'.                 
007683     EJECT                                                                
007684 01  W001-DETALJRAD-10.                                                   
007685     03  FILLER                  PIC XX      VALUE SPACE.                 
007686     03  HORIZTAB                PIC X       VALUE X'05'.                 
007687     03  FILLER                  PIC X(12)   VALUE 'OMSHASTIGHET'.        
007688     03  HORIZTAB                PIC X       VALUE X'05'.                 
007689     03  W001-DET10-OMSHASTA     PIC Z(5)9V,9.                            
007690     03  HORIZTAB                PIC X       VALUE X'05'.                 
007691     03  W001-DET10-P-OMSHASTA   PIC Z9V,9.                               
007692     03  HORIZTAB                PIC X       VALUE X'05'.                 
007693     03  W001-DET10-OMSHASTB     PIC Z(5)9V,9.                            
007694     03  HORIZTAB                PIC X       VALUE X'05'.                 
007695     03  W001-DET10-P-OMSHASTB   PIC Z9V,9.                               
007696     03  HORIZTAB                PIC X       VALUE X'05'.                 
007697     03  W001-DET10-OMSHASTC     PIC Z(5)9V,9.                            
007698     03  HORIZTAB                PIC X       VALUE X'05'.                 
007699     03  W001-DET10-P-OMSHASTC   PIC Z9V,9.                               
007700     03  HORIZTAB                PIC X       VALUE X'05'.                 
007701     03  W001-DET10-OMSHASTD     PIC Z(5)9V,9.                            
007702     03  HORIZTAB                PIC X       VALUE X'05'.                 
007703     03  W001-DET10-P-OMSHASTD   PIC Z9V,9.                               
007704     03  HORIZTAB                PIC X       VALUE X'05'.                 
007705     03  W001-DET10-OMSHASTE     PIC Z(5)9V,9.                            
007706     03  HORIZTAB                PIC X       VALUE X'05'.                 
007707     03  W001-DET10-P-OMSHASTE   PIC Z9V,9.                               
007708     03  HORIZTAB                PIC X       VALUE X'05'.                 
007709     03  W001-DET10-OMSHASTF     PIC Z(5)9V,9.                            
007710     03  HORIZTAB                PIC X       VALUE X'05'.                 
007711     03  W001-DET10-P-OMSHASTF   PIC Z9V,9.                               
007712     03  HORIZTAB                PIC X       VALUE X'05'.                 
007713     03  W001-DET10-OMSHASTG     PIC Z(5)9V,9.                            
007714     03  HORIZTAB                PIC X       VALUE X'05'.                 
007715     03  W001-DET10-P-OMSHASTG   PIC Z9V,9.                               
007716     03  HORIZTAB                PIC X       VALUE X'05'.                 
007717     03  W001-DET10-OMSHASTH     PIC Z(5)9V,9.                            
007718     03  HORIZTAB                PIC X       VALUE X'05'.                 
007719     03  W001-DET10-P-OMSHASTH   PIC Z9V,9.                               
007720     03  HORIZTAB                PIC X       VALUE X'05'.                 
007721     03  W001-DET10-OMSHASTI     PIC Z(5)9V,9.                            
007722     03  HORIZTAB                PIC X       VALUE X'05'.                 
007723     03  W001-DET10-P-OMSHASTI   PIC Z9V,9.                               
007724     03  HORIZTAB                PIC X       VALUE X'05'.                 
007725     03  W001-DET10-OMSHASTJ     PIC Z(5)9V,9.                            
007726     03  HORIZTAB                PIC X       VALUE X'05'.                 
007727     03  W001-DET10-P-OMSHASTJ   PIC Z9V,9.                               
007728     03  HORIZTAB                PIC X       VALUE X'05'.                 
007729     03  W001-DET10-TOT          PIC Z(11)9V,9.                           
007730     03  HORIZTAB                PIC X       VALUE X'05'.                 
007731     03  W001-DET10-P-TOT        PIC ZZ9V,9  BLANK WHEN ZERO.             
007732     03  HORIZTAB                PIC X       VALUE X'05'.                 
007733     EJECT                                                                
007734 01  W001-DETALJRAD-11.                                                   
007735     03  FILLER                  PIC XX      VALUE SPACE.                 
007736     03  HORIZTAB                PIC X       VALUE X'05'.                 
007737     03  FILLER                  PIC X(12)   VALUE 'TACKTID     '.        
007738     03  HORIZTAB                PIC X       VALUE X'05'.                 
007739     03  W001-DET11-TACKTIDA     PIC Z(5)9V,9.                            
007740     03  HORIZTAB                PIC X       VALUE X'05'.                 
007741     03  W001-DET11-P-TACKTIDA   PIC Z9V,9   BLANK WHEN ZERO.             
007742     03  HORIZTAB                PIC X       VALUE X'05'.                 
007743     03  W001-DET11-TACKTIDB     PIC Z(5)9V,9.                            
007744     03  HORIZTAB                PIC X       VALUE X'05'.                 
007745     03  W001-DET11-P-TACKTIDB   PIC Z9V,9   BLANK WHEN ZERO.             
007746     03  HORIZTAB                PIC X       VALUE X'05'.                 
007747     03  W001-DET11-TACKTIDC     PIC Z(5)9V,9.                            
007748     03  HORIZTAB                PIC X       VALUE X'05'.                 
007749     03  W001-DET11-P-TACKTIDC   PIC Z9V,9   BLANK WHEN ZERO.             
007750     03  HORIZTAB                PIC X       VALUE X'05'.                 
007751     03  W001-DET11-TACKTIDD     PIC Z(5)9V,9.                            
007752     03  HORIZTAB                PIC X       VALUE X'05'.                 
007753     03  W001-DET11-P-TACKTIDD   PIC Z9V,9   BLANK WHEN ZERO.             
007754     03  HORIZTAB                PIC X       VALUE X'05'.                 
007755     03  W001-DET11-TACKTIDE     PIC Z(5)9V,9.                            
007756     03  HORIZTAB                PIC X       VALUE X'05'.                 
007757     03  W001-DET11-P-TACKTIDE   PIC Z9V,9   BLANK WHEN ZERO.             
007758     03  HORIZTAB                PIC X       VALUE X'05'.                 
007759     03  W001-DET11-TACKTIDF     PIC Z(5)9V,9.                            
007760     03  HORIZTAB                PIC X       VALUE X'05'.                 
007761     03  W001-DET11-P-TACKTIDF   PIC Z9V,9   BLANK WHEN ZERO.             
007762     03  HORIZTAB                PIC X       VALUE X'05'.                 
007763     03  W001-DET11-TACKTIDG     PIC Z(5)9V,9.                            
007764     03  HORIZTAB                PIC X       VALUE X'05'.                 
007765     03  W001-DET11-P-TACKTIDG   PIC Z9V,9   BLANK WHEN ZERO.             
007766     03  HORIZTAB                PIC X       VALUE X'05'.                 
007767     03  W001-DET11-TACKTIDH     PIC Z(5)9V,9.                            
007768     03  HORIZTAB                PIC X       VALUE X'05'.                 
007769     03  W001-DET11-P-TACKTIDH   PIC Z9V,9   BLANK WHEN ZERO.             
007770     03  HORIZTAB                PIC X       VALUE X'05'.                 
007771     03  W001-DET11-TACKTIDI     PIC Z(5)9V,9.                            
007772     03  HORIZTAB                PIC X       VALUE X'05'.                 
007773     03  W001-DET11-P-TACKTIDI   PIC Z9V,9   BLANK WHEN ZERO.             
007774     03  HORIZTAB                PIC X       VALUE X'05'.                 
007775     03  W001-DET11-TACKTIDJ     PIC Z(5)9V,9.                            
007776     03  HORIZTAB                PIC X       VALUE X'05'.                 
007777     03  W001-DET11-P-TACKTIDJ   PIC Z9V,9   BLANK WHEN ZERO.             
007778     03  HORIZTAB                PIC X       VALUE X'05'.                 
007779     03  W001-DET11-TOT          PIC Z(11)9V,9.                           
007780     03  HORIZTAB                PIC X       VALUE X'05'.                 
007781     03  W001-DET11-P-TOT        PIC ZZ9V,9  BLANK WHEN ZERO.             
007782     03  HORIZTAB                PIC X       VALUE X'05'.                 
007783     EJECT                                                                
007784 01  W001-DETALJRAD-12.                                                   
007785     03  FILLER                  PIC XX      VALUE SPACE.                 
007786     03  HORIZTAB                PIC X       VALUE X'05'.                 
007787     03  FILLER                  PIC X(12)   VALUE 'SERVICEGRAD '.        
007788     03  HORIZTAB                PIC X       VALUE X'05'.                 
007789     03  W001-DET12-SERVGA-NTO   PIC Z9V,9.                               
007790     03  HORIZTAB                PIC X       VALUE X'05'.                 
007791     03  W001-DET12-SERVGA-BTO   PIC Z9V,9.                               
007792     03  HORIZTAB                PIC X       VALUE X'05'.                 
007793     03  W001-DET12-SERVGB-NTO   PIC Z9V,9.                               
007794     03  HORIZTAB                PIC X       VALUE X'05'.                 
007795     03  W001-DET12-SERVGB-BTO   PIC Z9V,9.                               
007796     03  HORIZTAB                PIC X       VALUE X'05'.                 
007797     03  W001-DET12-SERVGC-NTO   PIC Z9V,9.                               
007798     03  HORIZTAB                PIC X       VALUE X'05'.                 
007799     03  W001-DET12-SERVGC-BTO   PIC Z9V,9.                               
007800     03  HORIZTAB                PIC X       VALUE X'05'.                 
007801     03  W001-DET12-SERVGD-NTO   PIC Z9V,9.                               
007802     03  HORIZTAB                PIC X       VALUE X'05'.                 
007803     03  W001-DET12-SERVGD-BTO   PIC Z9V,9.                               
007804     03  HORIZTAB                PIC X       VALUE X'05'.                 
007805     03  W001-DET12-SERVGE-NTO   PIC Z9V,9.                               
007806     03  HORIZTAB                PIC X       VALUE X'05'.                 
007807     03  W001-DET12-SERVGE-BTO   PIC Z9V,9.                               
007808     03  HORIZTAB                PIC X       VALUE X'05'.                 
007809     03  W001-DET12-SERVGF-NTO   PIC Z9V,9.                               
007810     03  HORIZTAB                PIC X       VALUE X'05'.                 
007811     03  W001-DET12-SERVGF-BTO   PIC Z9V,9.                               
007812     03  HORIZTAB                PIC X       VALUE X'05'.                 
007813     03  W001-DET12-SERVGG-NTO   PIC Z9V,9.                               
007814     03  HORIZTAB                PIC X       VALUE X'05'.                 
007815     03  W001-DET12-SERVGG-BTO   PIC Z9V,9.                               
007816     03  HORIZTAB                PIC X       VALUE X'05'.                 
007817     03  W001-DET12-SERVGH-NTO   PIC Z9V,9.                               
007818     03  HORIZTAB                PIC X       VALUE X'05'.                 
007819     03  W001-DET12-SERVGH-BTO   PIC Z9V,9.                               
007820     03  HORIZTAB                PIC X       VALUE X'05'.                 
007821     03  W001-DET12-SERVGI-NTO   PIC Z9V,9.                               
007822     03  HORIZTAB                PIC X       VALUE X'05'.                 
007823     03  W001-DET12-SERVGI-BTO   PIC Z9V,9.                               
007824     03  HORIZTAB                PIC X       VALUE X'05'.                 
007825     03  W001-DET12-SERVGJ-NTO   PIC Z9V,9.                               
007826     03  HORIZTAB                PIC X       VALUE X'05'.                 
007827     03  W001-DET12-SERVGJ-BTO   PIC Z9V,9.                               
007828     03  HORIZTAB                PIC X       VALUE X'05'.                 
007829     03  W001-DET12-TOT          PIC Z9V,9.                               
007830     03  HORIZTAB                PIC X       VALUE X'05'.                 
007831     03  W001-DET12-P-TOT        PIC Z(2)9V,9 BLANK WHEN ZERO.            
007832     03  HORIZTAB                PIC X       VALUE X'05'.                 
007833     EJECT                                                                
007834 PROCEDURE DIVISION.                                                      
007835                                                                          
007836     PERFORM A-INIT                                                       
007837     PERFORM B-SKAPA-LISTA                                                
007838     IF SW-LISTAOK = JA                                                   
007839        PERFORM C-SKRIV-LISTA                                             
007840     END-IF                                                               
007841     PERFORM Z-FINIT                                                      
007842                                                                          
007843     MOVE ZERO TO RETURN-CODE                                             
007844     GOBACK                                                               
007845     .                                                                    
007846     EJECT                                                                
007847 A-INIT SECTION.                                                          
007848                                                                          
007849     OPEN INPUT  W23178                                                   
007850                 W231PP                                                   
007851     OPEN OUTPUT W23181-001                                               
007852                                                                          
007860     CALL DATKORT USING PROG-ID KORT-ID DATUMKORT                         
007900     MOVE D-AAR    TO DAGENS-AAR                                          
007901                      D-VECKA-AAR                                         
007910     MOVE D-MAANAD TO DAGENS-MAANAD                                       
007911     MOVE D-VECKA  TO D-VECKA-VECKA                                       
007920     MOVE D-DAG    TO DAGENS-DAG                                          
007930     ACCEPT WS-DAGENS-DATUM FROM DATE                                     
007940                                                                          
007950     MOVE 'IDAG' TO DAT-KDDATFORM                                         
007960     CALL WDATKONV USING DAT-KDDATFORM                                    
007970                         DAT-I-TIDATUM                                    
007980                         DAT-O-TIDATUM                                    
007990                         DAT-KDSVAR                                       
008000     IF DAT-KDSVAR-OK                                                     
008010        MOVE DAT-TIAAVVD TO AAVVD                                         
008011        MOVE AAVV        TO W009VADD-DATUM                                
008012        MOVE -1          TO W009VADD-ANTAL                                
008013     ELSE                                                                 
008014        MOVE ZERO        TO W009VADD-DATUM                                
008015     END-IF                                                               
008020     CALL W009VADD USING W009VADD-DATUM W009VADD-ANTAL                    
008040                                                                          
008100     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
008200                                                                          
008300********* URVAL FRÅN BILD 2324 LÄSES IN                                   
008400                                                                          
008500     PERFORM S02-LAS-W231PP                                               
008600     .                                                                    
008700     EJECT                                                                
008800 B-SKAPA-LISTA SECTION.                                                   
008900                                                                          
009000     PERFORM BA-NOLLSTALL                                                 
009100     PERFORM S01-LAS-W23178                                               
009200     PERFORM UNTIL END-OF-W23178                                          
009300       IF PARM-AREA = ZERO                                                
009400          MOVE JA TO SW-TRAEFF                                            
009500                     SW-TOTALLISTA                                        
009600       ELSE                                                               
009700          PERFORM BD-KOLLA-URVAL                                          
009800       END-IF                                                             
009900       IF SW-TRAEFF = JA                                                  
010000          PERFORM BB-SKAPA-TABELLER                                       
010010          MOVE JA TO SW-LISTAOK                                           
010100       END-IF                                                             
010200       PERFORM S01-LAS-W23178                                             
010300     END-PERFORM                                                          
010310     IF SW-LISTAOK = JA                                                   
010400        PERFORM BC-SUMMERA                                                
010410     END-IF                                                               
010500     .                                                                    
010600     EJECT                                                                
010700 BA-NOLLSTALL SECTION.                                                    
010800******************************************************************        
010900*  LISTAN BESTÅR AV ARTIKELUPPGIFTER PER PRISKLASS OCH           *        
011000*  FREKVENSKLASS                                                 *        
011100*     PRIS-KLASSER   = 1 2 3 4 5 6 7 8 9                         *        
011200*     FREKV-KLASSER  = A B C D E F G H I J                       *        
011300*  SUMMERING GÖRS PER PRISKLASS OBEROENDE AV FREKVENSKLASS       *        
011400*                 PER FREKVENSKLASS OBEROENDE AV PRISKLASS       *        
011500*                 TOTAL-SUMMERING                                *        
011600* ****************************************************************        
011700                                                                          
011800     MOVE NEJ TO SW-TRAEFF                                                
011810                 SW-TOTALLISTA                                            
011811                 SW-LISTAOK                                               
011820                                                                          
011830******* NOLLSTÄLLNING AV 90 'RUTOR' PER PRISKLASS/FREKVKLASS              
011831                                                                          
011832     MOVE +1  TO ART-IX                                                   
011833     MOVE +90 TO ART-IX-MAX                                               
011834     PERFORM UNTIL ART-IX > ART-IX-MAX                                    
011835        MOVE ZERO TO     ART-KVANT(ART-IX)                                
011836                         ART-PROC-KVANT(ART-IX)                           
011837                         ART-ARSFORB(ART-IX)                              
011838                         ART-PROC-ARSFORB(ART-IX)                         
011839                         ART-ARSPROG(ART-IX)                              
011840                         ART-PROC-ARSPROG(ART-IX)                         
011841                         ART-SLAGER(ART-IX)                               
011842                         ART-PROC-SLAGER(ART-IX)                          
011843                         ART-OLAGER(ART-IX)                               
011844                         ART-PROC-OLAGER(ART-IX)                          
011845                         ART-AVVIK(ART-IX)                                
011846                         ART-PROC-AVVIK(ART-IX)                           
011847                         ART-KVINORD(ART-IX)                              
011848                         ART-PROC-KVINORD(ART-IX)                         
011849                         ART-EJAVBOK(ART-IX)                              
011850                         ART-PROC-EJAVBOK(ART-IX)                         
011851                         ART-KVFYSAVV(ART-IX)                             
011852                         ART-PROC-KVFYSAVV(ART-IX)                        
011853                         ART-OMSHAST(ART-IX)                              
011854                         ART-PROC-OMSHAST(ART-IX)                         
011855                         ART-TACKTID(ART-IX)                              
011856                         ART-PROC-TACKTID(ART-IX)                         
011857                         ART-SERVG-BTO(ART-IX)                            
011858                         ART-SERVG-NTO(ART-IX)                            
011859********* ARBETSFÄLT                                                      
011860                         WS-ART-KVLS(ART-IX)                              
011861                         WS-ART-KVRESS(ART-IX)                            
011862                         WS-ART-KVOKS(ART-IX)                             
011863                         WS-ART-KVOI(ART-IX)                              
011864                         WS-ART-KVDISP-SNITT(ART-IX)                      
011865                         WS-ART-KVDISP(ART-IX)                            
011866                         WS-ART-KVDISP-PR(ART-IX)                         
011867                         WS-ART-KVINORD(ART-IX)                           
011868                         WS-ART-KVAVBRAD(ART-IX)                          
011869                         WS-ART-KVFYSAVV(ART-IX)                          
011870                         WS-ART-ARSPROG(ART-IX)                           
011880                         WS-ART-ARSFORB(ART-IX)                           
011881                         WS-ART-SLAGER(ART-IX)                            
011882                         WS-ART-OLAGER(ART-IX)                            
011883                         WS-ART-AVVIK(ART-IX)                             
011884                         WS-ART-EJAVBOK(ART-IX)                           
011885                                                                          
011886        ADD +1 TO ART-IX                                                  
011887     END-PERFORM                                                          
011888                                                                          
011889******* NOLLSTÄLLNING AV 9 'RUTOR' TOTALSUMMA PER PRISKLASS               
011890*******                          OBEROENDE AV FREKVENSKLASS               
011891                                                                          
011892     MOVE +1 TO PSUM-IX                                                   
011893     MOVE +9 TO PSUM-IX-MAX                                               
011894     PERFORM UNTIL PSUM-IX > PSUM-IX-MAX                                  
011895        MOVE ZERO TO     PSUM-KVANT(PSUM-IX)                              
011896                         PSUM-PROC-KVANT(PSUM-IX)                         
011897                         PSUM-ARSFORB(PSUM-IX)                            
011898                         PSUM-PROC-ARSFORB(PSUM-IX)                       
011899                         PSUM-ARSPROG(PSUM-IX)                            
011900                         PSUM-PROC-ARSPROG(PSUM-IX)                       
011901                         PSUM-SLAGER(PSUM-IX)                             
011902                         PSUM-PROC-SLAGER(PSUM-IX)                        
011903                         PSUM-OLAGER(PSUM-IX)                             
011904                         PSUM-PROC-OLAGER(PSUM-IX)                        
011905                         PSUM-AVVIK(PSUM-IX)                              
011906                         PSUM-PROC-AVVIK(PSUM-IX)                         
011907                         PSUM-KVINORD(PSUM-IX)                            
011908                         PSUM-PROC-KVINORD(PSUM-IX)                       
011909                         PSUM-EJAVBOK(PSUM-IX)                            
011910                         PSUM-PROC-EJAVBOK(PSUM-IX)                       
011911                         PSUM-KVFYSAVV(PSUM-IX)                           
011912                         PSUM-PROC-KVFYSAVV(PSUM-IX)                      
011913                         PSUM-OMSHAST(PSUM-IX)                            
011914                         PSUM-PROC-OMSHAST(PSUM-IX)                       
011915                         PSUM-TACKTID(PSUM-IX)                            
011916                         PSUM-PROC-TACKTID(PSUM-IX)                       
011917                         PSUM-SERVG-BTO(PSUM-IX)                          
011918                         PSUM-SERVG-NTO(PSUM-IX)                          
011919*******  ARBETSFÄLT                                                       
011920                         WS-PSUM-KVLS(PSUM-IX)                            
011921                         WS-PSUM-KVRESS(PSUM-IX)                          
011922                         WS-PSUM-KVOKS(PSUM-IX)                           
011923                         WS-PSUM-KVOI(PSUM-IX)                            
011924                         WS-PSUM-KVDISP-SNITT(PSUM-IX)                    
011925                         WS-PSUM-ARSPROG(PSUM-IX)                         
011926                         WS-PSUM-KVDISP(PSUM-IX)                          
011927                         WS-PSUM-KVDISP-PR(PSUM-IX)                       
011928                         WS-PSUM-KVINORD(PSUM-IX)                         
011929                         WS-PSUM-KVAVBRAD(PSUM-IX)                        
011930                         WS-PSUM-KVFYSAVV(PSUM-IX)                        
011931                         WS-PSUM-ARSPROG(PSUM-IX)                         
011932                         WS-PSUM-ARSFORB(PSUM-IX)                         
011933                         WS-PSUM-OLAGER(PSUM-IX)                          
011934                         WS-PSUM-SLAGER(PSUM-IX)                          
011935                         WS-PSUM-AVVIK(PSUM-IX)                           
011936                         WS-PSUM-EJAVBOK(PSUM-IX)                         
011937                                                                          
011938        ADD +1 TO PSUM-IX                                                 
011939     END-PERFORM                                                          
011940                                                                          
011941******* NOLLSTÄLLNING AV 10 'RUTOR' TOTALSUMMA PER FREKVENSKLASS          
011942*******                            OBEROENDE AV PRISKLASS                 
011943                                                                          
011944     MOVE +1  TO FSUM-IX                                                  
011945     MOVE +10 TO FSUM-IX-MAX                                              
011946     PERFORM UNTIL FSUM-IX > FSUM-IX-MAX                                  
011947        MOVE ZERO TO     FSUM-KVANT(FSUM-IX)                              
011948                         FSUM-PROC-KVANT(FSUM-IX)                         
011949                         FSUM-ARSFORB(FSUM-IX)                            
011950                         FSUM-PROC-ARSFORB(FSUM-IX)                       
011951                         FSUM-ARSPROG(FSUM-IX)                            
011952                         FSUM-PROC-ARSPROG(FSUM-IX)                       
011953                         FSUM-SLAGER(FSUM-IX)                             
011954                         FSUM-PROC-SLAGER(FSUM-IX)                        
011955                         FSUM-OLAGER(FSUM-IX)                             
011956                         FSUM-PROC-OLAGER(FSUM-IX)                        
011957                         FSUM-AVVIK(FSUM-IX)                              
011958                         FSUM-PROC-AVVIK(FSUM-IX)                         
011959                         FSUM-KVINORD(FSUM-IX)                            
011960                         FSUM-PROC-KVINORD(FSUM-IX)                       
011961                         FSUM-EJAVBOK(FSUM-IX)                            
011962                         FSUM-PROC-EJAVBOK(FSUM-IX)                       
011963                         FSUM-KVFYSAVV(FSUM-IX)                           
011964                         FSUM-PROC-KVFYSAVV(FSUM-IX)                      
011965                         FSUM-OMSHAST(FSUM-IX)                            
011966                         FSUM-PROC-OMSHAST(FSUM-IX)                       
011967                         FSUM-TACKTID(FSUM-IX)                            
011968                         FSUM-PROC-TACKTID(FSUM-IX)                       
011969                         FSUM-SERVG-BTO(FSUM-IX)                          
011970                         FSUM-SERVG-NTO(FSUM-IX)                          
011971*******  ARBETSFÄLT                                                       
011972                         WS-FSUM-KVLS(FSUM-IX)                            
011973                         WS-FSUM-KVRESS(FSUM-IX)                          
011974                         WS-FSUM-KVOKS(FSUM-IX)                           
011975                         WS-FSUM-KVOI(FSUM-IX)                            
011976                         WS-FSUM-KVDISP-SNITT(FSUM-IX)                    
011977                         WS-FSUM-ARSPROG(FSUM-IX)                         
011978                         WS-FSUM-KVDISP(FSUM-IX)                          
011979                         WS-FSUM-KVDISP-PR(FSUM-IX)                       
011980                         WS-FSUM-KVINORD(FSUM-IX)                         
011981                         WS-FSUM-KVAVBRAD(FSUM-IX)                        
011982                         WS-FSUM-KVFYSAVV(FSUM-IX)                        
011983                         WS-FSUM-ARSFORB(FSUM-IX)                         
011984                         WS-FSUM-ARSPROG(FSUM-IX)                         
011985                         WS-FSUM-SLAGER(FSUM-IX)                          
011986                         WS-FSUM-OLAGER(FSUM-IX)                          
011987                         WS-FSUM-AVVIK(FSUM-IX)                           
011988                         WS-FSUM-EJAVBOK(FSUM-IX)                         
011989                                                                          
011990        ADD +1 TO FSUM-IX                                                 
011991     END-PERFORM                                                          
011992                                                                          
011993******* NOLLSTÄLLNING AV TOTALRUTA                                        
011994                                                                          
011995     MOVE ZERO TO     TOT-KVANT                                           
011996                      TOT-ARSFORB                                         
011997                      TOT-ARSPROG                                         
011998                      TOT-SLAGER                                          
011999                      TOT-PROC-SLAGER                                     
012000                      TOT-OLAGER                                          
012001                      TOT-PROC-OLAGER                                     
012002                      TOT-AVVIK                                           
012003                      TOT-PROC-AVVIK                                      
012004                      TOT-KVINORD                                         
012005                      TOT-EJAVBOK                                         
012006                      TOT-KVFYSAVV                                        
012007                      TOT-OMSHAST                                         
012008                      TOT-TACKTID                                         
012009                      TOT-SERVG-BTO                                       
012010                      TOT-SERVG-NTO                                       
012011*******  ARBETSFÄLT                                                       
012012                      WS-TOT-KVLS                                         
012013                      WS-TOT-KVRESS                                       
012014                      WS-TOT-KVOKS                                        
012015                      WS-TOT-KVOI                                         
012016                      WS-TOT-KVDISP-SNITT                                 
012017                      WS-TOT-ARSPROG                                      
012018                      WS-TOT-KVDISP                                       
012019                      WS-TOT-KVDISP-PR                                    
012020                      WS-TOT-KVINORD                                      
012021                      WS-TOT-KVAVBRAD                                     
012022                      WS-TOT-ARSPROG                                      
012023                      WS-TOT-ARSFORB                                      
012024                      WS-TOT-OLAGER                                       
012025                      WS-TOT-SLAGER                                       
012026                      WS-TOT-AVVIK                                        
012027                      WS-TOT-EJAVBOK                                      
012028     .                                                                    
012029     EJECT                                                                
012030 BB-SKAPA-TABELLER SECTION.                                               
012031                                                                          
012032     PERFORM BBA-SAETT-ART-IX                                             
012033     PERFORM BBB-UPPDATERA-TABELLER                                       
012034     .                                                                    
012035     EJECT                                                                
012036 BBA-SAETT-ART-IX SECTION.                                                
012037******************************************************************        
012038* ART-IX SÄTTS BEROENDE PÅ PRISKLASS OCH FREKVENSKLASS           *        
012039******************************************************************        
012040                                                                          
012041     EVALUATE TRUE                                                        
012042     WHEN IN-KDPRISKL = '1' AND IN-KDFREKKL = 'A'                         
012043          MOVE +1 TO ART-IX                                               
012044     WHEN IN-KDPRISKL = '1' AND IN-KDFREKKL = 'B'                         
012045          MOVE +2 TO ART-IX                                               
012046     WHEN IN-KDPRISKL = '1' AND IN-KDFREKKL = 'C'                         
012047          MOVE +3 TO ART-IX                                               
012048     WHEN IN-KDPRISKL = '1' AND IN-KDFREKKL = 'D'                         
012049          MOVE +4 TO ART-IX                                               
012050     WHEN IN-KDPRISKL = '1' AND IN-KDFREKKL = 'E'                         
012051          MOVE +5 TO ART-IX                                               
012052     WHEN IN-KDPRISKL = '1' AND IN-KDFREKKL = 'F'                         
012053          MOVE +6 TO ART-IX                                               
012054     WHEN IN-KDPRISKL = '1' AND IN-KDFREKKL = 'G'                         
012055          MOVE +7 TO ART-IX                                               
012056     WHEN IN-KDPRISKL = '1' AND IN-KDFREKKL = 'H'                         
012057          MOVE +8 TO ART-IX                                               
012058     WHEN IN-KDPRISKL = '1' AND IN-KDFREKKL = 'I'                         
012059          MOVE +9 TO ART-IX                                               
012060     WHEN IN-KDPRISKL = '1' AND IN-KDFREKKL = 'J'                         
012061          MOVE +10 TO ART-IX                                              
012062                                                                          
012063     WHEN IN-KDPRISKL = '2' AND IN-KDFREKKL = 'A'                         
012064          MOVE +11 TO ART-IX                                              
012065     WHEN IN-KDPRISKL = '2' AND IN-KDFREKKL = 'B'                         
012066          MOVE +12 TO ART-IX                                              
012067     WHEN IN-KDPRISKL = '2' AND IN-KDFREKKL = 'C'                         
012068          MOVE +13 TO ART-IX                                              
012069     WHEN IN-KDPRISKL = '2' AND IN-KDFREKKL = 'D'                         
012070          MOVE +14 TO ART-IX                                              
012071     WHEN IN-KDPRISKL = '2' AND IN-KDFREKKL = 'E'                         
012072          MOVE +15 TO ART-IX                                              
012073     WHEN IN-KDPRISKL = '2' AND IN-KDFREKKL = 'F'                         
012074          MOVE +16 TO ART-IX                                              
012075     WHEN IN-KDPRISKL = '2' AND IN-KDFREKKL = 'G'                         
012076          MOVE +17 TO ART-IX                                              
012077     WHEN IN-KDPRISKL = '2' AND IN-KDFREKKL = 'H'                         
012078          MOVE +18 TO ART-IX                                              
012079     WHEN IN-KDPRISKL = '2' AND IN-KDFREKKL = 'I'                         
012080          MOVE +19 TO ART-IX                                              
012081     WHEN IN-KDPRISKL = '2' AND IN-KDFREKKL = 'J'                         
012082          MOVE +20 TO ART-IX                                              
012083                                                                          
012084     WHEN IN-KDPRISKL = '3' AND IN-KDFREKKL = 'A'                         
012085          MOVE +21 TO ART-IX                                              
012086     WHEN IN-KDPRISKL = '3' AND IN-KDFREKKL = 'B'                         
012087          MOVE +22 TO ART-IX                                              
012088     WHEN IN-KDPRISKL = '3' AND IN-KDFREKKL = 'C'                         
012089          MOVE +23 TO ART-IX                                              
012090     WHEN IN-KDPRISKL = '3' AND IN-KDFREKKL = 'D'                         
012091          MOVE +24 TO ART-IX                                              
012092     WHEN IN-KDPRISKL = '3' AND IN-KDFREKKL = 'E'                         
012093          MOVE +25 TO ART-IX                                              
012094     WHEN IN-KDPRISKL = '3' AND IN-KDFREKKL = 'F'                         
012095          MOVE +26 TO ART-IX                                              
012096     WHEN IN-KDPRISKL = '3' AND IN-KDFREKKL = 'G'                         
012097          MOVE +27 TO ART-IX                                              
012098     WHEN IN-KDPRISKL = '3' AND IN-KDFREKKL = 'H'                         
012099          MOVE +28 TO ART-IX                                              
012100     WHEN IN-KDPRISKL = '3' AND IN-KDFREKKL = 'I'                         
012101          MOVE +29 TO ART-IX                                              
012102     WHEN IN-KDPRISKL = '3' AND IN-KDFREKKL = 'J'                         
012103          MOVE +30 TO ART-IX                                              
012104                                                                          
012105     WHEN IN-KDPRISKL = '4' AND IN-KDFREKKL = 'A'                         
012106          MOVE +31 TO ART-IX                                              
012107     WHEN IN-KDPRISKL = '4' AND IN-KDFREKKL = 'B'                         
012108          MOVE +32 TO ART-IX                                              
012109     WHEN IN-KDPRISKL = '4' AND IN-KDFREKKL = 'C'                         
012110          MOVE +33 TO ART-IX                                              
012111     WHEN IN-KDPRISKL = '4' AND IN-KDFREKKL = 'D'                         
012112          MOVE +34 TO ART-IX                                              
012113     WHEN IN-KDPRISKL = '4' AND IN-KDFREKKL = 'E'                         
012114          MOVE +35 TO ART-IX                                              
012115     WHEN IN-KDPRISKL = '4' AND IN-KDFREKKL = 'F'                         
012116          MOVE +36 TO ART-IX                                              
012117     WHEN IN-KDPRISKL = '4' AND IN-KDFREKKL = 'G'                         
012118          MOVE +37 TO ART-IX                                              
012119     WHEN IN-KDPRISKL = '4' AND IN-KDFREKKL = 'H'                         
012120          MOVE +38 TO ART-IX                                              
012121     WHEN IN-KDPRISKL = '4' AND IN-KDFREKKL = 'I'                         
012122          MOVE +39 TO ART-IX                                              
012123     WHEN IN-KDPRISKL = '4' AND IN-KDFREKKL = 'J'                         
012124          MOVE +40 TO ART-IX                                              
012125                                                                          
012126     WHEN IN-KDPRISKL = '5' AND IN-KDFREKKL = 'A'                         
012127          MOVE +41 TO ART-IX                                              
012128     WHEN IN-KDPRISKL = '5' AND IN-KDFREKKL = 'B'                         
012129          MOVE +42 TO ART-IX                                              
012130     WHEN IN-KDPRISKL = '5' AND IN-KDFREKKL = 'C'                         
012131          MOVE +43 TO ART-IX                                              
012132     WHEN IN-KDPRISKL = '5' AND IN-KDFREKKL = 'D'                         
012133          MOVE +44 TO ART-IX                                              
012134     WHEN IN-KDPRISKL = '5' AND IN-KDFREKKL = 'E'                         
012135          MOVE +45 TO ART-IX                                              
012136     WHEN IN-KDPRISKL = '5' AND IN-KDFREKKL = 'F'                         
012137          MOVE +46 TO ART-IX                                              
012138     WHEN IN-KDPRISKL = '5' AND IN-KDFREKKL = 'G'                         
012139          MOVE +47 TO ART-IX                                              
012140     WHEN IN-KDPRISKL = '5' AND IN-KDFREKKL = 'H'                         
012141          MOVE +48 TO ART-IX                                              
012142     WHEN IN-KDPRISKL = '5' AND IN-KDFREKKL = 'I'                         
012143          MOVE +49 TO ART-IX                                              
012144     WHEN IN-KDPRISKL = '5' AND IN-KDFREKKL = 'J'                         
012145          MOVE +50 TO ART-IX                                              
012146                                                                          
012147     WHEN IN-KDPRISKL = '6' AND IN-KDFREKKL = 'A'                         
012148          MOVE +51 TO ART-IX                                              
012149     WHEN IN-KDPRISKL = '6' AND IN-KDFREKKL = 'B'                         
012150          MOVE +52 TO ART-IX                                              
012151     WHEN IN-KDPRISKL = '6' AND IN-KDFREKKL = 'C'                         
012152          MOVE +53 TO ART-IX                                              
012153     WHEN IN-KDPRISKL = '6' AND IN-KDFREKKL = 'D'                         
012154          MOVE +54 TO ART-IX                                              
012155     WHEN IN-KDPRISKL = '6' AND IN-KDFREKKL = 'E'                         
012156          MOVE +55 TO ART-IX                                              
012157     WHEN IN-KDPRISKL = '6' AND IN-KDFREKKL = 'F'                         
012158          MOVE +56 TO ART-IX                                              
012159     WHEN IN-KDPRISKL = '6' AND IN-KDFREKKL = 'G'                         
012160          MOVE +57 TO ART-IX                                              
012161     WHEN IN-KDPRISKL = '6' AND IN-KDFREKKL = 'H'                         
012162          MOVE +58 TO ART-IX                                              
012163     WHEN IN-KDPRISKL = '6' AND IN-KDFREKKL = 'I'                         
012164          MOVE +59 TO ART-IX                                              
012165     WHEN IN-KDPRISKL = '6' AND IN-KDFREKKL = 'J'                         
012166          MOVE +60 TO ART-IX                                              
012167                                                                          
012168     WHEN IN-KDPRISKL = '7' AND IN-KDFREKKL = 'A'                         
012169          MOVE +61 TO ART-IX                                              
012170     WHEN IN-KDPRISKL = '7' AND IN-KDFREKKL = 'B'                         
012171          MOVE +62 TO ART-IX                                              
012172     WHEN IN-KDPRISKL = '7' AND IN-KDFREKKL = 'C'                         
012173          MOVE +63 TO ART-IX                                              
012174     WHEN IN-KDPRISKL = '7' AND IN-KDFREKKL = 'D'                         
012175          MOVE +64 TO ART-IX                                              
012176     WHEN IN-KDPRISKL = '7' AND IN-KDFREKKL = 'E'                         
012177          MOVE +65 TO ART-IX                                              
012178     WHEN IN-KDPRISKL = '7' AND IN-KDFREKKL = 'F'                         
012179          MOVE +66 TO ART-IX                                              
012180     WHEN IN-KDPRISKL = '7' AND IN-KDFREKKL = 'G'                         
012181          MOVE +67 TO ART-IX                                              
012182     WHEN IN-KDPRISKL = '7' AND IN-KDFREKKL = 'H'                         
012183          MOVE +68 TO ART-IX                                              
012184     WHEN IN-KDPRISKL = '7' AND IN-KDFREKKL = 'I'                         
012185          MOVE +69 TO ART-IX                                              
012186     WHEN IN-KDPRISKL = '7' AND IN-KDFREKKL = 'J'                         
012187          MOVE +70 TO ART-IX                                              
012188                                                                          
012189     WHEN IN-KDPRISKL = '8' AND IN-KDFREKKL = 'A'                         
012190          MOVE +71 TO ART-IX                                              
012191     WHEN IN-KDPRISKL = '8' AND IN-KDFREKKL = 'B'                         
012192          MOVE +72 TO ART-IX                                              
012193     WHEN IN-KDPRISKL = '8' AND IN-KDFREKKL = 'C'                         
012194          MOVE +73 TO ART-IX                                              
012195     WHEN IN-KDPRISKL = '8' AND IN-KDFREKKL = 'D'                         
012196          MOVE +74 TO ART-IX                                              
012197     WHEN IN-KDPRISKL = '8' AND IN-KDFREKKL = 'E'                         
012198          MOVE +75 TO ART-IX                                              
012199     WHEN IN-KDPRISKL = '8' AND IN-KDFREKKL = 'F'                         
012200          MOVE +76 TO ART-IX                                              
012201     WHEN IN-KDPRISKL = '8' AND IN-KDFREKKL = 'G'                         
012202          MOVE +77 TO ART-IX                                              
012203     WHEN IN-KDPRISKL = '8' AND IN-KDFREKKL = 'H'                         
012204          MOVE +78 TO ART-IX                                              
012205     WHEN IN-KDPRISKL = '8' AND IN-KDFREKKL = 'I'                         
012206          MOVE +79 TO ART-IX                                              
012207     WHEN IN-KDPRISKL = '8' AND IN-KDFREKKL = 'J'                         
012208          MOVE +80 TO ART-IX                                              
012209                                                                          
012210     WHEN IN-KDPRISKL = '9' AND IN-KDFREKKL = 'A'                         
012211          MOVE +81 TO ART-IX                                              
012212     WHEN IN-KDPRISKL = '9' AND IN-KDFREKKL = 'B'                         
012213          MOVE +82 TO ART-IX                                              
012214     WHEN IN-KDPRISKL = '9' AND IN-KDFREKKL = 'C'                         
012215          MOVE +83 TO ART-IX                                              
012216     WHEN IN-KDPRISKL = '9' AND IN-KDFREKKL = 'D'                         
012217          MOVE +84 TO ART-IX                                              
012218     WHEN IN-KDPRISKL = '9' AND IN-KDFREKKL = 'E'                         
012219          MOVE +85 TO ART-IX                                              
012220     WHEN IN-KDPRISKL = '9' AND IN-KDFREKKL = 'F'                         
012221          MOVE +86 TO ART-IX                                              
012222     WHEN IN-KDPRISKL = '9' AND IN-KDFREKKL = 'G'                         
012223          MOVE +87 TO ART-IX                                              
012224     WHEN IN-KDPRISKL = '9' AND IN-KDFREKKL = 'H'                         
012225          MOVE +88 TO ART-IX                                              
012226     WHEN IN-KDPRISKL = '9' AND IN-KDFREKKL = 'I'                         
012227          MOVE +89 TO ART-IX                                              
012228     WHEN IN-KDPRISKL = '9' AND IN-KDFREKKL = 'J'                         
012229          MOVE +90 TO ART-IX                                              
012230                                                                          
012231     WHEN OTHER                                                           
012232          CONTINUE                                                        
012233     END-EVALUATE                                                         
012234     .                                                                    
012235     EJECT                                                                
012236 BBB-UPPDATERA-TABELLER SECTION.                                          
012237                                                                          
012238*********  ANTAL ARTIKLAR (STYCK)                                         
012239     ADD +1 TO ART-KVANT(ART-IX)                                          
012240                                                                          
012241*********  ÅRSFÖRBRUKNING/ARTIKEL (KRONOR)                                
012242     COMPUTE WS-ARSFORB = IN-KVOI-TOT * IN-PRARTSTD                       
012243     ADD WS-ARSFORB TO WS-ART-ARSFORB(ART-IX)                             
012244                                                                          
012245*********  ÅRSPROGNOS/ARTIKEL (KRONOR)                                    
012246     COMPUTE WS-KVPB = IN-KVPB-TOT + IN-KVPB-TPO                          
012247     COMPUTE WS-ARSPROG = WS-KVPB * 12.0 * IN-PRARTSTD                    
012248     ADD WS-ARSPROG TO WS-ART-ARSPROG(ART-IX)                             
012249                                                                          
012250*********  SÄKERHETSLAGER/ARTIKEL (KRONOR)                                
012251     COMPUTE WS-SLAGER = IN-KVSLAGER * IN-PRARTSTD                        
012252     ADD WS-SLAGER TO WS-ART-SLAGER(ART-IX)                               
012253                                                                          
012254*********  OMSLAGER/ARTIKEL (KRONOR)                                      
012255     COMPUTE WS-KVQ = IN-KVQ / 2                                          
012256     COMPUTE WS-OLAGER = WS-KVQ * IN-PRARTSTD                             
012257     ADD WS-OLAGER TO WS-ART-OLAGER(ART-IX)                               
012258                                                                          
012259*********  ÖVERLAGER/ARTIKEL (KRONOR)                                     
012260     COMPUTE WS-KVDISP-DEC =                                              
012261                  IN-KVLS - (IN-KVRESS + IN-KVOKS-TOT) -                  
012262                         (IN-KVSLAGER + WS-KVQ)                           
012263     COMPUTE WS-AVVIK-KRONOR = WS-KVDISP-DEC * IN-PRARTSTD                
012264     ADD WS-AVVIK-KRONOR TO WS-ART-AVVIK(ART-IX)                          
012265                                                                          
012266*********  INKOMNA ORDERRADER/ARTIKEL (STYCK)                             
012267     ADD IN-KVINORD-TOT TO ART-KVINORD(ART-IX)                            
012268                           WS-ART-KVINORD(ART-IX)                         
012269                                                                          
012270*********  EJ AVBOKADE ORDERRADER/ARTIKEL (STYCK)                         
012271     COMPUTE WS-EJAVBOK  = IN-KVINORD-TOT -                               
012272                             IN-KVAVBRAD-TOT                              
012273     ADD WS-EJAVBOK TO WS-ART-EJAVBOK(ART-IX)                             
012274                                                                          
012275*********  ARBETSFÄLT                                                     
012276                                                                          
012277     ADD IN-KVLS                    TO WS-ART-KVLS(ART-IX)                
012278     ADD IN-KVRESS                  TO WS-ART-KVRESS(ART-IX)              
012279     ADD IN-KVOKS-TOT               TO WS-ART-KVOKS(ART-IX)               
012280                                                                          
012281     COMPUTE WS-KVOI = IN-KVOI-TOT * IN-PRARTSTD                          
012282     COMPUTE WS-KVDISP-SNITT = IN-KVDISP-SNITT-VECKA * IN-PRARTSTD        
012283     ADD WS-KVOI                    TO WS-ART-KVOI(ART-IX)                
012284     ADD WS-KVDISP-SNITT            TO WS-ART-KVDISP-SNITT(ART-IX)        
012285                                                                          
012286     COMPUTE WS-KVDISP = IN-KVLS - (IN-KVRESS + IN-KVOKS-TOT)             
012287     COMPUTE WS-KVDISP-PRIS = WS-KVDISP * IN-PRARTSTD                     
012288     ADD WS-KVDISP-PRIS             TO WS-ART-KVDISP-PR(ART-IX)           
012289                                                                          
012290     ADD IN-KVAVBRAD-TOT            TO WS-ART-KVAVBRAD(ART-IX)            
012291     ADD IN-KVFYSAVV-TOT            TO WS-ART-KVFYSAVV(ART-IX)            
012292     .                                                                    
012293     EJECT                                                                
012294 BC-SUMMERA SECTION.                                                      
012295                                                                          
012296     PERFORM BCA-SUMMERA-RUTA                                             
012297     PERFORM BCB-SUMMERA-PRISKLASS                                        
012298     PERFORM BCC-SUMMERA-FREKVENSKLASS                                    
012299     PERFORM BCD-SUMMERA-TOTAL                                            
012300     PERFORM BCE-BERAKNINGAR-AV-TOTAL                                     
012301     .                                                                    
012302     EJECT                                                                
012303 BCA-SUMMERA-RUTA SECTION.                                                
012304                                                                          
012305     MOVE +1  TO ART-IX                                                   
012306     MOVE +90 TO ART-IX-MAX                                               
012307     PERFORM UNTIL ART-IX > ART-IX-MAX                                    
012308                                                                          
012309*********  SUMMERING ARSFÖRBRUKNING/RUTA                                  
012310        IF WS-ART-ARSFORB(ART-IX) = ZERO                                  
012311           CONTINUE                                                       
012312        ELSE                                                              
012313           MOVE WS-ART-ARSFORB(ART-IX) TO WS-ARSFORB                      
012314           COMPUTE WS-ARSFORB-KR ROUNDED = WS-ARSFORB / 1000              
012315           MOVE WS-ARSFORB-KR TO ART-ARSFORB(ART-IX)                      
012316        END-IF                                                            
012317                                                                          
012318*********  SUMMERING ARSPROGNOS/RUTA                                      
012319        IF WS-ART-ARSPROG(ART-IX) = ZERO                                  
012320           CONTINUE                                                       
012321        ELSE                                                              
012322           MOVE WS-ART-ARSPROG(ART-IX) TO WS-ARSPROG                      
012323           COMPUTE WS-ARSPROG-KR ROUNDED = WS-ARSPROG / 1000              
012324           MOVE WS-ARSPROG-KR TO ART-ARSPROG(ART-IX)                      
012325        END-IF                                                            
012326                                                                          
012327*********  SUMMERING SÄKERHETSLAGER/RUTA                                  
012328        IF WS-ART-SLAGER(ART-IX) = ZERO                                   
012329           CONTINUE                                                       
012330        ELSE                                                              
012331           MOVE WS-ART-SLAGER(ART-IX) TO WS-SLAGER                        
012332           COMPUTE WS-SLAGER-KR ROUNDED = WS-SLAGER / 1000                
012333           MOVE WS-SLAGER-KR TO ART-SLAGER(ART-IX)                        
012334        END-IF                                                            
012335                                                                          
012336*********  SUMMERING OMSLAGER/RUTA                                        
012337        IF WS-ART-OLAGER(ART-IX) = ZERO                                   
012338           CONTINUE                                                       
012339        ELSE                                                              
012340           MOVE WS-ART-OLAGER(ART-IX) TO WS-OLAGER                        
012341           COMPUTE WS-OLAGER-KR ROUNDED = WS-OLAGER / 1000                
012342           MOVE WS-OLAGER-KR TO ART-OLAGER(ART-IX)                        
012343        END-IF                                                            
012344                                                                          
012345*********  SUMMERING ÖVERLAGER/RUTA                                       
012346        IF WS-ART-AVVIK(ART-IX) = ZERO                                    
012347           CONTINUE                                                       
012348        ELSE                                                              
012349           MOVE WS-ART-AVVIK(ART-IX) TO WS-AVVIK-RAKN                     
012350           COMPUTE WS-AVVIK-KR ROUNDED = WS-AVVIK-RAKN / 1000             
012351           MOVE WS-AVVIK-KR TO ART-AVVIK(ART-IX)                          
012352        END-IF                                                            
012353                                                                          
012354*********  BERÄKNING AV OMSHASTIGHET/RUTA                                 
012355        IF WS-ART-KVDISP-SNITT(ART-IX) = ZERO                             
012356           CONTINUE                                                       
012357        ELSE                                                              
012358           COMPUTE WS-OMSHAST ROUNDED =                                   
012359               WS-ART-KVOI(ART-IX) /  WS-ART-KVDISP-SNITT(ART-IX)         
012360           MOVE WS-OMSHAST TO ART-OMSHAST(ART-IX)                         
012361        END-IF                                                            
012362                                                                          
012363*********  BERÄKNING AV TÄCKTID/RUTA (VECKOR)                             
012364                                                                          
012365        IF WS-ART-ARSPROG(ART-IX) = ZERO                                  
012366           AND WS-ART-KVDISP-PR(ART-IX) > ZERO                            
012367              MOVE 999999999 TO ART-TACKTID(ART-IX)                       
012368        ELSE                                                              
012369          IF WS-ART-KVDISP-PR(ART-IX) > ZERO                              
012370             COMPUTE WS-TACKTID = WS-ART-KVDISP-PR(ART-IX) /              
012371                                     WS-ART-ARSPROG(ART-IX)               
012372             COMPUTE WS-TACKTID-VECKOR ROUNDED =                          
012373                                     WS-TACKTID * 52                      
012374             MOVE WS-TACKTID-VECKOR TO ART-TACKTID(ART-IX)                
012375          END-IF                                                          
012376        END-IF                                                            
012377                                                                          
012378*********  BERÄKNING AV SERVICEGRAD/RUTA                                  
012379        IF WS-ART-KVINORD(ART-IX) = ZERO                                  
012380           MOVE 99.9   TO ART-SERVG-NTO(ART-IX)                           
012381                          ART-SERVG-BTO(ART-IX)                           
012382        ELSE                                                              
012383           COMPUTE WS-SERVG ROUNDED =                                     
012384                              WS-ART-KVAVBRAD (ART-IX) * 100  /           
012385                              WS-ART-KVINORD (ART-IX)                     
012386           IF WS-SERVG = 100.0                                            
012387              MOVE 99.9 TO ART-SERVG-BTO(ART-IX)                          
012388           ELSE                                                           
012389              MOVE WS-SERVG TO ART-SERVG-BTO(ART-IX)                      
012390           END-IF                                                         
012391                                                                          
012392           COMPUTE WS-SERVG ROUNDED =                                     
012393          ( WS-ART-KVAVBRAD(ART-IX) - WS-ART-KVFYSAVV(ART-IX))            
012394                      * 100 / WS-ART-KVINORD(ART-IX)                      
012395           IF WS-SERVG = 100.0                                            
012396              MOVE 99.9 TO ART-SERVG-NTO(ART-IX)                          
012397           ELSE                                                           
012398              MOVE WS-SERVG TO ART-SERVG-NTO(ART-IX)                      
012399           END-IF                                                         
012400        END-IF                                                            
012401                                                                          
012402*********  RADER MED FYSISK AVVIKELSE/RUTA                                
012403        MOVE WS-ART-KVFYSAVV(ART-IX) TO ART-KVFYSAVV(ART-IX)              
012404                                                                          
012405*********  EJ AVBOKADE ORDERRADER/RUTA                                    
012406        MOVE WS-ART-EJAVBOK(ART-IX) TO ART-EJAVBOK(ART-IX)                
012407                                                                          
012408        ADD +1 TO ART-IX                                                  
012409     END-PERFORM                                                          
012410     .                                                                    
012411     EJECT                                                                
012412 BCB-SUMMERA-PRISKLASS SECTION.                                           
012413******************************************************************        
012414* SUMMERING PER PRISKLASS                                        *        
012415******************************************************************        
012416                                                                          
012417     MOVE +1  TO PSUM-IX                                                  
012418                 ART-IX                                                   
012419     MOVE +10 TO ART-IX-MAX                                               
012420                                                                          
012421     PERFORM UNTIL PSUM-IX > PSUM-IX-MAX                                  
012422                                                                          
012423        PERFORM UNTIL ART-IX > ART-IX-MAX                                 
012424                                                                          
012425           ADD ART-KVANT(ART-IX) TO PSUM-KVANT(PSUM-IX)                   
012426           ADD ART-KVINORD(ART-IX) TO PSUM-KVINORD(PSUM-IX)               
012427                                                                          
012428           ADD WS-ART-EJAVBOK(ART-IX) TO WS-PSUM-EJAVBOK(PSUM-IX)         
012429           ADD WS-ART-AVVIK(ART-IX) TO WS-PSUM-AVVIK(PSUM-IX)             
012430           ADD WS-ART-ARSFORB(ART-IX) TO WS-PSUM-ARSFORB(PSUM-IX)         
012431           ADD WS-ART-ARSPROG(ART-IX) TO WS-PSUM-ARSPROG(PSUM-IX)         
012432           ADD WS-ART-SLAGER(ART-IX) TO WS-PSUM-SLAGER(PSUM-IX)           
012433           ADD WS-ART-OLAGER(ART-IX) TO WS-PSUM-OLAGER(PSUM-IX)           
012434           ADD WS-ART-KVLS(ART-IX) TO WS-PSUM-KVLS(PSUM-IX)               
012435           ADD WS-ART-KVRESS(ART-IX) TO WS-PSUM-KVRESS(PSUM-IX)           
012436           ADD WS-ART-KVOKS(ART-IX) TO WS-PSUM-KVOKS(PSUM-IX)             
012437                                                                          
012438           ADD WS-ART-KVOI(ART-IX)  TO WS-PSUM-KVOI(PSUM-IX)              
012439           ADD WS-ART-KVDISP-SNITT(ART-IX) TO                             
012440                                   WS-PSUM-KVDISP-SNITT(PSUM-IX)          
012441           ADD WS-ART-KVDISP-PR(ART-IX) TO                                
012442                                    WS-PSUM-KVDISP-PR(PSUM-IX)            
012443           ADD WS-ART-KVAVBRAD(ART-IX) TO                                 
012444                                    WS-PSUM-KVAVBRAD(PSUM-IX)             
012445           ADD WS-ART-KVFYSAVV(ART-IX) TO                                 
012446                                   WS-PSUM-KVFYSAVV(PSUM-IX)              
012447           ADD WS-ART-KVINORD(ART-IX) TO                                  
012448                                   WS-PSUM-KVINORD(PSUM-IX)               
012449                                                                          
012450           ADD +1 TO ART-IX                                               
012451        END-PERFORM                                                       
012452                                                                          
012453        ADD +1  TO PSUM-IX                                                
012454        ADD +10 TO ART-IX-MAX                                             
012455     END-PERFORM                                                          
012456                                                                          
012457     MOVE +1 TO PSUM-IX                                                   
012458     MOVE +9 TO PSUM-IX-MAX                                               
012459     PERFORM UNTIL PSUM-IX > PSUM-IX-MAX                                  
012460                                                                          
012461*********  SUMMERING ARSFÖRBRUKNING/PRISKLASS                             
012462        IF WS-PSUM-ARSFORB(PSUM-IX) = ZERO                                
012463           CONTINUE                                                       
012464        ELSE                                                              
012465           MOVE WS-PSUM-ARSFORB(PSUM-IX) TO WS-ARSFORB                    
012466           COMPUTE WS-ARSFORB-KR ROUNDED = WS-ARSFORB / 1000              
012467           MOVE WS-ARSFORB-KR TO PSUM-ARSFORB(PSUM-IX)                    
012468        END-IF                                                            
012469                                                                          
012470*********  SUMMERING ARSPROGNOS/PRISKLASS                                 
012471        IF WS-PSUM-ARSPROG(PSUM-IX) = ZERO                                
012472           CONTINUE                                                       
012473        ELSE                                                              
012474           MOVE WS-PSUM-ARSPROG(PSUM-IX) TO WS-ARSPROG                    
012475           COMPUTE WS-ARSPROG-KR ROUNDED = WS-ARSPROG / 1000              
012476           MOVE WS-ARSPROG-KR TO PSUM-ARSPROG(PSUM-IX)                    
012477        END-IF                                                            
012478                                                                          
012479*********  SUMMERING SÄKERHETSLAGER/PRISKLASS                             
012480        IF WS-PSUM-SLAGER(PSUM-IX) = ZERO                                 
012481           CONTINUE                                                       
012482        ELSE                                                              
012483           MOVE WS-PSUM-SLAGER(PSUM-IX) TO WS-SLAGER                      
012484           COMPUTE WS-SLAGER-KR ROUNDED = WS-SLAGER / 1000                
012485           MOVE WS-SLAGER-KR TO PSUM-SLAGER(PSUM-IX)                      
012486        END-IF                                                            
012487                                                                          
012488*********  SUMMERING OMSLAGER/PRISKLASS                                   
012489        IF WS-PSUM-OLAGER(PSUM-IX) = ZERO                                 
012490           CONTINUE                                                       
012491        ELSE                                                              
012492           MOVE WS-PSUM-OLAGER(PSUM-IX) TO WS-OLAGER                      
012493           COMPUTE WS-OLAGER-KR ROUNDED = WS-OLAGER / 1000                
012494           MOVE WS-OLAGER-KR TO PSUM-OLAGER(PSUM-IX)                      
012495        END-IF                                                            
012496                                                                          
012497*********  SUMMERING ÖVERLAGER/PRISKLASS                                  
012498        IF WS-PSUM-AVVIK(PSUM-IX) = ZERO                                  
012499           CONTINUE                                                       
012500        ELSE                                                              
012501           MOVE WS-PSUM-AVVIK(PSUM-IX) TO WS-AVVIK-RAKN                   
012502           COMPUTE WS-AVVIK-KR ROUNDED = WS-AVVIK-RAKN / 1000             
012503           MOVE WS-AVVIK-KR TO PSUM-AVVIK(PSUM-IX)                        
012504        END-IF                                                            
012505                                                                          
012506*********  BERÄKNING AV OMSHASTIGHET/PRISKLASS                            
012507           IF WS-PSUM-KVDISP-SNITT(PSUM-IX) = ZERO                        
012508              CONTINUE                                                    
012509           ELSE                                                           
012510              COMPUTE WS-OMSHAST ROUNDED = WS-PSUM-KVOI(PSUM-IX)/         
012511                                    WS-PSUM-KVDISP-SNITT(PSUM-IX)         
012512              MOVE WS-OMSHAST TO PSUM-OMSHAST(PSUM-IX)                    
012513           END-IF                                                         
012514                                                                          
012515*********  BERÄKNING AV TÄCKTID/PRISKLASS                                 
012516           IF WS-PSUM-ARSPROG(PSUM-IX) = ZERO                             
012517              AND WS-PSUM-KVDISP-PR(PSUM-IX) > ZERO                       
012518              MOVE 999999999 TO PSUM-TACKTID(PSUM-IX)                     
012519           ELSE                                                           
012520             IF WS-PSUM-KVDISP-PR(PSUM-IX) > ZERO                         
012521                COMPUTE WS-TACKTID =                                      
012522                      WS-PSUM-KVDISP-PR(PSUM-IX) /                        
012523                      WS-PSUM-ARSPROG(PSUM-IX)                            
012524                COMPUTE WS-TACKTID-VECKOR ROUNDED =                       
012525                                        WS-TACKTID * 52                   
012526                MOVE WS-TACKTID-VECKOR TO PSUM-TACKTID(PSUM-IX)           
012527              END-IF                                                      
012528           END-IF                                                         
012529                                                                          
012530*********  BERÄKNING AV SERVICEGRAD/PRISKLASS                             
012531           IF WS-PSUM-KVINORD(PSUM-IX) = ZERO                             
012532              MOVE 99.9            TO PSUM-SERVG-NTO(PSUM-IX)             
012533                                      PSUM-SERVG-BTO(PSUM-IX)             
012534           ELSE                                                           
012535              COMPUTE WS-SERVG ROUNDED =                                  
012536                      WS-PSUM-KVAVBRAD(PSUM-IX) * 100 /                   
012537                                 WS-PSUM-KVINORD (PSUM-IX)                
012538              IF WS-SERVG = 100.0                                         
012539                 MOVE 99.9 TO PSUM-SERVG-BTO(PSUM-IX)                     
012540              ELSE                                                        
012541                 MOVE WS-SERVG TO PSUM-SERVG-BTO(PSUM-IX)                 
012542              END-IF                                                      
012543                                                                          
012544             COMPUTE WS-SERVG ROUNDED = (WS-PSUM-KVAVBRAD(PSUM-IX)        
012545                        - WS-PSUM-KVFYSAVV(PSUM-IX)) * 100 /              
012546                            WS-PSUM-KVINORD(PSUM-IX)                      
012547              IF WS-SERVG = 100.0                                         
012548                 MOVE 99.9 TO PSUM-SERVG-NTO(PSUM-IX)                     
012549              ELSE                                                        
012550                 MOVE WS-SERVG TO PSUM-SERVG-NTO(PSUM-IX)                 
012551              END-IF                                                      
012552           END-IF                                                         
012553                                                                          
012554*********  RADER MED FYSISK AVVIKELSE/PRISKLASS                           
012555        MOVE WS-PSUM-KVFYSAVV(PSUM-IX) TO PSUM-KVFYSAVV(PSUM-IX)          
012556                                                                          
012557*********  EJ AVBOKADE ORDERRADER/PRISKLASS                               
012558        MOVE WS-PSUM-EJAVBOK(PSUM-IX) TO PSUM-EJAVBOK(PSUM-IX)            
012559                                                                          
012560        ADD +1 TO PSUM-IX                                                 
012561     END-PERFORM                                                          
012562     .                                                                    
012563     EJECT                                                                
012564 BCC-SUMMERA-FREKVENSKLASS SECTION.                                       
012565******************************************************************        
012566* SUMMERING PER FREKVENSKLASS                                    *        
012567******************************************************************        
012568                                                                          
012569     MOVE +1 TO FSUM-IX                                                   
012570                ART-IX                                                    
012571     MOVE +81 TO ART-IX-MAX                                               
012572     PERFORM UNTIL FSUM-IX > FSUM-IX-MAX                                  
012573                                                                          
012574        PERFORM UNTIL ART-IX > ART-IX-MAX                                 
012575                                                                          
012576           ADD ART-KVANT(ART-IX) TO FSUM-KVANT(FSUM-IX)                   
012577           ADD ART-KVINORD(ART-IX) TO FSUM-KVINORD(FSUM-IX)               
012578                                                                          
012579******* ARBETSFÄLT                                                        
012580           ADD WS-ART-EJAVBOK(ART-IX) TO WS-FSUM-EJAVBOK(FSUM-IX)         
012581           ADD WS-ART-AVVIK(ART-IX) TO WS-FSUM-AVVIK(FSUM-IX)             
012582           ADD WS-ART-SLAGER(ART-IX) TO WS-FSUM-SLAGER(FSUM-IX)           
012583           ADD WS-ART-OLAGER(ART-IX) TO WS-FSUM-OLAGER(FSUM-IX)           
012584           ADD WS-ART-ARSFORB(ART-IX) TO WS-FSUM-ARSFORB(FSUM-IX)         
012585           ADD WS-ART-ARSPROG(ART-IX) TO WS-FSUM-ARSPROG(FSUM-IX)         
012586           ADD WS-ART-KVLS(ART-IX)  TO WS-FSUM-KVLS(FSUM-IX)              
012587           ADD WS-ART-KVRESS(ART-IX) TO WS-FSUM-KVRESS(FSUM-IX)           
012588           ADD WS-ART-KVOKS(ART-IX) TO WS-FSUM-KVOKS(FSUM-IX)             
012589           ADD WS-ART-KVOI(ART-IX)  TO WS-FSUM-KVOI(FSUM-IX)              
012590           ADD WS-ART-KVDISP-SNITT(ART-IX) TO                             
012591                                   WS-FSUM-KVDISP-SNITT(FSUM-IX)          
012592           ADD WS-ART-KVDISP-PR(ART-IX) TO                                
012593                                    WS-FSUM-KVDISP-PR(FSUM-IX)            
012594           ADD WS-ART-KVAVBRAD(ART-IX) TO                                 
012595                                    WS-FSUM-KVAVBRAD(FSUM-IX)             
012596           ADD WS-ART-KVFYSAVV(ART-IX) TO                                 
012597                                   WS-FSUM-KVFYSAVV(FSUM-IX)              
012598           ADD WS-ART-KVINORD(ART-IX) TO                                  
012599                                   WS-FSUM-KVINORD(FSUM-IX)               
012600                                                                          
012601           ADD +10 TO ART-IX                                              
012602        END-PERFORM                                                       
012603        ADD +1 TO FSUM-IX                                                 
012604                                                                          
012605        EVALUATE TRUE                                                     
012606           WHEN  FSUM-IX = 1                                              
012607                 MOVE +1 TO ART-IX                                        
012608           WHEN  FSUM-IX = 2                                              
012609                 MOVE +2  TO ART-IX                                       
012610                 MOVE +82 TO ART-IX-MAX                                   
012611           WHEN  FSUM-IX = 3                                              
012612                 MOVE +3  TO ART-IX                                       
012613                 MOVE +83 TO ART-IX-MAX                                   
012614           WHEN  FSUM-IX = 4                                              
012615                 MOVE +4  TO ART-IX                                       
012616                 MOVE +84 TO ART-IX-MAX                                   
012617           WHEN  FSUM-IX = 5                                              
012618                 MOVE +5  TO ART-IX                                       
012619                 MOVE +85 TO ART-IX-MAX                                   
012620           WHEN  FSUM-IX = 6                                              
012621                 MOVE +6  TO ART-IX                                       
012622                 MOVE +86 TO ART-IX-MAX                                   
012623           WHEN  FSUM-IX = 7                                              
012624                 MOVE +7  TO ART-IX                                       
012625                 MOVE +87 TO ART-IX-MAX                                   
012626           WHEN  FSUM-IX = 8                                              
012627                 MOVE +8  TO ART-IX                                       
012628                 MOVE +88 TO ART-IX-MAX                                   
012629           WHEN  FSUM-IX = 9                                              
012630                 MOVE +9  TO ART-IX                                       
012631                 MOVE +89 TO ART-IX-MAX                                   
012632           WHEN  FSUM-IX = 10                                             
012633                 MOVE +10 TO ART-IX                                       
012634                 MOVE +90 TO ART-IX-MAX                                   
012635           WHEN OTHER                                                     
012636                CONTINUE                                                  
012637        END-EVALUATE                                                      
012638                                                                          
012639     END-PERFORM                                                          
012640                                                                          
012641     MOVE +1  TO FSUM-IX                                                  
012642     MOVE +10 TO FSUM-IX-MAX                                              
012643     PERFORM UNTIL FSUM-IX > FSUM-IX-MAX                                  
012644                                                                          
012645*********  SUMMERING ARSFORBRUKNING/FREKVENSKLASS                         
012646        IF WS-FSUM-ARSFORB(FSUM-IX) = ZERO                                
012647           CONTINUE                                                       
012648        ELSE                                                              
012649           MOVE WS-FSUM-ARSFORB(FSUM-IX) TO WS-ARSFORB                    
012650           COMPUTE WS-ARSFORB-KR ROUNDED = WS-ARSFORB / 1000              
012651           MOVE WS-ARSFORB-KR TO FSUM-ARSFORB(FSUM-IX)                    
012652        END-IF                                                            
012653                                                                          
012654*********  SUMMERING ARSPROGNOS/FREKVENSKLASS                             
012655        IF WS-FSUM-ARSPROG(FSUM-IX) = ZERO                                
012656           CONTINUE                                                       
012657        ELSE                                                              
012658           MOVE WS-FSUM-ARSPROG(FSUM-IX) TO WS-ARSPROG                    
012659           COMPUTE WS-ARSPROG-KR ROUNDED = WS-ARSPROG / 1000              
012660           MOVE WS-ARSPROG-KR TO FSUM-ARSPROG(FSUM-IX)                    
012661        END-IF                                                            
012662                                                                          
012663*********  SUMMERING SÄKERHETSLAGER/FREKVENSKLASS                         
012664        IF WS-FSUM-SLAGER(FSUM-IX) = ZERO                                 
012665           CONTINUE                                                       
012666        ELSE                                                              
012667           MOVE WS-FSUM-SLAGER(FSUM-IX) TO WS-SLAGER                      
012668           COMPUTE WS-SLAGER-KR ROUNDED = WS-SLAGER / 1000                
012669           MOVE WS-SLAGER-KR TO FSUM-SLAGER(FSUM-IX)                      
012670        END-IF                                                            
012671                                                                          
012672*********  SUMMERING OMSLAGER/FREKVENSKLASS                               
012673        IF WS-FSUM-OLAGER(FSUM-IX) = ZERO                                 
012674           CONTINUE                                                       
012675        ELSE                                                              
012676           MOVE WS-FSUM-OLAGER(FSUM-IX) TO WS-OLAGER                      
012677           COMPUTE WS-OLAGER-KR ROUNDED = WS-OLAGER / 1000                
012678           MOVE WS-OLAGER-KR TO FSUM-OLAGER(FSUM-IX)                      
012679        END-IF                                                            
012680                                                                          
012681*********  SUMMERING ÖVERLAGER/FREKVENSKLASS                              
012682        IF WS-FSUM-AVVIK(FSUM-IX) = ZERO                                  
012683           CONTINUE                                                       
012684        ELSE                                                              
012685           MOVE WS-FSUM-AVVIK(FSUM-IX) TO WS-AVVIK-RAKN                   
012686           COMPUTE WS-AVVIK-KR ROUNDED = WS-AVVIK-RAKN / 1000             
012687           MOVE WS-AVVIK-KR TO FSUM-AVVIK(FSUM-IX)                        
012688        END-IF                                                            
012689                                                                          
012690*********  BERÄKNING AV OMSHASTIGHET/FREKVENSKLASS                        
012691           IF WS-FSUM-KVDISP-SNITT(FSUM-IX) = ZERO                        
012692              CONTINUE                                                    
012693           ELSE                                                           
012694              COMPUTE WS-OMSHAST ROUNDED = WS-FSUM-KVOI(FSUM-IX)/         
012695                                    WS-FSUM-KVDISP-SNITT(FSUM-IX)         
012696              MOVE WS-OMSHAST TO FSUM-OMSHAST(FSUM-IX)                    
012697           END-IF                                                         
012698                                                                          
012699*********  BERÄKNING AV TÄCKTID/FREKVENSKLASS                             
012700           IF WS-FSUM-ARSPROG(FSUM-IX) = ZERO                             
012701              AND WS-FSUM-KVDISP-PR(FSUM-IX) > ZERO                       
012702              MOVE 999999999 TO FSUM-TACKTID(FSUM-IX)                     
012703           ELSE                                                           
012704             IF WS-FSUM-KVDISP-PR(FSUM-IX) > ZERO                         
012705                COMPUTE WS-TACKTID =                                      
012706                      WS-FSUM-KVDISP-PR(FSUM-IX) /                        
012707                      WS-FSUM-ARSPROG(FSUM-IX)                            
012708                COMPUTE WS-TACKTID-VECKOR ROUNDED =                       
012709                                        WS-TACKTID * 52                   
012710                MOVE WS-TACKTID-VECKOR TO FSUM-TACKTID(FSUM-IX)           
012711              END-IF                                                      
012712           END-IF                                                         
012713                                                                          
012714*********  BERÄKNING AV SERVICEGRAD/FREKVENSKLASS                         
012715           IF WS-FSUM-KVINORD(FSUM-IX) = ZERO                             
012716              MOVE 99.9   TO FSUM-SERVG-NTO(FSUM-IX)                      
012717                             FSUM-SERVG-BTO(FSUM-IX)                      
012718           ELSE                                                           
012719             COMPUTE WS-SERVG ROUNDED =                                   
012720                                WS-FSUM-KVAVBRAD(FSUM-IX) * 100 /         
012721                                WS-FSUM-KVINORD (FSUM-IX)                 
012722              IF WS-SERVG = 100.0                                         
012723                 MOVE 99.9 TO FSUM-SERVG-BTO(FSUM-IX)                     
012724              ELSE                                                        
012725                 MOVE WS-SERVG TO FSUM-SERVG-BTO(FSUM-IX)                 
012726              END-IF                                                      
012727                                                                          
012728             COMPUTE WS-SERVG ROUNDED = (WS-FSUM-KVAVBRAD(FSUM-IX)        
012729                             - WS-FSUM-KVFYSAVV(FSUM-IX)) * 100 /         
012730                             WS-FSUM-KVINORD(FSUM-IX)                     
012731              IF WS-SERVG = 100.0                                         
012732                 MOVE 99.9 TO FSUM-SERVG-NTO(FSUM-IX)                     
012733              ELSE                                                        
012734                 MOVE WS-SERVG TO FSUM-SERVG-NTO(FSUM-IX)                 
012735              END-IF                                                      
012736           END-IF                                                         
012737                                                                          
012738*********  RADER MED FYSISK AVVIKELSE/FREKVENSKLASS                       
012739        MOVE WS-FSUM-KVFYSAVV(FSUM-IX) TO FSUM-KVFYSAVV(FSUM-IX)          
012740                                                                          
012741*********  EJ AVBOKADE ORDERRADER/FREKVENSKLASS                           
012742        MOVE WS-FSUM-EJAVBOK(FSUM-IX) TO FSUM-EJAVBOK(FSUM-IX)            
012743                                                                          
012744        ADD +1 TO FSUM-IX                                                 
012745                                                                          
012746     END-PERFORM                                                          
012747     .                                                                    
012748     EJECT                                                                
012749 BCD-SUMMERA-TOTAL SECTION.                                               
012750******************************************************************        
012751* TOTALSUMMERING SAMTLIGA PRISKLASSER                            *        
012752******************************************************************        
012753                                                                          
012754     MOVE +1 TO PSUM-IX                                                   
012755     PERFORM UNTIL PSUM-IX > PSUM-IX-MAX                                  
012756                                                                          
012757        ADD PSUM-KVANT(PSUM-IX) TO TOT-KVANT                              
012758        ADD PSUM-KVINORD(PSUM-IX) TO TOT-KVINORD                          
012759                                                                          
012760******* ARBETSFÄLT                                                        
012761        ADD WS-PSUM-EJAVBOK(PSUM-IX) TO WS-TOT-EJAVBOK                    
012762        ADD WS-PSUM-AVVIK(PSUM-IX) TO WS-TOT-AVVIK                        
012763        ADD WS-PSUM-SLAGER(PSUM-IX) TO WS-TOT-SLAGER                      
012764        ADD WS-PSUM-OLAGER(PSUM-IX) TO WS-TOT-OLAGER                      
012765        ADD WS-PSUM-ARSFORB(PSUM-IX) TO WS-TOT-ARSFORB                    
012766        ADD WS-PSUM-ARSPROG(PSUM-IX) TO WS-TOT-ARSPROG                    
012767        ADD WS-PSUM-KVLS(PSUM-IX) TO WS-TOT-KVLS                          
012768        ADD WS-PSUM-KVRESS(PSUM-IX) TO WS-TOT-KVRESS                      
012769        ADD WS-PSUM-KVOKS(PSUM-IX) TO WS-TOT-KVOKS                        
012770        ADD WS-PSUM-KVOI (PSUM-IX) TO WS-TOT-KVOI                         
012771        ADD WS-PSUM-KVDISP-SNITT(PSUM-IX) TO WS-TOT-KVDISP-SNITT          
012772        ADD WS-PSUM-KVDISP-PR(PSUM-IX) TO WS-TOT-KVDISP-PR                
012773        ADD WS-PSUM-KVAVBRAD(PSUM-IX) TO WS-TOT-KVAVBRAD                  
012774        ADD WS-PSUM-KVFYSAVV(PSUM-IX) TO WS-TOT-KVFYSAVV                  
012775        ADD WS-PSUM-KVINORD(PSUM-IX) TO WS-TOT-KVINORD                    
012776                                                                          
012777        ADD +1 TO PSUM-IX                                                 
012778     END-PERFORM                                                          
012779                                                                          
012780*********  SUMMERING ARSFÖRBRUKNING TOTALT                                
012781     IF WS-TOT-ARSFORB = ZERO                                             
012782        CONTINUE                                                          
012783     ELSE                                                                 
012784        MOVE WS-TOT-ARSFORB TO WS-ARSFORB                                 
012785        COMPUTE WS-ARSFORB-KR ROUNDED = WS-ARSFORB / 1000                 
012786        MOVE WS-ARSFORB-KR TO TOT-ARSFORB                                 
012787     END-IF                                                               
012788                                                                          
012789*********  SUMMERING ARSPROGNOS TOTALT                                    
012790     IF WS-TOT-ARSPROG = ZERO                                             
012791        CONTINUE                                                          
012792     ELSE                                                                 
012793        MOVE WS-TOT-ARSPROG TO WS-ARSPROG                                 
012794        COMPUTE WS-ARSPROG-KR ROUNDED = WS-ARSPROG / 1000                 
012795        MOVE WS-ARSPROG-KR TO TOT-ARSPROG                                 
012796     END-IF                                                               
012797                                                                          
012798*********  SUMMERING SLAGER TOTALT                                        
012799     IF WS-TOT-SLAGER = ZERO                                              
012800        CONTINUE                                                          
012801     ELSE                                                                 
012802        MOVE WS-TOT-SLAGER TO WS-SLAGER                                   
012803        COMPUTE WS-SLAGER-KR ROUNDED = WS-SLAGER / 1000                   
012804        MOVE WS-SLAGER-KR TO TOT-SLAGER                                   
012805     END-IF                                                               
012806                                                                          
012807*********  SUMMERING OLAGER TOTALT                                        
012808     IF WS-TOT-OLAGER = ZERO                                              
012809        CONTINUE                                                          
012810     ELSE                                                                 
012811        MOVE WS-TOT-OLAGER TO WS-OLAGER                                   
012812        COMPUTE WS-OLAGER-KR ROUNDED = WS-OLAGER / 1000                   
012813        MOVE WS-OLAGER-KR TO TOT-OLAGER                                   
012814     END-IF                                                               
012815                                                                          
012816*********  SUMMERING ÖVERLAGER TOTALT                                     
012817     IF WS-TOT-AVVIK = ZERO                                               
012818        CONTINUE                                                          
012819     ELSE                                                                 
012820        MOVE WS-TOT-AVVIK TO WS-AVVIK-RAKN                                
012821        COMPUTE WS-AVVIK-KR ROUNDED = WS-AVVIK-RAKN / 1000                
012822        MOVE WS-AVVIK-KR TO TOT-AVVIK                                     
012823     END-IF                                                               
012824                                                                          
012825*********  BERÄKNING AV OMSHASTIGHET TOTALT                               
012826     IF WS-TOT-KVDISP-SNITT = ZERO                                        
012827        CONTINUE                                                          
012828     ELSE                                                                 
012829        COMPUTE WS-OMSHAST ROUNDED = WS-TOT-KVOI /                        
012830                              WS-TOT-KVDISP-SNITT                         
012831        MOVE WS-OMSHAST TO TOT-OMSHAST                                    
012832     END-IF                                                               
012833                                                                          
012834*********  BERÄKNING AV TÄCKTID TOTALT                                    
012835     IF WS-TOT-ARSPROG = ZERO                                             
012836        CONTINUE                                                          
012837     ELSE                                                                 
012838       IF WS-TOT-KVDISP-PR > ZERO                                         
012839          COMPUTE WS-TACKTID =                                            
012840                WS-TOT-KVDISP-PR /                                        
012841                WS-TOT-ARSPROG                                            
012842          COMPUTE WS-TACKTID-VECKOR ROUNDED =                             
012843                                  WS-TACKTID * 52                         
012844          MOVE WS-TACKTID-VECKOR TO TOT-TACKTID                           
012845        END-IF                                                            
012846     END-IF                                                               
012847                                                                          
012848*********  BERÄKNING AV SERVICEGRAD TOTALT                                
012849     IF WS-TOT-KVINORD = ZERO                                             
012850        MOVE 99.9                 TO TOT-SERVG-NTO                        
012851                                     TOT-SERVG-BTO                        
012852     ELSE                                                                 
012853        COMPUTE WS-SERVG ROUNDED = WS-TOT-KVAVBRAD * 100 /                
012854                                   WS-TOT-KVINORD                         
012855        IF WS-SERVG = 100.0                                               
012856           MOVE 99.9 TO TOT-SERVG-BTO                                     
012857        ELSE                                                              
012858           MOVE WS-SERVG TO TOT-SERVG-BTO                                 
012859        END-IF                                                            
012860                                                                          
012861        COMPUTE WS-SERVG ROUNDED = (WS-TOT-KVAVBRAD                       
012862                      - WS-TOT-KVFYSAVV) * 100  / WS-TOT-KVINORD          
012863        IF WS-SERVG = 100.0                                               
012864           MOVE 99.9 TO TOT-SERVG-NTO                                     
012865        ELSE                                                              
012866           MOVE WS-SERVG TO TOT-SERVG-NTO                                 
012867        END-IF                                                            
012868     END-IF                                                               
012869                                                                          
012870*********  RADER MED FYSISK AVVIKELSE TOTALT                              
012871     MOVE WS-TOT-KVFYSAVV TO TOT-KVFYSAVV                                 
012872                                                                          
012873*********  EJ AVBOKADE ORDERRADER TOTALT                                  
012874     MOVE WS-TOT-EJAVBOK TO TOT-EJAVBOK                                   
012875                                                                          
012876*******  % SÄKERHETSLAGER AV TOT-DISP-LAGER                               
012877        IF WS-TOT-KVDISP-PR = ZERO                                        
012878           CONTINUE                                                       
012879        ELSE                                                              
012880           COMPUTE WS-PROC-SLAGER = WS-TOT-SLAGER * 100                   
012881                                     / WS-TOT-KVDISP-PR                   
012882           MOVE WS-PROC-SLAGER TO TOT-PROC-SLAGER                         
012883        END-IF                                                            
012884        IF WS-TOT-SLAGER > WS-TOT-KVDISP-PR                               
012885           MOVE 99.9 TO TOT-PROC-SLAGER                                   
012886        END-IF                                                            
012887                                                                          
012888*******  % OMSLAGER AV TOT DISP-LAGER                                     
012889        IF WS-TOT-KVDISP-PR = ZERO                                        
012890           CONTINUE                                                       
012891        ELSE                                                              
012892           COMPUTE WS-PROC-OLAGER = WS-TOT-OLAGER * 100                   
012893                                      / WS-TOT-KVDISP-PR                  
012894           MOVE WS-PROC-OLAGER TO TOT-PROC-OLAGER                         
012895        END-IF                                                            
012896        IF WS-TOT-OLAGER > WS-TOT-KVDISP-PR                               
012897           MOVE 99.9 TO TOT-PROC-OLAGER                                   
012898        END-IF                                                            
012899                                                                          
012900*******  % AVVIKANDE LAGER AV TOT DISP-LAGER                              
012901        IF WS-TOT-KVDISP-PR = ZERO                                        
012902           CONTINUE                                                       
012903        ELSE                                                              
012904           IF WS-TOT-AVVIK > ZERO                                         
012905              COMPUTE WS-PROC-AVVIK = WS-TOT-AVVIK * 100                  
012906                                  / WS-TOT-KVDISP-PR                      
012907              MOVE WS-PROC-AVVIK TO TOT-PROC-AVVIK                        
012908           END-IF                                                         
012909        END-IF                                                            
012910     .                                                                    
012911     EJECT                                                                
012912 BCE-BERAKNINGAR-AV-TOTAL SECTION.                                        
012913******************************************************************        
012914* % BERÄKNING PER RUTA / PRISKLASS / FREKVENSKLASS               *        
012915******************************************************************        
012916                                                                          
012917     MOVE +1 TO ART-IX                                                    
012918     MOVE +90 TO ART-IX-MAX                                               
012919     PERFORM UNTIL ART-IX > ART-IX-MAX                                    
012920                                                                          
012921******** % ANTAL ARTIKLAR AV TOTAL-ANTAL                                  
012922        IF TOT-KVANT = ZERO                                               
012923           CONTINUE                                                       
012924        ELSE                                                              
012925           COMPUTE WS-PROC-KVANT = ART-KVANT(ART-IX) * 100 /              
012926                                                  TOT-KVANT               
012927           MOVE WS-PROC-KVANT          TO ART-PROC-KVANT(ART-IX)          
012928        END-IF                                                            
012929                                                                          
012930*******  % ÅRSFORBRUKNING AV TOTAL ÅRSFÖRBRUKNING                         
012931        IF WS-TOT-ARSFORB = ZERO                                          
012932           CONTINUE                                                       
012933        ELSE                                                              
012934           COMPUTE WS-PROC-ARSFORB = WS-ART-ARSFORB(ART-IX) * 100         
012935                                        / WS-TOT-ARSFORB                  
012936           MOVE WS-PROC-ARSFORB        TO ART-PROC-ARSFORB(ART-IX)        
012937          END-IF                                                          
012938                                                                          
012939*******  % ARSPROGNOS AV TOTAL ÅRSPROGNOS                                 
012940        IF WS-TOT-ARSPROG = ZERO                                          
012941           CONTINUE                                                       
012942        ELSE                                                              
012943           COMPUTE WS-PROC-ARSPROG = WS-ART-ARSPROG(ART-IX) * 100         
012944                                             / WS-TOT-ARSPROG             
012945           MOVE WS-PROC-ARSPROG        TO ART-PROC-ARSPROG(ART-IX)        
012946        END-IF                                                            
012947                                                                          
012948*******  % INKOMNA ORDERRADER AV TOTALA ANTALET INK ORDERRADER            
012949        IF TOT-KVINORD = ZERO                                             
012950           CONTINUE                                                       
012951        ELSE                                                              
012952           COMPUTE WS-PROC-KVINORD = ART-KVINORD(ART-IX) * 100 /          
012953                                                  TOT-KVINORD             
012954           MOVE WS-PROC-KVINORD        TO ART-PROC-KVINORD(ART-IX)        
012955        END-IF                                                            
012956                                                                          
012957*******  % EJ AVBOK ORDERADER AV TOT ANTALET EJ AVBOK ORDERRADER          
012958        IF WS-TOT-EJAVBOK = ZERO                                          
012959           CONTINUE                                                       
012960        ELSE                                                              
012961           COMPUTE WS-PROC-EJAVBOK = WS-ART-EJAVBOK(ART-IX)               
012962                                     * 100 / WS-TOT-EJAVBOK               
012963           MOVE WS-PROC-EJAVBOK TO ART-PROC-EJAVBOK(ART-IX)               
012964        END-IF                                                            
012965                                                                          
012966*******  % RADER MED FYS AVVIKELSE AV TOT ANTALET FYS AVV RADER           
012967        IF WS-TOT-KVFYSAVV = ZERO                                         
012968           CONTINUE                                                       
012969        ELSE                                                              
012970           COMPUTE WS-PROC-KVFYSAVV = WS-ART-KVFYSAVV(ART-IX)             
012971                                     * 100 / WS-TOT-KVFYSAVV              
012972           MOVE WS-PROC-KVFYSAVV TO ART-PROC-KVFYSAVV(ART-IX)             
012973        END-IF                                                            
012974                                                                          
012975*******  % SÄKERHETSLAGER AV RUTAN DISP-LAGER                             
012976        IF WS-ART-KVDISP-PR(ART-IX) = ZERO                                
012977           CONTINUE                                                       
012978        ELSE                                                              
012979           COMPUTE WS-PROC-SLAGER = WS-ART-SLAGER(ART-IX) * 100 /         
012980                                      WS-ART-KVDISP-PR(ART-IX)            
012981           MOVE WS-PROC-SLAGER TO ART-PROC-SLAGER(ART-IX)                 
012982        END-IF                                                            
012983        IF WS-ART-SLAGER(ART-IX) > WS-ART-KVDISP-PR(ART-IX)               
012984           MOVE 99.9 TO ART-PROC-SLAGER(ART-IX)                           
012985        END-IF                                                            
012986                                                                          
012987*******  % OMSLAGER AV RUTANS DISP-LAGER                                  
012988        IF WS-ART-KVDISP-PR(ART-IX) = ZERO                                
012989           CONTINUE                                                       
012990        ELSE                                                              
012991           COMPUTE WS-PROC-OLAGER = WS-ART-OLAGER(ART-IX) * 100 /         
012992                                    WS-ART-KVDISP-PR(ART-IX)              
012993           MOVE WS-PROC-OLAGER TO ART-PROC-OLAGER(ART-IX)                 
012994        END-IF                                                            
012995        IF WS-ART-OLAGER(ART-IX) > WS-ART-KVDISP-PR(ART-IX)               
012996           MOVE 99.9 TO ART-PROC-OLAGER(ART-IX)                           
012997        END-IF                                                            
012998                                                                          
012999*******  % AVVIKANDE LAGER AV RUTANS DISP-LAGER                           
013000        IF WS-ART-KVDISP-PR(ART-IX) = ZERO                                
013001           CONTINUE                                                       
013002        ELSE                                                              
013003           IF WS-ART-AVVIK(ART-IX) > ZERO                                 
013004              COMPUTE WS-PROC-AVVIK = WS-ART-AVVIK(ART-IX) * 100 /        
013005                                         WS-ART-KVDISP-PR(ART-IX)         
013006              MOVE WS-PROC-AVVIK TO ART-PROC-AVVIK(ART-IX)                
013007           END-IF                                                         
013008        END-IF                                                            
013009                                                                          
013010*******  % FÖR OMSHASTIGHET                                               
013011        IF WS-ART-KVDISP-PR(ART-IX) > ZERO                                
013012           COMPUTE WS-PROC-OMSHAST = WS-ART-KVDISP-PR(ART-IX)             
013013                                   * 100 / WS-TOT-KVDISP-PR               
013014           MOVE WS-PROC-OMSHAST       TO ART-PROC-OMSHAST(ART-IX)         
013015        END-IF                                                            
013016                                                                          
013017        ADD +1 TO ART-IX                                                  
013018     END-PERFORM                                                          
013019                                                                          
013020     MOVE +1 TO PSUM-IX                                                   
013021     MOVE +9 TO PSUM-IX-MAX                                               
013022     PERFORM UNTIL PSUM-IX > PSUM-IX-MAX                                  
013023                                                                          
013024******** % ANTAL ARTIKLAR AV TOTAL-ANTAL                                  
013025        IF TOT-KVANT = ZERO                                               
013026           CONTINUE                                                       
013027        ELSE                                                              
013028           COMPUTE WS-PROC-KVANT = PSUM-KVANT(PSUM-IX) * 100 /            
013029                                                  TOT-KVANT               
013030           MOVE WS-PROC-KVANT       TO PSUM-PROC-KVANT(PSUM-IX)           
013031        END-IF                                                            
013032                                                                          
013033*******  % ÅRSFORBRUKNING AV TOTAL ÅRSFÖRBRUKNING                         
013034        IF WS-TOT-ARSFORB = ZERO                                          
013035           CONTINUE                                                       
013036        ELSE                                                              
013037           COMPUTE WS-PROC-ARSFORB = WS-PSUM-ARSFORB(PSUM-IX)             
013038                                      * 100 / WS-TOT-ARSFORB              
013039           MOVE WS-PROC-ARSFORB           TO PSUM-PROC-ARSFORB            
013040                                          (PSUM-IX)                       
013041        END-IF                                                            
013042*******  % ARSPROGNOS AV TOTAL ÅRSPROGNOS                                 
013043        IF WS-TOT-ARSPROG = ZERO                                          
013044           CONTINUE                                                       
013045        ELSE                                                              
013046           COMPUTE WS-PROC-ARSPROG = WS-PSUM-ARSPROG(PSUM-IX)             
013047                                      * 100 /  WS-TOT-ARSPROG             
013048           MOVE WS-PROC-ARSPROG           TO PSUM-PROC-ARSPROG            
013049                                          (PSUM-IX)                       
013050        END-IF                                                            
013051                                                                          
013052*******  % INKOMNA ORDERRADER AV TOTALA ANTALET INK ORDERRADER            
013053        IF TOT-KVINORD = ZERO                                             
013054           CONTINUE                                                       
013055        ELSE                                                              
013056           COMPUTE WS-PROC-KVINORD = PSUM-KVINORD(PSUM-IX) * 100 /        
013057                                                  TOT-KVINORD             
013058           MOVE WS-PROC-KVINORD           TO PSUM-PROC-KVINORD            
013059                                           (PSUM-IX)                      
013060        END-IF                                                            
013061                                                                          
013062*******  % EJ AVBOK ORDERADER AV TOT ANTALET EJ AVBOK ORDERRADER          
013063        IF WS-TOT-EJAVBOK = ZERO                                          
013064           CONTINUE                                                       
013065        ELSE                                                              
013066           COMPUTE WS-PROC-EJAVBOK = WS-PSUM-EJAVBOK(PSUM-IX)             
013067                                    * 100 / WS-TOT-EJAVBOK                
013068           MOVE WS-PROC-EJAVBOK TO PSUM-PROC-EJAVBOK(PSUM-IX)             
013069        END-IF                                                            
013070                                                                          
013071*******  % RADER MED FYS AVVIKELSE AV TOT ANTALET FYS AVV RADER           
013072        IF WS-TOT-KVFYSAVV = ZERO                                         
013073           CONTINUE                                                       
013074        ELSE                                                              
013075           COMPUTE WS-PROC-KVFYSAVV = WS-PSUM-KVFYSAVV(PSUM-IX)           
013076                                        * 100 / WS-TOT-KVFYSAVV           
013077           MOVE WS-PROC-KVFYSAVV TO PSUM-PROC-KVFYSAVV(PSUM-IX)           
013078       END-IF                                                             
013079                                                                          
013080*******  % SÄKERHETSLAGER AV PRISKLASSENS DISP-LAGER                      
013081        IF WS-PSUM-KVDISP-PR(PSUM-IX) = ZERO                              
013082           CONTINUE                                                       
013083        ELSE                                                              
013084           COMPUTE WS-PROC-SLAGER = WS-PSUM-SLAGER(PSUM-IX) * 100         
013085                                     / WS-PSUM-KVDISP-PR(PSUM-IX)         
013086           MOVE WS-PROC-SLAGER TO PSUM-PROC-SLAGER(PSUM-IX)               
013087        END-IF                                                            
013088        IF WS-PSUM-SLAGER(PSUM-IX) > WS-PSUM-KVDISP-PR(PSUM-IX)           
013089           MOVE 99.9 TO PSUM-PROC-SLAGER(PSUM-IX)                         
013090        END-IF                                                            
013091                                                                          
013092*******  % OMSLAGER AV PRISKLASSENS DISP-LAGER                            
013093        IF WS-PSUM-KVDISP-PR(PSUM-IX) = ZERO                              
013094           CONTINUE                                                       
013095        ELSE                                                              
013096           COMPUTE WS-PROC-OLAGER = WS-PSUM-OLAGER(PSUM-IX) * 100         
013097                                      / WS-PSUM-KVDISP-PR(PSUM-IX)        
013098           MOVE WS-PROC-OLAGER TO PSUM-PROC-OLAGER(PSUM-IX)               
013099        END-IF                                                            
013100        IF WS-PSUM-OLAGER(PSUM-IX) > WS-PSUM-KVDISP-PR(PSUM-IX)           
013101           MOVE 99.9 TO PSUM-PROC-OLAGER(PSUM-IX)                         
013102        END-IF                                                            
013103                                                                          
013104*******  % AVVIKANDE LAGER AV PRISKLASSENS DISP-LAGER                     
013105        IF WS-PSUM-KVDISP-PR(PSUM-IX) = ZERO                              
013106           CONTINUE                                                       
013107        ELSE                                                              
013108           IF WS-PSUM-AVVIK(PSUM-IX) > ZERO                               
013109              COMPUTE WS-PROC-AVVIK = WS-PSUM-AVVIK(PSUM-IX) * 100        
013110                                          / WS-PSUM-KVDISP-PR             
013111                                          (PSUM-IX)                       
013112              MOVE WS-PROC-AVVIK           TO PSUM-PROC-AVVIK             
013113                                              (PSUM-IX)                   
013114           END-IF                                                         
013115        END-IF                                                            
013116                                                                          
013117*******  % FÖR OMSHASTIGHET                                               
013118        IF WS-TOT-KVDISP-PR = ZERO                                        
013119           CONTINUE                                                       
013120        ELSE                                                              
013121           COMPUTE WS-PROC-OMSHAST = WS-PSUM-KVDISP-PR(PSUM-IX)           
013122                                   * 100 / WS-TOT-KVDISP-PR               
013123           MOVE WS-PROC-OMSHAST     TO PSUM-PROC-OMSHAST(PSUM-IX)         
013124        END-IF                                                            
013125                                                                          
013126        ADD +1 TO PSUM-IX                                                 
013127     END-PERFORM                                                          
013128                                                                          
013129     MOVE +1  TO FSUM-IX                                                  
013130     MOVE +10 TO FSUM-IX-MAX                                              
013131     PERFORM UNTIL FSUM-IX > FSUM-IX-MAX                                  
013132                                                                          
013133******** % ANTAL ARTIKLAR AV TOTAL-ANTAL                                  
013134        IF TOT-KVANT = ZERO                                               
013135           CONTINUE                                                       
013136        ELSE                                                              
013137           COMPUTE WS-PROC-KVANT = FSUM-KVANT(FSUM-IX) * 100 /            
013138                                                  TOT-KVANT               
013139           MOVE WS-PROC-KVANT             TO FSUM-PROC-KVANT              
013140                                                  (FSUM-IX)               
013141        END-IF                                                            
013142                                                                          
013143*******  % ÅRSFORBRUKNING AV TOTAL ÅRSFÖRBRUKNING                         
013144        IF WS-TOT-ARSFORB = ZERO                                          
013145           CONTINUE                                                       
013146        ELSE                                                              
013147           COMPUTE WS-PROC-ARSFORB = WS-FSUM-ARSFORB(FSUM-IX)             
013148                                   * 100 / WS-TOT-ARSFORB                 
013149           MOVE WS-PROC-ARSFORB TO FSUM-PROC-ARSFORB(FSUM-IX)             
013150        END-IF                                                            
013151                                                                          
013152*******  % ARSPROGNOS AV TOTAL ÅRSPROGNOS                                 
013153        IF WS-TOT-ARSPROG = ZERO                                          
013154           CONTINUE                                                       
013155        ELSE                                                              
013156           COMPUTE WS-PROC-ARSPROG = WS-FSUM-ARSPROG(FSUM-IX)             
013157                                     * 100 /  WS-TOT-ARSPROG              
013158           MOVE WS-PROC-ARSPROG TO FSUM-PROC-ARSPROG(FSUM-IX)             
013159        END-IF                                                            
013160                                                                          
013161*******  % INKOMNA ORDERRADER AV TOTALA ANTALET INK ORDERRADER            
013162        IF TOT-KVINORD = ZERO                                             
013163           CONTINUE                                                       
013164        ELSE                                                              
013165           COMPUTE WS-PROC-KVINORD = FSUM-KVINORD(FSUM-IX) * 100 /        
013166                                                  TOT-KVINORD             
013167           MOVE WS-PROC-KVINORD           TO FSUM-PROC-KVINORD            
013168                                              (FSUM-IX)                   
013169        END-IF                                                            
013170                                                                          
013171*******  % EJ AVBOK ORDERADER AV TOT ANTALET EJ AVBOK ORDERRADER          
013172        IF WS-TOT-EJAVBOK = ZERO                                          
013173           CONTINUE                                                       
013174        ELSE                                                              
013175           COMPUTE WS-PROC-EJAVBOK = WS-FSUM-EJAVBOK(FSUM-IX)             
013176                                * 100 / WS-TOT-EJAVBOK                    
013177           MOVE WS-PROC-EJAVBOK TO FSUM-PROC-EJAVBOK(FSUM-IX)             
013178        END-IF                                                            
013179                                                                          
013180*******  % RADER MED FYS AVVIKELSE AV TOT ANTALET FYS AVV RADER           
013181        IF WS-TOT-KVFYSAVV = ZERO                                         
013182           CONTINUE                                                       
013183        ELSE                                                              
013184           COMPUTE WS-PROC-KVFYSAVV = WS-FSUM-KVFYSAVV(FSUM-IX)           
013185                                      * 100 / WS-TOT-KVFYSAVV             
013186           MOVE WS-PROC-KVFYSAVV TO FSUM-PROC-KVFYSAVV(FSUM-IX)           
013187        END-IF                                                            
013188                                                                          
013189*******  % SÄKERHETSLAGER AV FREKVENS-KLASSENS DISP-LAGER                 
013190        IF WS-FSUM-KVDISP-PR(FSUM-IX) = ZERO                              
013191           CONTINUE                                                       
013192        ELSE                                                              
013193           COMPUTE WS-PROC-SLAGER = WS-FSUM-SLAGER(FSUM-IX) * 100         
013194                                     / WS-FSUM-KVDISP-PR(FSUM-IX)         
013195           MOVE WS-PROC-SLAGER TO FSUM-PROC-SLAGER(FSUM-IX)               
013196        END-IF                                                            
013197        IF WS-FSUM-SLAGER(FSUM-IX) > WS-FSUM-KVDISP-PR(FSUM-IX)           
013198           MOVE 99.9 TO FSUM-PROC-SLAGER(FSUM-IX)                         
013199        END-IF                                                            
013200                                                                          
013201*******  % OMSLAGER AV FREKVENS-KLASSENS DISP-LAGER                       
013202        IF WS-FSUM-KVDISP-PR(FSUM-IX) = ZERO                              
013203           CONTINUE                                                       
013204        ELSE                                                              
013205           COMPUTE WS-PROC-OLAGER = WS-FSUM-OLAGER(FSUM-IX) * 100         
013206                                      / WS-FSUM-KVDISP-PR(FSUM-IX)        
013207           MOVE WS-PROC-OLAGER TO FSUM-PROC-OLAGER(FSUM-IX)               
013208        END-IF                                                            
013209        IF WS-FSUM-OLAGER(FSUM-IX) > WS-FSUM-KVDISP-PR(FSUM-IX)           
013210           MOVE 99.9 TO FSUM-PROC-OLAGER(FSUM-IX)                         
013211        END-IF                                                            
013212                                                                          
013213*******  % AVVIKANDE LAGER AV FREKVENS-KLASSENS DISP-LAGER                
013214        IF WS-FSUM-KVDISP-PR(FSUM-IX) = ZERO                              
013215           CONTINUE                                                       
013216        ELSE                                                              
013217           IF WS-FSUM-AVVIK(FSUM-IX) > ZERO                               
013218             COMPUTE WS-PROC-AVVIK = WS-FSUM-AVVIK(FSUM-IX) * 100         
013219                                 / WS-FSUM-KVDISP-PR(FSUM-IX)             
013220             MOVE WS-PROC-AVVIK           TO FSUM-PROC-AVVIK              
013221                                             (FSUM-IX)                    
013222           END-IF                                                         
013223        END-IF                                                            
013224                                                                          
013225*******  % FÖR OMSHASTIGHET                                               
013226        IF WS-TOT-KVDISP-PR = ZERO                                        
013227           CONTINUE                                                       
013228        ELSE                                                              
013229           COMPUTE WS-PROC-OMSHAST = WS-FSUM-KVDISP-PR(FSUM-IX)           
013230                                   * 100 / WS-TOT-KVDISP-PR               
013231           MOVE WS-PROC-OMSHAST       TO FSUM-PROC-OMSHAST                
013232                                         (FSUM-IX)                        
013233        END-IF                                                            
013234                                                                          
013235        ADD +1 TO FSUM-IX                                                 
013236     END-PERFORM                                                          
013237     .                                                                    
013238     EJECT                                                                
013239 BD-KOLLA-URVAL SECTION.                                                  
013240                                                                          
013241     MOVE NEJ TO SW-TRAEFF                                                
013242                                                                          
013243     IF IN-KATEGORI NOT < PARM-KATEGORI-FOM AND                           
013244        IN-KATEGORI NOT > PARM-KATEGORI-TOM                               
013245           MOVE JA TO SW-TRAEFF                                           
013246     END-IF                                                               
013247                                                                          
013248     IF SW-TRAEFF = JA                                                    
013249       IF IN-IDLEVNR = PARM-IDLEVNR                                       
013250       OR PARM-IDLEVNR = ZERO                                             
013251          MOVE JA TO SW-TRAEFF                                            
013252       ELSE                                                               
013253          MOVE NEJ TO SW-TRAEFF                                           
013254       END-IF                                                             
013255     END-IF                                                               
013256                                                                          
013257     IF SW-TRAEFF = JA                                                    
013258       IF IN-KDPRODSL NOT < PARM-IN-KDPRODSL-FOM AND                      
013259          IN-KDPRODSL NOT > PARM-IN-KDPRODSL-TOM                          
013260             MOVE JA TO SW-TRAEFF                                         
013261       ELSE                                                               
013262          MOVE NEJ TO SW-TRAEFF                                           
013263       END-IF                                                             
013264     END-IF                                                               
013265                                                                          
013266     IF SW-TRAEFF = JA                                                    
013267       IF IN-IDANSK NOT < PARM-IN-IDANSK-FOM AND                          
013268          IN-IDANSK NOT > PARM-IN-IDANSK-TOM                              
013269             MOVE JA TO SW-TRAEFF                                         
013270       ELSE                                                               
013271          MOVE NEJ TO SW-TRAEFF                                           
013272       END-IF                                                             
013273     END-IF                                                               
013274     .                                                                    
013275     EJECT                                                                
013276 C-SKRIV-LISTA SECTION.                                                   
013277******************************************************************        
013278*  SID 1 BESTÅR AV 4 RUTRADER INKL PRISKLASS-TOTAL               *        
013279*      2           3 RUTRADER INKL PRISKLASS-TOTAL               *        
013280*      3           2 RUTRADER INKL PRISKLASS-TOTAL               *        
013281*                  1 RUTRADER FREKVENS-TOTAL OCH TOTAL-TOTAL     *        
013282******************************************************************        
013283                                                                          
013284     PERFORM CC-FIXA-RUBRIKER                                             
013285                                                                          
013286     MOVE +1  TO IX1                                                      
013287     MOVE +2  TO IX2                                                      
013288     MOVE +3  TO IX3                                                      
013289     MOVE +4  TO IX4                                                      
013290     MOVE +5  TO IX5                                                      
013291     MOVE +6  TO IX6                                                      
013292     MOVE +7  TO IX7                                                      
013293     MOVE +8  TO IX8                                                      
013294     MOVE +9  TO IX9                                                      
013295     MOVE +10 TO IX10                                                     
013296     MOVE +1  TO PSUM-IX                                                  
013297                                                                          
013298*********** SKRIVER SID-1                                                 
013299     PERFORM S21A-SKRIV-RUBRIKER                                          
013300     MOVE '1' TO W001-DET1-PRISKLASS                                      
013301     PERFORM CA-FLYTTA-SKRIV-RAD                                          
013302                                                                          
013303     ADD +10  TO IX1 IX2 IX3 IX4 IX5 IX6 IX7 IX8 IX9 IX10                 
013304     ADD +1   TO PSUM-IX                                                  
013305     MOVE '2' TO W001-DET1-PRISKLASS                                      
013306     PERFORM CA-FLYTTA-SKRIV-RAD                                          
013307                                                                          
013308     ADD +10  TO IX1 IX2 IX3 IX4 IX5 IX6 IX7 IX8 IX9 IX10                 
013309     ADD +1   TO PSUM-IX                                                  
013310     MOVE '3' TO W001-DET1-PRISKLASS                                      
013311     PERFORM CA-FLYTTA-SKRIV-RAD                                          
013312                                                                          
013313     ADD +10  TO IX1 IX2 IX3 IX4 IX5 IX6 IX7 IX8 IX9 IX10                 
013314     ADD +1   TO PSUM-IX                                                  
013315     MOVE '4' TO W001-DET1-PRISKLASS                                      
013316     PERFORM CA-FLYTTA-SKRIV-RAD                                          
013317                                                                          
013318*********** SKRIVER SID-2                                                 
013319     PERFORM S21A-SKRIV-RUBRIKER                                          
013320     ADD +1   TO PSUM-IX                                                  
013321     ADD +10  TO IX1 IX2 IX3 IX4 IX5 IX6 IX7 IX8 IX9 IX10                 
013322     MOVE '5' TO W001-DET1-PRISKLASS                                      
013323     PERFORM CA-FLYTTA-SKRIV-RAD                                          
013324                                                                          
013325     ADD +1   TO PSUM-IX                                                  
013326     ADD +10  TO IX1 IX2 IX3 IX4 IX5 IX6 IX7 IX8 IX9 IX10                 
013327     MOVE '6' TO W001-DET1-PRISKLASS                                      
013328     PERFORM CA-FLYTTA-SKRIV-RAD                                          
013329                                                                          
013330     ADD +1   TO PSUM-IX                                                  
013331     ADD +10  TO IX1 IX2 IX3 IX4 IX5 IX6 IX7 IX8 IX9 IX10                 
013332     MOVE '7' TO W001-DET1-PRISKLASS                                      
013333     PERFORM CA-FLYTTA-SKRIV-RAD                                          
013334                                                                          
013335*********** SKRIVER SID-3                                                 
013336     PERFORM S21A-SKRIV-RUBRIKER                                          
013337     ADD +1   TO PSUM-IX                                                  
013338     ADD +10  TO IX1 IX2 IX3 IX4 IX5 IX6 IX7 IX8 IX9 IX10                 
013339     MOVE '8' TO W001-DET1-PRISKLASS                                      
013340     PERFORM CA-FLYTTA-SKRIV-RAD                                          
013341                                                                          
013342     ADD +1   TO PSUM-IX                                                  
013343     ADD +10  TO IX1 IX2 IX3 IX4 IX5 IX6 IX7 IX8 IX9 IX10                 
013344     MOVE '9' TO W001-DET1-PRISKLASS                                      
013345     PERFORM CA-FLYTTA-SKRIV-RAD                                          
013346                                                                          
013347     MOVE +1  TO IX1                                                      
013348     MOVE +2  TO IX2                                                      
013349     MOVE +3  TO IX3                                                      
013350     MOVE +4  TO IX4                                                      
013351     MOVE +5  TO IX5                                                      
013352     MOVE +6  TO IX6                                                      
013353     MOVE +7  TO IX7                                                      
013354     MOVE +8  TO IX8                                                      
013355     MOVE +9  TO IX9                                                      
013356     MOVE +10 TO IX10                                                     
013357     MOVE SPACE TO W001-DET1-PRISKLASS                                    
013358     PERFORM CB-FLYTTA-SKRIV-TOT                                          
013359     .                                                                    
013360     EJECT                                                                
013361 CA-FLYTTA-SKRIV-RAD SECTION.                                             
013362                                                                          
013363     MOVE ART-KVANT(IX1)           TO  W001-DET1-KVANTA                   
013364     MOVE ART-PROC-KVANT(IX1)      TO  W001-DET1-P-KVANTA                 
013365     MOVE ART-KVANT(IX2)           TO  W001-DET1-KVANTB                   
013366     MOVE ART-PROC-KVANT(IX2)      TO  W001-DET1-P-KVANTB                 
013367     MOVE ART-KVANT(IX3)           TO  W001-DET1-KVANTC                   
013368     MOVE ART-PROC-KVANT(IX3)      TO  W001-DET1-P-KVANTC                 
013369     MOVE ART-KVANT(IX4)           TO  W001-DET1-KVANTD                   
013370     MOVE ART-PROC-KVANT(IX4)      TO  W001-DET1-P-KVANTD                 
013371     MOVE ART-KVANT(IX5)           TO  W001-DET1-KVANTE                   
013372     MOVE ART-PROC-KVANT(IX5)      TO  W001-DET1-P-KVANTE                 
013373     MOVE ART-KVANT(IX6)           TO  W001-DET1-KVANTF                   
013374     MOVE ART-PROC-KVANT(IX6)      TO  W001-DET1-P-KVANTF                 
013375     MOVE ART-KVANT(IX7)           TO  W001-DET1-KVANTG                   
013376     MOVE ART-PROC-KVANT(IX7)      TO  W001-DET1-P-KVANTG                 
013377     MOVE ART-KVANT(IX8)           TO  W001-DET1-KVANTH                   
013378     MOVE ART-PROC-KVANT(IX8)      TO  W001-DET1-P-KVANTH                 
013379     MOVE ART-KVANT(IX9)           TO  W001-DET1-KVANTI                   
013380     MOVE ART-PROC-KVANT(IX9)      TO  W001-DET1-P-KVANTI                 
013381     MOVE ART-KVANT(IX10)          TO  W001-DET1-KVANTJ                   
013382     MOVE ART-PROC-KVANT(IX10)     TO  W001-DET1-P-KVANTJ                 
013383     MOVE PSUM-KVANT(PSUM-IX)      TO  W001-DET1-TOT                      
013384     MOVE PSUM-PROC-KVANT(PSUM-IX) TO  W001-DET1-P-TOT                    
013385     MOVE W001-DETALJRAD-1 TO W001-RAD                                    
013386     MOVE +2 TO W001-SKIP                                                 
013387     PERFORM S21-SKRIV-LISTA                                              
013388                                                                          
013389     MOVE ART-ARSFORB(IX1)         TO  W001-DET2-ARSFORBA                 
013390     MOVE ART-PROC-ARSFORB(IX1)    TO  W001-DET2-P-ARSFORBA               
013391     MOVE ART-ARSFORB(IX2)         TO  W001-DET2-ARSFORBB                 
013392     MOVE ART-PROC-ARSFORB(IX2)    TO  W001-DET2-P-ARSFORBB               
013393     MOVE ART-ARSFORB(IX3)         TO  W001-DET2-ARSFORBC                 
013394     MOVE ART-PROC-ARSFORB(IX3)    TO  W001-DET2-P-ARSFORBC               
013395     MOVE ART-ARSFORB(IX4)         TO  W001-DET2-ARSFORBD                 
013396     MOVE ART-PROC-ARSFORB(IX4)    TO  W001-DET2-P-ARSFORBD               
013397     MOVE ART-ARSFORB(IX5)         TO  W001-DET2-ARSFORBE                 
013398     MOVE ART-PROC-ARSFORB(IX5)    TO  W001-DET2-P-ARSFORBE               
013399     MOVE ART-ARSFORB(IX6)         TO  W001-DET2-ARSFORBF                 
013400     MOVE ART-PROC-ARSFORB(IX6)    TO  W001-DET2-P-ARSFORBF               
013401     MOVE ART-ARSFORB(IX7)         TO  W001-DET2-ARSFORBG                 
013402     MOVE ART-PROC-ARSFORB(IX7)    TO  W001-DET2-P-ARSFORBG               
013403     MOVE ART-ARSFORB(IX8)         TO  W001-DET2-ARSFORBH                 
013404     MOVE ART-PROC-ARSFORB(IX8)    TO  W001-DET2-P-ARSFORBH               
013405     MOVE ART-ARSFORB(IX9)         TO  W001-DET2-ARSFORBI                 
013406     MOVE ART-PROC-ARSFORB(IX9)    TO  W001-DET2-P-ARSFORBI               
013407     MOVE ART-ARSFORB(IX10)        TO  W001-DET2-ARSFORBJ                 
013408     MOVE ART-PROC-ARSFORB(IX10)   TO  W001-DET2-P-ARSFORBJ               
013409     MOVE PSUM-ARSFORB(PSUM-IX)    TO  W001-DET2-TOT                      
013410     MOVE PSUM-PROC-ARSFORB                                               
013411          (PSUM-IX)                TO  W001-DET2-P-TOT                    
013412     MOVE W001-DETALJRAD-2 TO W001-RAD                                    
013413     MOVE +1 TO W001-SKIP                                                 
013414     PERFORM S21-SKRIV-LISTA                                              
013415                                                                          
013416     MOVE ART-ARSPROG(IX1)         TO  W001-DET3-ARSPROGA                 
013417     MOVE ART-PROC-ARSPROG(IX1)    TO  W001-DET3-P-ARSPROGA               
013418     MOVE ART-ARSPROG(IX2)         TO  W001-DET3-ARSPROGB                 
013419     MOVE ART-PROC-ARSPROG(IX2)    TO  W001-DET3-P-ARSPROGB               
013420     MOVE ART-ARSPROG(IX3)         TO  W001-DET3-ARSPROGC                 
013421     MOVE ART-PROC-ARSPROG(IX3)    TO  W001-DET3-P-ARSPROGC               
013422     MOVE ART-ARSPROG(IX4)         TO  W001-DET3-ARSPROGD                 
013423     MOVE ART-PROC-ARSPROG(IX4)    TO  W001-DET3-P-ARSPROGD               
013424     MOVE ART-ARSPROG(IX5)         TO  W001-DET3-ARSPROGE                 
013425     MOVE ART-PROC-ARSPROG(IX5)    TO  W001-DET3-P-ARSPROGE               
013426     MOVE ART-ARSPROG(IX6)         TO  W001-DET3-ARSPROGF                 
013427     MOVE ART-PROC-ARSPROG(IX6)    TO  W001-DET3-P-ARSPROGF               
013428     MOVE ART-ARSPROG(IX7)         TO  W001-DET3-ARSPROGG                 
013429     MOVE ART-PROC-ARSPROG(IX7)    TO  W001-DET3-P-ARSPROGG               
013430     MOVE ART-ARSPROG(IX8)         TO  W001-DET3-ARSPROGH                 
013431     MOVE ART-PROC-ARSPROG(IX8)    TO  W001-DET3-P-ARSPROGH               
013432     MOVE ART-ARSPROG(IX9)         TO  W001-DET3-ARSPROGI                 
013433     MOVE ART-PROC-ARSPROG(IX9)    TO  W001-DET3-P-ARSPROGI               
013434     MOVE ART-ARSPROG(IX10)        TO  W001-DET3-ARSPROGJ                 
013435     MOVE ART-PROC-ARSPROG(IX10)   TO  W001-DET3-P-ARSPROGJ               
013436     MOVE PSUM-ARSPROG(PSUM-IX)    TO  W001-DET3-TOT                      
013437     MOVE PSUM-PROC-ARSPROG                                               
013438          (PSUM-IX)                TO  W001-DET3-P-TOT                    
013439     MOVE W001-DETALJRAD-3 TO W001-RAD                                    
013440     MOVE +1 TO W001-SKIP                                                 
013441     PERFORM S21-SKRIV-LISTA                                              
013442                                                                          
013443     MOVE ART-SLAGER(IX1)          TO  W001-DET4-SLAGERA                  
013444     MOVE ART-PROC-SLAGER(IX1)     TO  W001-DET4-P-SLAGERA                
013445     MOVE ART-SLAGER(IX2)          TO  W001-DET4-SLAGERB                  
013446     MOVE ART-PROC-SLAGER(IX2)     TO  W001-DET4-P-SLAGERB                
013447     MOVE ART-SLAGER(IX3)          TO  W001-DET4-SLAGERC                  
013448     MOVE ART-PROC-SLAGER(IX3)     TO  W001-DET4-P-SLAGERC                
013449     MOVE ART-SLAGER(IX4)          TO  W001-DET4-SLAGERD                  
013450     MOVE ART-PROC-SLAGER(IX4)     TO  W001-DET4-P-SLAGERD                
013451     MOVE ART-SLAGER(IX5)          TO  W001-DET4-SLAGERE                  
013452     MOVE ART-PROC-SLAGER(IX5)     TO  W001-DET4-P-SLAGERE                
013453     MOVE ART-SLAGER(IX6)          TO  W001-DET4-SLAGERF                  
013454     MOVE ART-PROC-SLAGER(IX6)     TO  W001-DET4-P-SLAGERF                
013455     MOVE ART-SLAGER(IX7)          TO  W001-DET4-SLAGERG                  
013456     MOVE ART-PROC-SLAGER(IX7)     TO  W001-DET4-P-SLAGERG                
013457     MOVE ART-SLAGER(IX8)          TO  W001-DET4-SLAGERH                  
013458     MOVE ART-PROC-SLAGER(IX8)     TO  W001-DET4-P-SLAGERH                
013459     MOVE ART-SLAGER(IX9)          TO  W001-DET4-SLAGERI                  
013460     MOVE ART-PROC-SLAGER(IX9)     TO  W001-DET4-P-SLAGERI                
013461     MOVE ART-SLAGER(IX10)         TO  W001-DET4-SLAGERJ                  
013462     MOVE ART-PROC-SLAGER(IX10)    TO  W001-DET4-P-SLAGERJ                
013463     MOVE PSUM-SLAGER(PSUM-IX)     TO  W001-DET4-TOT                      
013464     MOVE PSUM-PROC-SLAGER                                                
013465          (PSUM-IX)                TO  W001-DET4-P-TOT                    
013466     MOVE W001-DETALJRAD-4 TO W001-RAD                                    
013467     MOVE +1 TO W001-SKIP                                                 
013468     PERFORM S21-SKRIV-LISTA                                              
013469                                                                          
013470     MOVE ART-OLAGER(IX1)          TO  W001-DET5-OLAGERA                  
013471     MOVE ART-PROC-OLAGER(IX1)     TO  W001-DET5-P-OLAGERA                
013472     MOVE ART-OLAGER(IX2)          TO  W001-DET5-OLAGERB                  
013473     MOVE ART-PROC-OLAGER(IX2)     TO  W001-DET5-P-OLAGERB                
013474     MOVE ART-OLAGER(IX3)          TO  W001-DET5-OLAGERC                  
013475     MOVE ART-PROC-OLAGER(IX3)     TO  W001-DET5-P-OLAGERC                
013476     MOVE ART-OLAGER(IX4)          TO  W001-DET5-OLAGERD                  
013477     MOVE ART-PROC-OLAGER(IX4)     TO  W001-DET5-P-OLAGERD                
013478     MOVE ART-OLAGER(IX5)          TO  W001-DET5-OLAGERE                  
013479     MOVE ART-PROC-OLAGER(IX5)     TO  W001-DET5-P-OLAGERE                
013480     MOVE ART-OLAGER(IX6)          TO  W001-DET5-OLAGERF                  
013481     MOVE ART-PROC-OLAGER(IX6)     TO  W001-DET5-P-OLAGERF                
013482     MOVE ART-OLAGER(IX7)          TO  W001-DET5-OLAGERG                  
013483     MOVE ART-PROC-OLAGER(IX7)     TO  W001-DET5-P-OLAGERG                
013484     MOVE ART-OLAGER(IX8)          TO  W001-DET5-OLAGERH                  
013485     MOVE ART-PROC-OLAGER(IX8)     TO  W001-DET5-P-OLAGERH                
013486     MOVE ART-OLAGER(IX9)          TO  W001-DET5-OLAGERI                  
013487     MOVE ART-PROC-OLAGER(IX9)     TO  W001-DET5-P-OLAGERI                
013488     MOVE ART-OLAGER(IX10)         TO  W001-DET5-OLAGERJ                  
013489     MOVE ART-PROC-OLAGER(IX10)    TO  W001-DET5-P-OLAGERJ                
013490     MOVE PSUM-OLAGER(PSUM-IX)     TO  W001-DET5-TOT                      
013491     MOVE PSUM-PROC-OLAGER                                                
013492          (PSUM-IX)                TO  W001-DET5-P-TOT                    
013493     MOVE W001-DETALJRAD-5 TO W001-RAD                                    
013494     MOVE +1 TO W001-SKIP                                                 
013495     PERFORM S21-SKRIV-LISTA                                              
013496                                                                          
013497     MOVE ART-AVVIK(IX1)           TO  W001-DET6-AVVIKA                   
013498     MOVE ART-PROC-AVVIK(IX1)      TO  W001-DET6-P-AVVIKA                 
013499     MOVE ART-AVVIK(IX2)           TO  W001-DET6-AVVIKB                   
013500     MOVE ART-PROC-AVVIK(IX2)      TO  W001-DET6-P-AVVIKB                 
013501     MOVE ART-AVVIK(IX3)           TO  W001-DET6-AVVIKC                   
013502     MOVE ART-PROC-AVVIK(IX3)      TO  W001-DET6-P-AVVIKC                 
013503     MOVE ART-AVVIK(IX4)           TO  W001-DET6-AVVIKD                   
013504     MOVE ART-PROC-AVVIK(IX4)      TO  W001-DET6-P-AVVIKD                 
013505     MOVE ART-AVVIK(IX5)           TO  W001-DET6-AVVIKE                   
013506     MOVE ART-PROC-AVVIK(IX5)      TO  W001-DET6-P-AVVIKE                 
013507     MOVE ART-AVVIK(IX6)           TO  W001-DET6-AVVIKF                   
013508     MOVE ART-PROC-AVVIK(IX6)      TO  W001-DET6-P-AVVIKF                 
013509     MOVE ART-AVVIK(IX7)           TO  W001-DET6-AVVIKG                   
013510     MOVE ART-PROC-AVVIK(IX7)      TO  W001-DET6-P-AVVIKG                 
013511     MOVE ART-AVVIK(IX8)           TO  W001-DET6-AVVIKH                   
013512     MOVE ART-PROC-AVVIK(IX8)      TO  W001-DET6-P-AVVIKH                 
013513     MOVE ART-AVVIK(IX9)           TO  W001-DET6-AVVIKI                   
013514     MOVE ART-PROC-AVVIK(IX9)      TO  W001-DET6-P-AVVIKI                 
013515     MOVE ART-AVVIK(IX10)          TO  W001-DET6-AVVIKJ                   
013516     MOVE ART-PROC-AVVIK(IX10)     TO  W001-DET6-P-AVVIKJ                 
013517     MOVE PSUM-AVVIK(PSUM-IX)      TO  W001-DET6-TOT                      
013518     MOVE PSUM-PROC-AVVIK                                                 
013519          (PSUM-IX)                TO  W001-DET6-P-TOT                    
013520     MOVE W001-DETALJRAD-6 TO W001-RAD                                    
013521     MOVE +1 TO W001-SKIP                                                 
013522     PERFORM S21-SKRIV-LISTA                                              
013523                                                                          
013524     MOVE ART-KVINORD(IX1)         TO  W001-DET7-KVINORDA                 
013525     MOVE ART-PROC-KVINORD(IX1)    TO  W001-DET7-P-KVINORDA               
013526     MOVE ART-KVINORD(IX2)         TO  W001-DET7-KVINORDB                 
013527     MOVE ART-PROC-KVINORD(IX2)    TO  W001-DET7-P-KVINORDB               
013528     MOVE ART-KVINORD(IX3)         TO  W001-DET7-KVINORDC                 
013529     MOVE ART-PROC-KVINORD(IX3)    TO  W001-DET7-P-KVINORDC               
013530     MOVE ART-KVINORD(IX4)         TO  W001-DET7-KVINORDD                 
013531     MOVE ART-PROC-KVINORD(IX4)    TO  W001-DET7-P-KVINORDD               
013532     MOVE ART-KVINORD(IX5)         TO  W001-DET7-KVINORDE                 
013533     MOVE ART-PROC-KVINORD(IX5)    TO  W001-DET7-P-KVINORDE               
013534     MOVE ART-KVINORD(IX6)         TO  W001-DET7-KVINORDF                 
013535     MOVE ART-PROC-KVINORD(IX6)    TO  W001-DET7-P-KVINORDF               
013536     MOVE ART-KVINORD(IX7)         TO  W001-DET7-KVINORDG                 
013537     MOVE ART-PROC-KVINORD(IX7)    TO  W001-DET7-P-KVINORDG               
013538     MOVE ART-KVINORD(IX8)         TO  W001-DET7-KVINORDH                 
013539     MOVE ART-PROC-KVINORD(IX8)    TO  W001-DET7-P-KVINORDH               
013540     MOVE ART-KVINORD(IX9)         TO  W001-DET7-KVINORDI                 
013541     MOVE ART-PROC-KVINORD(IX9)    TO  W001-DET7-P-KVINORDI               
013542     MOVE ART-KVINORD(IX10)        TO  W001-DET7-KVINORDJ                 
013543     MOVE ART-PROC-KVINORD(IX10)   TO  W001-DET7-P-KVINORDJ               
013544     MOVE PSUM-KVINORD(PSUM-IX)    TO  W001-DET7-TOT                      
013545     MOVE PSUM-PROC-KVINORD                                               
013546          (PSUM-IX)                TO  W001-DET7-P-TOT                    
013547     MOVE W001-DETALJRAD-7 TO W001-RAD                                    
013548     MOVE +1 TO W001-SKIP                                                 
013549     PERFORM S21-SKRIV-LISTA                                              
013550                                                                          
013551     MOVE ART-EJAVBOK(IX1)         TO  W001-DET8-EJAVBOKA                 
013552     MOVE ART-PROC-EJAVBOK(IX1)    TO  W001-DET8-P-EJAVBOKA               
013553     MOVE ART-EJAVBOK(IX2)         TO  W001-DET8-EJAVBOKB                 
013554     MOVE ART-PROC-EJAVBOK(IX2)    TO  W001-DET8-P-EJAVBOKB               
013555     MOVE ART-EJAVBOK(IX3)         TO  W001-DET8-EJAVBOKC                 
013556     MOVE ART-PROC-EJAVBOK(IX3)    TO  W001-DET8-P-EJAVBOKC               
013557     MOVE ART-EJAVBOK(IX4)         TO  W001-DET8-EJAVBOKD                 
013558     MOVE ART-PROC-EJAVBOK(IX4)    TO  W001-DET8-P-EJAVBOKD               
013559     MOVE ART-EJAVBOK(IX5)         TO  W001-DET8-EJAVBOKE                 
013560     MOVE ART-PROC-EJAVBOK(IX5)    TO  W001-DET8-P-EJAVBOKE               
013561     MOVE ART-EJAVBOK(IX6)         TO  W001-DET8-EJAVBOKF                 
013562     MOVE ART-PROC-EJAVBOK(IX6)    TO  W001-DET8-P-EJAVBOKF               
013563     MOVE ART-EJAVBOK(IX7)         TO  W001-DET8-EJAVBOKG                 
013564     MOVE ART-PROC-EJAVBOK(IX7)    TO  W001-DET8-P-EJAVBOKG               
013565     MOVE ART-EJAVBOK(IX8)         TO  W001-DET8-EJAVBOKH                 
013566     MOVE ART-PROC-EJAVBOK(IX8)    TO  W001-DET8-P-EJAVBOKH               
013567     MOVE ART-EJAVBOK(IX9)         TO  W001-DET8-EJAVBOKI                 
013568     MOVE ART-PROC-EJAVBOK(IX9)    TO  W001-DET8-P-EJAVBOKI               
013569     MOVE ART-EJAVBOK(IX10)        TO  W001-DET8-EJAVBOKJ                 
013570     MOVE ART-PROC-EJAVBOK(IX10)   TO  W001-DET8-P-EJAVBOKJ               
013571     MOVE PSUM-EJAVBOK(PSUM-IX)    TO  W001-DET8-TOT                      
013572     MOVE PSUM-PROC-EJAVBOK                                               
013573          (PSUM-IX)                TO  W001-DET8-P-TOT                    
013574     MOVE W001-DETALJRAD-8 TO W001-RAD                                    
013575     MOVE +1 TO W001-SKIP                                                 
013576     PERFORM S21-SKRIV-LISTA                                              
013577                                                                          
013578     MOVE ART-KVFYSAVV(IX1)        TO  W001-DET9-KVFYSAVVA                
013579     MOVE ART-PROC-KVFYSAVV(IX1)   TO  W001-DET9-P-KVFYSAVVA              
013580     MOVE ART-KVFYSAVV(IX2)        TO  W001-DET9-KVFYSAVVB                
013581     MOVE ART-PROC-KVFYSAVV(IX2)   TO  W001-DET9-P-KVFYSAVVB              
013582     MOVE ART-KVFYSAVV(IX3)        TO  W001-DET9-KVFYSAVVC                
013583     MOVE ART-PROC-KVFYSAVV(IX3)   TO  W001-DET9-P-KVFYSAVVC              
013584     MOVE ART-KVFYSAVV(IX4)        TO  W001-DET9-KVFYSAVVD                
013585     MOVE ART-PROC-KVFYSAVV(IX4)   TO  W001-DET9-P-KVFYSAVVD              
013586     MOVE ART-KVFYSAVV(IX5)        TO  W001-DET9-KVFYSAVVE                
013587     MOVE ART-PROC-KVFYSAVV(IX5)   TO  W001-DET9-P-KVFYSAVVE              
013588     MOVE ART-KVFYSAVV(IX6)        TO  W001-DET9-KVFYSAVVF                
013589     MOVE ART-PROC-KVFYSAVV(IX6)   TO  W001-DET9-P-KVFYSAVVF              
013590     MOVE ART-KVFYSAVV(IX7)        TO  W001-DET9-KVFYSAVVG                
013591     MOVE ART-PROC-KVFYSAVV(IX7)   TO  W001-DET9-P-KVFYSAVVG              
013592     MOVE ART-KVFYSAVV(IX8)        TO  W001-DET9-KVFYSAVVH                
013593     MOVE ART-PROC-KVFYSAVV(IX8)   TO  W001-DET9-P-KVFYSAVVH              
013594     MOVE ART-KVFYSAVV(IX9)        TO  W001-DET9-KVFYSAVVI                
013595     MOVE ART-PROC-KVFYSAVV(IX9)   TO  W001-DET9-P-KVFYSAVVI              
013596     MOVE ART-KVFYSAVV(IX10)       TO  W001-DET9-KVFYSAVVJ                
013597     MOVE ART-PROC-KVFYSAVV(IX10)  TO  W001-DET9-P-KVFYSAVVJ              
013598     MOVE PSUM-KVFYSAVV(PSUM-IX)   TO  W001-DET9-TOT                      
013599     MOVE PSUM-PROC-KVFYSAVV                                              
013600          (PSUM-IX)                TO  W001-DET9-P-TOT                    
013601     MOVE W001-DETALJRAD-9 TO W001-RAD                                    
013602     MOVE +1 TO W001-SKIP                                                 
013603     PERFORM S21-SKRIV-LISTA                                              
013604                                                                          
013605     MOVE ART-OMSHAST(IX1)         TO  W001-DET10-OMSHASTA                
013606     MOVE ART-PROC-OMSHAST(IX1)    TO  W001-DET10-P-OMSHASTA              
013607     MOVE ART-OMSHAST(IX2)         TO  W001-DET10-OMSHASTB                
013608     MOVE ART-PROC-OMSHAST(IX2)    TO  W001-DET10-P-OMSHASTB              
013609     MOVE ART-OMSHAST(IX3)         TO  W001-DET10-OMSHASTC                
013610     MOVE ART-PROC-OMSHAST(IX3)    TO  W001-DET10-P-OMSHASTC              
013611     MOVE ART-OMSHAST(IX4)         TO  W001-DET10-OMSHASTD                
013612     MOVE ART-PROC-OMSHAST(IX4)    TO  W001-DET10-P-OMSHASTD              
013613     MOVE ART-OMSHAST(IX5)         TO  W001-DET10-OMSHASTE                
013614     MOVE ART-PROC-OMSHAST(IX5)    TO  W001-DET10-P-OMSHASTE              
013615     MOVE ART-OMSHAST(IX6)         TO  W001-DET10-OMSHASTF                
013616     MOVE ART-PROC-OMSHAST(IX6)    TO  W001-DET10-P-OMSHASTF              
013617     MOVE ART-OMSHAST(IX7)         TO  W001-DET10-OMSHASTG                
013618     MOVE ART-PROC-OMSHAST(IX7)    TO  W001-DET10-P-OMSHASTG              
013619     MOVE ART-OMSHAST(IX8)         TO  W001-DET10-OMSHASTH                
013620     MOVE ART-PROC-OMSHAST(IX8)    TO  W001-DET10-P-OMSHASTH              
013621     MOVE ART-OMSHAST(IX9)         TO  W001-DET10-OMSHASTI                
013622     MOVE ART-PROC-OMSHAST(IX9)    TO  W001-DET10-P-OMSHASTI              
013623     MOVE ART-OMSHAST(IX10)        TO  W001-DET10-OMSHASTJ                
013624     MOVE ART-PROC-OMSHAST(IX10)   TO  W001-DET10-P-OMSHASTJ              
013625     MOVE PSUM-OMSHAST(PSUM-IX)    TO  W001-DET10-TOT                     
013626     MOVE PSUM-PROC-OMSHAST                                               
013627          (PSUM-IX)                TO  W001-DET10-P-TOT                   
013628     MOVE W001-DETALJRAD-10 TO W001-RAD                                   
013629     MOVE +1 TO W001-SKIP                                                 
013630     PERFORM S21-SKRIV-LISTA                                              
013631                                                                          
013632     MOVE ART-TACKTID(IX1)         TO  W001-DET11-TACKTIDA                
013633     MOVE ART-PROC-TACKTID(IX1)    TO  W001-DET11-P-TACKTIDA              
013634     MOVE ART-TACKTID(IX2)         TO  W001-DET11-TACKTIDB                
013635     MOVE ART-PROC-TACKTID(IX2)    TO  W001-DET11-P-TACKTIDB              
013636     MOVE ART-TACKTID(IX3)         TO  W001-DET11-TACKTIDC                
013637     MOVE ART-PROC-TACKTID(IX3)    TO  W001-DET11-P-TACKTIDC              
013638     MOVE ART-TACKTID(IX4)         TO  W001-DET11-TACKTIDD                
013639     MOVE ART-PROC-TACKTID(IX4)    TO  W001-DET11-P-TACKTIDD              
013640     MOVE ART-TACKTID(IX5)         TO  W001-DET11-TACKTIDE                
013641     MOVE ART-PROC-TACKTID(IX5)    TO  W001-DET11-P-TACKTIDE              
013642     MOVE ART-TACKTID(IX6)         TO  W001-DET11-TACKTIDF                
013643     MOVE ART-PROC-TACKTID(IX6)    TO  W001-DET11-P-TACKTIDF              
013644     MOVE ART-TACKTID(IX7)         TO  W001-DET11-TACKTIDG                
013645     MOVE ART-PROC-TACKTID(IX7)    TO  W001-DET11-P-TACKTIDG              
013646     MOVE ART-TACKTID(IX8)         TO  W001-DET11-TACKTIDH                
013647     MOVE ART-PROC-TACKTID(IX8)    TO  W001-DET11-P-TACKTIDH              
013648     MOVE ART-TACKTID(IX9)         TO  W001-DET11-TACKTIDI                
013649     MOVE ART-PROC-TACKTID(IX9)    TO  W001-DET11-P-TACKTIDI              
013650     MOVE ART-TACKTID(IX10)        TO  W001-DET11-TACKTIDJ                
013651     MOVE ART-PROC-TACKTID(IX10)   TO  W001-DET11-P-TACKTIDJ              
013652     MOVE PSUM-TACKTID(PSUM-IX)    TO  W001-DET11-TOT                     
013653     MOVE PSUM-PROC-TACKTID                                               
013654          (PSUM-IX)                TO  W001-DET11-P-TOT                   
013655     MOVE W001-DETALJRAD-11 TO W001-RAD                                   
013656     MOVE +1 TO W001-SKIP                                                 
013657     PERFORM S21-SKRIV-LISTA                                              
013658                                                                          
013659     MOVE ART-SERVG-NTO(IX1)       TO  W001-DET12-SERVGA-NTO              
013660     MOVE ART-SERVG-BTO(IX1)       TO  W001-DET12-SERVGA-BTO              
013661     MOVE ART-SERVG-NTO(IX2)       TO  W001-DET12-SERVGB-NTO              
013662     MOVE ART-SERVG-BTO(IX2)       TO  W001-DET12-SERVGB-BTO              
013663     MOVE ART-SERVG-NTO(IX3)       TO  W001-DET12-SERVGC-NTO              
013664     MOVE ART-SERVG-BTO(IX3)       TO  W001-DET12-SERVGC-BTO              
013665     MOVE ART-SERVG-NTO(IX4)       TO  W001-DET12-SERVGD-NTO              
013666     MOVE ART-SERVG-BTO(IX4)       TO  W001-DET12-SERVGD-BTO              
013667     MOVE ART-SERVG-NTO(IX5)       TO  W001-DET12-SERVGE-NTO              
013668     MOVE ART-SERVG-BTO(IX5)       TO  W001-DET12-SERVGE-BTO              
013669     MOVE ART-SERVG-NTO(IX6)       TO  W001-DET12-SERVGF-NTO              
013670     MOVE ART-SERVG-BTO(IX6)       TO  W001-DET12-SERVGF-BTO              
013671     MOVE ART-SERVG-NTO(IX7)       TO  W001-DET12-SERVGG-NTO              
013672     MOVE ART-SERVG-BTO(IX7)       TO  W001-DET12-SERVGG-BTO              
013673     MOVE ART-SERVG-NTO(IX8)       TO  W001-DET12-SERVGH-NTO              
013674     MOVE ART-SERVG-BTO(IX8)       TO  W001-DET12-SERVGH-BTO              
013675     MOVE ART-SERVG-NTO(IX9)       TO  W001-DET12-SERVGI-NTO              
013676     MOVE ART-SERVG-BTO(IX9)       TO  W001-DET12-SERVGI-BTO              
013677     MOVE ART-SERVG-NTO(IX10)      TO  W001-DET12-SERVGJ-NTO              
013678     MOVE ART-SERVG-BTO(IX10)      TO  W001-DET12-SERVGJ-BTO              
013679     MOVE PSUM-SERVG-NTO (PSUM-IX) TO  W001-DET12-TOT                     
013680     MOVE PSUM-SERVG-BTO                                                  
013681          (PSUM-IX)                TO  W001-DET12-P-TOT                   
013682     MOVE W001-DETALJRAD-12 TO W001-RAD                                   
013683     MOVE +1 TO W001-SKIP                                                 
013684     PERFORM S21-SKRIV-LISTA                                              
013685     .                                                                    
013686     EJECT                                                                
013687 CB-FLYTTA-SKRIV-TOT SECTION.                                             
013688                                                                          
013689     MOVE FSUM-KVANT(IX1)          TO  W001-DET1-KVANTA                   
013690     MOVE FSUM-PROC-KVANT(IX1)     TO  W001-DET1-P-KVANTA                 
013691     MOVE FSUM-KVANT(IX2)          TO  W001-DET1-KVANTB                   
013692     MOVE FSUM-PROC-KVANT(IX2)     TO  W001-DET1-P-KVANTB                 
013693     MOVE FSUM-KVANT(IX3)          TO  W001-DET1-KVANTC                   
013694     MOVE FSUM-PROC-KVANT(IX3)     TO  W001-DET1-P-KVANTC                 
013695     MOVE FSUM-KVANT(IX4)          TO  W001-DET1-KVANTD                   
013696     MOVE FSUM-PROC-KVANT(IX4)     TO  W001-DET1-P-KVANTD                 
013697     MOVE FSUM-KVANT(IX5)          TO  W001-DET1-KVANTE                   
013698     MOVE FSUM-PROC-KVANT(IX5)     TO  W001-DET1-P-KVANTE                 
013699     MOVE FSUM-KVANT(IX6)          TO  W001-DET1-KVANTF                   
013700     MOVE FSUM-PROC-KVANT(IX6)     TO  W001-DET1-P-KVANTF                 
013701     MOVE FSUM-KVANT(IX7)          TO  W001-DET1-KVANTG                   
013702     MOVE FSUM-PROC-KVANT(IX7)     TO  W001-DET1-P-KVANTG                 
013703     MOVE FSUM-KVANT(IX8)          TO  W001-DET1-KVANTH                   
013704     MOVE FSUM-PROC-KVANT(IX8)     TO  W001-DET1-P-KVANTH                 
013705     MOVE FSUM-KVANT(IX9)          TO  W001-DET1-KVANTI                   
013706     MOVE FSUM-PROC-KVANT(IX9)     TO  W001-DET1-P-KVANTI                 
013707     MOVE FSUM-KVANT(IX10)         TO  W001-DET1-KVANTJ                   
013708     MOVE FSUM-PROC-KVANT(IX10)    TO  W001-DET1-P-KVANTJ                 
013709     MOVE TOT-KVANT                TO  W001-DET1-TOT                      
013710     MOVE ZERO                     TO  W001-DET1-P-TOT                    
013711     MOVE W001-DETALJRAD-1 TO W001-RAD                                    
013712     MOVE +2 TO W001-SKIP                                                 
013713     PERFORM S21-SKRIV-LISTA                                              
013714                                                                          
013715     MOVE FSUM-ARSFORB(IX1)        TO  W001-DET2-ARSFORBA                 
013716     MOVE FSUM-PROC-ARSFORB(IX1)   TO  W001-DET2-P-ARSFORBA               
013717     MOVE FSUM-ARSFORB(IX2)        TO  W001-DET2-ARSFORBB                 
013718     MOVE FSUM-PROC-ARSFORB(IX2)   TO  W001-DET2-P-ARSFORBB               
013719     MOVE FSUM-ARSFORB(IX3)        TO  W001-DET2-ARSFORBC                 
013720     MOVE FSUM-PROC-ARSFORB(IX3)   TO  W001-DET2-P-ARSFORBC               
013721     MOVE FSUM-ARSFORB(IX4)        TO  W001-DET2-ARSFORBD                 
013722     MOVE FSUM-PROC-ARSFORB(IX4)   TO  W001-DET2-P-ARSFORBD               
013723     MOVE FSUM-ARSFORB(IX5)        TO  W001-DET2-ARSFORBE                 
013724     MOVE FSUM-PROC-ARSFORB(IX5)   TO  W001-DET2-P-ARSFORBE               
013725     MOVE FSUM-ARSFORB(IX6)        TO  W001-DET2-ARSFORBF                 
013726     MOVE FSUM-PROC-ARSFORB(IX6)   TO  W001-DET2-P-ARSFORBF               
013727     MOVE FSUM-ARSFORB(IX7)        TO  W001-DET2-ARSFORBG                 
013728     MOVE FSUM-PROC-ARSFORB(IX7)   TO  W001-DET2-P-ARSFORBG               
013729     MOVE FSUM-ARSFORB(IX8)        TO  W001-DET2-ARSFORBH                 
013730     MOVE FSUM-PROC-ARSFORB(IX8)   TO  W001-DET2-P-ARSFORBH               
013731     MOVE FSUM-ARSFORB(IX9)        TO  W001-DET2-ARSFORBI                 
013732     MOVE FSUM-PROC-ARSFORB(IX9)   TO  W001-DET2-P-ARSFORBI               
013733     MOVE FSUM-ARSFORB(IX10)       TO  W001-DET2-ARSFORBJ                 
013734     MOVE FSUM-PROC-ARSFORB(IX10)  TO  W001-DET2-P-ARSFORBJ               
013735     MOVE TOT-ARSFORB              TO  W001-DET2-TOT                      
013736     MOVE ZERO                     TO  W001-DET2-P-TOT                    
013737     MOVE W001-DETALJRAD-2  TO W001-RAD                                   
013738     MOVE +1 TO W001-SKIP                                                 
013739     PERFORM S21-SKRIV-LISTA                                              
013740                                                                          
013741     MOVE FSUM-ARSPROG(IX1)        TO  W001-DET3-ARSPROGA                 
013742     MOVE FSUM-PROC-ARSPROG(IX1)   TO  W001-DET3-P-ARSPROGA               
013743     MOVE FSUM-ARSPROG(IX2)        TO  W001-DET3-ARSPROGB                 
013744     MOVE FSUM-PROC-ARSPROG(IX2)   TO  W001-DET3-P-ARSPROGB               
013745     MOVE FSUM-ARSPROG(IX3)        TO  W001-DET3-ARSPROGC                 
013746     MOVE FSUM-PROC-ARSPROG(IX3)   TO  W001-DET3-P-ARSPROGC               
013747     MOVE FSUM-ARSPROG(IX4)        TO  W001-DET3-ARSPROGD                 
013748     MOVE FSUM-PROC-ARSPROG(IX4)   TO  W001-DET3-P-ARSPROGD               
013749     MOVE FSUM-ARSPROG(IX5)        TO  W001-DET3-ARSPROGE                 
013750     MOVE FSUM-PROC-ARSPROG(IX5)   TO  W001-DET3-P-ARSPROGE               
013751     MOVE FSUM-ARSPROG(IX6)        TO  W001-DET3-ARSPROGF                 
013752     MOVE FSUM-PROC-ARSPROG(IX6)   TO  W001-DET3-P-ARSPROGF               
013753     MOVE FSUM-ARSPROG(IX7)        TO  W001-DET3-ARSPROGG                 
013754     MOVE FSUM-PROC-ARSPROG(IX7)   TO  W001-DET3-P-ARSPROGG               
013755     MOVE FSUM-ARSPROG(IX8)        TO  W001-DET3-ARSPROGH                 
013756     MOVE FSUM-PROC-ARSPROG(IX8)   TO  W001-DET3-P-ARSPROGH               
013757     MOVE FSUM-ARSPROG(IX9)        TO  W001-DET3-ARSPROGI                 
013758     MOVE FSUM-PROC-ARSPROG(IX9)   TO  W001-DET3-P-ARSPROGI               
013759     MOVE FSUM-ARSPROG(IX10)       TO  W001-DET3-ARSPROGJ                 
013760     MOVE FSUM-PROC-ARSPROG(IX10)  TO  W001-DET3-P-ARSPROGJ               
013761     MOVE TOT-ARSPROG              TO  W001-DET3-TOT                      
013762     MOVE ZERO                     TO W001-DET3-P-TOT                     
013763     MOVE W001-DETALJRAD-3  TO W001-RAD                                   
013764     MOVE +1 TO W001-SKIP                                                 
013765     PERFORM S21-SKRIV-LISTA                                              
013766                                                                          
013767     MOVE FSUM-SLAGER(IX1)         TO  W001-DET4-SLAGERA                  
013768     MOVE FSUM-PROC-SLAGER(IX1)    TO  W001-DET4-P-SLAGERA                
013769     MOVE FSUM-SLAGER(IX2)         TO  W001-DET4-SLAGERB                  
013770     MOVE FSUM-PROC-SLAGER(IX2)    TO  W001-DET4-P-SLAGERB                
013771     MOVE FSUM-SLAGER(IX3)         TO  W001-DET4-SLAGERC                  
013772     MOVE FSUM-PROC-SLAGER(IX3)    TO  W001-DET4-P-SLAGERC                
013773     MOVE FSUM-SLAGER(IX4)         TO  W001-DET4-SLAGERD                  
013774     MOVE FSUM-PROC-SLAGER(IX4)    TO  W001-DET4-P-SLAGERD                
013775     MOVE FSUM-SLAGER(IX5)         TO  W001-DET4-SLAGERE                  
013776     MOVE FSUM-PROC-SLAGER(IX5)    TO  W001-DET4-P-SLAGERE                
013777     MOVE FSUM-SLAGER(IX6)         TO  W001-DET4-SLAGERF                  
013778     MOVE FSUM-PROC-SLAGER(IX6)    TO  W001-DET4-P-SLAGERF                
013779     MOVE FSUM-SLAGER(IX7)         TO  W001-DET4-SLAGERG                  
013780     MOVE FSUM-PROC-SLAGER(IX7)    TO  W001-DET4-P-SLAGERG                
013781     MOVE FSUM-SLAGER(IX8)         TO  W001-DET4-SLAGERH                  
013782     MOVE FSUM-PROC-SLAGER(IX8)    TO  W001-DET4-P-SLAGERH                
013783     MOVE FSUM-SLAGER(IX9)         TO  W001-DET4-SLAGERI                  
013784     MOVE FSUM-PROC-SLAGER(IX9)    TO  W001-DET4-P-SLAGERI                
013785     MOVE FSUM-SLAGER(IX10)        TO  W001-DET4-SLAGERJ                  
013786     MOVE FSUM-PROC-SLAGER(IX10)   TO  W001-DET4-P-SLAGERJ                
013787     MOVE TOT-SLAGER               TO  W001-DET4-TOT                      
013788     MOVE TOT-PROC-SLAGER          TO  W001-DET4-P-TOT                    
013789     MOVE W001-DETALJRAD-4 TO W001-RAD                                    
013790     MOVE +1 TO W001-SKIP                                                 
013791     PERFORM S21-SKRIV-LISTA                                              
013792                                                                          
013793     MOVE FSUM-OLAGER(IX1)         TO  W001-DET5-OLAGERA                  
013794     MOVE FSUM-PROC-OLAGER(IX1)    TO  W001-DET5-P-OLAGERA                
013795     MOVE FSUM-OLAGER(IX2)         TO  W001-DET5-OLAGERB                  
013796     MOVE FSUM-PROC-OLAGER(IX2)    TO  W001-DET5-P-OLAGERB                
013797     MOVE FSUM-OLAGER(IX3)         TO  W001-DET5-OLAGERC                  
013798     MOVE FSUM-PROC-OLAGER(IX3)    TO  W001-DET5-P-OLAGERC                
013799     MOVE FSUM-OLAGER(IX4)         TO  W001-DET5-OLAGERD                  
013800     MOVE FSUM-PROC-OLAGER(IX4)    TO  W001-DET5-P-OLAGERD                
013801     MOVE FSUM-OLAGER(IX5)         TO  W001-DET5-OLAGERE                  
013802     MOVE FSUM-PROC-OLAGER(IX5)    TO  W001-DET5-P-OLAGERE                
013803     MOVE FSUM-OLAGER(IX6)         TO  W001-DET5-OLAGERF                  
013804     MOVE FSUM-PROC-OLAGER(IX6)    TO  W001-DET5-P-OLAGERF                
013805     MOVE FSUM-OLAGER(IX7)         TO  W001-DET5-OLAGERG                  
013806     MOVE FSUM-PROC-OLAGER(IX7)    TO  W001-DET5-P-OLAGERG                
013807     MOVE FSUM-OLAGER(IX8)         TO  W001-DET5-OLAGERH                  
013808     MOVE FSUM-PROC-OLAGER(IX8)    TO  W001-DET5-P-OLAGERH                
013809     MOVE FSUM-OLAGER(IX9)         TO  W001-DET5-OLAGERI                  
013810     MOVE FSUM-PROC-OLAGER(IX9)    TO  W001-DET5-P-OLAGERI                
013811     MOVE FSUM-OLAGER(IX10)        TO  W001-DET5-OLAGERJ                  
013812     MOVE FSUM-PROC-OLAGER(IX10)   TO  W001-DET5-P-OLAGERJ                
013813     MOVE TOT-OLAGER               TO  W001-DET5-TOT                      
013814     MOVE TOT-PROC-OLAGER          TO  W001-DET5-P-TOT                    
013815     MOVE W001-DETALJRAD-5 TO W001-RAD                                    
013816     MOVE +1 TO W001-SKIP                                                 
013817     PERFORM S21-SKRIV-LISTA                                              
013818                                                                          
013819     MOVE FSUM-AVVIK(IX1)          TO  W001-DET6-AVVIKA                   
013820     MOVE FSUM-PROC-AVVIK(IX1)     TO  W001-DET6-P-AVVIKA                 
013821     MOVE FSUM-AVVIK(IX2)          TO  W001-DET6-AVVIKB                   
013822     MOVE FSUM-PROC-AVVIK(IX2)     TO  W001-DET6-P-AVVIKB                 
013823     MOVE FSUM-AVVIK(IX3)          TO  W001-DET6-AVVIKC                   
013824     MOVE FSUM-PROC-AVVIK(IX3)     TO  W001-DET6-P-AVVIKC                 
013825     MOVE FSUM-AVVIK(IX4)          TO  W001-DET6-AVVIKD                   
013826     MOVE FSUM-PROC-AVVIK(IX4)     TO  W001-DET6-P-AVVIKD                 
013827     MOVE FSUM-AVVIK(IX5)          TO  W001-DET6-AVVIKE                   
013828     MOVE FSUM-PROC-AVVIK(IX5)     TO  W001-DET6-P-AVVIKE                 
013829     MOVE FSUM-AVVIK(IX6)          TO  W001-DET6-AVVIKF                   
013830     MOVE FSUM-PROC-AVVIK(IX6)     TO  W001-DET6-P-AVVIKF                 
013831     MOVE FSUM-AVVIK(IX7)          TO  W001-DET6-AVVIKG                   
013832     MOVE FSUM-PROC-AVVIK(IX7)     TO  W001-DET6-P-AVVIKG                 
013833     MOVE FSUM-AVVIK(IX8)          TO  W001-DET6-AVVIKH                   
013834     MOVE FSUM-PROC-AVVIK(IX8)     TO  W001-DET6-P-AVVIKH                 
013835     MOVE FSUM-AVVIK(IX9)          TO  W001-DET6-AVVIKI                   
013836     MOVE FSUM-PROC-AVVIK(IX9)     TO  W001-DET6-P-AVVIKI                 
013837     MOVE FSUM-AVVIK(IX10)         TO  W001-DET6-AVVIKJ                   
013838     MOVE FSUM-PROC-AVVIK(IX10)    TO  W001-DET6-P-AVVIKJ                 
013839     MOVE TOT-AVVIK                TO  W001-DET6-TOT                      
013840     MOVE TOT-PROC-AVVIK           TO  W001-DET6-P-TOT                    
013841     MOVE W001-DETALJRAD-6 TO W001-RAD                                    
013842     MOVE +1 TO W001-SKIP                                                 
013843     PERFORM S21-SKRIV-LISTA                                              
013844                                                                          
013845     MOVE FSUM-KVINORD(IX1)        TO  W001-DET7-KVINORDA                 
013846     MOVE FSUM-PROC-KVINORD(IX1)   TO  W001-DET7-P-KVINORDA               
013847     MOVE FSUM-KVINORD(IX2)        TO  W001-DET7-KVINORDB                 
013848     MOVE FSUM-PROC-KVINORD(IX2)   TO  W001-DET7-P-KVINORDB               
013849     MOVE FSUM-KVINORD(IX3)        TO  W001-DET7-KVINORDC                 
013850     MOVE FSUM-PROC-KVINORD(IX3)   TO  W001-DET7-P-KVINORDC               
013851     MOVE FSUM-KVINORD(IX4)        TO  W001-DET7-KVINORDD                 
013852     MOVE FSUM-PROC-KVINORD(IX4)   TO  W001-DET7-P-KVINORDD               
013853     MOVE FSUM-KVINORD(IX5)        TO  W001-DET7-KVINORDE                 
013854     MOVE FSUM-PROC-KVINORD(IX5)   TO  W001-DET7-P-KVINORDE               
013855     MOVE FSUM-KVINORD(IX6)        TO  W001-DET7-KVINORDF                 
013856     MOVE FSUM-PROC-KVINORD(IX6)   TO  W001-DET7-P-KVINORDF               
013857     MOVE FSUM-KVINORD(IX7)        TO  W001-DET7-KVINORDG                 
013858     MOVE FSUM-PROC-KVINORD(IX7)   TO  W001-DET7-P-KVINORDG               
013859     MOVE FSUM-KVINORD(IX8)        TO  W001-DET7-KVINORDH                 
013860     MOVE FSUM-PROC-KVINORD(IX8)   TO  W001-DET7-P-KVINORDH               
013861     MOVE FSUM-KVINORD(IX9)        TO  W001-DET7-KVINORDI                 
013862     MOVE FSUM-PROC-KVINORD(IX9)   TO  W001-DET7-P-KVINORDI               
013863     MOVE FSUM-KVINORD(IX10)       TO  W001-DET7-KVINORDJ                 
013864     MOVE FSUM-PROC-KVINORD(IX10)  TO  W001-DET7-P-KVINORDJ               
013865     MOVE TOT-KVINORD              TO  W001-DET7-TOT                      
013866     MOVE ZERO                     TO  W001-DET7-P-TOT                    
013867     MOVE W001-DETALJRAD-7 TO W001-RAD                                    
013868     MOVE +1 TO W001-SKIP                                                 
013869     PERFORM S21-SKRIV-LISTA                                              
013870                                                                          
013871     MOVE FSUM-EJAVBOK(IX1)        TO  W001-DET8-EJAVBOKA                 
013872     MOVE FSUM-PROC-EJAVBOK(IX1)   TO  W001-DET8-P-EJAVBOKA               
013873     MOVE FSUM-EJAVBOK(IX2)        TO  W001-DET8-EJAVBOKB                 
013874     MOVE FSUM-PROC-EJAVBOK(IX2)   TO  W001-DET8-P-EJAVBOKB               
013875     MOVE FSUM-EJAVBOK(IX3)        TO  W001-DET8-EJAVBOKC                 
013876     MOVE FSUM-PROC-EJAVBOK(IX3)   TO  W001-DET8-P-EJAVBOKC               
013877     MOVE FSUM-EJAVBOK(IX4)        TO  W001-DET8-EJAVBOKD                 
013878     MOVE FSUM-PROC-EJAVBOK(IX4)   TO  W001-DET8-P-EJAVBOKD               
013879     MOVE FSUM-EJAVBOK(IX5)        TO  W001-DET8-EJAVBOKE                 
013880     MOVE FSUM-PROC-EJAVBOK(IX5)   TO  W001-DET8-P-EJAVBOKE               
013881     MOVE FSUM-EJAVBOK(IX6)        TO  W001-DET8-EJAVBOKF                 
013882     MOVE FSUM-PROC-EJAVBOK(IX6)   TO  W001-DET8-P-EJAVBOKF               
013883     MOVE FSUM-EJAVBOK(IX7)        TO  W001-DET8-EJAVBOKG                 
013884     MOVE FSUM-PROC-EJAVBOK(IX7)   TO  W001-DET8-P-EJAVBOKG               
013885     MOVE FSUM-EJAVBOK(IX8)        TO  W001-DET8-EJAVBOKH                 
013886     MOVE FSUM-PROC-EJAVBOK(IX8)   TO  W001-DET8-P-EJAVBOKH               
013887     MOVE FSUM-EJAVBOK(IX9)        TO  W001-DET8-EJAVBOKI                 
013888     MOVE FSUM-PROC-EJAVBOK(IX9)   TO  W001-DET8-P-EJAVBOKI               
013889     MOVE FSUM-EJAVBOK(IX10)       TO  W001-DET8-EJAVBOKJ                 
013890     MOVE FSUM-PROC-EJAVBOK(IX10)  TO  W001-DET8-P-EJAVBOKJ               
013891     MOVE TOT-EJAVBOK              TO  W001-DET8-TOT                      
013892     MOVE ZERO                     TO  W001-DET8-P-TOT                    
013893     MOVE W001-DETALJRAD-8 TO W001-RAD                                    
013894     MOVE +1 TO W001-SKIP                                                 
013895     PERFORM S21-SKRIV-LISTA                                              
013896                                                                          
013897     MOVE FSUM-KVFYSAVV(IX1)       TO  W001-DET9-KVFYSAVVA                
013898     MOVE FSUM-PROC-KVFYSAVV(IX1)  TO  W001-DET9-P-KVFYSAVVA              
013899     MOVE FSUM-KVFYSAVV(IX2)       TO  W001-DET9-KVFYSAVVB                
013900     MOVE FSUM-PROC-KVFYSAVV(IX2)  TO  W001-DET9-P-KVFYSAVVB              
013901     MOVE FSUM-KVFYSAVV(IX3)       TO  W001-DET9-KVFYSAVVC                
013902     MOVE FSUM-PROC-KVFYSAVV(IX3)  TO  W001-DET9-P-KVFYSAVVC              
013903     MOVE FSUM-KVFYSAVV(IX4)       TO  W001-DET9-KVFYSAVVD                
013904     MOVE FSUM-PROC-KVFYSAVV(IX4)  TO  W001-DET9-P-KVFYSAVVD              
013905     MOVE FSUM-KVFYSAVV(IX5)       TO  W001-DET9-KVFYSAVVE                
013906     MOVE FSUM-PROC-KVFYSAVV(IX5)  TO  W001-DET9-P-KVFYSAVVE              
013907     MOVE FSUM-KVFYSAVV(IX6)       TO  W001-DET9-KVFYSAVVF                
013908     MOVE FSUM-PROC-KVFYSAVV(IX6)  TO  W001-DET9-P-KVFYSAVVF              
013909     MOVE FSUM-KVFYSAVV(IX7)       TO  W001-DET9-KVFYSAVVG                
013910     MOVE FSUM-PROC-KVFYSAVV(IX7)  TO  W001-DET9-P-KVFYSAVVG              
013911     MOVE FSUM-KVFYSAVV(IX8)       TO  W001-DET9-KVFYSAVVH                
013912     MOVE FSUM-PROC-KVFYSAVV(IX8)  TO  W001-DET9-P-KVFYSAVVH              
013913     MOVE FSUM-KVFYSAVV(IX9)       TO  W001-DET9-KVFYSAVVI                
013914     MOVE FSUM-PROC-KVFYSAVV(IX9)  TO  W001-DET9-P-KVFYSAVVI              
013915     MOVE FSUM-KVFYSAVV(IX10)      TO  W001-DET9-KVFYSAVVJ                
013916     MOVE FSUM-PROC-KVFYSAVV(IX10) TO  W001-DET9-P-KVFYSAVVJ              
013917     MOVE TOT-KVFYSAVV             TO  W001-DET9-TOT                      
013918     MOVE ZERO                     TO  W001-DET9-P-TOT                    
013919     MOVE W001-DETALJRAD-9 TO W001-RAD                                    
013920     MOVE +1 TO W001-SKIP                                                 
013921     PERFORM S21-SKRIV-LISTA                                              
013922                                                                          
013923     MOVE FSUM-OMSHAST(IX1)        TO  W001-DET10-OMSHASTA                
013924     MOVE FSUM-PROC-OMSHAST(IX1)   TO  W001-DET10-P-OMSHASTA              
013925     MOVE FSUM-OMSHAST(IX2)        TO  W001-DET10-OMSHASTB                
013926     MOVE FSUM-PROC-OMSHAST(IX2)   TO  W001-DET10-P-OMSHASTB              
013927     MOVE FSUM-OMSHAST(IX3)        TO  W001-DET10-OMSHASTC                
013928     MOVE FSUM-PROC-OMSHAST(IX3)   TO  W001-DET10-P-OMSHASTC              
013929     MOVE FSUM-OMSHAST(IX4)        TO  W001-DET10-OMSHASTD                
013930     MOVE FSUM-PROC-OMSHAST(IX4)   TO  W001-DET10-P-OMSHASTD              
013931     MOVE FSUM-OMSHAST(IX5)        TO  W001-DET10-OMSHASTE                
013932     MOVE FSUM-PROC-OMSHAST(IX5)   TO  W001-DET10-P-OMSHASTE              
013933     MOVE FSUM-OMSHAST(IX6)        TO  W001-DET10-OMSHASTF                
013934     MOVE FSUM-PROC-OMSHAST(IX6)   TO  W001-DET10-P-OMSHASTF              
013935     MOVE FSUM-OMSHAST(IX7)        TO  W001-DET10-OMSHASTG                
013936     MOVE FSUM-PROC-OMSHAST(IX7)   TO  W001-DET10-P-OMSHASTG              
013937     MOVE FSUM-OMSHAST(IX8)        TO  W001-DET10-OMSHASTH                
013938     MOVE FSUM-PROC-OMSHAST(IX8)   TO  W001-DET10-P-OMSHASTH              
013939     MOVE FSUM-OMSHAST(IX9)        TO  W001-DET10-OMSHASTI                
013940     MOVE FSUM-PROC-OMSHAST(IX9)   TO  W001-DET10-P-OMSHASTI              
013941     MOVE FSUM-OMSHAST(IX10)       TO  W001-DET10-OMSHASTJ                
013942     MOVE FSUM-PROC-OMSHAST(IX10)  TO  W001-DET10-P-OMSHASTJ              
013943     MOVE TOT-OMSHAST              TO  W001-DET10-TOT                     
013944     MOVE ZERO                     TO  W001-DET10-P-TOT                   
013945     MOVE W001-DETALJRAD-10 TO W001-RAD                                   
013946     MOVE +1 TO W001-SKIP                                                 
013947     PERFORM S21-SKRIV-LISTA                                              
013948                                                                          
013949     MOVE FSUM-TACKTID(IX1)        TO  W001-DET11-TACKTIDA                
013950     MOVE FSUM-PROC-TACKTID(IX1)   TO  W001-DET11-P-TACKTIDA              
013951     MOVE FSUM-TACKTID(IX2)        TO  W001-DET11-TACKTIDB                
013952     MOVE FSUM-PROC-TACKTID(IX2)   TO  W001-DET11-P-TACKTIDB              
013953     MOVE FSUM-TACKTID(IX3)        TO  W001-DET11-TACKTIDC                
013954     MOVE FSUM-PROC-TACKTID(IX3)   TO  W001-DET11-P-TACKTIDC              
013955     MOVE FSUM-TACKTID(IX4)        TO  W001-DET11-TACKTIDD                
013956     MOVE FSUM-PROC-TACKTID(IX4)   TO  W001-DET11-P-TACKTIDD              
013957     MOVE FSUM-TACKTID(IX5)        TO  W001-DET11-TACKTIDE                
013958     MOVE FSUM-PROC-TACKTID(IX5)   TO  W001-DET11-P-TACKTIDE              
013959     MOVE FSUM-TACKTID(IX6)        TO  W001-DET11-TACKTIDF                
013960     MOVE FSUM-PROC-TACKTID(IX6)   TO  W001-DET11-P-TACKTIDF              
013961     MOVE FSUM-TACKTID(IX7)        TO  W001-DET11-TACKTIDG                
013962     MOVE FSUM-PROC-TACKTID(IX7)   TO  W001-DET11-P-TACKTIDG              
013963     MOVE FSUM-TACKTID(IX8)        TO  W001-DET11-TACKTIDH                
013964     MOVE FSUM-PROC-TACKTID(IX8)   TO  W001-DET11-P-TACKTIDH              
013965     MOVE FSUM-TACKTID(IX9)        TO  W001-DET11-TACKTIDI                
013966     MOVE FSUM-PROC-TACKTID(IX9)   TO  W001-DET11-P-TACKTIDI              
013967     MOVE FSUM-TACKTID(IX10)       TO  W001-DET11-TACKTIDJ                
013968     MOVE FSUM-PROC-TACKTID(IX10)  TO  W001-DET11-P-TACKTIDJ              
013969     MOVE TOT-TACKTID              TO  W001-DET11-TOT                     
013970     MOVE ZERO                     TO  W001-DET11-P-TOT                   
013971     MOVE W001-DETALJRAD-11 TO W001-RAD                                   
013972     MOVE +1 TO W001-SKIP                                                 
013973     PERFORM S21-SKRIV-LISTA                                              
013974                                                                          
013975     MOVE FSUM-SERVG-NTO(IX1)      TO  W001-DET12-SERVGA-NTO              
013976     MOVE FSUM-SERVG-BTO(IX1)      TO  W001-DET12-SERVGA-BTO              
013977     MOVE FSUM-SERVG-NTO(IX2)      TO  W001-DET12-SERVGB-NTO              
013978     MOVE FSUM-SERVG-BTO(IX2)      TO  W001-DET12-SERVGB-BTO              
013979     MOVE FSUM-SERVG-NTO(IX3)      TO  W001-DET12-SERVGC-NTO              
013980     MOVE FSUM-SERVG-BTO(IX3)      TO  W001-DET12-SERVGC-BTO              
013981     MOVE FSUM-SERVG-NTO(IX4)      TO  W001-DET12-SERVGD-NTO              
013982     MOVE FSUM-SERVG-BTO(IX4)      TO  W001-DET12-SERVGD-BTO              
013983     MOVE FSUM-SERVG-NTO(IX5)      TO  W001-DET12-SERVGE-NTO              
013984     MOVE FSUM-SERVG-BTO(IX5)      TO  W001-DET12-SERVGE-BTO              
013985     MOVE FSUM-SERVG-NTO(IX6)      TO  W001-DET12-SERVGF-NTO              
013986     MOVE FSUM-SERVG-BTO(IX6)      TO  W001-DET12-SERVGF-BTO              
013987     MOVE FSUM-SERVG-NTO(IX7)      TO  W001-DET12-SERVGG-NTO              
013988     MOVE FSUM-SERVG-BTO(IX7)      TO  W001-DET12-SERVGG-BTO              
013989     MOVE FSUM-SERVG-NTO(IX8)      TO  W001-DET12-SERVGH-NTO              
013990     MOVE FSUM-SERVG-BTO(IX8)      TO  W001-DET12-SERVGH-BTO              
013991     MOVE FSUM-SERVG-NTO(IX9)      TO  W001-DET12-SERVGI-NTO              
013992     MOVE FSUM-SERVG-BTO(IX9)      TO  W001-DET12-SERVGI-BTO              
013993     MOVE FSUM-SERVG-NTO(IX10)     TO  W001-DET12-SERVGJ-NTO              
013994     MOVE FSUM-SERVG-BTO(IX10)     TO  W001-DET12-SERVGJ-BTO              
013995     MOVE TOT-SERVG-NTO            TO  W001-DET12-TOT                     
013996     MOVE TOT-SERVG-BTO            TO  W001-DET12-P-TOT                   
013997     MOVE W001-DETALJRAD-12 TO W001-RAD                                   
013998     MOVE +1 TO W001-SKIP                                                 
013999     PERFORM S21-SKRIV-LISTA                                              
014000     .                                                                    
014001     EJECT                                                                
014002 CC-FIXA-RUBRIKER SECTION.                                                
014003                                                                          
014004     IF SW-TOTALLISTA = JA                                                
014005        MOVE DAGENS-DATUM TO W001-DATUM                                   
014006        MOVE DAGENS-VECKA TO W001-AKTUELL-VECKA                           
014007     ELSE                                                                 
014008        MOVE WS-DAGENS-DATUM              TO W001-DATUM                   
014009        MOVE W009VADD-DATUM               TO W001-AKTUELL-VECKA           
014010        MOVE 'USER:'                      TO W001-USER                    
014011        MOVE PARM-IN-IDUSER               TO W001-IDUSER                  
014012        IF PARM-IN-KATEGORI-FOM = ZERO                                    
014013           MOVE '1'                       TO W001-KAT-FOM                 
014014        ELSE                                                              
014015           MOVE PARM-IN-KATEGORI-FOM      TO W001-KAT-FOM                 
014016        END-IF                                                            
014017        IF PARM-IN-KATEGORI-TOM = ZERO                                    
014018           MOVE '3'                       TO W001-KAT-TOM                 
014019        ELSE                                                              
014020           MOVE PARM-IN-KATEGORI-TOM      TO W001-KAT-TOM                 
014021        END-IF                                                            
014022                                                                          
014023        IF PARM-IN-KDPRODSL-FOM = ZERO                                    
014024           MOVE ZERO                      TO W001-KDPRODSL-FOM            
014025        ELSE                                                              
014026           MOVE PARM-IN-KDPRODSL-FOM      TO W001-KDPRODSL-FOM            
014027        END-IF                                                            
014028        IF PARM-IN-KDPRODSL-TOM = ZERO                                    
014029           MOVE 99                        TO W001-KDPRODSL-TOM            
014030        ELSE                                                              
014031           MOVE PARM-IN-KDPRODSL-TOM      TO W001-KDPRODSL-TOM            
014032        END-IF                                                            
014033                                                                          
014034        MOVE PARM-IN-IDLEVNR              TO W001-IDLEVNR                 
014035        IF PARM-IDLEVNR = ZERO                                            
014036           MOVE SPACE                     TO W001-IDLEVNR                 
014037        END-IF                                                            
014038                                                                          
014039        IF PARM-IN-IDANSK-FOM = ZERO                                      
014040           MOVE ZERO                      TO W001-IDANSK-FOM              
014041        ELSE                                                              
014042           MOVE PARM-IN-IDANSK-FOM        TO W001-IDANSK-FOM              
014043        END-IF                                                            
014044        IF PARM-IN-IDANSK-TOM = ZERO                                      
014045           MOVE 999                       TO W001-IDANSK-TOM              
014046        ELSE                                                              
014047           MOVE PARM-IN-IDANSK-TOM        TO W001-IDANSK-TOM              
014048        END-IF                                                            
014049     END-IF                                                               
014050     .                                                                    
014051     EJECT                                                                
014052 Z-FINIT SECTION.                                                         
014053                                                                          
014054     CLOSE W23178                                                         
014055           W231PP                                                         
014056           W23181-001                                                     
014057                                                                          
014058     MOVE 'S' TO POSTSUM-OPKOD                                            
014059     CALL POSTSUM USING POSTSUM-PARM                                      
014060     .                                                                    
014061     EJECT                                                                
014062 S01-LAS-W23178 SECTION.                                                  
014063                                                                          
014064     READ W23178 INTO IN-AREA                                             
014065     AT END                                                               
014066         SET END-OF-W23178 TO TRUE                                        
014067                                                                          
014068     NOT AT END                                                           
014069        MOVE 'W23178'      TO POSTSUM-FDNAMN                              
014070        MOVE 'W23178D1'    TO POSTSUM-DDNAMN2                             
014071        MOVE SPACE         TO POSTSUM-TRANSTYP                            
014072        CALL POSTSUM USING POSTSUM-PARM                                   
014073     .                                                                    
014074     EJECT                                                                
014075 S02-LAS-W231PP SECTION.                                                  
014076                                                                          
014077     READ W231PP INTO PARM-AREA                                           
014078     AT END                                                               
014079         SET END-OF-W231PP TO TRUE                                        
014080                                                                          
014081     NOT AT END                                                           
014082        MOVE 'PARM  '      TO POSTSUM-FDNAMN                              
014083        MOVE 'W23178D3'    TO POSTSUM-DDNAMN2                             
014084        MOVE SPACE         TO POSTSUM-TRANSTYP                            
014085        CALL POSTSUM USING POSTSUM-PARM                                   
014086     .                                                                    
014087     EJECT                                                                
014088 S21-SKRIV-LISTA SECTION.                                                 
014089                                                                          
014090     WRITE W23181-001-RAD FROM W001-RAD AFTER W001-SKIP                   
014091                                                                          
014092     MOVE SPACE TO W001-RAD                                               
014093     ADD  +1 TO W001-ANTAL-RADER                                          
014094     .                                                                    
014095     EJECT                                                                
014096 S21A-SKRIV-RUBRIKER SECTION.                                             
014097                                                                          
014098     ADD +1 TO W001-SIDRAKNARE                                            
014099     MOVE W001-SIDRAKNARE TO W001-SID                                     
014100**** WRITE W23181-001-RAD FROM W001-RUBRIK1 AFTER PAGE                    
014101* ?  IF SW-TOTALLISTA = NEJ                                               
014102* ?     WRITE W23181-001-RAD FROM W001-RUBRIK2 AFTER 1                    
014103* ?  END-IF                                                               
014104     WRITE W23181-001-RAD FROM W001-RUBRIK3 AFTER 2                       
014105     MOVE +2 TO W001-ANTAL-RADER                                          
014110     MOVE +2 TO W001-SKIP                                                 
014200     .                                                                    
