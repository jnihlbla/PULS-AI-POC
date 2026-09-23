000010*** EDIT ALLOWED                                                          
000100**************************************                                    
000200****     SYSTEMGEMENSAMMA KONSTANTER                                      
000300**************************************                                    
000400 01      W402W001.                                                        
000500     03  FILLER          PIC X(32)   VALUE 'SYSTEM KONSTANTER'.           
000600                                                                          
000700     03  JA              PIC X       VALUE 'J'.                           
000800     03  NEJ             PIC X       VALUE 'N'.                           
000900     03  ALFA            PIC X       VALUE 'A'.                           
001000     03  NUM             PIC X       VALUE 'N'.                           
001100     03  OSYNLIGT        PIC X       VALUE 'O'.                           
001200     03  RETT            PIC X       VALUE 'R'.                           
001300     03  FEL             PIC X       VALUE 'F'.                           
001400     03  FEL-SAKNAS      PIC X       VALUE 'S'.                           
001500     03  EJ-IFYLLD       PIC X       VALUE 'E'.                           
001600     03  MANGA-RADER     PIC X       VALUE 'M'.                           
001700     03  OK-VISA-ALLT    PIC X       VALUE ' '.                           
001800     03  OBEHORIG        PIC X       VALUE 'F'.                           
001900     03  IMPORTER        PIC X       VALUE '1'.                           
002000     03  DEALER          PIC X       VALUE '2'.                           
002100     03  INTERNDISTRIKT  PIC X       VALUE '3'.                           
002200     03  EJVOR           PIC X       VALUE '5'.                           
002300     03  VOR-DEALER      PIC X       VALUE '6'.                           
002400     EJECT                                                                
002500**************************************                                    
002600****     SYSTEMGEMENSAMMA SWITCHAR                                        
002700**************************************                                    
002800     03  FILLER          PIC X(32) VALUE 'SYSTEM SWITCHAR'.               
002900                                                                          
003000     03  FLPERSON        PIC X.                                           
003100         88  FLPERSON-GODKAND    VALUE 'J'.                               
003200         88  FLPERSON-OKAND      VALUE 'N'.                               
003300                                                                          
003400     03  FLNYCKEL        PIC X.                                           
003500         88  FLNYCKEL-RETT       VALUE 'J'.                               
003600         88  FLNYCKEL-FEL        VALUE 'N'.                               
003700                                                                          
003800     03  KDINDATA         PIC X.                                          
003900         88  KDINDATA-RETT        VALUE 'R'.                              
004000         88  KDINDATA-FEL         VALUE 'F'.                              
004100         88  KDINDATA-SAKNAS      VALUE 'S'.                              
004200         88  KDINDATA-EJ-IFYLLD   VALUE 'E'.                              
004300         88  KDINDATA-MANGA-RADER VALUE 'M'.                              
004400                                                                          
004500     03  FLFEL            PIC X.                                          
004600         88  FLFEL-JA             VALUE 'J'.                              
004700         88  FLFEL-NEJ            VALUE 'N'.                              
004800                                                                          
004900     03  FLNYUPP         PIC X.                                           
005000         88  FLNYUPP-TILLATET     VALUE 'J'.                              
005100         88  FLNYUPP-FORBUD       VALUE 'N'.                              
005200                                                                          
005300     03  FLANDRA         PIC X.                                           
005400         88  FLANDRA-TILLATET     VALUE 'J'.                              
005500         88  FLANDRA-FORBUD       VALUE 'N'.                              
005600                                                                          
005700     03  FLBORT          PIC X.                                           
005800         88  FLBORT-TILLATET      VALUE 'J'.                              
005900         88  FLBORT-FORBUD        VALUE 'N'.                              
006000     03  FLRELEAS        PIC X.                                           
006100         88  FLRELEAS-TILLATET    VALUE 'J'.                              
006200         88  FLRELEAS-FORBUD      VALUE 'N'.                              
006300*** END COPY W402W001C0  LENGTH=87    OLD LENGTH=86                       
