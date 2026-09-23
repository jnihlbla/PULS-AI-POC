000010*** EDIT ALLOWED                                                          
000100*****************************************************************         
000200* TABELL FÖR ATT HITTA RÄTT BLANKETT FÖR ETT DISTRIKT           *         
000300* MED EN VISS FRAKTKOD                                          *         
000500*                                                               *         
000600* FILLER    PIC X(20) VALUE '13/1000-1099/IMDG-SF'              *         
000700*                            I     I          I                 *         
000800*   FRAKTKOD            -----I     I          I                 *         
000900*   DISTRIKTSINTERVALL  -----------I          I                 *         
001400*   BLANKETT            ----------------------I                 *         
001500*                                                               *         
002400*                                                               *         
002500*****************************************************************         
002600                                                                          
002700 01  BLANKETT-TABELL-DATA-CDC.                                            
002801   03  FILLER PIC X(20) VALUE '09/1000-1099/IMDG   '.                     
002802   03  FILLER PIC X(20) VALUE '11/0800-0899/DGR    '.                     
002803   03  FILLER PIC X(20) VALUE '13/1478-1478/IMDG   '.                     
002804   03  FILLER PIC X(20) VALUE '13/2270-2278/IMDG   '.                     
002805   03  FILLER PIC X(20) VALUE '16/1378-1378/DGR    '.                     
002806   03  FILLER PIC X(20) VALUE '17/0001-9999/DGR    '.                     
002807   03  FILLER PIC X(20) VALUE '18/1378-1378/DGR    '.                     
002808   03  FILLER PIC X(20) VALUE '18/6204-6204/DGR    '.                     
002809   03  FILLER PIC X(20) VALUE '19/6785-6785/DGR    '.                     
002810   03  FILLER PIC X(20) VALUE '19/7040-7040/DGR    '.                     
002810   03  FILLER PIC X(20) VALUE '19/7836-7836/DGR    '.                     
002811   03  FILLER PIC X(20) VALUE '19/8162-8162/DGR    '.                     
002812   03  FILLER PIC X(20) VALUE '20/1000-1099/IMDG   '.                     
002813   03  FILLER PIC X(20) VALUE '31/0800-0899/IMDG   '.                     
002814   03  FILLER PIC X(20) VALUE '31/1100-9999/IMDG   '.                     
002815   03  FILLER PIC X(20) VALUE '32/0778-0778/IMDG   '.                     
002815   03  FILLER PIC X(20) VALUE '32/1200-1299/IMDG   '.                     
002815   03  FILLER PIC X(20) VALUE '32/2600-2699/IMDG   '.                     
002816   03  FILLER PIC X(20) VALUE '34/0900-0999/IMDG   '.                     
002817   03  FILLER PIC X(20) VALUE '35/0900-0999/IMDG   '.                     
002820   03  FILLER PIC X(20) VALUE '41/1110-9999/IMDG   '.                     
002830   03  FILLER PIC X(20) VALUE '42/1119-9999/IMDG   '.                     
002840   03  FILLER PIC X(20) VALUE '43/1110-7511/IMDG   '.                     
002841   03  FILLER PIC X(20) VALUE '43/7513-7530/IMDG   '.                     
002842   03  FILLER PIC X(20) VALUE '43/7532-7551/IMDG   '.                     
002843   03  FILLER PIC X(20) VALUE '43/7553-7624/IMDG   '.                     
002844   03  FILLER PIC X(20) VALUE '43/7626-9999/IMDG   '.                     
002850   03  FILLER PIC X(20) VALUE '44/1110-9999/IMDG   '.                     
002851   03  FILLER PIC X(20) VALUE '50/0001-9999/DGR    '.                     
002852   03  FILLER PIC X(20) VALUE '55/2278-2278/DGR    '.                     
002860   03  FILLER PIC X(20) VALUE '61/1000-1099/IMDG   '.                     
002870   03  FILLER PIC X(20) VALUE '99/0900-9999/IMDG   '.                     
017400     EJECT                                                                
019300                                                                          
019400 01  FILLER REDEFINES BLANKETT-TABELL-DATA-CDC.                           
019500   03  BLANKETT-TABELL-CDC OCCURS 32 INDEXED BY BLK-IX.                   
019600     05  BLK-CDC-KDFRAKT         PIC X(2).                                
019700     05  FILLER                  PIC X(1).                                
019800     05  BLK-CDC-IDDISTR-FOM     PIC X(4).                                
019900     05  FILLER                  PIC X(1).                                
020000     05  BLK-CDC-IDDISTR-TOM     PIC X(4).                                
020100     05  FILLER                  PIC X(1).                                
021200     05  BLK-CDC-BLANKETT-ID     PIC X(7).                                
021300                                                                          
021600     EJECT                                                                
021700 01  BLANKETT-TABELL-DATA-DC21.                                           
022100   03  FILLER PIC X(20) VALUE '13/2270-2278/IMDG   '.                     
022300   03  FILLER PIC X(20) VALUE '17/0001-9999/DGR    '.                     
022900   03  FILLER PIC X(20) VALUE '31/1100-9999/IMDG   '.                     
023000   03  FILLER PIC X(20) VALUE '32/1200-1299/IMDG   '.                     
023300   03  FILLER PIC X(20) VALUE '41/1110-9999/IMDG   '.                     
023400   03  FILLER PIC X(20) VALUE '42/1119-9999/IMDG   '.                     
023500   03  FILLER PIC X(20) VALUE '43/1110-7511/IMDG   '.                     
024000   03  FILLER PIC X(20) VALUE '44/1110-9999/IMDG   '.                     
024100   03  FILLER PIC X(20) VALUE '50/0001-9999/DGR    '.                     
024200   03  FILLER PIC X(20) VALUE '55/2278-2278/DGR    '.                     
024500   03  FILLER PIC X(20) VALUE '99/0900-7499/IMDG   '.                     
024800     EJECT                                                                
025100                                                                          
025200 01  FILLER REDEFINES BLANKETT-TABELL-DATA-DC21.                          
025300   03  BLANKETT-TABELL-DC21 OCCURS 11 INDEXED BY BLK21-IX.                
025400     05  BLK-DC21-KDFRAKT        PIC X(2).                                
025500     05  FILLER                  PIC X(1).                                
025600     05  BLK-DC21-IDDISTR-FOM    PIC X(4).                                
025700     05  FILLER                  PIC X(1).                                
025800     05  BLK-DC21-IDDISTR-TOM    PIC X(4).                                
025900     05  FILLER                  PIC X(1).                                
026000     05  BLK-DC21-BLANKETT-ID    PIC X(7).                                
026100                                                                          
026200     EJECT                                                                
