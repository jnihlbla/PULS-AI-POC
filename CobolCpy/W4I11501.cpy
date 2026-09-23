000100 01  MID-W4I11501.                                                        
000200*                                 COPYTEXT FÖR MID W4I11501               
000300*                                                                         
000400     03 MID-IDKVAFELI        PIC 9(2).                                    
000500*                                 KVALITET FELKOD FÖR ARTIKEL             
000600     03 MID-IDSKYLTI         PIC X(3).                                    
000700      88 MID-GODK-IDSKYLT    VALUE 'D  '                                  
000800                             'E  '                                        
000900                             'F  '                                        
001000                             'GB '                                        
001100                             'I  '                                        
001200                             'NL '                                        
001300                             'P  '                                        
001400                             'S  '                                        
001500                             'SF '                                        
001600                             'USA'.                                       
001700*                                 NATIONALITETSTECKEN                     
001800     03 MID-IDKVAFELU        PIC 9(2).                                    
001900*                                 KVALITET FELKOD FÖR ARTIKEL             
002000     03 MID-IDSKYLTU         PIC X(3).                                    
002100      88 MID-GODK-IDSKYLT    VALUE 'D  '                                  
002200                             'E  '                                        
002300                             'F  '                                        
002400                             'GB '                                        
002500                             'I  '                                        
002600                             'NL '                                        
002700                             'P  '                                        
002800                             'S  '                                        
002900                             'SF '                                        
003000                             'USA'.                                       
003100*                                 NATIONALITETSTECKEN                     
003200     03 MID-IDKVAFEL-NX      PIC 9(2).                                    
003300*                                 KVALITET FELKOD FÖR ARTIKEL             
003400     03 MID-IDSKYLT-NX       PIC X(3).                                    
003500      88 MID-GODK-IDSKYLT    VALUE 'D  '                                  
003600                             'E  '                                        
003700                             'F  '                                        
003800                             'GB '                                        
003900                             'I  '                                        
004000                             'NL '                                        
004100                             'P  '                                        
004200                             'S  '                                        
004300                             'SF '                                        
004400                             'USA'.                                       
004500*                                 NATIONALITETSTECKEN                     
004600     03 MID-IDKVAFEL-EN      PIC 9(2).                                    
004700*                                 KVALITET FELKOD FÖR ARTIKEL             
004800     03 MID-IDSKYLT-EN       PIC X(3).                                    
004900      88 MID-GODK-IDSKYLT    VALUE 'D  '                                  
005000                             'E  '                                        
005100                             'F  '                                        
005200                             'GB '                                        
005300                             'I  '                                        
005400                             'NL '                                        
005500                             'P  '                                        
005600                             'S  '                                        
005700                             'SF '                                        
005800                             'USA'.                                       
005900*                                 NATIONALITETSTECKEN                     
006000     03 MID-INPUT.                                                        
006100*                                 COPYTEXT FOR MID W4O11501 ENDAS         
006200*                                 T INDATA-FÄLT                           
006300        05 MID-IDKVAFEL-IN   PIC 9(2).                                    
006400*                                 KVALITET FELKOD FÖR ARTIKEL             
006500        05 MID-KDKVAFG-IN    PIC 9.                                       
006600         88 MID-KDCLAGER-BADA                                             
006700                             VALUE 0.                                     
006800         88 MID-KDCLAGER-C1  VALUE 1.                                     
006900         88 MID-KDCLAGER-C2  VALUE 2.                                     
007000*                                 FELGRUPP FÖR FELKOD                     
007100        05 MID-BEKVAFEL-IN   PIC X(40).                                   
007200*                                 KVALITET FELKODSBETECKNING              
007300        05 MID-BEKVAFGR-IN   PIC X(20).                                   
007400*                                 KVALITET ALLVARLIGHETSGRAD FELK         
007500*                                 OD                                      
007600        05 MID-KVKVAFPO-IN   PIC 9(3).                                    
007700*                                 KVALITET POÄNG FÖR FELKOD               
007800        05 MID-IDSKYLT-IN    PIC X(3).                                    
007900         88 MID-GODK-IDSKYLT VALUE 'D  '                                  
008000                             'E  '                                        
008100                             'F  '                                        
008200                             'GB '                                        
008300                             'I  '                                        
008400                             'NL '                                        
008500                             'P  '                                        
008600                             'S  '                                        
008700                             'SF '                                        
008800                             'USA'.                                       
008900*                                 NATIONALITETSTECKEN                     
009000        05 MID-KDCMD-IN      PIC X.                                       
009100         88 MID-KDCMD-INGENTING                                           
009200                             VALUE ' '.                                   
009300         88 MID-KDCMD-DELETE VALUE 'D'                                    
009400                             'B'.                                         
009500         88 MID-KDCMD-REPLACE                                             
009600                             VALUE 'R'                                    
009700                             'Ä'.                                         
009800         88 MID-KDCMD-INSERT VALUE 'I'                                    
009900                             'N'.                                         
010000*                                 RAD-UPPDATERINGSKOMMANDO                
010100*** END COPY W4I11501C0  LENGTH=90                                        
