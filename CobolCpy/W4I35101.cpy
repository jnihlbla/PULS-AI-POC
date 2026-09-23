000100 01  MID-W4I35101.                                                        
000200*                                 MID-COPYTEXT FÖR BILD  4351             
000300*                                 UTSKRIFT AV PLOCKSATS                   
000400     03 MID-IDPRC-IN.                                                     
000500*                                 PRODUKTIONSKANAL                        
000600        05 MID-IDPRCBAS      PIC X(3).                                    
000700*                                 PRC-BAS                                 
000800        05 MID-IDPRCVAR      PIC X.                                       
000900*                                 PRC-VARIANT                             
001000     03 MID-IDPRC-UT.                                                     
001100*                                 PRODUKTIONSKANAL                        
001200        05 MID-IDPRCBAS      PIC X(3).                                    
001300*                                 PRC-BAS                                 
001400        05 MID-IDPRCVAR      PIC X.                                       
001500*                                 PRC-VARIANT                             
001600     03 MID-IDUSER-IN        PIC X(8).                                    
001700*                                 ANVÄNDARENS SÄKERHETS ID                
001800     03 MID-IDUSER-UT        PIC X(8).                                    
001900*                                 ANVÄNDARENS SÄKERHETS ID                
002000     03 MID-IDBORD-IN        PIC X(3).                                    
002100*                                 PACK-BORD                               
002200     03 MID-IDBORD-UT        PIC X(3).                                    
002300*                                 PACK-BORD                               
002400     03 MID-IDDC-IN          PIC X(2).                                    
002500*                                 IDENTIFIERARE LAGER                     
002600     03 MID-IDDC-UT          PIC X(2).                                    
002700*                                 IDENTIFIERARE LAGER                     
002800     03 MID-KDPRT-PU         PIC X(3).                                    
002900*                                 PRINTERKOD PACKUNDERLAG                 
003000     03 MID-KDPRT-PLE        PIC X(3).                                    
003100*                                 PRINTERKOD PLOCKETIKETTER               
003200     03 MID-FLORDKNY         PIC X.                                       
003300*                                 KNYTER ORDER ELLER ORDERDEL             
003400*                                 TILL EN PLOCKARE                        
003500*                                 ANVÄNDS NÄR INTE HELA ORDERN            
003600*                                 PLOCKAS SAMTIDIGT                       
003700*                                 J=HELA ORDERDELEN TILL                  
003800*                                 SAMMA PACKARE                           
003900*                                 N=VEM SOM HELST FÅR TA UT               
004000*                                 RESTERANDE RADER                        
004100     03 MID-MIXAT            PIC X.                                       
004200*                                 ALLMÄN SVARSFLAGGA                      
004300     03 MID-UTSKR-EJ-KOMPL-PS                                             
004400                             PIC X.                                       
004500*                                 ALLMÄN SVARSFLAGGA                      
004600     03 MID-WDQ3BSEQ.                                                     
004700*                                 B-INDEX PÅ Q3-BASEN                     
004800        05 MID-IDDC-BSEQ     PIC X(2).                                    
004900*                                 IDENTIFIERARE LAGER                     
005000        05 MID-IDPRCBAS-BSEQ PIC X(3).                                    
005100*                                 PRC-BAS                                 
005200        05 MID-TIUTSKR-BSEQ  PIC 9(6).                                    
005300*                                 UTSKRIFTDATUM  (ÅÅMMDD)                 
005400        05 MID-TIUTSTID-BSEQ PIC 9(6).                                    
005500*                                 UTSKRIFTSTID (TTMMSS)                   
005600        05 MID-TIRFS-BSEQ    PIC 9(10).                                   
005700*                                 KLART FÖR TRANSPORT ÅÅMMDDTTMM          
005800        05 MID-TILST-O-BSEQ  PIC 9(10).                                   
005900*                                 SENASTE STARTTIDPUNKT FÖR ORDER         
006000        05 MID-IDPRCVAR-BSEQ PIC X.                                       
006100*                                 PRC-VARIANT                             
006200*** END OF VILMAII-COPY LENGTH= 81 BYTES                                  
