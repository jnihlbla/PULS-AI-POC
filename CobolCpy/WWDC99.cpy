000100*** EDIT ALLOWED                                                          
000200******************************************************************        
000300**                                                                        
000400**                     W W D C 9 9                                        
000500**                                                                        
000600**   USAGE :                                                              
000700**                                                                        
000800**         THIS COPY TEXT CONTAINS ALL THE VALID DC CODES                 
000900**         THAT MUST BE USED IN THE PROGRAMS.                             
001000**                                                                        
001100**         AN NEW DC CODE INTRODUCED MUST BE INITIALISED HERE             
001200**         WITH A 88 LEVEL VALIDITY ITEM.                                 
001300**                                                                        
001400**         THE COPYTEXT IS SEPERATED IN 2 PARTS CALLED                    
001500**           # INDIVIDUAL VALIDATION ITEMS                                
001600**           # GROUP VALIDATIONS ITEMS                                    
001700**                                                                        
001800**   IMPORTANT :                                                          
001900**                                                                        
002000**         IF THERE IS ANY CHANGE MADE TO THIS COPYTEXT, THEN ALL         
002100**         PROGRAMS THAT HAS THIS COPYTEXT MUST BE RECOMPILED.            
002200**                                                                        
002300******************************************************************        
002400                                                                          
002500*                                                                         
002600*    --- INDIVIDUAL VALIDATION DC ITEMS.                                  
002700*                                                                         
002800 01  WS-IDDC               PIC X(2).                                      
002900*                                                                         
003000       88  CDC-SE              VALUE  '11'.                               
003100*                                                                         
003200       88  CDC-TR              VALUE  '12'.                               
003300*                                                                         
003400       88  SDC-NL              VALUE  '21'.                               
003500*                                                                         
003600       88  SDC-ES              VALUE  '24'.                               
003700*                                                                         
003800       88  SDC-IT              VALUE  '25'.                               
003900*                                                                         
004000       88  SDC-AT              VALUE  '26'.                               
004100*                                                                         
004200       88  SDC-NL-ET           VALUE  '91'.                               
004300*                                                                         
004400       88  LDC-SE-1A           VALUE  '1A'.                               
004500*                                                                         
004600       88  LDC-SE-1B           VALUE  '1B'.                               
004700*                                                                         
004800       88  LDC-SE-1C           VALUE  '1C'.                               
004900*                                                                         
005000       88  LDC-SE-1D           VALUE  '1D'.                               
005100*                                                                         
005200       88  LDC-SE-1E           VALUE  '1E'.                               
005300*                                                                         
005400       88  LDC-SE-1F           VALUE  '1F'.                               
005500*                                                                         
005600       88  LDC-SE-1G           VALUE  '1G'.                               
005700*                                                                         
005800       88  LDC-SE-1K           VALUE  '1K'.                               
005900*                                                                         
006000       88  LDC-GB-2C           VALUE  '2C'.                               
006100*                                                                         
006200       88  LDC-GB-2H           VALUE  '2H'.                               
006300*                                                                         
006400       88  LDC-DE-2I           VALUE  '2I'.                               
006500*                                                                         
006600       88  LDC-DE-2J           VALUE  '2J'.                               
006700*                                                                         
006800       88  LDC-DE-2L           VALUE  '2L'.                               
006900*                                                                         
007000       88  LDC-NL-2M           VALUE  '2M'.                               
007100*                                                                         
007200       88  LDC-XX-2N           VALUE  '2N'.                               
007300*                                                                         
007400       88  LDC-CH-2O           VALUE  '2O'.                               
007500*                                                                         
007600       88  LDC-GB-3A           VALUE  '3A'.                               
007700*                                                                         
007800       88  LDC-GB-3B           VALUE  '3B'.                               
007900*                                                                         
008000       88  LDC-DE-3C           VALUE  '3C'.                               
008100*                                                                         
008200       88  LDC-IT-3D           VALUE  '3D'.                               
008300*                                                                         
008400       88  LDC-DE-3E           VALUE  '3E'.                               
008500*                                                                         
008600       88  LDC-IT-3F           VALUE  '3F'.                               
008700*                                                                         
008800       88  LDC-DE-3G           VALUE  '3G'.                               
008900*                                                                         
009000       88  LDC-CH-3H           VALUE  '3H'.                               
009100*                                                                         
009200       88  LDC-NO-3J           VALUE  '3J'.                               
009300*                                                                         
009400       88  LDC-DE-3K           VALUE  '3K'.                               
009500*                                                                         
009600       88  LDC-BE-3L           VALUE  '3L'.                               
009700*                                                                         
009800       88  LDC-DE-3M           VALUE  '3M'.                               
009900*                                                                         
010000       88  LDC-NL-3N           VALUE  '3N'.                               
010100*                                                                         
010200       88  LDC-FI-3O           VALUE  '3O'.                               
010300*                                                                         
010400       88  LDC-FR-3P           VALUE  '3P'.                               
010500*                                                                         
010600       88  LDC-NL-3R           VALUE  '3R'.                               
010700*                                                                         
010800       88  LDC-PL-3S           VALUE  '3S'.                               
010900*                                                                         
011000       88  LDC-DE-3T           VALUE  '3T'.                               
011100*                                                                         
011200       88  LDC-XX-4A           VALUE  '4A'.                               
011300*                                                                         
011400       88  LDC-XX-4B           VALUE  '4B'.                               
011500*                                                                         
011600       88  LDC-XX-4C           VALUE  '4C'.                               
011700*                                                                         
011800       88  LDC-XX-4D           VALUE  '4D'.                               
011900*                                                                         
012000       88  LDC-CN-7A           VALUE  '7A'.                               
012100*                                                                         
012200       88  LDC-CN-7B           VALUE  '7B'.                               
012300*                                                                         
012400       88  LDC-CN-7C           VALUE  '7C'.                               
012500*                                                                         
012600       88  LDC-CN-7D           VALUE  '7D'.                               
012700*                                                                         
012800       88  LDC-CN-7E           VALUE  '7E'.                               
012900*                                                                         
013000       88  LDC-CN-7F           VALUE  '7F'.                               
013100*                                                                         
013200       88  LDC-CN-7G           VALUE  '7G'.                               
013300*                                                                         
013400       88  LDC-CN-7H           VALUE  '7H'.                               
013500*                                                                         
013600       88  NDC-US-RU           VALUE  '41'.                               
013700*                                                                         
013800       88  NDC-US-LA           VALUE  '43'.                               
013900*                                                                         
014000       88  NDC-US-SE           VALUE  '44'.                               
014100*                                                                         
014200       88  NDC-US-CH           VALUE  '45'.                               
014300*                                                                         
014400       88  NDC-US-JA           VALUE  '46'.                               
014500*                                                                         
014600       88  NDC-US-DA           VALUE  '47'.                               
014700*                                                                         
014800       88  NDC-CA              VALUE  '51'.                               
014900*                                                                         
015000       88  NDC-BR              VALUE  '52'.                               
015100*                                                                         
015200       88  NDC-MX              VALUE  '53'.                               
015300*                                                                         
015400       88  NDC-JP-6A           VALUE  '6A'.                               
015500*                                                                         
015600       88  NDC-JP-61           VALUE  '61'.                               
015700*                                                                         
015800       88  NDC-AU              VALUE  '62'.                               
015900*                                                                         
016000       88  NDC-TH-63           VALUE  '63'.                               
016100*                                                                         
016200       88  NDC-TW              VALUE  '64'.                               
016300*                                                                         
016400       88  NDC-KR              VALUE  '65'.                               
016500*                                                                         
016600       88  NDC-MY              VALUE  '66'.                               
016700*                                                                         
016800       88  NDC-IN              VALUE  '67'.                               
016900*                                                                         
017000       88  NDC-CN-71           VALUE  '71'.                               
017100*                                                                         
017200       88  NDC-CN-72           VALUE  '72'.                               
017300*                                                                         
017400       88  NDC-CN-73           VALUE  '73'.                               
017500*                                                                         
017600       88  NDC-CN-74           VALUE  '74'.                               
017700*                                                                         
017800       88  NDC-RU-81           VALUE  '81'.                               
017900*                                                                         
018000       88  NDC-RU-82           VALUE  '82'.                               
018100*                                                                         
018200       88  NDC-ZA              VALUE  '85'.                               
018300*                                                                         
018400       88  NDC-TR              VALUE  '86'.                               
018500*                                                                         
018600       88  NDC-AE              VALUE  '87'.                               
018700*                                                                         
018800       88  NDC-US-BAT          VALUE  '92'.                               
018900*                                                                         
018910       88  NDC-TH-93           VALUE  '93'.                               
018920*                                                                         
019000       88  DDC-SE              VALUE  'SE'.                               
019100*                                                                         
019200       88  DDC-NO              VALUE  'NO'.                               
019300*                                                                         
019400       88  DDC-FI              VALUE  'FI'.                               
019500*                                                                         
019600       88  DDC-BE              VALUE  'BE'.                               
019700*                                                                         
019800       88  DDC-DE              VALUE  'DE'.                               
019900*                                                                         
020000       88  DDC-NL              VALUE  'NL'.                               
020100*                                                                         
020200       88  DDC-GB              VALUE  'GB'.                               
020300*                                                                         
020400       88  DDC-FR              VALUE  'FR'.                               
020500*                                                                         
020600       88  DDC-KR              VALUE  'KR'.                               
020700*                                                                         
020800       88  DDC-US              VALUE  'US'.                               
020900*                                                                         
021000       88  DDC-TR              VALUE  'TR'.                               
021100*                                                                         
021200       88  DDC-HU              VALUE  'HU'.                               
021300*                                                                         
021400       88  DDC-PL              VALUE  'PL'.                               
021500*                                                                         
021600       88  DDC-MA              VALUE  'MA'.                               
021700*                                                                         
021800       88  DDC-CN              VALUE  'CN'.                               
021700*                                                                         
021800       88  DDC-AU              VALUE  'AU'.                               
021900*                                                                         
022000*    --- GROUP VALIDATION DC ITEMS.                                       
022100*                                                                         
022200*                                                                         
022300       88  GOOD-DC             VALUE  '11' '12'                           
022400                                      '21' THRU '26'                      
022500                                      '41' THRU '49'                      
022600                                      '51' THRU '53'                      
022700                                      '61' THRU '67'                      
022800                                      '71' THRU '74'                      
022900                                      '81' THRU '87'                      
023000                                      '91' THRU '93'                      
023100                                      '1A' THRU '1Z'                      
023200                                      '2A' THRU '2Z'                      
023300                                      '3A' THRU '3Z'                      
023400                                      '4A' THRU '4Z'                      
023500                                      '6A' THRU '6Z'                      
023600                                      '7A' THRU '7Z'.                     
023700*                                                                         
023800       88  CDC                 VALUE  '11' '12'.                          
023900*                                                                         
024000       88  SDC                 VALUE  '21' '24' '25' '26'                 
024100                                      '91'.                               
024200*                                                                         
024300       88  NDC                 VALUE  '41' THRU '49' '51'                 
024400                                      '51' THRU '53'                      
024500                                      '61' THRU '67' '6A'                 
024600                                      '71' THRU '74'                      
024700                                      '81' THRU '87'                      
024800                                      '92' '93'.                          
024900*                                                                         
025000       88  NDC-NA              VALUE  '41' THRU '49' '51'                 
025100                                      '92'.                               
025200*                                                                         
025300       88  NDC-US              VALUE  '41' THRU '49'                      
025400                                      '92'.                               
025500*                                                                         
025510       88  NDC-NS              VALUE  '52' '53'.                          
025520*                                                                         
025530                                                                          
025600       88  NDC-PACIFIC         VALUE  '61' THRU '67' '6A' '93'.           
025700*                                                                         
025800       88  NDC-CN              VALUE  '71' THRU '74'.                     
025900*                                                                         
026000       88  NDC-JP              VALUE  '6A' THRU '61'.                     
026100*                                                                         
026200       88  NDC-NX              VALUE  '81' THRU '87'.                     
026300*                                                                         
026301       88  NDC-TH              VALUE  '63' '93'.                          
026302*                                                                         
026310       88  XDC-NON-VCC-OWNED   VALUE  '52' '53'                           
026320                                      '63' THRU '67'                      
026500                                      '71' THRU '74'                      
026600                                      '81' THRU '87'                      
026610                                      '93'.                               
026700*                                                                         
026800       88  LDC                 VALUE  '1A' THRU '1Z'                      
026900                                      '2A' THRU '2Z'                      
027000                                      '3A' THRU '3Z'                      
027100                                      '4A' THRU '4Z'                      
027200                                      '7A' THRU '7Z'.                     
027300*                                                                         
027400       88  LDC-SE              VALUE  '1A' THRU '1Z'.                     
027500*                                                                         
027600       88  LDC-GB              VALUE  '2C' '2H'                           
027700                                      '3A' '3B'.                          
027800*                                                                         
027900       88  LDC-DE              VALUE  '2I' '2J' '2K' '2L' '3C'            
028000                                      '3E' '3G' '3K' '3M' '3T'.           
028100*                                                                         
028200       88  LDC-BE              VALUE  '3L'.                               
028300*                                                                         
028400       88  LDC-CH              VALUE  '2O' '3H'.                          
028500*                                                                         
028600       88  LDC-FI              VALUE  '3O'.                               
028700*                                                                         
028800       88  LDC-FR              VALUE  '3P'.                               
028900*                                                                         
029000       88  LDC-IT              VALUE  '3D' '3F'.                          
029100*                                                                         
029200       88  LDC-NL              VALUE  '2M' '3N' '3R'.                     
029300*                                                                         
029400       88  LDC-NO              VALUE  '3J'.                               
029500*                                                                         
029600       88  LDC-NA              VALUE  '4A' THRU '4Z'.                     
029700*                                                                         
029800       88  LDC-PL              VALUE  '3S'.                               
029900*                                                                         
030000       88  LDC-PACIFIC         VALUE  '6A' THRU '6Z'.                     
030100*                                                                         
030200       88  LDC-CN              VALUE  '7A' THRU '7H'.                     
030201*                                                                         
030202       88  LDC-EU              VALUE  '1A' THRU '1Z'                      
030203                                      '2A' THRU '2Z'                      
030204                                      '3A' THRU '3Z'.                     
030300*                                                                         
030400       88  GOOD-DDC            VALUE  'SE' 'NO' 'FI' 'BE' 'AU'            
030500                                      'DE' 'PL' 'KR' 'FR' 'GB'.           
030600*** END COPY WWDC01    LENGTH=2                                           
