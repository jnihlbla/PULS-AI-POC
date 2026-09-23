000100***EDIT ALLOWED                                                           
000200 01  MSG-TAB1.                                                            
000300     03 PIC X(29)  VALUE                                                  
000400        'A01                PULS-A01  '.                                  
000401     03 PIC X(100) VALUE                                                  
000402            'Part Number not found'.                                      
000403     03 PIC X(29)  VALUE                                                  
000404        'A02                PULS-A02  '.                                  
000405     03 PIC X(100) VALUE                                                  
000406            'Customer not found'.                                         
000407     03 PIC X(29)  VALUE                                                  
000408        'A03                PULS-A03  '.                                  
000409     03 PIC X(100) VALUE                                                  
000410            'Postal code not found'.                                      
000411     03 PIC X(29)  VALUE                                                  
000420        '00A                PULS-00A  '.                                  
000500     03 PIC X(100) VALUE                                                  
000600            'Sorry, you are not authorized for this action'.              
000700     03 PIC X(29)  VALUE                                                  
000800        '001                PULS-001  '.                                  
000900     03 PIC X(100) VALUE                                                  
001000            'OK, update done              '.                              
001100     03 PIC X(29)  VALUE                                                  
001200        '002                PULS-002  '.                                  
001300     03 PIC X(100) VALUE                                                  
001400            'OK, insert done              '.                              
001500     03 PIC X(29)  VALUE                                                  
001600        '003                PULS-003  '.                                  
001700     03 PIC X(100) VALUE                                                  
001800            'OK, delete done              '.                              
001900     03 PIC X(29)  VALUE                                                  
002000        '004                PULS-004  '.                                  
002100     03 PIC X(100) VALUE                                                  
002200            'Nothing has been updated     '.                              
002300     03 PIC X(29)  VALUE                                                  
002400        '005                PULS-005  '.                                  
002500     03 PIC X(100) VALUE                                                  
002600            'Nothing has been inserted    '.                              
002700     03 PIC X(29)  VALUE                                                  
002800        '006                PULS-006  '.                                  
002900     03 PIC X(100) VALUE                                                  
003000            'Nothing has been deleted     '.                              
003100     03 PIC X(29)  VALUE                                                  
003200        '007                PULS-007  '.                                  
003300     03 PIC X(100) VALUE                                                  
003400            'Update not allowed           '.                              
003500     03 PIC X(29)  VALUE                                                  
003600        '008                PULS-008  '.                                  
003700     03 PIC X(100) VALUE                                                  
003800            'Insert not allowed           '.                              
003900     03 PIC X(29)  VALUE                                                  
004000        '009                PULS-009  '.                                  
004100     03 PIC X(100) VALUE                                                  
004200            'Delete not allowed           '.                              
004300     03 PIC X(29)  VALUE                                                  
004400        '011                PULS-011  '.                                  
004500     03 PIC X(100) VALUE                                                  
004600            'More lines exist             '.                              
004700     03 PIC X(29)  VALUE                                                  
004800        '012                PULS-012  '.                                  
004900     03 PIC X(100) VALUE                                                  
005000            'Last line shown              '.                              
005100     03 PIC X(29)  VALUE                                                  
005200        '015                PULS-015  '.                                  
005300     03 PIC X(100) VALUE                                                  
005400            'OK, process started          '.                              
005500     03 PIC X(29)  VALUE                                                  
005600        '020                PULS-020  '.                                  
005700     03 PIC X(100) VALUE                                                  
005800            'Invalid data                 '.                              
005900     03 PIC X(29)  VALUE                                                  
006000        '022                PULS-022  '.                                  
006100     03 PIC X(100) VALUE                                                  
006200            'Invalid key fields ¤       '.                                
006300     03 PIC X(29)  VALUE                                                  
006400        '023                PULS-023  '.                                  
006500     03 PIC X(100) VALUE                                                  
006600            '¤ is invalid               '.                                
006700     03 PIC X(29)  VALUE                                                  
006800        '024                PULS-024  '.                                  
006900     03 PIC X(100) VALUE                                                  
007000            '¤ must be numeric          '.                                
007100     03 PIC X(29)  VALUE                                                  
007200        '025                PULS-025  '.                                  
007300     03 PIC X(100) VALUE                                                  
007400            '¤ not found                '.                                
007500     03 PIC X(29)  VALUE                                                  
007600        '026                PULS-026  '.                                  
007700     03 PIC X(100) VALUE                                                  
007800            '¤ must be provided         '.                                
007900     03 PIC X(29)  VALUE                                                  
008000        '027                PULS-027  '.                                  
008100     03 PIC X(100) VALUE                                                  
008200            'Line(s) not found            '.                              
008300     03 PIC X(29)  VALUE                                                  
008400        '028                PULS-028  '.                                  
008500     03 PIC X(100) VALUE                                                  
008600            'Too many lines               '.                              
008700     03 PIC X(29)  VALUE                                                  
008800        '029                PULS-029  '.                                  
008900     03 PIC X(100) VALUE                                                  
009000            'Duplicate lines              '.                              
009100     03 PIC X(29)  VALUE                                                  
009200        '030                PULS-030  '.                                  
009300     03 PIC X(100) VALUE                                                  
009400            '¤ already exists           '.                                
009500     03 PIC X(29)  VALUE                                                  
009600        '031                PULS-031  '.                                  
009700     03 PIC X(100) VALUE                                                  
009800            '¤ must not be changed      '.                                
009900     03 PIC X(29)  VALUE                                                  
010000        '032                PULS-032  '.                                  
010100     03 PIC X(100) VALUE                                                  
010200            'An invalid combination of keys was entered'.                 
010300     03 PIC X(29)  VALUE                                                  
010400        '033                PULS-033  '.                                  
010500     03 PIC X(100) VALUE                                                  
010600            '¤ must not be entered      '.                                
010700     03 PIC X(29)  VALUE                                                  
010800        '034                PULS-034  '.                                  
010900     03 PIC X(100) VALUE                                                  
011000            '¤ not selected             '.                                
011100     03 PIC X(29)  VALUE                                                  
011200        '035                PULS-035  '.                                  
011300     03 PIC X(100) VALUE                                                  
011400            '¤ deleted                  '.                                
011500     03 PIC X(29)  VALUE                                                  
011600        '041                PULS-041  '.                                  
011700     03 PIC X(100) VALUE                                                  
011800            '¤ missing                  '.                                
011900     03 PIC X(29)  VALUE                                                  
012000        '042                PULS-042  '.                                  
012100     03 PIC X(100) VALUE                                                  
012200            'Multiple functions           '.                              
012300     03 PIC X(29)  VALUE                                                  
012400        '043                PULS-043  '.                                  
012500     03 PIC X(100) VALUE                                                  
012600            'Invalid key field(s)         '.                              
012700     03 PIC X(29)  VALUE                                                  
012800        '046                PULS-046  '.                                  
012900     03 PIC X(100) VALUE                                                  
013000            'Conflicting fields           '.                              
013100     03 PIC X(29)  VALUE                                                  
013200        '047                PULS-047  '.                                  
013300     03 PIC X(100) VALUE                                                  
013400            'Already exists               '.                              
013500     03 PIC X(29)  VALUE                                                  
013600        '048                PULS-048  '.                                  
013700     03 PIC X(100) VALUE                                                  
013800            'Arithmetic overflow in ¤   '.                                
013900     03 PIC X(29)  VALUE                                                  
014000        '099                PULS-099  '.                                  
014100     03 PIC X(100) VALUE                                                  
014200            'SYSTEM ERROR, (¤ is invalid)     '.                          
014300     03 PIC X(29)  VALUE                                                  
014400        '100                PULS-100  '.                                  
014500     03 PIC X(100) VALUE                                                  
014600            'Case not loaded              '.                              
014700     03 PIC X(29)  VALUE                                                  
014800        '101                PULS-101  '.                                  
014900     03 PIC X(100) VALUE                                                  
015000            'For more info, see location inquiry'.                        
015100     03 PIC X(29)  VALUE                                                  
015200        '102                PULS-102  '.                                  
015300     03 PIC X(100) VALUE                                                  
015400            'Goods address missing        '.                              
015500     03 PIC X(29)  VALUE                                                  
015600        '103                PULS-103  '.                                  
015700     03 PIC X(100) VALUE                                                  
015800            'Confirm not last line in case'.                              
015900     03 PIC X(29)  VALUE                                                  
016000        '105                PULS-105  '.                                  
016100     03 PIC X(100) VALUE                                                  
016200            'Case not reported            '.                              
016300     03 PIC X(29)  VALUE                                                  
016400        '106                PULS-106  '.                                  
016500     03 PIC X(100) VALUE                                                  
016600            'Confirm not last line in case'.                              
016700     03 PIC X(29)  VALUE                                                  
016800        '107                PULS-107  '.                                  
016900     03 PIC X(100) VALUE                                                  
017000            'Part No. in same buffer location'.                           
017100     03 PIC X(29)  VALUE                                                  
017200        '108                PULS-108  '.                                  
017300     03 PIC X(100) VALUE                                                  
017400            'Case ready not valid         '.                              
017500     03 PIC X(29)  VALUE                                                  
017600        '109                PULS-109  '.                                  
017700     03 PIC X(100) VALUE                                                  
017800            'Reporting per order in progress '.                           
017900     03 PIC X(29)  VALUE                                                  
018000        '110                PULS-110  '.                                  
018100     03 PIC X(100) VALUE                                                  
018200            'Software order               '.                              
018300     03 PIC X(29)  VALUE                                                  
018400        '111                PULS-111  '.                                  
018500     03 PIC X(100) VALUE                                                  
018600            'Packer and order do not match'.                              
018700     03 PIC X(29)  VALUE                                                  
018800        '112                PULS-112  '.                                  
018900     03 PIC X(100) VALUE                                                  
019000            'No remaining order parts for this packer'.                   
019100     03 PIC X(29)  VALUE                                                  
019200        '113                PULS-113  '.                                  
019300     03 PIC X(100) VALUE                                                  
019400            'Reporting in progress ord ready'.                            
019500     03 PIC X(29)  VALUE                                                  
019600        '114                PULS-114  '.                                  
019700     03 PIC X(100) VALUE                                                  
019800            'No reporting for direct supplier'.                           
019900     03 PIC X(29)  VALUE                                                  
020000        '115                PULS-115  '.                                  
020100     03 PIC X(100) VALUE                                                  
020200            'Previous printing is still processing'.                      
020300     03 PIC X(29)  VALUE                                                  
020400        '116                PULS-116  '.                                  
020500     03 PIC X(100) VALUE                                                  
020600            'Orderpart ready              '.                              
020700     03 PIC X(29)  VALUE                                                  
020800        '117                PULS-117  '.                                  
020900     03 PIC X(100) VALUE                                                  
021000            'Emballage too small, please change'.                         
021100     03 PIC X(29)  VALUE                                                  
021200        '118                PULS-118  '.                                  
021300     03 PIC X(100) VALUE                                                  
021400            'Order already zeroed         '.                              
021500     03 PIC X(29)  VALUE                                                  
021600        '119                PULS-119  '.                                  
021700     03 PIC X(100) VALUE                                                  
021800            'No dangerous goods allowed   '.                              
021900     03 PIC X(29)  VALUE                                                  
022000        '120                PULS-120  '.                                  
022100     03 PIC X(100) VALUE                                                  
022200            'District not approved        '.                              
022300     03 PIC X(29)  VALUE                                                  
022400        '121                PULS-121  '.                                  
022500     03 PIC X(100) VALUE                                                  
022600            'Mix case on other transport  '.                              
022700     03 PIC X(29)  VALUE                                                  
022800        '122                PULS-122  '.                                  
022900     03 PIC X(100) VALUE                                                  
023000            'Mix case has no transport    '.                              
023100     03 PIC X(29)  VALUE                                                  
023200        '123                PULS-123  '.                                  
023300     03 PIC X(100) VALUE                                                  
023400            'Error in goods ready address '.                              
023500     03 PIC X(29)  VALUE                                                  
023600        '124                PULS-124  '.                                  
023700     03 PIC X(100) VALUE                                                  
023800            'Wrong action key             '.                              
023900     03 PIC X(29)  VALUE                                                  
024000        '126                PULS-126  '.                                  
024100     03 PIC X(100) VALUE                                                  
024200            '¤ should not be zero       '.                                
024300     03 PIC X(29)  VALUE                                                  
024400        '127                PULS-127  '.                                  
024500     03 PIC X(100) VALUE                                                  
024600            'Already printed              '.                              
024700     03 PIC X(29)  VALUE                                                  
024800        '128                PULS-128  '.                                  
024900     03 PIC X(100) VALUE                                                  
025000            'One or more selected order(s) already printed'.              
025100     03 PIC X(29)  VALUE                                                  
025200        '129                PULS-129  '.                                  
025300     03 PIC X(100) VALUE                                                  
025400            'Too many lines, max. 200 lines allowed'.                     
025500     03 PIC X(29)  VALUE                                                  
025600        '130                PULS-130  '.                                  
025700     03 PIC X(100) VALUE                                                  
025800            'Prc 9998 cannot be printed manually'.                        
025900     03 PIC X(29)  VALUE                                                  
026000        '131                PULS-131  '.                                  
026100     03 PIC X(100) VALUE                                                  
026200            'Picking unit on printer queue'.                              
026300     03 PIC X(29)  VALUE                                                  
026400        '132                PULS-132  '.                                  
026500     03 PIC X(100) VALUE                                                  
026600            'Non completed picking unit on printer queue'.                
026700     03 PIC X(29)  VALUE                                                  
026800        '133                PULS-133  '.                                  
026900     03 PIC X(100) VALUE                                                  
027000            'No approved case found       '.                              
027100     03 PIC X(29)  VALUE                                                  
027200        '134                PULS-134  '.                                  
027300     03 PIC X(100) VALUE                                                  
027400            'Error in any of the these lines'.                            
027500     03 PIC X(29)  VALUE                                                  
027600        '136                PULS-136  '.                                  
027700     03 PIC X(100) VALUE                                                  
027800            'Both case and interval not allowed'.                         
027900     03 PIC X(29)  VALUE                                                  
028000        '137                PULS-137  '.                                  
028100     03 PIC X(100) VALUE                                                  
028200            'Error in case interval       '.                              
028300     03 PIC X(29)  VALUE                                                  
028400        '138                PULS-138  '.                                  
028500     03 PIC X(100) VALUE                                                  
028600            'Max 100 cases in interval & 1 printer'.                      
028700     03 PIC X(29)  VALUE                                                  
028800        '139                PULS-139  '.                                  
028900     03 PIC X(100) VALUE                                                  
029000            'Max 50 cases in interval with twoprnt'.                      
029100     03 PIC X(29)  VALUE                                                  
029200        '140                PULS-140  '.                                  
029300     03 PIC X(100) VALUE                                                  
029400            'More case information needed '.                              
029500     03 PIC X(29)  VALUE                                                  
029600        '141                PULS-141  '.                                  
029700     03 PIC X(100) VALUE                                                  
029800            'Enter line interval          '.                              
029900     03 PIC X(29)  VALUE                                                  
030000        '142                PULS-142  '.                                  
030100     03 PIC X(100) VALUE                                                  
030200            'Wrong interval information   '.                              
030300     03 PIC X(29)  VALUE                                                  
030400        '143                PULS-143  '.                                  
030500     03 PIC X(100) VALUE                                                  
030600            'Too many lines, max. 200 lines allowed'.                     
030700     03 PIC X(29)  VALUE                                                  
030800        '144                PULS-144  '.                                  
030900     03 PIC X(100) VALUE                                                  
031000            'Only one line in interval allowed'.                          
031100     03 PIC X(29)  VALUE                                                  
031200        '145                PULS-145  '.                                  
031300     03 PIC X(100) VALUE                                                  
031400            'More than one interval not allowed'.                         
031500     03 PIC X(29)  VALUE                                                  
031600        '146                PULS-146  '.                                  
031700     03 PIC X(100) VALUE                                                  
031800            'Order totally reported       '.                              
031900     03 PIC X(29)  VALUE                                                  
032000        '147                PULS-147  '.                                  
032100     03 PIC X(100) VALUE                                                  
032200            'Case in interval already reported'.                          
032300     03 PIC X(29)  VALUE                                                  
032400        '148                PULS-148  '.                                  
032500     03 PIC X(100) VALUE                                                  
032600            'It is not allowed to pack direct suppl. case.'.              
032700     03 PIC X(29)  VALUE                                                  
032800        '149                PULS-149  '.                                  
032900     03 PIC X(100) VALUE                                                  
033000            'Case already reported        '.                              
033100     03 PIC X(29)  VALUE                                                  
033200        '150                PULS-150  '.                                  
033300     03 PIC X(100) VALUE                                                  
033400            'Packer and order do not match'.                              
033500     03 PIC X(29)  VALUE                                                  
033600        '151                PULS-151  '.                                  
033700     03 PIC X(100) VALUE                                                  
033800            'Order part of packer ready   '.                              
033900     03 PIC X(29)  VALUE                                                  
034000        '152                PULS-152  '.                                  
034100     03 PIC X(100) VALUE                                                  
034200            'Deviation control in progress'.                              
034300     03 PIC X(29)  VALUE                                                  
034400        '154                PULS-154  '.                                  
034500     03 PIC X(100) VALUE                                                  
034600            'Error when generating address'.                              
034700     03 PIC X(29)  VALUE                                                  
034800        '156                PULS-156  '.                                  
034900     03 PIC X(100) VALUE                                                  
035000            'Mixed case on other transport'.                              
035100     03 PIC X(29)  VALUE                                                  
035200        '157                PULS-157  '.                                  
035300     03 PIC X(100) VALUE                                                  
035400            'Mixed case has no transport  '.                              
035500     03 PIC X(29)  VALUE                                                  
035600        '159                PULS-159  '.                                  
035700     03 PIC X(100) VALUE                                                  
035800            'District is not a mixed district'.                           
035900     03 PIC X(29)  VALUE                                                  
036000        '160                PULS-160  '.                                  
036100     03 PIC X(100) VALUE                                                  
036200            'Interval does not belong to packer'.                         
036300     03 PIC X(29)  VALUE                                                  
036400        '161                PULS-161  '.                                  
036500     03 PIC X(100) VALUE                                                  
036600            'Order has not been split     '.                              
036700     03 PIC X(29)  VALUE                                                  
036800        '162                PULS-162  '.                                  
036900     03 PIC X(100) VALUE                                                  
037000            'Interval already reported    '.                              
037100     03 PIC X(29)  VALUE                                                  
037200        '163                PULS-163  '.                                  
037300     03 PIC X(100) VALUE                                                  
037400            'Line zeroed by zero-hunter   '.                              
037500     03 PIC X(29)  VALUE                                                  
037600        '164                PULS-164  '.                                  
037700     03 PIC X(100) VALUE                                                  
037800            'No dangerous cargo in mixed case'.                           
037900     03 PIC X(29)  VALUE                                                  
038000        '165                PULS-165  '.                                  
038100     03 PIC X(100) VALUE                                                  
038200            'Too large quantity           '.                              
038300     03 PIC X(29)  VALUE                                                  
038400        '166                PULS-166  '.                                  
038500     03 PIC X(100) VALUE                                                  
038600            'Line qty. not evenly divided into cases'.                    
038700     03 PIC X(29)  VALUE                                                  
038800        '168                PULS-168  '.                                  
038900     03 PIC X(100) VALUE                                                  
039000            'Zero not allowed             '.                              
039100     03 PIC X(29)  VALUE                                                  
039200        '169                PULS-169  '.                                  
039300     03 PIC X(100) VALUE                                                  
039400            'Case and,or Order missing    '.                              
039500     03 PIC X(29)  VALUE                                                  
039600        '170                PULS-170  '.                                  
039700     03 PIC X(100) VALUE                                                  
039800            'Control-ok-indicator must not be marked'.                    
039900     03 PIC X(29)  VALUE                                                  
040000        '171                PULS-171  '.                                  
040100     03 PIC X(100) VALUE                                                  
040200            'Control-ok-indicator is invalid'.                            
040300     03 PIC X(29)  VALUE                                                  
040400        '172                PULS-172  '.                                  
040500     03 PIC X(100) VALUE                                                  
040600            'Next-orderpart-indicator is invalid'.                        
040700     03 PIC X(29)  VALUE                                                  
040800        '173                PULS-173  '.                                  
040900     03 PIC X(100) VALUE                                                  
041000            'Mark control-ok-indicator    '.                              
041100     03 PIC X(29)  VALUE                                                  
041200        '174                PULS-174  '.                                  
041300     03 PIC X(100) VALUE                                                  
041400            'Order part ready             '.                              
041500     03 PIC X(29)  VALUE                                                  
041600        '175                PULS-175  '.                                  
041700     03 PIC X(100) VALUE                                                  
041800            'Order part not ended         '.                              
041900     03 PIC X(29)  VALUE                                                  
042000        '177                PULS-177  '.                                  
042100     03 PIC X(100) VALUE                                                  
042200            'Order part of packer ready   '.                              
042300     03 PIC X(29)  VALUE                                                  
042400        '178                PULS-178  '.                                  
042500     03 PIC X(100) VALUE                                                  
042600            'Unpacked cases exist         '.                              
042700     03 PIC X(29)  VALUE                                                  
042800        '179                PULS-179  '.                                  
042900     03 PIC X(100) VALUE                                                  
043000            'Last order part, no more finished order parts'.              
043100     03 PIC X(29)  VALUE                                                  
043200        '180                PULS-180  '.                                  
043300     03 PIC X(100) VALUE                                                  
043400            'More order parts exist       '.                              
043500     03 PIC X(29)  VALUE                                                  
043600        '181                PULS-181  '.                                  
043700     03 PIC X(100) VALUE                                                  
043800            'If this is the last order part you may mark the contr        
043900-           'ol-ok-indicator'.                                            
044000     03 PIC X(29)  VALUE                                                  
044100        '182                PULS-182  '.                                  
044200     03 PIC X(100) VALUE                                                  
044300            'New keys and input not allowed'.                             
044400     03 PIC X(29)  VALUE                                                  
044500        '183                PULS-183  '.                                  
044600     03 PIC X(100) VALUE                                                  
044700            'Control problems, see manual    '.                           
044800     03 PIC X(29)  VALUE                                                  
044900        '184                PULS-184  '.                                  
045000     03 PIC X(100) VALUE                                                  
045100            'Lines+Cases remain to be reported. '.                        
045200     03 PIC X(29)  VALUE                                                  
045300        '185                PULS-185  '.                                  
045400     03 PIC X(100) VALUE                                                  
045500            'Inventory-print-indicator is invalid '.                      
045600     03 PIC X(29)  VALUE                                                  
045700        '186                PULS-186  '.                                  
045800     03 PIC X(100) VALUE                                                  
045900            'Treat-whole-selection-indicator is invalid '.                
046000     03 PIC X(29)  VALUE                                                  
046100        '187                PULS-187  '.                                  
046200     03 PIC X(100) VALUE                                                  
046300            'More than one function selected '.                           
046400     03 PIC X(29)  VALUE                                                  
046500        '188                PULS-188  '.                                  
046600     03 PIC X(100) VALUE                                                  
046700            'Case partly loaded              '.                           
046800     03 PIC X(29)  VALUE                                                  
046900        '191                PULS-191  '.                                  
047000     03 PIC X(100) VALUE                                                  
047100            'Entered data for update         '.                           
047200     03 PIC X(29)  VALUE                                                  
047300        '192                PULS-192  '.                                  
047400     03 PIC X(100) VALUE                                                  
047500            'Use NEW Case Reporting 1 or 2 (WL0197 & 99) '.               
047600     03 PIC X(29)  VALUE                                                  
047700        '193                PULS-193  '.                                  
047800     03 PIC X(100) VALUE                                                  
047900            'Use OLD Case Reporting 1 or 2 (Wl0121 & 22) '.               
048000     03 PIC X(29)  VALUE                                                  
048100        '194                PULS-194  '.                                  
048200     03 PIC X(100) VALUE                                                  
048300            'Order not completed             '.                           
048400     03 PIC X(29)  VALUE                                                  
048500        '197                PULS-197  '.                                  
048600     03 PIC X(100) VALUE                                                  
048700            'Order is cancelled              '.                           
048800     03 PIC X(29)  VALUE                                                  
048900        '200                PULS-200  '.                                  
049000     03 PIC X(100) VALUE                                                  
049100            'Transport not booked            '.                           
049200     03 PIC X(29)  VALUE                                                  
049300        '201                PULS-201  '.                                  
049400     03 PIC X(100) VALUE                                                  
049500            'Order not ready                 '.                           
049600     03 PIC X(29)  VALUE                                                  
049700        '203                PULS-203  '.                                  
049800     03 PIC X(100) VALUE                                                  
049900            'Order ready                     '.                           
050000     03 PIC X(29)  VALUE                                                  
050100        '204                PULS-204  '.                                  
050200     03 PIC X(100) VALUE                                                  
050300            'More cases follows              '.                           
050400     03 PIC X(29)  VALUE                                                  
050500        '205                PULS-205  '.                                  
050600     03 PIC X(100) VALUE                                                  
050700            'Not reported lines cannot be approved '.                     
050800     03 PIC X(29)  VALUE                                                  
050900        '206                PULS-206  '.                                  
051000     03 PIC X(100) VALUE                                                  
051100            'Customer No. and,or Report No. invalid '.                    
051200     03 PIC X(29)  VALUE                                                  
051300        '207                PULS-207  '.                                  
051400     03 PIC X(100) VALUE                                                  
051500            'Employee id and,or Receiving area and,or Freight lett        
051600-           'er No. invalid '.                                            
051700     03 PIC X(29)  VALUE                                                  
051800        '208                PULS-208  '.                                  
051900     03 PIC X(100) VALUE                                                  
052000            'Case No. and,or Case No. from and,or Case No. to and,        
052100-           'or District No. invalid '.                                   
052200     03 PIC X(29)  VALUE                                                  
052300        '209                PULS-209  '.                                  
052400     03 PIC X(100) VALUE                                                  
052500            'Both case No. and case No. from-to cannot be entered'        
052600            .                                                             
052700     03 PIC X(29)  VALUE                                                  
052800        '210                PULS-210  '.                                  
052900     03 PIC X(100) VALUE                                                  
053000            'Return created, please continue (no of cases for good        
053100-           's returning) '.                                              
053200     03 PIC X(29)  VALUE                                                  
053300        '211                PULS-211  '.                                  
053400     03 PIC X(100) VALUE                                                  
053500            'Line has not been split         '.                           
053600     03 PIC X(29)  VALUE                                                  
053700        '213                PULS-213  '.                                  
053800     03 PIC X(100) VALUE                                                  
053900            'Line already reported           '.                           
054000     03 PIC X(29)  VALUE                                                  
054100        '214                PULS-214  '.                                  
054200     03 PIC X(100) VALUE                                                  
054300            'Line partly reported            '.                           
051700     03 PIC X(29)  VALUE                                                  
051800        '215                PULS-215  '.                                  
051900     03 PIC X(100) VALUE                                                  
052000            'Supplements need to be added,Please close the                
052100-           'transport in WEB PULS '.                                     
054400     03 PIC X(29)  VALUE                                                  
054500        '217                PULS-217  '.                                  
054600     03 PIC X(100) VALUE                                                  
054700            'Line reported by zero-hunter    '.                           
054800     03 PIC X(29)  VALUE                                                  
054900        '218                PULS-218  '.                                  
055000     03 PIC X(100) VALUE                                                  
055100            'Bad core will not be updated    '.                           
055200     03 PIC X(29)  VALUE                                                  
055300        '219                PULS-219  '.                                  
055400     03 PIC X(100) VALUE                                                  
055500            'Cancellation registered         '.                           
055600     03 PIC X(29)  VALUE                                                  
055700        '220                PULS-220  '.                                  
055800     03 PIC X(100) VALUE                                                  
055900            'Inventory information missing   '.                           
056000     03 PIC X(29)  VALUE                                                  
056100        '221                PULS-221  '.                                  
056200     03 PIC X(100) VALUE                                                  
056300            'No Inventory Information found  '.                           
056400     03 PIC X(29)  VALUE                                                  
056500        '222                PULS-222  '.                                  
056600     03 PIC X(100) VALUE                                                  
056700            'Inventory not completed         '.                           
056800     03 PIC X(29)  VALUE                                                  
056900        '223                PULS-223  '.                                  
057000     03 PIC X(100) VALUE                                                  
057100            'Part is superseded              '.                           
057200     03 PIC X(29)  VALUE                                                  
057300        '224                PULS-224  '.                                  
057400     03 PIC X(100) VALUE                                                  
057500            'Whole lot not on location       '.                           
057600     03 PIC X(29)  VALUE                                                  
057700        '226                PULS-226  '.                                  
057800     03 PIC X(100) VALUE                                                  
057900            'Add case information            '.                           
058000     03 PIC X(29)  VALUE                                                  
058100        '227                PULS-227  '.                                  
058200     03 PIC X(100) VALUE                                                  
058300            'Loading employee id must not be zero'.                       
058400     03 PIC X(29)  VALUE                                                  
058500        '228                PULS-228  '.                                  
058600     03 PIC X(100) VALUE                                                  
058700            'Loading not allowed for this return status'.                 
058800     03 PIC X(29)  VALUE                                                  
058900        '230                PULS-230  '.                                  
059000     03 PIC X(100) VALUE                                                  
059100            'Invalid disrepancy status for this command'.                 
059200     03 PIC X(29)  VALUE                                                  
059300        '231                PULS-231  '.                                  
059400     03 PIC X(100) VALUE                                                  
059500            'District No. must be > 0 and,or Customer No. and,or R        
059600-           'eport No. must be numeric'.                                  
059700     03 PIC X(29)  VALUE                                                  
059800        '232                PULS-232  '.                                  
059900     03 PIC X(100) VALUE                                                  
060000            'Deviation not allowed for this return'.                      
060100     03 PIC X(29)  VALUE                                                  
060200        '233                PULS-233  '.                                  
060300     03 PIC X(100) VALUE                                                  
060400            'Reception not allowed for this case status'.                 
060500     03 PIC X(29)  VALUE                                                  
060600        '234                PULS-234  '.                                  
060700     03 PIC X(100) VALUE                                                  
060800            'Deletion not allowed for this return'.                       
060900     03 PIC X(29)  VALUE                                                  
061000        '235                PULS-235  '.                                  
061100     03 PIC X(100) VALUE                                                  
061200            '¤ Must be either 4, 5 or 6    '.                             
061300     03 PIC X(29)  VALUE                                                  
061400        '237                PULS-237  '.                                  
061500     03 PIC X(100) VALUE                                                  
061600            'Reporting started               '.                           
061700     03 PIC X(29)  VALUE                                                  
061800        '238                PULS-238  '.                                  
061900     03 PIC X(100) VALUE                                                  
062000            'Reporting done                  '.                           
062100     03 PIC X(29)  VALUE                                                  
062200        '239                PULS-239  '.                                  
062300     03 PIC X(100) VALUE                                                  
062400            'Binned-indicator is invalid     '.                           
062500     03 PIC X(29)  VALUE                                                  
062600        '240                PULS-240  '.                                  
062700     03 PIC X(100) VALUE                                                  
062800            'Update-all-for-binninglist-indicator is invalid'.            
062900     03 PIC X(29)  VALUE                                                  
063000        '241                PULS-241  '.                                  
063100     03 PIC X(100) VALUE                                                  
063200            'Scrapped-indicator is invalid   '.                           
063300     03 PIC X(29)  VALUE                                                  
063400        '242                PULS-242  '.                                  
063500     03 PIC X(100) VALUE                                                  
063600            'Deviation-indicator is invalid  '.                           
063700     03 PIC X(29)  VALUE                                                  
063800        '244                PULS-244  '.                                  
063900     03 PIC X(100) VALUE                                                  
064000            'Line already exists. Update not possible'.                   
064100     03 PIC X(29)  VALUE                                                  
064200        '246                PULS-246  '.                                  
064300     03 PIC X(100) VALUE                                                  
064400            'Must not be greater than reported quantity'.                 
064500     03 PIC X(29)  VALUE                                                  
064600        '247                PULS-247  '.                                  
064700     03 PIC X(100) VALUE                                                  
064800            'New-case-indicator is invalid   '.                           
064900     03 PIC X(29)  VALUE                                                  
065000        '248                PULS-248  '.                                  
065100     03 PIC X(100) VALUE                                                  
065200            'District No. and,or Customer No. and,or Report No. is        
065300-           ' invalid '.                                                  
065400     03 PIC X(29)  VALUE                                                  
065500        '249                PULS-249  '.                                  
065600     03 PIC X(100) VALUE                                                  
065700            'New-return-indicator is invalid '.                           
065800     03 PIC X(29)  VALUE                                                  
065900        '250                PULS-250  '.                                  
066000     03 PIC X(100) VALUE                                                  
066100            'Nothing printed                 '.                           
066200     03 PIC X(29)  VALUE                                                  
066300        '251                PULS-251  '.                                  
066400     03 PIC X(100) VALUE                                                  
066500            'Too many cases in interval      '.                           
066600     03 PIC X(29)  VALUE                                                  
066700        '252                PULS-252  '.                                  
066800     03 PIC X(100) VALUE                                                  
066900            'Last-case-indicator is invalid  '.                           
067000     03 PIC X(29)  VALUE                                                  
067100        '253                PULS-253  '.                                  
067200     03 PIC X(100) VALUE                                                  
067300            'OK, but no printing selected    '.                           
067400     03 PIC X(29)  VALUE                                                  
067500        '254                PULS-254  '.                                  
067600     03 PIC X(100) VALUE                                                  
067700            'Update not done, try again      '.                           
067800     03 PIC X(29)  VALUE                                                  
067900        '255                PULS-255  '.                                  
068000     03 PIC X(100) VALUE                                                  
068100            '¤ already invoiced            '.                             
068200     03 PIC X(29)  VALUE                                                  
068300        '256                PULS-256  '.                                  
068400     03 PIC X(100) VALUE                                                  
068500            '¤ may not be completly empty  '.                             
068600     03 PIC X(29)  VALUE                                                  
068700        '257                PULS-257  '.                                  
068800     03 PIC X(100) VALUE                                                  
068900            'Summary-indicator is invalid    '.                           
069000     03 PIC X(29)  VALUE                                                  
069100        '258                PULS-258  '.                                  
069200     03 PIC X(100) VALUE                                                  
069300            'Backout-all-indicator is invalid'.                           
069400     03 PIC X(29)  VALUE                                                  
069500        '259                PULS-259  '.                                  
069600     03 PIC X(100) VALUE                                                  
069700            'Backout-indicator is invalid    '.                           
069800     03 PIC X(29)  VALUE                                                  
069900        '260                PULS-260  '.                                  
070000     03 PIC X(100) VALUE                                                  
070100            'Price information is invalid or missing'.                    
070200     03 PIC X(29)  VALUE                                                  
070300        '261                PULS-261  '.                                  
070400     03 PIC X(100) VALUE                                                  
070500            'Supplier id and,or Case No. is invalid'.                     
070600     03 PIC X(29)  VALUE                                                  
070700        '262                PULS-262  '.                                  
070800     03 PIC X(100) VALUE                                                  
070900            'Multiple functions choosen      '.                           
071000     03 PIC X(29)  VALUE                                                  
071100        '263                PULS-263  '.                                  
071200     03 PIC X(100) VALUE                                                  
071300            'Part location history           '.                           
071400     03 PIC X(29)  VALUE                                                  
071500        '264                PULS-264  '.                                  
071600     03 PIC X(100) VALUE                                                  
071700            'Only one line command at a time '.                           
071800     03 PIC X(29)  VALUE                                                  
071900        '265                PULS-265  '.                                  
072000     03 PIC X(100) VALUE                                                  
072100            'Update not done, investigation balance is zero or les        
072200-           's than zero '.                                               
072300     03 PIC X(29)  VALUE                                                  
072400        '267                PULS-267  '.                                  
072500     03 PIC X(100) VALUE                                                  
072600            'Delete adjustment already done  '.                           
072700     03 PIC X(29)  VALUE                                                  
072800        '268                PULS-268  '.                                  
072900     03 PIC X(100) VALUE                                                  
073000            '¤ already updated             '.                             
073100     03 PIC X(29)  VALUE                                                  
073200        '269                PULS-269  '.                                  
073300     03 PIC X(100) VALUE                                                  
073400            'Investigation balance adjusted, category 8'.                 
073500     03 PIC X(29)  VALUE                                                  
073600        '270                PULS-270  '.                                  
073700     03 PIC X(100) VALUE                                                  
073800            'Investigation balance updated   '.                           
073900     03 PIC X(29)  VALUE                                                  
074000        '271                PULS-271  '.                                  
074100     03 PIC X(100) VALUE                                                  
074200            'Inventory already exist, printed'.                           
074300     03 PIC X(29)  VALUE                                                  
074400        '272                PULS-272  '.                                  
074500     03 PIC X(100) VALUE                                                  
074600            'Inventory already exist, not printed'.                       
074700     03 PIC X(29)  VALUE                                                  
074800        '273                PULS-273  '.                                  
074900     03 PIC X(100) VALUE                                                  
075000            'Order has wrong status          '.                           
075100     03 PIC X(29)  VALUE                                                  
075200        '274                PULS-274  '.                                  
075300     03 PIC X(100) VALUE                                                  
075400            'Max. 10 lines selected for printing'.                        
075500     03 PIC X(29)  VALUE                                                  
075600        '275                PULS-275  '.                                  
075700     03 PIC X(100) VALUE                                                  
075800            'Max. 15 lines selected for printing'.                        
075900     03 PIC X(29)  VALUE                                                  
076000        '276                PULS-276  '.                                  
076100     03 PIC X(100) VALUE                                                  
076200            'Press Execute for update        '.                           
076300     03 PIC X(29)  VALUE                                                  
076400        '277                PULS-277  '.                                  
076500     03 PIC X(100) VALUE                                                  
076600            'Only Bill-IT markets allowed    '.                           
076700     03 PIC X(29)  VALUE                                                  
076800        '278                PULS-278  '.                                  
076900     03 PIC X(100) VALUE                                                  
077000            'Both Shipment id and Transport id cannot be entered'.        
077100     03 PIC X(29)  VALUE                                                  
077200        '279                PULS-279  '.                                  
077300     03 PIC X(100) VALUE                                                  
077400            'Delete-ok-indicator is invalid  '.                           
077500     03 PIC X(29)  VALUE                                                  
077600        '281                PULS-281  '.                                  
077700     03 PIC X(100) VALUE                                                  
077800            'Load-page-indicator is invalid  '.                           
077900     03 PIC X(29)  VALUE                                                  
078000        '282                PULS-282  '.                                  
078100     03 PIC X(100) VALUE                                                  
078200            'Buffer-appendix-indicator is invalid'.                       
078300     03 PIC X(29)  VALUE                                                  
078400        '283                PULS-283  '.                                  
078500     03 PIC X(100) VALUE                                                  
078600            'Diff-indicator is invalid       '.                           
078700     03 PIC X(29)  VALUE                                                  
078800        '284                PULS-284  '.                                  
078900     03 PIC X(100) VALUE                                                  
079000            'District No. missing in block-register'.                     
079100     03 PIC X(29)  VALUE                                                  
079200        '285                PULS-285  '.                                  
079300     03 PIC X(100) VALUE                                                  
079400            'District No. already exists in block-register'.              
079500     03 PIC X(29)  VALUE                                                  
079600        '286                PULS-286  '.                                  
079700     03 PIC X(100) VALUE                                                  
079800            'Quality information missing     '.                           
079900     03 PIC X(29)  VALUE                                                  
080000        '287                PULS-287  '.                                  
080100     03 PIC X(100) VALUE                                                  
080200            'No cases found                  '.                           
080300     03 PIC X(29)  VALUE                                                  
080400        '288                PULS-288  '.                                  
080500     03 PIC X(100) VALUE                                                  
080600            'OK, new return id assigned      '.                           
080700     03 PIC X(29)  VALUE                                                  
080800        '289                PULS-289  '.                                  
080900     03 PIC X(100) VALUE                                                  
081000            'DC for this productionnumber does not match your prof        
081100-           'ile DC'.                                                     
081200     03 PIC X(29)  VALUE                                                  
081300        '290                PULS-290  '.                                  
081400     03 PIC X(100) VALUE                                                  
081500            'Line will go to re-inventory'.                               
081600     03 PIC X(29)  VALUE                                                  
081700        '291                PULS-291  '.                                  
081800     03 PIC X(100) VALUE                                                  
081900            'Referral-indicator is invalid   '.                           
082000     03 PIC X(29)  VALUE                                                  
082100        '292                PULS-292  '.                                  
082200     03 PIC X(100) VALUE                                                  
082300            'OK, new return terminal shipping No. assigned'.              
082400     03 PIC X(29)  VALUE                                                  
082500        '293                PULS-293  '.                                  
082600     03 PIC X(100) VALUE                                                  
082700            'No line(s) selected             '.                           
082800     03 PIC X(29)  VALUE                                                  
082900        '294                PULS-294  '.                                  
083000     03 PIC X(100) VALUE                                                  
083100            'OK, new case No. assigned       '.                           
083200     03 PIC X(29)  VALUE                                                  
083300        '295                PULS-295  '.                                  
083400     03 PIC X(100) VALUE                                                  
083500            'Financial customer stopped      '.                           
083600     03 PIC X(29)  VALUE                                                  
083700        '296                PULS-296  '.                                  
083800     03 PIC X(100) VALUE                                                  
083900            'VAT registration number missing '.                           
084000     03 PIC X(29)  VALUE                                                  
084100        '297                PULS-297  '.                                  
084200     03 PIC X(100) VALUE                                                  
084300            'Financial customer missing      '.                           
084400     03 PIC X(29)  VALUE                                                  
084500        '298                PULS-298  '.                                  
084600     03 PIC X(100) VALUE                                                  
084700            'Quantity too big                '.                           
084800     03 PIC X(29)  VALUE                                                  
084900        '299                PULS-299  '.                                  
085000     03 PIC X(100) VALUE                                                  
085100            'Order finished                  '.                           
085200     03 PIC X(29)  VALUE                                                  
085300        '300                PULS-300  '.                                  
085400     03 PIC X(100) VALUE                                                  
085500            'Wrong order screen              '.                           
085600     03 PIC X(29)  VALUE                                                  
085700        '301                PULS-301  '.                                  
085800     03 PIC X(100) VALUE                                                  
085900            'Order cancelled                 '.                           
086000     03 PIC X(29)  VALUE                                                  
086100        '302                PULS-302  '.                                  
086200     03 PIC X(100) VALUE                                                  
086300            'Registration ready - YES or NO must be chosen'.              
086400     03 PIC X(29)  VALUE                                                  
086500        '303                PULS-303  '.                                  
086600     03 PIC X(100) VALUE                                                  
086700            'Order number was ¤            '.                             
086800     03 PIC X(29)  VALUE                                                  
086900        '304                PULS-304  '.                                  
087000     03 PIC X(100) VALUE                                                  
087100            'Order missing                   '.                           
087200     03 PIC X(29)  VALUE                                                  
087300        '305                PULS-305  '.                                  
087400     03 PIC X(100) VALUE                                                  
087500            'Order does not belong to User id'.                           
087600     03 PIC X(29)  VALUE                                                  
087700        '306                PULS-306  '.                                  
087800     03 PIC X(100) VALUE                                                  
087900            'Order finished                  '.                           
088000     03 PIC X(29)  VALUE                                                  
088100        '308                PULS-308  '.                                  
088200     03 PIC X(100) VALUE                                                  
088300            'Press Execute to approve        '.                           
088400     03 PIC X(29)  VALUE                                                  
088500        '309                PULS-309  '.                                  
088600     03 PIC X(100) VALUE                                                  
088700            'Part No. is obsolete            '.                           
088800     03 PIC X(29)  VALUE                                                  
088900        '310                PULS-310  '.                                  
089000     03 PIC X(100) VALUE                                                  
089100            'Part Weight is missing          '.                           
089200     03 PIC X(29)  VALUE                                                  
089300        '311                PULS-311  '.                                  
089400     03 PIC X(100) VALUE                                                  
089500            'Part Volume is missing          '.                           
089600     03 PIC X(29)  VALUE                                                  
089700        '312                PULS-312  '.                                  
089800     03 PIC X(100) VALUE                                                  
089900            'Part Origin is missing          '.                           
090000     03 PIC X(29)  VALUE                                                  
090100        '313                PULS-313  '.                                  
090200     03 PIC X(100) VALUE                                                  
090300            'Part Address is missing         '.                           
090400     03 PIC X(29)  VALUE                                                  
090500        '314                PULS-314  '.                                  
090600     03 PIC X(100) VALUE                                                  
090700            'SVS address for part is missing '.                           
090800     03 PIC X(29)  VALUE                                                  
090900        '315                PULS-315  '.                                  
091000     03 PIC X(100) VALUE                                                  
091100            'Analysis No. and,or Cost center must be entered'.            
091200     03 PIC X(29)  VALUE                                                  
091300        '316                PULS-316  '.                                  
091400     03 PIC X(100) VALUE                                                  
091500            'No more lines                   '.                           
091600     03 PIC X(29)  VALUE                                                  
091700        '317                PULS-317  '.                                  
091800     03 PIC X(100) VALUE                                                  
091900            'Wrong area for order            '.                           
092000     03 PIC X(29)  VALUE                                                  
092100        '318                PULS-318  '.                                  
092200     03 PIC X(100) VALUE                                                  
092300            'No stocktaking exists           '.                           
092400     03 PIC X(29)  VALUE                                                  
092500        '319                PULS-319  '.                                  
092600     03 PIC X(100) VALUE                                                  
092700            'Only one option can be selected '.                           
092800     03 PIC X(29)  VALUE                                                  
092900        '320                PULS-320  '.                                  
093000     03 PIC X(100) VALUE                                                  
093100            'Part not in area 42             '.                           
093200     03 PIC X(29)  VALUE                                                  
093300        '321                PULS-321  '.                                  
093400     03 PIC X(100) VALUE                                                  
093500            'Part replaced                   '.                           
093600     03 PIC X(29)  VALUE                                                  
093700        '322                PULS-322  '.                                  
093800     03 PIC X(100) VALUE                                                  
093900            'Part expired                    '.                           
094000     03 PIC X(29)  VALUE                                                  
094100        '323                PULS-323  '.                                  
094200     03 PIC X(100) VALUE                                                  
094300            'Replacing part                  '.                           
094400     03 PIC X(29)  VALUE                                                  
094500        '324                PULS-324  '.                                  
094600     03 PIC X(100) VALUE                                                  
094700            'Unauthorized user               '.                           
094800     03 PIC X(29)  VALUE                                                  
094900        '325                PULS-325  '.                                  
095000     03 PIC X(100) VALUE                                                  
095100            'List on printer queue           '.                           
095200     03 PIC X(29)  VALUE                                                  
095300        '326                PULS-326  '.                                  
095400     03 PIC X(100) VALUE                                                  
095500            'Press Search or Execute         '.                           
095600     03 PIC X(29)  VALUE                                                  
095700        '327                PULS-327  '.                                  
095800     03 PIC X(100) VALUE                                                  
095900            'Lines are changed               '.                           
096000     03 PIC X(29)  VALUE                                                  
096100        '328                PULS-328  '.                                  
096200     03 PIC X(100) VALUE                                                  
096300            'Return id not shipped           '.                           
096400     03 PIC X(29)  VALUE                                                  
096500        '329                PULS-329  '.                                  
096600     03 PIC X(100) VALUE                                                  
096700            'Click OK for Reporting          '.                           
096800     03 PIC X(29)  VALUE                                                  
096900        '330                PULS-330  '.                                  
097000     03 PIC X(100) VALUE                                                  
097100            'Invalid quantity                '.                           
097200     03 PIC X(29)  VALUE                                                  
097300        '331                PULS-331  '.                                  
097400     03 PIC X(100) VALUE                                                  
097500            'ACS Inventory not allowed       '.                           
097600     03 PIC X(29)  VALUE                                                  
097700        '332                PULS-332  '.                                  
097800     03 PIC X(100) VALUE                                                  
097900            'Inventory not started           '.                           
098000     03 PIC X(29)  VALUE                                                  
098100        '333                PULS-333  '.                                  
098200     03 PIC X(100) VALUE                                                  
098300            '"Missing" action can not be combined with other actio        
098400-           'ns '.                                                        
098500     03 PIC X(29)  VALUE                                                  
098600        '334                PULS-334  '.                                  
098700     03 PIC X(100) VALUE                                                  
098800            'Kit marked case                 '.                           
098900     03 PIC X(29)  VALUE                                                  
099000        '335                PULS-335  '.                                  
099100     03 PIC X(100) VALUE                                                  
099200            'Inspection Report must be changed manually'.                 
099300     03 PIC X(29)  VALUE                                                  
099400        '336                PULS-336  '.                                  
099500     03 PIC X(100) VALUE                                                  
099600            'Calls missing                   '.                           
099700     03 PIC X(29)  VALUE                                                  
099800        '337                PULS-337  '.                                  
099900     03 PIC X(100) VALUE                                                  
100000            'Error in line one               '.                           
100100     03 PIC X(29)  VALUE                                                  
100200        '338                PULS-338  '.                                  
100300     03 PIC X(100) VALUE                                                  
100400            'SAP account missing in R3       '.                           
100500     03 PIC X(29)  VALUE                                                  
100600        '339                PULS-339  '.                                  
100700     03 PIC X(100) VALUE                                                  
100800            'SAP account must be registred   '.                           
100900     03 PIC X(29)  VALUE                                                  
101000        '340                PULS-340  '.                                  
101100     03 PIC X(100) VALUE                                                  
101200            'SAP costcenter not allowed      '.                           
101300     03 PIC X(29)  VALUE                                                  
101400        '341                PULS-341  '.                                  
101500     03 PIC X(100) VALUE                                                  
101600            'SAP costcenter missing in R3    '.                           
101700     03 PIC X(29)  VALUE                                                  
101800        '342                PULS-342  '.                                  
101900     03 PIC X(100) VALUE                                                  
102000            'SAP analysis No. not allowed    '.                           
102100     03 PIC X(29)  VALUE                                                  
102200        '343                PULS-343  '.                                  
102300     03 PIC X(100) VALUE                                                  
102400            'SAP analysis No. missing in R3  '.                           
102500     03 PIC X(29)  VALUE                                                  
102600        '344                PULS-344  '.                                  
102700     03 PIC X(100) VALUE                                                  
102800            'SAP analysis or costcenter must be registred'.               
102900     03 PIC X(29)  VALUE                                                  
103000        '345                PULS-345  '.                                  
103100     03 PIC X(100) VALUE                                                  
103200            'Invalid command                 '.                           
103300     03 PIC X(29)  VALUE                                                  
103400        '346                PULS-346  '.                                  
103500     03 PIC X(100) VALUE                                                  
103600            'Already at last page            '.                           
103700     03 PIC X(29)  VALUE                                                  
103800        '347                PULS-347  '.                                  
103900     03 PIC X(100) VALUE                                                  
104000            'Invalid printer                 '.                           
104100     03 PIC X(29)  VALUE                                                  
104200        '348                PULS-348  '.                                  
104300     03 PIC X(100) VALUE                                                  
104400            'Keys not found                  '.                           
104500     03 PIC X(29)  VALUE                                                  
104600        '349                PULS-349  '.                                  
104700     03 PIC X(100) VALUE                                                  
104800            'Direct deliver part             '.                           
104900     03 PIC X(29)  VALUE                                                  
105000        '350                PULS-350  '.                                  
105100     03 PIC X(100) VALUE                                                  
105200            'Supplier missing                '.                           
105300     03 PIC X(29)  VALUE                                                  
105400        '351                PULS-351  '.                                  
105500     03 PIC X(100) VALUE                                                  
105600            'Quantity Inspection Report exists'.                          
105700     03 PIC X(29)  VALUE                                                  
105800        '352                PULS-352  '.                                  
105900     03 PIC X(100) VALUE                                                  
106000            'Technical Inspection Report exists'.                         
106100     03 PIC X(29)  VALUE                                                  
106200        '353                PULS-353  '.                                  
106300     03 PIC X(100) VALUE                                                  
106400            'Advice quantity was split       '.                           
106500     03 PIC X(29)  VALUE                                                  
106600        '354                PULS-354  '.                                  
106700     03 PIC X(100) VALUE                                                  
106800            'Miscellaneous Cases             '.                           
106900     03 PIC X(29)  VALUE                                                  
107000        '355                PULS-355  '.                                  
107100     03 PIC X(100) VALUE                                                  
107200            'Quarantine; back to inspection area'.                        
107300     03 PIC X(29)  VALUE                                                  
107400        '356                PULS-356  '.                                  
107500     03 PIC X(100) VALUE                                                  
107600            'Inspection not done             '.                           
107700     03 PIC X(29)  VALUE                                                  
107800        '357                PULS-357  '.                                  
107900     03 PIC X(100) VALUE                                                  
108000            'Quality error                   '.                           
108100     03 PIC X(29)  VALUE                                                  
108200        '358                PULS-358  '.                                  
108300     03 PIC X(100) VALUE                                                  
108400            'Inspection report already registred'.                        
108500     03 PIC X(29)  VALUE                                                  
108600        '359                PULS-359  '.                                  
108700     03 PIC X(100) VALUE                                                  
108800            'Deviation value too high        '.                           
108900     03 PIC X(29)  VALUE                                                  
109000        '360                PULS-360  '.                                  
109100     03 PIC X(100) VALUE                                                  
109200            'Part belongs to another company '.                           
109300     03 PIC X(29)  VALUE                                                  
109400        '361                PULS-361  '.                                  
109500     03 PIC X(100) VALUE                                                  
109600            'Verification No. exceeded       '.                           
109700     03 PIC X(29)  VALUE                                                  
109800        '362                PULS-362  '.                                  
109900     03 PIC X(100) VALUE                                                  
110000            'Hazardous goods                 '.                           
110100     03 PIC X(29)  VALUE                                                  
110200        '363                PULS-363  '.                                  
110300     03 PIC X(100) VALUE                                                  
110400            'Outcome has not been tested     '.                           
110500     03 PIC X(29)  VALUE                                                  
110600        '364                PULS-364  '.                                  
110700     03 PIC X(100) VALUE                                                  
110800            'Inspection report was not created.'.                         
110900     03 PIC X(29)  VALUE                                                  
111000        '365                PULS-365  '.                                  
111100     03 PIC X(100) VALUE                                                  
111200            'Documents for goods are needed  '.                           
111300     03 PIC X(29)  VALUE                                                  
111400        '366                PULS-366  '.                                  
111500     03 PIC X(100) VALUE                                                  
111600            'Only one miscellaneous case is allowed'.                     
111700     03 PIC X(29)  VALUE                                                  
111800        '367                PULS-367  '.                                  
111900     03 PIC X(100) VALUE                                                  
112000            'Case level batch - Case No. must be specified'.              
112100     03 PIC X(29)  VALUE                                                  
112200        '368                PULS-368  '.                                  
112300     03 PIC X(100) VALUE                                                  
112400            'Not a case level batch - Case No. can not be specifie        
112500-           'd'.                                                          
112600     03 PIC X(29)  VALUE                                                  
112700        '369                PULS-369  '.                                  
112800     03 PIC X(100) VALUE                                                  
112900            'Use Inventory Part Adjustment instead'.                      
113000     03 PIC X(29)  VALUE                                                  
113100        '370                PULS-370  '.                                  
113200     03 PIC X(100) VALUE                                                  
113300            'Kit and Prio not allowed at the same time'.                  
113400     03 PIC X(29)  VALUE                                                  
113500        '371                PULS-371  '.                                  
113600     03 PIC X(100) VALUE                                                  
113700            'Surplus delivery                '.                           
113800     03 PIC X(29)  VALUE                                                  
113900        '372                PULS-372  '.                                  
114000     03 PIC X(100) VALUE                                                  
114100            'Press NEXT for next batch       '.                           
114200     03 PIC X(29)  VALUE                                                  
114300        '373                PULS-373  '.                                  
114400     03 PIC X(100) VALUE                                                  
114500            'Deviation accounting type must be 3 or 77'.                  
114600     03 PIC X(29)  VALUE                                                  
114700        '374                PULS-374  '.                                  
114800     03 PIC X(100) VALUE                                                  
114900            'Please use panel "6122" instead '.                           
115000     03 PIC X(29)  VALUE                                                  
115100        '375                PULS-375  '.                                  
115200     03 PIC X(100) VALUE                                                  
115300            'Carrier has location/address    '.                           
115400     03 PIC X(29)  VALUE                                                  
115500        '376                PULS-376  '.                                  
115600     03 PIC X(100) VALUE                                                  
115700            'Printing requested              '.                           
115800     03 PIC X(29)  VALUE                                                  
115900        '377                PULS-377  '.                                  
116000     03 PIC X(100) VALUE                                                  
116100            'Primary/special inspection not done'.                        
116200     03 PIC X(29)  VALUE                                                  
116300        '378                PULS-378  '.                                  
116400     03 PIC X(100) VALUE                                                  
116500            'Inspection report remains as adm report'.                    
116600     03 PIC X(29)  VALUE                                                  
116700        '379                PULS-379  '.                                  
116800     03 PIC X(100) VALUE                                                  
116900            'Press PRINT button to request printing'.                     
117000     03 PIC X(29)  VALUE                                                  
117100        '380                PULS-380  '.                                  
117200     03 PIC X(100) VALUE                                                  
117300            'Update not allowed/Origin already exist'.                    
117400     03 PIC X(29)  VALUE                                                  
117500        '381                PULS-381  '.                                  
117600     03 PIC X(100) VALUE                                                  
117700            'Registration completed          '.                           
117800     03 PIC X(29)  VALUE                                                  
117900        '382                PULS-382  '.                                  
118000     03 PIC X(100) VALUE                                                  
118100            'Deviations; Please use screen 6133'.                         
118200     03 PIC X(29)  VALUE                                                  
118300        '383                PULS-383  '.                                  
118400     03 PIC X(100) VALUE                                                  
118500            'Only one kind of Labels/Min. Labels'.                        
118600     03 PIC X(29)  VALUE                                                  
118700        '384                PULS-384  '.                                  
118800     03 PIC X(100) VALUE                                                  
118900            'Too many Labels                 '.                           
119000     03 PIC X(29)  VALUE                                                  
119100        '385                PULS-385  '.                                  
119200     03 PIC X(100) VALUE                                                  
119300            'Error in address handling       '.                           
119400     03 PIC X(29)  VALUE                                                  
119500        '386                PULS-386  '.                                  
119600     03 PIC X(100) VALUE                                                  
119700            'Refill part                     '.                           
119800     03 PIC X(29)  VALUE                                                  
119900        '387                PULS-387  '.                                  
120000     03 PIC X(100) VALUE                                                  
120100            'Approved quantity not same as original quantity'.            
120200     03 PIC X(29)  VALUE                                                  
120300        '388                PULS-388  '.                                  
120400     03 PIC X(100) VALUE                                                  
120500            'Core No. already exists in report.'.                         
120600     03 PIC X(29)  VALUE                                                  
120700        '389                PULS-389  '.                                  
120800     03 PIC X(100) VALUE                                                  
120900            'Report No. not registered       '.                           
121000     03 PIC X(29)  VALUE                                                  
121100        '390                PULS-390  '.                                  
121200     03 PIC X(100) VALUE                                                  
121300            '¤ has wrong status            '.                             
121400     03 PIC X(29)  VALUE                                                  
121500        '391                PULS-391  '.                                  
121600     03 PIC X(100) VALUE                                                  
121700            '¤ is to high                  '.                             
121800     03 PIC X(29)  VALUE                                                  
121900        '392                PULS-392  '.                                  
122000     03 PIC X(100) VALUE                                                  
122100            'Quantity may not be increased   '.                           
122200     03 PIC X(29)  VALUE                                                  
122300        '393                PULS-393  '.                                  
122400     03 PIC X(100) VALUE                                                  
122500            'Handled quantity may not be less than original quanti        
122600-           'ty'.                                                         
122700     03 PIC X(29)  VALUE                                                  
122800        '394                PULS-394  '.                                  
122900     03 PIC X(100) VALUE                                                  
123000            'Warning, old VIPS user          '.                           
123100     03 PIC X(29)  VALUE                                                  
123200        '395                PULS-395  '.                                  
123300     03 PIC X(100) VALUE                                                  
123400            'Database unavailable            '.                           
123500     03 PIC X(29)  VALUE                                                  
123600        '396                PULS-396  '.                                  
123700     03 PIC X(100) VALUE                                                  
123800            'Try later or notify systems support'.                        
123900     03 PIC X(29)  VALUE                                                  
124000        '397                PULS-397  '.                                  
124100     03 PIC X(100) VALUE                                                  
124200            'Check missing parts             '.                           
124300     03 PIC X(29)  VALUE                                                  
124400        '398                PULS-398  '.                                  
124500     03 PIC X(100) VALUE                                                  
124600            'Report not finished             '.                           
124700     03 PIC X(29)  VALUE                                                  
124800        '399                PULS-399  '.                                  
124900     03 PIC X(100) VALUE                                                  
125000            'Empty case without lines        '.                           
125100     03 PIC X(29)  VALUE                                                  
125200        '400                PULS-400  '.                                  
125300     03 PIC X(100) VALUE                                                  
125400            'Case invoiced or released for loading'.                      
125500     03 PIC X(29)  VALUE                                                  
125600        '401                PULS-401  '.                                  
125700     03 PIC X(100) VALUE                                                  
125800            'Invalid combination of data entered'.                        
125900     03 PIC X(29)  VALUE                                                  
126000        '402                PULS-402  '.                                  
126100     03 PIC X(100) VALUE                                                  
126200            'Order content or status has changed - please restart'        
126300            .                                                             
126400     03 PIC X(29)  VALUE                                                  
126500        '403                PULS-403  '.                                  
126600     03 PIC X(100) VALUE                                                  
126700            'Update done, note down place: ¤.'.                           
126800     03 PIC X(29)  VALUE                                                  
126900        '404                PULS-404  '.                                  
127000     03 PIC X(100) VALUE                                                  
127100            'Reporting in progress           '.                           
127200     03 PIC X(29)  VALUE                                                  
127300        '405                PULS-405  '.                                  
127400     03 PIC X(100) VALUE                                                  
127500            'New case created                '.                           
127600     03 PIC X(29)  VALUE                                                  
127700        '406                PULS-406  '.                                  
127800     03 PIC X(100) VALUE                                                  
127900            'Invoice without any parts       '.                           
128000     03 PIC X(29)  VALUE                                                  
128100        '407                PULS-407  '.                                  
128200     03 PIC X(100) VALUE                                                  
128300            '¤ has been updated            '.                             
128400     03 PIC X(29)  VALUE                                                  
128500        '408                PULS-408  '.                                  
128600     03 PIC X(100) VALUE                                                  
128700            'Deleting all cases is not allowed. '.                        
128800     03 PIC X(29)  VALUE                                                  
128900        '409                PULS-409  '.                                  
129000     03 PIC X(100) VALUE                                                  
129100            'Case deleted. Press "Search" for last case '.                
129200     03 PIC X(29)  VALUE                                                  
129300        '410                PULS-410  '.                                  
129400     03 PIC X(100) VALUE                                                  
129500            'Part removed from the case      '.                           
129600     03 PIC X(29)  VALUE                                                  
129700        '411                PULS-411  '.                                  
129800     03 PIC X(100) VALUE                                                  
129900            'Part added to the case          '.                           
130000     03 PIC X(29)  VALUE                                                  
130100        '412                PULS-412  '.                                  
130200     03 PIC X(100) VALUE                                                  
130300            'Proforma printing initiated     '.                           
130400     03 PIC X(29)  VALUE                                                  
130500        '413                PULS-413  '.                                  
130600     03 PIC X(100) VALUE                                                  
130700            'The part is scrapped            '.                           
130800     03 PIC X(29)  VALUE                                                  
130900        '414                PULS-414  '.                                  
131000     03 PIC X(100) VALUE                                                  
131100            'Basic stock is too low          '.                           
131200     03 PIC X(29)  VALUE                                                  
131300        '415                PULS-415  '.                                  
131400     03 PIC X(100) VALUE                                                  
131500            'OK, Weight may have been adjusted during Lbs/Kg conve        
131600-           'rsion '.                                                     
131700     03 PIC X(29)  VALUE                                                  
131800        '416                PULS-416  '.                                  
131900     03 PIC X(100) VALUE                                                  
132000            'Record has been changed by another user, please do a         
132100-           'new Search before update        '.                           
132200     03 PIC X(29)  VALUE                                                  
132300        '417                PULS-417  '.                                  
132400     03 PIC X(100) VALUE                                                  
132500            'Average cost missing            '.                           
132600     03 PIC X(29)  VALUE                                                  
132700        '418                PULS-418  '.                                  
132800     03 PIC X(100) VALUE                                                  
132900            'Wrong picker                    '.                           
133000     03 PIC X(29)  VALUE                                                  
133100        '419                PULS-419  '.                                  
133200     03 PIC X(100) VALUE                                                  
133300            'Deviation in progress           '.                           
133400     03 PIC X(29)  VALUE                                                  
133500        '420                PULS-420  '.                                  
133600     03 PIC X(100) VALUE                                                  
133700            'Line not belong to picker       '.                           
133800     03 PIC X(29)  VALUE                                                  
133900        '421                PULS-421  '.                                  
134000     03 PIC X(100) VALUE                                                  
134100            'Invalid data                    '.                           
134200     03 PIC X(29)  VALUE                                                  
134300        '422                PULS-422  '.                                  
134400     03 PIC X(100) VALUE                                                  
134500            'Tracking-Id missing             '.                           
134600     03 PIC X(29)  VALUE                                                  
134700        '423                PULS-423  '.                                  
134800     03 PIC X(100) VALUE                                                  
134900            'Max qty on defined decl. id ¤ '.                             
135000     03 PIC X(29)  VALUE                                                  
135100        '424                PULS-424  '.                                  
135200     03 PIC X(100) VALUE                                                  
135300            'Stock balance not reduced more than ¤ '.                     
135000     03 PIC X(29)  VALUE                                                  
135100        '427                PULS-427  '.                                  
135200     03 PIC X(100) VALUE                                                  
135300            'Received at wrong DC send it to ¤ '.                         
135000     03 PIC X(29)  VALUE                                                  
135100        '428                PULS-428  '.                                  
135200     03 PIC X(100) VALUE                                                  
135300            'Update done next DC cross is ¤ '.                            
135000     03 PIC X(29)  VALUE                                                  
135100        '429                PULS-429  '.                                  
135200     03 PIC X(100) VALUE                                                  
135300            'Update done next trp num is ¤ '.                             
135400     03 PIC X(29)  VALUE                                                  
135500        '706                PULS-706  '.                                  
135600     03 PIC X(100) VALUE                                                  
135700            'Location missing                '.                           
135710     03 PIC X(29)  VALUE                                                  
135720        '920                PULS-920  '.                                  
135730     03 PIC X(100) VALUE                                                  
135740            '¤ is invalid               '.                                
135800 01  TAB1 REDEFINES MSG-TAB1.                                             
135900     03  TAB1-MSG         OCCURS 343 TIMES                                
136000                          ASCENDING TAB1-IDMSG-WEB                        
136100                          INDEXED BY TAB1-IX.                             
136200         05  TAB1-IDMSG-WEB      PIC X(3).                                
136300         05  TAB1-IDELMT         PIC X(16).                               
136400         05  TAB1-IDMSG          PIC X(10).                               
136500         05  TAB1-MESSAGE        PIC X(100).                              
136600                                                                          
