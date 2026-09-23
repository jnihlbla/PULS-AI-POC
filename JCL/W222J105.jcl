//W222J105 JOB (670W2220100W222J105,W100),'RTN W200V1',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST0                                                     
//     INCLUDE MEMBER=SYST2                                                     
//     INCLUDE MEMBER=SYSTÖ                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//W222     EXEC W222P005,                                                       
//             INDUT=W222.W200V1.W222052(+1)                                    
//*                                                                             
//* EXTRACT FILE - INCLUDE PARTS WHEN KVPB-SEP = 0 or > 0                       
EXTRFIL2                                                                        
//*                                                                             
//W22205.W22205D3 DD DUMMY                                                      
//* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
//COPY     EXEC PGM=V16459,PARM='ICEGENER/3/'                                   
//SYSPRINT DD  SYSOUT=*                                                         
//SYSIN    DD  DUMMY                                                            
//SYSUT1   DD  DSN=W222.W200V1.W222052(+1),DISP=SHR                             
//************                                                                  
//*                                                                             
//SYSUT2   DD  DSN=WXTR.PARTINFO.WREQWK2(+1),                  -LIM(6)          
//             DISP=(NEW,CATLG,DELETE),                                         
//************ KOPIA AV W222052/WXTR.PARTINFO.WREQWK2,                          
//             MGMTCLAS=NOBACKUP,DATACLAS=PSEB                                  
//*                                                                             
//* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
//SOPEND  EXEC WSOPEND,PROCESS=W222J105                                         
