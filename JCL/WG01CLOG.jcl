//WG01CLOG JOB (540VV401100WG01CLOG),'RTN WWBACK',                              
//             TIME=1440,NOTIFY=PC30174,                                        
//             CLASS=L                                                          
/*JOBPARM ROOM=PVV2,LINES=9,CARDS=0,FORMS=1800,LINECT=00                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//MSGOUT OUTPUT JESDS=ALL,DEFAULT=YES,                                          
//  ROOM=PVV2,DEPT='9231',ADDRESS=('VOLVO CAR PARTS',                           
//  'TORSLANDA','405 08 GOTHENBURG')                                            
//*                                                                             
//CLOSE   EXEC PGM=DFSULTR0                                                     
//STEPLIB  DD  DSN=&INDUSRL..USERLIB,DISP=SHR                                   
//SYSPRINT DD  SYSOUT=*                                                         
//IEFRDER  DD  DSN=&INDPLOG..BACKOUT(+0),DISP=SHR                               
//NEWRDER  DD  DSN=&INDPLOG..BACKOUT(+1),                                       
//             DCB=(RECFM=VB,LRECL=6229,BLKSIZE=27998,BUFNO=5),                 
//             DISP=(NEW,CATLG,DELETE),                                         
//             SPACE=(CYL,(900,700),RLSE),                                      
//             MGMTCLAS=EMID70                                                  
//SYSIN    DD  *                                                                
DUP ERRC=00000                                                                  
/*                                                                              
//*                                                                             
